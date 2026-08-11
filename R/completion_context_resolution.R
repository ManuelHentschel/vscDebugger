# Resolves completion ASTs while guarding promises, active bindings, and namespaces.
# Read a binding while guarding promises and active bindings.
.completion_read_binding <- function(
    name,
    environment,
    is_namespace = FALSE
) {
    # Active bindings might have side-effects.
    if (bindingIsActive(name, environment)) {
        if (!isTRUE(getOption("vsc.evaluateActiveBindings", FALSE))) {
            stop("Refusing to evaluate active binding: ", name)
        }
        return(get(name, envir = environment, inherits = FALSE))
    }

    # Preview promises by evaluating their code without forcing the binding.
    force_promises <- is_namespace && isTRUE(getOption(
        "vsc.completionsForceNamespacePromises",
        TRUE
    ))
    if (!force_promises && isPromise(name, environment)) {
        if (!isTRUE(getOption("vsc.previewPromises", FALSE))) {
            stop("Refusing to evaluate promise: ", name)
        }
        promise <- getPromiseInfo(name, environment)
        return(eval(promise$code, promise$environment))
    }

    # Fall back to normal lookup
    get(name, envir = environment, inherits = FALSE)
}

# Look up a binding in lexical environments, then the attached search path.
.completion_lookup_binding <- function(
    name,
    environments = list(),
    namespace_environments = list(),
    only_functions = FALSE
) {
    # Namespace environments may opt into forcing lazy-loaded promises.
    is_namespace <- c(
        rep(FALSE, length(environments)),
        rep(TRUE, length(namespace_environments))
    )
    environments <- c(environments, namespace_environments)

    for (i in seq_along(environments)) {
        environment <- environments[[i]]
        if (exists(name, envir = environment, inherits = FALSE)) {
            value <- .completion_read_binding(
                name,
                environment,
                is_namespace[[i]]
            )
            if (!only_functions || is.function(value)) {
                return(value)
            }
        }
    }

    if (only_functions) {
        return(NULL)
    }
    stop("Could not find binding: ", name)
}

# Resolve a bare or quoted package name, loading it when completion allows it.
.completion_resolve_namespace <- function(package) {
    if (is.name(package)) {
        package <- as.character(package)
    }
    if (
        !is.character(package) ||
        length(package) != 1L ||
        is.na(package) ||
        !nzchar(package)
    ) {
        stop("The namespace context is not a package name")
    }
    if (
        !isNamespaceLoaded(package)
        && !isTRUE(getOption("vsc.completionsLoadNamespaces", TRUE))
    ) {
        stop("Namespace is not loaded: ", package)
    }
    getNamespace(package)
}

.completion_resolve_ast_node <- function(
    node,
    environments,
    namespace_environments = list()
) {
    # Return missing arguments as such (e.g. my_matrix[,1])
    if (is.name(node) && as.character(node) == "") {
        return(node)
    }

    # Return atomic nodes (numbers, logicals, etc.) as is
    if (is.null(node) || is.atomic(node)) {
        return(node)
    }

    # If the node is a (variable) name, look it up and return
    if (is.name(node)) {
        name <- as.character(node)
        return(.completion_lookup_binding(
            name,
            environments,
            namespace_environments
        ))
    }

    # Remaining cases are calls which are handled recursively
    operator <- as.character(node[[1L]])

    # Resolve namespace accessors without loading unless completion allows it.
    if (operator %in% c("::", ":::")) {
        package <- node[[2L]]
        namespace <- .completion_resolve_namespace(package)
        child <- node[[3L]]
        child_environment <- namespace

        # `::` also exposes lazy-loaded datasets outside namespace exports.
        if (operator == "::" && !(child %in% getNamespaceExports(namespace))) {
            lazy_data <- getNamespaceInfo(namespace, "lazydata")
            if (!exists(child, envir = lazy_data, inherits = FALSE)) {
                stop(child, " is not exported by ", package)
            }
            child_environment <- lazy_data
        }

        return(.completion_lookup_binding(
            child,
            namespace_environments = list(child_environment)
        ))
    }

    # Resolve operator arguments recursively
    arguments <- lapply(
        as.list(node)[-1L],
        .completion_resolve_ast_node,
        environments = environments,
        namespace_environments = namespace_environments
    )

    # Guard promises and active environment children before normal dispatch.
    parent <- arguments[[1L]]
    if (
        operator %in% c("$", "[[") &&
        is.environment(parent) &&
        length(arguments) == 2L &&
        !identical(unname(arguments[2L]), unname(alist(value = )))
    ) {
        child <- arguments[[2L]]
        if (
            is.character(child) &&
            length(child) == 1L &&
            exists(child, envir = parent, inherits = FALSE) &&
            (bindingIsActive(child, parent) || isPromise(child, parent))
        ) {
            # Operation accesses a promise/active binding -> use safe binding lookup
            return(.completion_lookup_binding(child, list(parent)))
        }
    }

    # Use normal dispatch for all other cases
    # Might dispatch overwritten `[` methods, but that's on the user if there's side-effects
    accessor_function <- .completion_lookup_binding(
        operator,
        environments,
        namespace_environments,
        only_functions = TRUE
    )
    if (is.null(accessor_function)) {
        accessor_function <- get(operator, envir = baseenv(), inherits = FALSE)
    }
    do.call(accessor_function, arguments, envir = environments[[1L]])
}


# Utility function to report context resolution results
.completion_resolution_result <- function(
    status,
    value = NULL,
    reason = NULL
) {
    list(
        status = status,
        value = value,
        reason = reason
    )
}

# Error catching wrapper for single namespace resolution
.completion_resolve_namespace_context <- function(package) {
    tryCatch(
        .completion_resolution_result(
            "success",
            value = .completion_resolve_namespace(package)
        ),
        error = function(error) {
            .completion_resolution_result(
                "infeasible",
                reason = conditionMessage(error)
            )
        }
    )
}

# Error-catching wrapper for normal node resolution
.completion_resolve_ast <- function(ast, firstenv, lastenv) {
    # Search attached environments after the selected frame's lexical scope.
    environments <- getScopeEnvs(firstenv, lastenv)
    namespace_environments <- lapply(
        setdiff(search(), ".GlobalEnv"),
        as.environment
    )
    tryCatch(
        .completion_resolution_result(
            "success",
            value = .completion_resolve_ast_node(
                ast,
                environments,
                namespace_environments
            )
        ),
        error = function(error) {
            .completion_resolution_result(
                "infeasible",
                reason = conditionMessage(error)
            )
        }
    )
}

resolve_completion_context <- function(
    ast,
    accessor = NULL,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv
) {
    if (!is.null(accessor) && accessor %in% c("::", ":::")) {
        return(.completion_resolve_namespace_context(ast))
    }
    .completion_resolve_ast(ast, firstenv, lastenv)
}
