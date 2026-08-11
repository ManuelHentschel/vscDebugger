# Resolves completion ASTs while guarding promises, active bindings, and namespaces.
# Find a binding with the given name.
# Returns the (first) environment containing the binding, or NULL
.completion_find_binding <- function(name, environments) {
    for (environment in environments) {
        if (exists(name, envir = environment, inherits = FALSE)) {
            return(environment)
        }
    }
    NULL
}

# Look up a binding by name in the given environments.
# Avoids promise forcing and active bindings according to package settings
.completion_lookup_binding <- function(
    name,
    environments,
    force_promises = FALSE
) {
    environment <- .completion_find_binding(name, environments)
    if (is.null(environment)) {
        stop("Could not find binding: ", name)
    }

    # Active bindings might have side-effects.
    if (bindingIsActive(name, environment)) {
        if (!isTRUE(getOption("vsc.evaluateActiveBindings", FALSE))) {
            stop("Refusing to evaluate active binding: ", name)
        }
        return(get(name, envir = environment, inherits = FALSE))
    }

    # Preview promises by evaluating their code without forcing the binding.
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

.completion_find_function <- function(name, environments) {
    for (environment in environments) {
        if (!exists(name, envir = environment, inherits = FALSE)) {
            next
        }
        value <- .completion_lookup_binding(name, list(environment))
        if (is.function(value)) {
            return(value)
        }
    }
    NULL
}

.completion_resolution_result <- function(
    status,
    value = NULL,
    reason = NULL
) {
    result <- list(status = status)
    if (status == "success") {
        result["value"] <- list(value)
    }
    if (!is.null(reason)) {
        result$reason <- reason
    }
    result
}

.completion_resolve_ast_node <- function(
    node,
    environments
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
            environments
        ))
    }

    # Remaining cases are calls which are handled recursively
    operator <- as.character(node[[1L]])

    # Resolve namespace accessors without loading unless completion allows it.
    if (operator %in% c("::", ":::")) {
        package <- node[[2L]]
        namespace_result <- .completion_resolve_namespace(package)
        if (namespace_result$status != "success") {
            stop(namespace_result$reason)
        }
        namespace <- namespace_result$value
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

        force_promises <- isTRUE(getOption("vsc.completionsForceNamespacePromises", TRUE))

        return(.completion_lookup_binding(
            child,
            list(child_environment),
            force_promises = force_promises
        ))
    }

    # Resolve operator arguments recursively
    arguments <- lapply(
        as.list(node)[-1L],
        .completion_resolve_ast_node,
        environments = environments
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
    accessor_function <- .completion_find_function(operator, environments)
    if (is.null(accessor_function)) {
        accessor_function <- get(operator, envir = baseenv(), inherits = FALSE)
    }
    do.call(accessor_function, arguments, envir = environments[[1L]])
}

.completion_resolve_ast <- function(ast, firstenv, lastenv) {
    environments <- getScopeEnvs(firstenv, lastenv)
    tryCatch(
        .completion_resolution_result(
            "success",
            value = .completion_resolve_ast_node(ast, environments)
        ),
        error = function(error) {
            .completion_resolution_result(
                "infeasible",
                reason = conditionMessage(error)
            )
        }
    )
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
        return(.completion_resolution_result(
            "infeasible",
            reason = "The namespace context is not a package name"
        ))
    }
    if (!isNamespaceLoaded(package)) {
        if (!isTRUE(getOption("vsc.completionsLoadNamespaces", TRUE))) {
            return(.completion_resolution_result(
                "infeasible",
                reason = paste0("Namespace is not loaded: ", package)
            ))
        }
        namespace <- try(getNamespace(package), silent = TRUE)
        if (inherits(namespace, "try-error")) {
            return(.completion_resolution_result(
                "infeasible",
                reason = paste0("Could not load namespace: ", package)
            ))
        }
    } else {
        namespace <- getNamespace(package)
    }
    .completion_resolution_result(
        "success",
        value = namespace
    )
}

resolve_completion_context <- function(
    ast,
    accessor = NULL,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv
) {
    if (!is.null(accessor) && accessor %in% c("::", ":::")) {
        return(.completion_resolve_namespace(ast))
    }
    .completion_resolve_ast(ast, firstenv, lastenv)
}
