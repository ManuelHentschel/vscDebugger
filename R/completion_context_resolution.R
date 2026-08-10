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
    environments
) {
    environment <- .completion_find_binding(name, environments)
    if (is.null(environment)) {
        stop("Could not find binding: ", name)
    }

    # Active bindings might have side-effects
    if (
        bindingIsActive(name, environment) &&
        !isTRUE(getOption("vsc.evaluateActiveBindings", FALSE))
    ) {
        stop("Refusing to evaluate active binding: ", name)
    }

    # Preview promises by evaluating their code without forcing the binding.
    if (isPromise(name, environment)) {
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

# Return a namespace only if it is already loaded.
.completion_namespace <- function(package) {
    if (!isNamespaceLoaded(package)) {
        stop("Namespace is not loaded: ", package)
    }
    getNamespace(package)
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

    # Handle namespace accessors
    if (operator %in% c("::", ":::")) {
        package <- node[[2L]]
        child <- node[[3L]]
        namespace <- .completion_namespace(package)
        # Check namespace exports for `::`
        if (
            operator == "::" &&
            !(child %in% getNamespaceExports(namespace))
        ) {
            stop(child, " is not exported by ", package)
        }

        return(.completion_lookup_binding(
            child,
            list(namespace)
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

resolve_completion_context <- function(
    ast,
    accessor = NULL,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv
) {
    if (!is.null(accessor) && accessor %in% c("::", ":::")) {
        return(.completion_namespace(as.character(ast)))
    }
    environments <- getScopeEnvs(firstenv, lastenv)
    .completion_resolve_ast_node(ast, environments)
}
