# Resolves validated completion ASTs without forcing promises or loading namespaces.
# Find a variable/function with given name
# Returns the (first) environment containing the binding, or NULL
completion_find_binding <- function(name, environments) {
    for (environment in environments) {
        if (exists(name, envir = environment, inherits = FALSE)) {
            return(environment)
        }
    }
    NULL
}

# Look up the value of a variable/function by name in the given environments
# Avoids promise forcing and active bindings, depending on config
completion_lookup_binding <- function(
    name,
    environments
) {
    environment <- completion_find_binding(name, environments)
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

# Resolve the arguments of a function call AST node
# Basically lapply with recursion, but skips missing arguments
completion_namespace <- function(package) {
    if (!isNamespaceLoaded(package)) {
        stop("Namespace is not loaded: ", package)
    }
    getNamespace(package)
}

completion_resolve_ast_node <- function(
    node,
    environments
) {
    # Return missing argument nodes as such,
    # constructing a new `missing arg` representation to avoid errors
    if (missing(node)) {
        return(quote(expr = ))
    }
    if (is.name(node) && as.character(node) == "") {
        return(quote(expr = ))
    }

    # Return atomic nodes (numbers, logicals, etc.) as such
    if (is.null(node) || is.atomic(node)) {
        return(node)
    }

    # If the node is a (variable) name, look it up and return
    if(is.name(node)) {
        name <- as.character(node)
        return(completion_lookup_binding(
            name,
            environments
        ))
    }

    operator <- as.character(node[[1L]])

    # Handle namespace accessors
    if (operator %in% c("::", ":::")) {
        package <- node[[2L]]
        child <- node[[3L]]
        namespace <- completion_namespace(package)
        # Check namespace exports for `::`
        if (
            operator == "::" &&
            !(child %in% getNamespaceExports(namespace))
        ) {
            stop(child, " is not exported by ", package)
        }

        return(completion_lookup_binding(
            child,
            list(namespace)
        ))
    }

    # Resolve operator arguments recursively
    arguments <- lapply(
        as.list(node)[-1L],
        completion_resolve_ast_node,
        environments = environments
    )

    # Avoid promise forcing for `$` and `[[` into environments
    parent <- arguments[[1L]]
    if (
        operator %in% c("$", "[[") &&
        is.environment(parent) &&
        length(arguments) == 2L &&
        is.character(arguments[[2L]]) &&
        length(arguments[[2L]]) == 1L &&
        isPromise(arguments[[2L]], parent)
    ) {
        return(completion_lookup_binding(arguments[[2L]], list(parent)))
    }

    # Use normal dispatch for all other cases
    # Might dispatch overwritten `[` methods, but that's on the user
    operator_environment <- completion_find_binding(operator, environments)
    accessor <- if (is.null(operator_environment)) {
        operator
    } else {
        completion_lookup_binding(operator, list(operator_environment))
    }
    do.call(accessor, arguments, envir = environments[[1L]])
}

resolve_completion_ast <- function(
    ast,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv
) {
    environments <- getScopeEnvs(firstenv, lastenv)

    tryCatch({
        list(
            ok = TRUE,
            value = completion_resolve_ast_node(
                ast,
                environments
            ),
            reason = NULL,
            message = NULL
        )
    }, error = function(error) {
        list(
            ok = FALSE,
            value = NULL,
            reason = "resolution_error",
            message = conditionMessage(error)
        )
    })
}
