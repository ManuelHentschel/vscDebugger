# Resolves validated completion ASTs without evaluating arbitrary R code.
completion_scope_environments <- function(
    firstenv = parent.frame(),
    lastenv = .GlobalEnv
) {
    stopifnot(is.environment(firstenv), is.environment(lastenv))

    environments <- list()
    environment <- firstenv
    repeat {
        environments[[length(environments) + 1L]] <- environment
        if (
            identical(environment, lastenv) ||
            identical(environment, emptyenv())
        ) {
            break
        }
        environment <- parent.env(environment)
    }
    environments
}

completion_find_binding <- function(name, environments) {
    for (environment in environments) {
        if (exists(name, envir = environment, inherits = FALSE)) {
            return(environment)
        }
    }
    NULL
}

completion_lookup_binding <- function(
    name,
    environments,
    config,
    promise_depth
) {
    environment <- completion_find_binding(name, environments)
    if (is.null(environment)) {
        stop("Could not find binding: ", name)
    }

    # Active bindings can execute arbitrary code when read.
    if (bindingIsActive(name, environment)) {
        if (!config$evaluate_active_bindings) {
            stop("Refusing to evaluate active binding: ", name)
        }
        return(get(name, envir = environment, inherits = FALSE))
    }

    # Preview promises by recursively resolving their stored code.
    if (config$is_promise(name, environment)) {
        if (!config$preview_promises) {
            stop("Refusing to force promise: ", name)
        }
        if (promise_depth >= config$max_promise_depth) {
            stop("Maximum promise preview depth exceeded")
        }

        promise <- config$promise_info(name, environment)
        promise_ast <- completion_normalize_ast(promise$code)
        promise_environments <- completion_scope_environments(
            promise$environment,
            config$lastenv
        )
        return(completion_resolve_ast_node(
            promise_ast,
            promise_environments,
            config,
            promise_depth + 1L
        ))
    }

    get(name, envir = environment, inherits = FALSE)
}

completion_resolve_index <- function(
    node,
    environments,
    config,
    promise_depth
) {
    if (identical(node, quote(expr = ))) {
        return(TRUE)
    }
    value <- completion_resolve_ast_node(
        node,
        environments,
        config,
        promise_depth
    )
    if (
        !is.null(value) &&
        !(is.logical(value) || is.numeric(value) || is.character(value))
    ) {
        stop("Indices must resolve to logical, numeric, or character values")
    }
    value
}

completion_safe_subset <- function(parent, indices, double = FALSE) {
    subset <- if (double) base::.subset2 else base::.subset
    arguments <- c(list(parent), indices)
    if (double) {
        arguments$exact <- TRUE
    }
    tryCatch(
        do.call(subset, arguments),
        error = function(error) {
            stop(conditionMessage(error))
        }
    )
}

completion_safe_named_child <- function(
    parent,
    name,
    environments,
    config,
    promise_depth
) {
    if (is.environment(parent)) {
        return(completion_lookup_binding(
            name,
            list(parent),
            config,
            promise_depth
        ))
    }
    if (is.list(parent) || is.pairlist(parent)) {
        return(completion_safe_subset(parent, list(name), TRUE))
    }
    stop("$ access is supported only for environments and list-like objects")
}

completion_namespace <- function(package) {
    namespace <- base:::.getNamespace(package)
    if (is.null(namespace)) {
        stop("Namespace is not loaded: ", package)
    }
    namespace
}

completion_resolve_ast_node <- function(
    node,
    environments,
    config,
    promise_depth = 0L
) {
    if (identical(node, quote(expr = ))) {
        stop("A missing expression cannot be resolved as a value")
    }
    if (is.name(node)) {
        return(completion_lookup_binding(
            as.character(node),
            environments,
            config,
            promise_depth
        ))
    }
    if (is.null(node) || is.atomic(node)) {
        return(node)
    }

    operator <- as.character(node[[1L]])
    if (operator %in% c("::", ":::")) {
        package <- node[[2L]]
        child <- node[[3L]]
        namespace <- completion_namespace(package)
        if (
            operator == "::" &&
            !(child %in% getNamespaceExports(namespace))
        ) {
            stop(child, " is not exported by ", package)
        }
        return(completion_lookup_binding(
            child,
            list(namespace),
            config,
            promise_depth
        ))
    }

    parent <- completion_resolve_ast_node(
        node[[2L]],
        environments,
        config,
        promise_depth
    )
    if (operator == "$") {
        return(completion_safe_named_child(
            parent,
            node[[3L]],
            environments,
            config,
            promise_depth
        ))
    }
    if (operator == "@") {
        if (!isS4(parent) || !(node[[3L]] %in% methods::slotNames(parent))) {
            stop("Could not find S4 slot: ", node[[3L]])
        }
        return(methods::slot(parent, node[[3L]]))
    }

    indices <- lapply(
        as.list(node)[-seq_len(2L)],
        completion_resolve_index,
        environments = environments,
        config = config,
        promise_depth = promise_depth
    )
    if (operator == "[[" && is.environment(parent)) {
        if (
            length(indices) != 1L ||
            !is.character(indices[[1L]]) ||
            length(indices[[1L]]) != 1L
        ) {
            stop("Environment [[ access requires one character name")
        }
        return(completion_lookup_binding(
            indices[[1L]],
            list(parent),
            config,
            promise_depth
        ))
    }
    if (is.environment(parent)) {
        stop("[ access is not supported for environments")
    }
    completion_safe_subset(parent, indices, operator == "[[")
}

resolve_completion_ast <- function(
    ast,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv,
    preview_promises = getOption("vsc.previewPromises", FALSE),
    evaluate_active_bindings = getOption(
        "vsc.evaluateActiveBindings",
        FALSE
    ),
    max_promise_depth = 20L,
    is_promise = isPromise,
    promise_info = getPromiseInfo
) {
    environments <- completion_scope_environments(firstenv, lastenv)
    config <- list(
        lastenv = lastenv,
        preview_promises = isTRUE(preview_promises),
        evaluate_active_bindings = isTRUE(evaluate_active_bindings),
        max_promise_depth = max_promise_depth,
        is_promise = is_promise,
        promise_info = promise_info
    )

    tryCatch({
        list(
            ok = TRUE,
            value = completion_resolve_ast_node(
                ast,
                environments,
                config
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

completion_object_child_names <- function(object, accessor) {
    if (accessor == "@") {
        if (!isS4(object)) {
            stop("@ completion requires an S4 object")
        }
        return(methods::slotNames(object))
    }
    if (is.environment(object)) {
        if (accessor == "[") {
            stop("[ completion is not supported for environments")
        }
        return(ls(object, all.names = TRUE, sorted = FALSE))
    }
    if (accessor == "$" && !(is.list(object) || is.pairlist(object))) {
        stop("$ completion requires a list-like object")
    }

    children <- attr(object, "names", exact = TRUE)
    if (is.null(children) && accessor %in% c("[", "[[")) {
        children <- unlist(
            attr(object, "dimnames", exact = TRUE),
            use.names = FALSE
        )
    }
    if (is.null(children)) character() else children
}

completion_global_names <- function(environments) {
    unique(unlist(lapply(
        environments,
        ls,
        all.names = TRUE,
        sorted = FALSE
    ), use.names = FALSE))
}

completion_namespace_names <- function(ast, accessor) {
    package <- if (is.name(ast)) {
        as.character(ast)
    } else if (is.character(ast) && length(ast) == 1L) {
        ast
    } else {
        stop("Namespace completion requires a package name")
    }
    namespace <- completion_namespace(package)
    if (accessor == "::") {
        getNamespaceExports(namespace)
    } else {
        ls(namespace, all.names = TRUE, sorted = FALSE)
    }
}

completion_context_candidates <- function(
    text,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv,
    preview_promises = getOption("vsc.previewPromises", FALSE),
    evaluate_active_bindings = getOption(
        "vsc.evaluateActiveBindings",
        FALSE
    ),
    is_promise = isPromise,
    promise_info = getPromiseInfo
) {
    forward <- lex_forward(text)
    backward <- lex_backward(text, forward)
    if (!backward$feasible) {
        return(list(
            ok = FALSE,
            split = NULL,
            ast = NULL,
            matches = character(),
            reason = "infeasible_context",
            message = NULL
        ))
    }
    split <- split_completion_context(
        text,
        forward,
        backward$start,
        backward$end
    )
    environments <- completion_scope_environments(firstenv, lastenv)
    partial_child <- sub(
        "^[ \t\f\v]*['\"`]?",
        "",
        split$partial_child,
        perl = TRUE
    )

    if (is.null(split$accessor)) {
        children <- completion_global_names(environments)
        return(list(
            ok = TRUE,
            split = split,
            ast = NULL,
            matches = children[startsWith(children, partial_child)],
            reason = NULL,
            message = NULL
        ))
    }

    parsed <- parse_completion_context(split$context)
    if (!parsed$ok) {
        return(c(list(split = split, matches = character()), parsed))
    }

    result <- tryCatch({
        if (split$accessor %in% c("::", ":::")) {
            children <- completion_namespace_names(
                parsed$ast,
                split$accessor
            )
        } else {
            resolved <- resolve_completion_ast(
                parsed$ast,
                firstenv = firstenv,
                lastenv = lastenv,
                preview_promises = preview_promises,
                evaluate_active_bindings = evaluate_active_bindings,
                is_promise = is_promise,
                promise_info = promise_info
            )
            if (!resolved$ok) {
                return(list(
                    ok = FALSE,
                    split = split,
                    ast = parsed$ast,
                    matches = character(),
                    reason = resolved$reason,
                    message = resolved$message
                ))
            }
            children <- completion_object_child_names(
                resolved$value,
                split$accessor
            )
        }

        children <- children[!is.na(children)]
        list(
            ok = TRUE,
            split = split,
            ast = parsed$ast,
            matches = children[startsWith(
                children,
                partial_child
            )],
            reason = NULL,
            message = NULL
        )
    }, error = function(error) {
        list(
            ok = FALSE,
            split = split,
            ast = parsed$ast,
            matches = character(),
            reason = "resolution_error",
            message = conditionMessage(error)
        )
    })
    result
}
