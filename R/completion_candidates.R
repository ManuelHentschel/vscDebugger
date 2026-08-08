# Generates completion candidates from lexed, parsed, and resolved contexts.
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
    package <- as.character(ast)
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
    lastenv = .GlobalEnv
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
        backward$text,
        forward$regions,
        backward$start - 1L
    )
    partial_child <- sub(
        "^[ \t\f\v]*['\"`]?",
        "",
        split$partial_child,
        perl = TRUE
    )

    if (is.null(split$accessor)) {
        environments <- getScopeEnvs(firstenv, lastenv)
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
                lastenv = lastenv
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
