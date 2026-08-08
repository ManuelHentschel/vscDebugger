# Runs the completion pipeline and returns matching candidates.
completion_main <- function(
    text,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv,
    global_lastenv = emptyenv()
) {
    # Select the feasible expression suffix before the cursor.
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

    # Split the suffix into its context, accessor, and partial name.
    split <- split_completion_context(
        backward$text,
        forward$regions,
        backward$start - 1L
    )
    # Remove leading whitespace and an opening quote from the partial name.
    partial_name <- sub(
        "^[ \t\f\v]*['\"`]?",
        "",
        split$partial_child,
        perl = TRUE
    )

    ast <- NULL
    if (!is.null(split$accessor)) {
        # Parse and validate the context to the left of the final accessor.
        parsed <- parse_completion_context(split$context)
        if (!parsed$ok) {
            return(c(list(split = split, matches = character()), parsed))
        }
        ast <- parsed$ast
    }

    # Resolve the context and generate matching child names.
    tryCatch({
        if (
            is.null(split$accessor) &&
            forward$state %in% c(LS_SINGLE_QUOTED, LS_DOUBLE_QUOTED)
        ) {
            context <- list()
        } else if (is.null(split$accessor)) {
            context <- getScopeEnvs(firstenv, global_lastenv)
        } else if (split$accessor %in% c("::", ":::")) {
            context <- completion_namespace(as.character(ast))
        } else {
            context <- resolve_completion_ast(
                ast,
                firstenv = firstenv,
                lastenv = lastenv
            )
        }

        list(
            ok = TRUE,
            split = split,
            ast = ast,
            matches = completion_candidates(
                context,
                split$accessor,
                partial_name
            ),
            reason = NULL,
            message = NULL
        )
    }, error = function(error) {
        list(
            ok = FALSE,
            split = split,
            ast = ast,
            matches = character(),
            reason = "resolution_error",
            message = conditionMessage(error)
        )
    })
}
