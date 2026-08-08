# Runs the completion pipeline and returns matching candidates.
.completion_attempt <- function(expr) {
    tryCatch(
        list(value = force(expr)),
        error = function(error) NULL
    )
}

completion_main <- function(
    text,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv,
    global_lastenv = emptyenv()
) {
    # Select the expression suffix before the cursor.
    forward <- lex_forward(text)
    backward <- lex_backward(text, forward)
    if (backward$status != "candidate") {
        return(character())
    }
    selected <- substring(text, backward$start)

    # Split the suffix into its context, accessor, and partial name.
    split <- split_completion_context(
        selected,
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
    # TODO: Keep track of leading quotes to complete closing quotes

    if (is.null(split$accessor)) {
        # Only code and backtick names can be global-name completions.
        if (!forward$state %in% c(LS_CODE, LS_BACKTICK)) {
            return(character())
        }
        context <- getScopeEnvs(firstenv, global_lastenv)
    } else {
        # Parse the context without letting invalid syntax escape the pipeline.
        parsed <- .completion_attempt(
            parse_completion_context(split$context)
        )
        if (is.null(parsed)) {
            return(character())
        }

        # Resolve independently so parse and evaluation errors stay local.
        resolved <- .completion_attempt(
            resolve_completion_context(
                parsed$value,
                split$accessor,
                firstenv = firstenv,
                lastenv = lastenv
            )
        )
        if (is.null(resolved)) {
            return(character())
        }
        context <- resolved$value
    }

    completion_candidates(
        context,
        split$accessor,
        partial_name
    )
}
