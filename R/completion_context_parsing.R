# Validates and normalizes the supported completion-context AST shape.

.completion_ast_name_to_string <- function(node) {
    if (is.name(node)) {
        return(as.character(node))
    }
    node
}

.completion_normalize_ast <- function(node) {
    # Early returns
    if (is.null(node) || is.atomic(node) || is.name(node)) {
        return(node)
    }

    # Check that node is a call with a named operator
    if (!is.call(node) || !is.name(node[[1L]])) {
        stop("The context contains an unsupported expression")
    }

    operator <- as.character(node[[1L]])

    # Check recursively, normalize ambiguous operands to strings
    if (operator %in% c("$", "@")) {
        return(as.call(list(
            as.name(operator),
            .completion_normalize_ast(node[[2L]]),
            .completion_ast_name_to_string(node[[3L]])
        )))
    }

    if (operator %in% c("::", ":::")) {
        return(as.call(list(
            as.name(operator),
            .completion_ast_name_to_string(node[[2L]]),
            .completion_ast_name_to_string(node[[3L]])
        )))
    }

    if (operator %in% c("[", "[[")) {
        # Normalize all operands (`[` and `[[` remain unchanged)
        return(as.call(lapply(as.list(node), .completion_normalize_ast)))
    }

    stop("Function or operator call is not allowed: ", operator)
}

parse_completion_context <- function(context) {
    parsed <- parse(text = context, keep.source = FALSE)
    if (length(parsed) != 1L) {
        stop("The context must contain exactly one expression")
    }
    .completion_normalize_ast(parsed[[1L]])
}
