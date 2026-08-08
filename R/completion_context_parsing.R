# Splits a lexed completion suffix and validates its context as a safe AST.
COMPLETION_ACCESSORS <- c(":::", "::", "[[", "[", "$", "@") # decreasing length!

split_completion_context <- function(
    text,
    regions = lex_forward(text)$regions,
    offset = 0L
) {
    # Check length and return early for empty string
    n <- nchar(text)
    if (n == 0L) {
        return(list(
            context = "",
            accessor = NULL,
            partial_child = ""
        ))
    }

    # Character vector and slice helper for easier indexing
    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    slice <- function(start, end) {
        if (start >= end) {
            return("")
        }
        substr(text, start, end - 1L)
    }

    # Make logical vector indicating opaque regions (strings, comments, etc.)
    opaque <- rep(FALSE, n)
    for (region in regions) {
        region_start <- max(1L, region$start - offset)
        region_end <- min(n + 1L, region$end - offset)
        if (region_start < region_end) {
            opaque[seq.int(region_start, region_end - 1L)] <- TRUE
        }
    }


    # Find the right-most accessor, preferring longer tokens at each endpoint.
    accessor <- NULL
    accessor_start <- NA_integer_
    accessor_end <- NA_integer_

    for (candidate_end in rev(seq.int(2L, n+1L))) {
        # Skip if inside string
        if (opaque[candidate_end - 1L]) {
            next
        }
        # Check if we are at the end of an accessor token, order must be long ones first!
        for (candidate in COMPLETION_ACCESSORS) {
            candidate_start <- candidate_end - nchar(candidate)
            if (
                candidate_start >= 1L &&
                slice(candidate_start, candidate_end) == candidate
            ) {
                accessor <- candidate
                accessor_start <- candidate_start
                accessor_end <- candidate_end
                break
            }
        }
        if (!is.null(accessor)) {
            break
        }
    }

    # Compute context and partial child based on accessor position
    if (is.null(accessor)) {
        context <- ""
        partial_child <- text
    } else {
        context <- slice(1L, accessor_start)
        partial_child <- slice(accessor_end, n+1L)
    }

    list(
        context = context,
        accessor = accessor,
        partial_child = partial_child
    )
}

completion_ast_node_as_string <- function(node) {
    if (is.name(node)) {
        return(as.character(node))
    }
    if (is.character(node) && length(node) == 1L && !is.na(node)) {
        return(node)
    }
    stop("Expected a name or string")
}

completion_normalize_ast <- function(node) {
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
        if (length(node) != 3L) {
            stop(operator, " must have exactly two operands")
        }
        return(as.call(list(
            as.name(operator),
            completion_normalize_ast(node[[2L]]),
            completion_ast_node_as_string(node[[3L]])
        )))
    }

    if (operator %in% c("::", ":::")) {
        if (length(node) != 3L) {
            stop(operator, " must have exactly two operands")
        }
        return(as.call(list(
            as.name(operator),
            completion_ast_node_as_string(node[[2L]]),
            completion_ast_node_as_string(node[[3L]])
        )))
    }

    if (operator %in% c("[", "[[")) {
        # Normalize all operands (`[` and `[[` remain unchanged)
        return(as.call(lapply(as.list(node), completion_normalize_ast)))
    }

    stop("Function or operator call is not allowed: ", operator)
}

parse_completion_context <- function(context) {
    tryCatch({
        parsed <- parse(text = context, keep.source = FALSE)
        if (length(parsed) != 1L) {
            stop("The context must contain exactly one expression")
        }
        list(
            ok = TRUE,
            ast = completion_normalize_ast(parsed[[1L]]),
            reason = NULL,
            message = NULL
        )
    }, error = function(error) {
        list(
            ok = FALSE,
            ast = NULL,
            reason = "invalid_context",
            message = error$message
        )
    })
}
