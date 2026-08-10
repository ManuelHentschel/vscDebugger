# Count positions in the UTF-16 units used by DAP
.completion_utf16_length <- function(text) {
    codepoints <- utf8ToInt(enc2utf8(text))
    as.integer(length(codepoints) + sum(codepoints > 0xffffL))
}

# Parse the partial_child if it is quoted
.completion_unescape_quoted <- function(partial_child) {
    # We don't handle completion of multi-line strings
    if (grepl("[\r\n]", partial_child)) {
        return(NULL)
    }

    # Check if the partial child is quoted, return if not
    quote <- substr(partial_child, 1L, 1L)
    if(!quote %in% c("'", "\"", "`")) {
        return(list(
            name = partial_child,
            quote = NULL
        ))
    }

    # Add closing quote and try to parse
    parsed <- tryCatch(
        parse(text = paste0(partial_child, quote), keep.source = FALSE),
        error = function(error) NULL
    )
    if(is.null(parsed) || length(parsed) != 1L) {
        return(NULL)
    }

    return(list(
        name = as.character(parsed[[1L]]),
        quote = quote
    ))
}

# Run the completion stages and return DAP completion items.
completion_main <- function(
    text,
    firstenv = parent.frame(),
    lastenv = .GlobalEnv,
    global_lastenv = emptyenv(),
    text_after_cursor = ""
) {
    # Select and split the expression suffix before the cursor.
    forward <- lex_forward(text)
    backward <- lex_backward(text, forward)
    if (backward$status != "candidate") {
        return(list())
    }

    # If partial child is quoted, parse it to unescape and validate
    partial <- .completion_unescape_quoted(backward$partial_child)
    if (is.null(partial)) {
        return(list())
    }

    accessor <- backward$accessor
    if (is.null(accessor)) {
        # Top-level expressions must be code or backtick names.
        # They get all available environments as context.
        if (!forward$state %in% c(LS_CODE, LS_BACKTICK)) {
            return(list())
        }
        context <- getScopeEnvs(firstenv, global_lastenv)
    } else {
        # Parse the context
        parsed <- parse_completion_context(backward$context)
        if (parsed$status != "success") {
            return(list())
        }

        # Resolve the context to a single R value
        resolved <- resolve_completion_context(
            parsed$ast,
            accessor,
            firstenv = firstenv,
            lastenv = lastenv
        )
        if (resolved$status != "success") {
            return(list())
        }
        context <- resolved$value
    }

    # Replace the complete partial child, including an opening quote.
    replacement_length <- .completion_utf16_length(backward$partial_child)
    replacement_start <- .completion_utf16_length(text) - replacement_length + 1L

    items <- completion_candidates(
        context,
        accessor,
        partial$name,
        partial$quote,
        replacement_start,
        replacement_length,
        text_after_cursor
    )

    # If the expression is an empty `[` or `[[` index and we do not find
    # any named children, we treat it as empty context.
    if(
        !length(items) &&
        !is.null(accessor) &&
        accessor %in% c("[", "[[") &&
        partial$name == "" &&
        is.null(partial$quote)
    ) {
        items <- completion_candidates(
            getScopeEnvs(firstenv, global_lastenv),
            NULL,
            "",
            NULL,
            replacement_start,
            replacement_length,
            text_after_cursor
        )
    }

    items
}
