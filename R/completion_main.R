# Count positions in the UTF-16 units used by DAP and VS Code.
.completion_utf16_length <- function(text) {
    codepoints <- utf8ToInt(enc2utf8(text))
    as.integer(length(codepoints) + sum(codepoints > 0xffffL))
}

# Decode a quoted source prefix for matching against actual child names.
.completion_partial_name <- function(source, quote) {
    if (is.null(quote) || source == "") {
        return(source)
    }
    parsed <- tryCatch(
        parse(text = paste0(quote, source, quote), keep.source = FALSE),
        error = function(error) NULL
    )
    if (length(parsed) != 1L) {
        return(NULL)
    }
    value <- parsed[[1L]]
    if (quote == "`" && is.name(value)) {
        return(as.character(value))
    }
    if (quote != "`" && is.character(value) && length(value) == 1L) {
        return(value)
    }
    NULL
}

# Extract the typed name and its replacement range from the partial child.
.completion_partial <- function(text, partial_child) {
    source <- partial_child
    quote <- substr(source, 1L, 1L)
    if (quote %in% c("'", "\"", "`")) {
        source <- substring(source, 2L)
    } else {
        quote <- NULL
    }

    # VS Code can only replace text on the cursor's current line.
    if (grepl("[\r\n]", source)) {
        return(NULL)
    }
    name <- .completion_partial_name(source, quote)
    if (is.null(name)) {
        return(NULL)
    }

    before_source <- substr(text, 1L, nchar(text) - nchar(source))
    list(
        name = name,
        quote = quote,
        start = .completion_utf16_length(before_source) + 1L,
        length = .completion_utf16_length(source)
    )
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
    accessor <- backward$accessor
    partial <- .completion_partial(text, backward$partial_child)
    if (is.null(partial)) {
        return(list())
    }

    if (is.null(accessor)) {
        # Only code and backtick names can be global-name completions.
        if (!forward$state %in% c(LS_CODE, LS_BACKTICK)) {
            return(list())
        }
        context <- getScopeEnvs(firstenv, global_lastenv)
    } else {
        # Parse and resolve the context through their stage-local error boundaries.
        parsed <- parse_completion_context(backward$context)
        if (parsed$status != "success") {
            return(list())
        }

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

    items <- completion_candidates(
        context,
        accessor,
        partial$name,
        partial$quote,
        partial$start,
        partial$length,
        text_after_cursor
    )

    # Empty-index policy: prefer named children, then fall back to globals.
    empty_index <- (
        !is.null(accessor) &&
        accessor %in% c("[", "[[") &&
        partial$name == "" &&
        is.null(partial$quote)
    )
    if (!length(items) && empty_index) {
        items <- completion_candidates(
            getScopeEnvs(firstenv, global_lastenv),
            NULL,
            "",
            NULL,
            partial$start,
            partial$length,
            text_after_cursor
        )
    }

    items
}
