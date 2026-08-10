# Keep only identifier-like text and closing syntax to the right.
.completion_available_right_text <- function(text_after_cursor) {
    closing_delimiters <- c("'", "\"", "`", ")", "]", "}")
    for (i in seq_len(nchar(text_after_cursor))) {
        ch <- substr(text_after_cursor, i, i)
        if (
            !grepl("^[[:alnum:]_.]$", ch) &&
            !ch %in% closing_delimiters
        ) {
            return(substr(text_after_cursor, 1L, i - 1L))
        }
    }
    text_after_cursor
}

# Remove the part of an insertion that is already present to the right.
.completion_trim_right_overlap <- function(insertion, right_candidate) {
    max_overlap <- min(nchar(insertion), nchar(right_candidate))
    if (max_overlap == 0L) {
        return(insertion)
    }
    for (overlap in rev(seq_len(max_overlap))) {
        insertion_suffix <- substr(
            insertion,
            nchar(insertion) - overlap + 1L,
            nchar(insertion)
        )
        right_prefix <- substr(right_candidate, 1L, overlap)
        if (insertion_suffix == right_prefix) {
            return(substr(insertion, 1L, nchar(insertion) - overlap))
        }
    }
    insertion
}

# Spell a candidate as valid R code while preserving an existing quote.
.completion_candidate_text <- function(
    child_name,
    accessor,
    quote,
    right_candidate
) {
    if (!is.null(quote)) {
        insertion <- substring(encodeString(child_name, quote = quote), 2L)
    } else if (!is.null(accessor) && accessor %in% c("[", "[[")) {
        insertion <- encodeString(child_name, quote = "\"")
    } else if (make.names(child_name) == child_name) {
        insertion <- child_name
    } else {
        insertion <- encodeString(child_name, quote = "`")
    }
    .completion_trim_right_overlap(insertion, right_candidate)
}

# Generates DAP completion items from an already resolved context.
completion_candidates <- function(
    context,
    accessor,
    partial_name,
    quote,
    replacement_start,
    replacement_length,
    text_after_cursor
) {
    if (is.null(accessor)) {
        # Top-level expression, context is list of available environments.
        child_names <- unlist(lapply(
            context,
            ls,
            all.names = TRUE,
            sorted = FALSE
        ), use.names = FALSE)
    } else if (accessor == "::") {
        child_names <- getNamespaceExports(context)
    } else if (accessor == ":::") {
        child_names <- ls(context, all.names = TRUE, sorted = FALSE)
    } else if (accessor == "@") {
        child_names <- methods::slotNames(context)
    } else if (is.environment(context)) {
        child_names <- if (accessor == "[" && !is.object(context)) {
            NULL
        } else {
            ls(context, all.names = TRUE, sorted = FALSE)
        }
    } else if (
        accessor == "$" &&
        !is.recursive(context) &&
        !is.object(context)
    ) {
        child_names <- NULL
    } else if (accessor %in% c("$", "[", "[[")) {
        child_names <- attr(context, "names", exact = TRUE)
        if (is.null(child_names) && accessor %in% c("[", "[[")) {
            child_names <- unlist(
                attr(context, "dimnames", exact = TRUE),
                use.names = FALSE
            )
        }
    } else {
        stop("Unsupported completion accessor: ", accessor)
    }

    if (is.null(child_names)) {
        return(list())
    }
    child_names <- unique(child_names[!is.na(child_names) & nzchar(child_names)])
    child_names <- unname(child_names[startsWith(child_names, partial_name)])

    item_type <- if (is.null(accessor)) {
        "variable"
    } else if (accessor == "@") {
        "field"
    } else {
        "property"
    }

    right_candidate <- .completion_available_right_text(text_after_cursor)
    lapply(child_names, function(child_name) {
        list(
            label = child_name,
            text = .completion_candidate_text(
                child_name,
                accessor,
                quote,
                right_candidate
            ),
            type = item_type,
            start = replacement_start,
            length = replacement_length
        )
    })
}
