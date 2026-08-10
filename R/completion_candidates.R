# Keep only text before the first whitespace to the right.
.completion_available_right_text <- function(text_after_cursor) {
    whitespace <- regexpr("[[:space:]]", text_after_cursor)
    if (whitespace == -1L) {
        return(text_after_cursor)
    }
    substr(text_after_cursor, 1L, whitespace - 1L)
}

# Reuse a matching right-hand suffix when the full name fragment matches.
.completion_trim_right_overlap <- function(insertion, available_right_text) {
    max_overlap <- min(nchar(insertion), nchar(available_right_text))
    if (max_overlap == 0L) {
        return(insertion)
    }

    matched_overlap <- 0L
    for (overlap in rev(seq_len(max_overlap))) {
        insertion_suffix <- substr(
            insertion,
            nchar(insertion) - overlap + 1L,
            nchar(insertion)
        )
        right_prefix <- substr(available_right_text, 1L, overlap)
        if (insertion_suffix == right_prefix) {
            matched_overlap <- overlap
            break
        }
    }

    # Reuse only complete name fragments; otherwise insert the full text.
    right_name <- sub("[^[:alnum:]_.].*$", "", available_right_text)
    if (matched_overlap < nchar(right_name)) {
        return(insertion)
    }
    substr(insertion, 1L, nchar(insertion) - matched_overlap)
}

# Spell a candidate as valid R code using the partial child's quote style.
.completion_candidate_text <- function(
    child_name,
    accessor,
    quote
) {
    if (!is.null(quote)) {
        return(encodeString(child_name, quote = quote))
    } else if (!is.null(accessor) && accessor %in% c("[", "[[")) {
        return(encodeString(child_name, quote = "\""))
    } else if (make.names(child_name) == child_name) {
        return(child_name)
    }
    encodeString(child_name, quote = "`")
}

# Generate DAP completion items from an already resolved context.
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
        # Read names without dispatching another user-defined method.
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

    # Remove unusable names and keep those matching the typed prefix.
    child_names <- unique(child_names[!is.na(child_names) & nzchar(child_names)])
    child_names <- unname(child_names[startsWith(child_names, partial_name)])

    # Keep item types generic without inspecting or forcing child bindings.
    item_type <- if (is.null(accessor)) {
        "variable"
    } else if (accessor == "@") {
        "field"
    } else {
        "property"
    }

    # Consider right-hand text up to first whitespace
    available_right_text <- .completion_available_right_text(text_after_cursor)

    # Build DAP items, omitting only exact no-op overlaps.
    items <- lapply(child_names, function(child_name) {
        escaped_text <- .completion_candidate_text(child_name, accessor, quote)
        trimmed_text <- .completion_trim_right_overlap(
            escaped_text,
            available_right_text
        )
        if (nchar(trimmed_text) == 0L) {
            return(NULL)
        }
        list(
            label = child_name,
            text = trimmed_text,
            # DAP says 1-based, but vscode interprets 0-based for `start`
            # (Temporary?) fix by converting to 0-based
            start = replacement_start - 1L,
            length = replacement_length
        )
    })
    Filter(Negate(is.null), items)
}
