# Finds and splits a feasible expression suffix for completion. This pass deliberately
# does not decide whether evaluating the expression or following its accessors
# is safe; those checks belong to semantic resolution.

.completion_region_ending_at <- function(regions, position) {
    for (region in rev(regions)) {
        if (region$end == position + 1L) {
            return(region)
        }
    }
    NULL
}

.completion_is_name_char <- function(ch) {
    grepl("^[[:alnum:]_.]$", ch)
}

# Returns a list with the following elements:
# - status: one of "candidate", "no_completion", or "infeasible"
# - context: the text of the context expression
# - accessor: the accessor character
# - partial_child: the text of the child expression
# - reason: an optional explanation when status is "infeasible"
# Context, accessor, and partial_child are present only for candidates.
.completion_backward_result <- function(
    status,
    text = NULL,
    start = NULL,
    accessor = NULL,
    accessor_start = NULL,
    reason = NULL
) {
    result <- list(status = status)
    if (!is.null(reason)) {
        result$reason <- reason
    }
    if (!is.null(start)) {
        if (is.null(accessor)) {
            context <- ""
            partial_child <- substring(text, start)
        } else {
            context <- substr(text, start, accessor_start - 1L)
            partial_child <- substring(
                text,
                accessor_start + nchar(accessor)
            )
        }
        result$context <- context
        result$accessor <- accessor
        result$partial_child <- partial_child
    }
    result
}

lex_backward <- function(text, forward = lex_forward(text)) {

    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)
    end <- n + 1L

    # An empty request starts an empty global-name completion.
    if (n == 0L) {
        return(.completion_backward_result("candidate", text, end))
    }

    # Identify the partial child or handle cursor locations without one.
    if (forward$state %in% QUOTED_STATES) {
        # The entire unfinished quoted region is the child.
        child_region <- .completion_region_ending_at(forward$regions, n)
        child_start <- child_region$start
        child_kind <- if (forward$state == LS_BACKTICK) "name" else "string"
    } else if (forward$state == LS_CODE) {
        ch <- chars[n]
        if (.completion_is_name_char(ch)) {
            # If we are in a name, scan backward to find the start of the name.
            child_start <- n
            while (
                child_start > 1L &&
                .completion_is_name_char(chars[child_start - 1L])
            ) {
                child_start <- child_start - 1L
            }
            child_kind <- "name"
        } else if (ch %in% c("$", "@", ":", "[")) {
            # If we are on an accessor, the child is empty.
            child_start <- end
            child_kind <- "empty"
        } else if (ch %in% c(")", "]", "}", "\"", "'", "`")) {
            # Completed strings and closing delimiters cannot be extended.
            return(.completion_backward_result("no_completion"))
        } else {
            # Whitespace and other punctuation start an empty expression.
            return(.completion_backward_result("candidate", text, end))
        }
    } else {
        # Raw strings, comments, and unfinished special operators are infeasible.
        return(.completion_backward_result(
            "infeasible",
            reason = "Invalid cursor location"
        ))
    }

    # Identify the accessor preceding the partial child.
    accessor <- NULL
    accessor_start <- NULL
    position <- child_start - 1L
    if (position >= 1L) {
        ch <- chars[position]
        if (ch %in% c("$", "@")) {
            accessor <- ch
            accessor_start <- position
        } else if (ch == ":") {
            # Count the complete colon run before accepting `::` or `:::`.
            colon_start <- position
            while (colon_start > 1L && chars[colon_start - 1L] == ":") {
                colon_start <- colon_start - 1L
            }
            colon_width <- position - colon_start + 1L
            if (colon_width %in% c(2L, 3L)) {
                accessor <- substr(text, colon_start, position)
                accessor_start <- colon_start
            }
        } else if (ch == "[" && child_kind %in% c("empty", "string")) {
            # Empty/string indices are child access; name/backtick indices are expressions.
            if (position > 1L && chars[position - 1L] == "[") {
                accessor <- "[["
                accessor_start <- position - 1L
            } else {
                accessor <- "["
                accessor_start <- position
            }
        }
    }

    # Without an accessor, return only the partial child.
    if (is.null(accessor)) {
        return(.completion_backward_result(
            "candidate",
            text,
            child_start
        ))
    }

    # Scan backward to find the start of the context expression.
    allowed_regions <- c(
        LS_SINGLE_QUOTED,
        LS_DOUBLE_QUOTED,
        LS_BACKTICK,
        LS_RAW_QUOTED
    )
    bracket_depth <- 0L
    position <- accessor_start - 1L

    while (position >= 1L) {
        # Treat quoted regions as indivisible context tokens.
        region <- .completion_region_ending_at(forward$regions, position)
        if (!is.null(region)) {
            if (region$state %in% allowed_regions) {
                position <- region$start - 1L
                next
            }
            break
        }

        # Handle ordinary code characters
        ch <- chars[position]

        # Stop looking on line breaks
        if (ch %in% c("\n", "\r")) {
            break
        }

        # Stop looking on horizontal whitespace unless it is inside an index expression
        if (bracket_depth == 0L && ch %in% c(" ", "\t", "\f", "\v")) {
            break
        }

        # Keep scanning on name characters and accessors `$`, `@`
        if (.completion_is_name_char(ch) || ch %in% c("$", "@")) {
            position <- position - 1L
            next
        }

        # Allow complete namespace accessors inside the context.
        # (in particular, break on a single `:`!)
        if (ch == ":") {
            colon_start <- position
            while (colon_start > 1L && chars[colon_start - 1L] == ":") {
                colon_start <- colon_start - 1L
            }
            colon_width <- position - colon_start + 1L
            if (!colon_width %in% c(2L, 3L)) {
                break
            }
            position <- colon_start - 1L
            next
        }

        # Commas are valid only inside a balanced indexing expression.
        if (ch == "," && bracket_depth > 0L) {
            position <- position - 1L
            next
        }
        if (ch == "]") {
            bracket_depth <- bracket_depth + 1L
            position <- position - 1L
            next
        }
        if (ch == "[") {
            if (bracket_depth == 0L) {
                break
            }
            bracket_depth <- bracket_depth - 1L
            position <- position - 1L
            next
        }

        # Break on any other character
        break
    }

    # A missing opening bracket means the context is incomplete.
    if (bracket_depth > 0L) {
        return(.completion_backward_result(
            "infeasible",
            reason = "Could not find a complete context expression"
        ))
    }

    # Every accessor requires a context expression to its left.
    context_start <- position + 1L
    if (context_start == accessor_start) {
        return(.completion_backward_result(
            "infeasible",
            reason = "The accessor has no context expression"
        ))
    }

    .completion_backward_result(
        "candidate",
        text,
        context_start,
        accessor,
        accessor_start
    )
}
