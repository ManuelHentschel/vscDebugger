# Finds a feasible expression suffix for completion. This pass deliberately
# does not decide whether evaluating the expression or following its accessors
# is safe; those checks belong to semantic resolution.
BACKWARD_NAME_PATTERN <- "^[[:alnum:]_.]$"
BACKWARD_HORIZONTAL_WHITESPACE <- c(" ", "\t", "\f", "\v")

completion_backward_slice <- function(chars, start, end) {
    if (start >= end) {
        return("")
    }
    paste0(chars[seq.int(start, end - 1L)], collapse = "")
}

completion_region_ending_at <- function(regions, position) {
    for (region in rev(regions)) {
        if (region$end == position + 1L) {
            return(region)
        }
    }
    NULL
}

completion_is_name_char <- function(ch) {
    grepl(BACKWARD_NAME_PATTERN, ch)
}

completion_backward_result <- function(chars, start, end, feasible = TRUE) {
    list(
        feasible = feasible,
        start = if (feasible) start else NA_integer_,
        end = end,
        text = if (feasible) {
            completion_backward_slice(chars, start, end)
        } else {
            NULL
        }
    )
}

lex_backward <- function(text, forward = lex_forward(text)) {

    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)
    end <- n + 1L

    # Empty context for empty suffix
    if (n == 0L) {
        return(completion_backward_result(chars, end, end))
    }

    # For now, completion inside raw strings, comments, and special operators is not supported.
    # (might add later)
    if (forward$state %in% c(
        LS_RAW_PREFIX,
        LS_RAW_QUOTED,
        LS_RAW_INVALID,
        LS_COMMENT,
        LS_SPECIAL_OPERATOR
    )) {
        return(completion_backward_result(chars, end, end, FALSE))
    }

    # If the suffix ends with a closing delimiter or non-string space, return empty context
    if (chars[n] %in% c(")", "]")) {
        return(completion_backward_result(chars, end, end))
    }
    if (
        chars[n] %in% BACKWARD_HORIZONTAL_WHITESPACE &&
        !(forward$state %in% QUOTED_STATES)
    ) {
        return(completion_backward_result(chars, end, end))
    }

    # If cursor is right after a completed string etc., return empty context
    if (forward$state == LS_CODE) {
        ending_region <- completion_region_ending_at(forward$regions, n)
        if (!is.null(ending_region)) {
            return(completion_backward_result(chars, end, end))
        }
    }

    # These regions (strings) are allowed as part of an accessor chain
    ALLOWED_REGIONS <- c(
        LS_SINGLE_QUOTED,
        LS_DOUBLE_QUOTED,
        LS_BACKTICK,
        LS_RAW_QUOTED
    )

    # Keep track of closed brackets that need matching opening ones
    bracket_depth <- 0L

    # Track whether the suffix is just an incomplete string
    suffix_is_incomplete_string <- forward$state %in% c(
        LS_SINGLE_QUOTED,
        LS_DOUBLE_QUOTED
    )

    position <- n

    while (position >= 1L) {
        region <- completion_region_ending_at(forward$regions, position)
        if (!is.null(region)) {
            if (region$state %in% ALLOWED_REGIONS) {
                is_current_incomplete_string <-
                    suffix_is_incomplete_string &&
                    region$state == forward$state &&
                    region$start == forward$state_start &&
                    region$end == end
                if (!is_current_incomplete_string) {
                    suffix_is_incomplete_string <- FALSE
                }
                position <- region$start - 1L
                next
            }
            # unsupported region (comment, raw string prefix)
            break
        }

        ch <- chars[position]
        # Break on newline, not allowing multiline element access.
        if (ch %in% c("\n", "\r")) {
            break
        }

        # continue left on normal characters, whitespace, and accessors
        if (completion_is_name_char(ch)) {
            suffix_is_incomplete_string <- FALSE
            position <- position - 1L
            next
        }
        # Allow whitespace next to special operators, not between normal names
        if (ch %in% BACKWARD_HORIZONTAL_WHITESPACE) {
            # Consume the complete whitespace run before inspecting its edges.
            whitespace_start <- position
            while (
                whitespace_start > 1L &&
                chars[whitespace_start - 1L] %in%
                    BACKWARD_HORIZONTAL_WHITESPACE
            ) {
                whitespace_start <- whitespace_start - 1L
            }

            # Whitespace between two name characters separates two tokens.
            left <- whitespace_start - 1L
            right <- position + 1L
            if (
                left >= 1L &&
                right <= n &&
                completion_is_name_char(chars[left]) &&
                completion_is_name_char(chars[right])
            ) {
                break
            }
            position <- left
            next
        }

        # Check for accessors
        if (ch %in% c("$", "@")) {
            suffix_is_incomplete_string <- FALSE
            position <- position - 1L
            next
        }

        # Count colons, only :: or ::: are allowed
        if (ch == ":") {
            colon_start <- position
            while (colon_start > 1L && chars[colon_start - 1L] == ":") {
                colon_start <- colon_start - 1L
            }
            colon_width <- position - colon_start + 1L
            # break for : or ::::
            if (!colon_width %in% c(2L, 3L)) {
                break
            }
            suffix_is_incomplete_string <- FALSE
            position <- colon_start - 1L
            next
        }

        # Allow commas while scanning a completed indexing expression.
        if (ch == "," && bracket_depth > 0L) {
            position <- position - 1L
            next
        }

        # Track closing brackets and match them with opening ones.
        if (ch == "]") {
            suffix_is_incomplete_string <- FALSE
            bracket_depth <- bracket_depth + 1L
            position <- position - 1L
            next
        }
        if (ch == "[") {
            if (bracket_depth == 0L) {
                # An unmatched [ is useful only for an unfinished string
                # accessor such as `x[["item`.
                if (!suffix_is_incomplete_string) {
                    break
                }
                position <- position - 1L
                # Check for double [[, to avoid mismatch break after first [
                if (position >= 1L && chars[position] == "[") {
                    position <- position - 1L
                }
                suffix_is_incomplete_string <- FALSE
                next
            }
            bracket_depth <- bracket_depth - 1L
            position <- position - 1L
            next
        }

        # Break on any other character
        break
    }

    # Unmatched closing brackets do not form a feasible completion suffix.
    if (bracket_depth > 0L) {
        return(completion_backward_result(chars, end, end, FALSE))
    }

    # Find first non-whitespace character in the suffix
    candidate_start <- position + 1L
    while (
        candidate_start < end &&
        chars[candidate_start] %in% BACKWARD_HORIZONTAL_WHITESPACE
    ) {
        candidate_start <- candidate_start + 1L
    }
    completion_backward_result(chars, candidate_start, end)
}
