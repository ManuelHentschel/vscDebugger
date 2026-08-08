# Finds a feasible expression suffix for completion. This pass deliberately
# does not decide whether evaluating the expression or following its accessors
# is safe; those checks belong to semantic resolution.
.backward_name_pattern <- "^[[:alnum:]_.]$"
.backward_horizontal_whitespace <- c(" ", "\t", "\f", "\v")

.completion_region_ending_at <- function(regions, position) {
    for (region in rev(regions)) {
        if (region$end == position + 1L) {
            return(region)
        }
    }
    NULL
}

.completion_is_name_char <- function(ch) {
    grepl(.backward_name_pattern, ch)
}

.completion_backward_result <- function(
    status,
    start = NULL
) {
    result <- list(status = status)
    if (!is.null(start)) {
        result$start <- start
    }
    result
}

lex_backward <- function(text, forward = lex_forward(text)) {

    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)
    end <- n + 1L

    # Empty context for empty suffix
    if (n == 0L) {
        return(.completion_backward_result("candidate", end))
    }

    # Raw strings, comments, and unfinished special operators are unsupported.
    if (forward$state %in% c(
        LS_RAW_PREFIX,
        LS_RAW_QUOTED,
        LS_RAW_INVALID,
        LS_COMMENT,
        LS_SPECIAL_OPERATOR
    )) {
        return(.completion_backward_result("infeasible"))
    }

    # A closing delimiter or non-string space ends a completed value.
    if (chars[n] %in% c(")", "]", "}")) {
        return(.completion_backward_result("no_completion"))
    }
    if (
        chars[n] %in% .backward_horizontal_whitespace &&
        !(forward$state %in% QUOTED_STATES)
    ) {
        return(.completion_backward_result("no_completion"))
    }

    # A completed string is a value that cannot continue without an operator.
    if (forward$state == LS_CODE) {
        ending_region <- .completion_region_ending_at(forward$regions, n)
        if (
            !is.null(ending_region) &&
            ending_region$state %in% c(QUOTED_STATES, LS_RAW_QUOTED)
        ) {
            return(.completion_backward_result("no_completion"))
        }
    }

    # These regions (strings) are allowed as part of an accessor chain
    allowed_regions <- c(
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
        region <- .completion_region_ending_at(forward$regions, position)
        if (!is.null(region)) {
            if (region$state %in% allowed_regions) {
                is_current_incomplete_string <-
                    suffix_is_incomplete_string &&
                    region$state == forward$state &&
                    region$end == end
                if (!is_current_incomplete_string) {
                    suffix_is_incomplete_string <- FALSE
                }
                position <- region$start - 1L
                next
            }
            # Stop at any unsupported lexical region.
            break
        }

        ch <- chars[position]
        # Break on newlines outside quoted regions.
        if (ch %in% c("\n", "\r")) {
            break
        }

        # Continue left through a name.
        if (.completion_is_name_char(ch)) {
            suffix_is_incomplete_string <- FALSE
            position <- position - 1L
            next
        }
        # Allow whitespace next to special operators, not between normal names
        if (ch %in% .backward_horizontal_whitespace) {
            # Consume the complete whitespace run before inspecting its edges.
            whitespace_start <- position
            while (
                whitespace_start > 1L &&
                chars[whitespace_start - 1L] %in%
                    .backward_horizontal_whitespace
            ) {
                whitespace_start <- whitespace_start - 1L
            }

            # Whitespace between two name characters separates two tokens.
            left <- whitespace_start - 1L
            right <- position + 1L
            if (
                left >= 1L &&
                right <= n &&
                .completion_is_name_char(chars[left]) &&
                .completion_is_name_char(chars[right])
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
            # Only namespace accessors use colons here.
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
                    opening_start <- if (
                        position > 1L && chars[position - 1L] == "["
                    ) {
                        position - 1L
                    } else {
                        position
                    }
                    receiver_end <- opening_start - 1L
                    while (
                        receiver_end >= 1L &&
                        chars[receiver_end] %in% .backward_horizontal_whitespace
                    ) {
                        receiver_end <- receiver_end - 1L
                    }
                    if (receiver_end < 1L) {
                        return(.completion_backward_result("no_completion"))
                    }
                    break
                }
                position <- position - 1L
                # Consume the second opening bracket of `[[`.
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
        return(.completion_backward_result("infeasible"))
    }

    # Find first non-whitespace character in the suffix
    candidate_start <- position + 1L
    while (
        candidate_start < end &&
        chars[candidate_start] %in% .backward_horizontal_whitespace
    ) {
        candidate_start <- candidate_start + 1L
    }
    .completion_backward_result("candidate", candidate_start)
}
