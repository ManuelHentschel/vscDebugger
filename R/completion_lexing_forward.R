# Marks strings, comments, and special operators for later completion passes.
LS_CODE <- 0L
LS_SINGLE_QUOTED <- 1L
LS_DOUBLE_QUOTED <- 2L
LS_BACKTICK <- 3L
LS_RAW_PREFIX <- 4L
LS_RAW_QUOTED <- 5L
LS_RAW_INVALID <- 6L
LS_COMMENT <- 7L
LS_SPECIAL_OPERATOR <- 8L

QUOTED_STATES <- c(LS_SINGLE_QUOTED, LS_DOUBLE_QUOTED, LS_BACKTICK)
QUOTE_SYMBOLS <- c("'", "\"", "`")
QUOTE_BY_STATE <- setNames(as.list(QUOTE_SYMBOLS), QUOTED_STATES)
STATE_BY_QUOTE <- setNames(as.list(QUOTED_STATES), QUOTE_SYMBOLS)

RAW_OPENING_DELIMITERS <- c("(", "[", "{", "|")
RAW_CLOSING_DELIMITERS <- c(")", "]", "}", "|")
RAW_OPENING_TO_CLOSING_DELIMITERS <- setNames(
    RAW_CLOSING_DELIMITERS,
    RAW_OPENING_DELIMITERS
)

lex_forward <- function(text) {
    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)

    # Opaque regions include strings, comments, and complete special operators.
    # Their characters must not be reinterpreted by the rest of this scanner.
    #
    # Positions are 1-based and half-open: [start, end).
    # For an unfinished region extending to the cursor,
    # end == n + 1.
    regions <- list()

    add_region <- function(state, start, end) {
        regions[[length(regions) + 1L]] <<- list(
            state = state,
            start = start,
            end = end
        )
    }

    state <- LS_CODE
    state_start <- NA_integer_

    # Metadata used by the raw-string states.
    raw_quote <- NA_character_
    raw_dash_count <- 0L
    raw_open <- NA_character_
    raw_close <- NA_character_
    raw_invalid_position <- NA_integer_

    reset_raw <- function() {
        raw_quote <<- NA_character_
        raw_dash_count <<- 0L
        raw_open <<- NA_character_
        raw_close <<- NA_character_
        raw_invalid_position <<- NA_integer_
    }

    i <- 1L

    while (i <= n) {
        ch <- chars[i]

        # Ordinary quoted strings / backtick names
        quote <- QUOTE_BY_STATE[[as.character(state)]]
        if (!is.null(quote)) {
            # A backslash escapes the following character for the purpose of
            # finding the end of this quoted region. We don't need to
            # interpret the escape itself.
            if (ch == "\\") {
                if (i < n) {
                    i <- i + 2L
                } else {
                    i <- i + 1L
                }
                next
            }

            if (ch == quote) {
                add_region(
                    state,
                    state_start,
                    i + 1L
                )
                state <- LS_CODE
                state_start <- NA_integer_
            }

            i <- i + 1L
            next
        }

        # Inside an incomplete but so-far valid raw-string prefix
        # (r"|, r"-|, r"---|, etc.)
        if (state == LS_RAW_PREFIX) {
            if (ch == "-") {
                raw_dash_count <- raw_dash_count + 1L
                i <- i + 1L
                next
            }

            if (ch %in% RAW_OPENING_DELIMITERS) {
                raw_open <- ch
                raw_close <- RAW_OPENING_TO_CLOSING_DELIMITERS[[ch]]
                state <- LS_RAW_QUOTED
                # The opening delimiter itself is part of the raw literal;
                # scanning raw contents starts after it.
                i <- i + 1L
                next
            }

            # Once r" / r' has been recognized, do not reinterpret its quote
            # as the beginning of an ordinary string. This is instead a
            # malformed raw-string prefix.
            #
            # For completion-oriented error recovery, an invalid raw prefix
            # is considered opaque until the next newline.
            raw_invalid_position <- i
            state <- LS_RAW_INVALID
            # Do not advance yet: process this same character as part of the
            # invalid region.
            next
        }

        # Valid raw-string contents
        if (state == LS_RAW_QUOTED) {
            if (ch == raw_close) {
                # Required terminator:
                #
                #   closing delimiter
                #   same number of dashes as the prefix
                #   original quote character
                #
                # e.g. )---"
                quote_pos <- i + raw_dash_count + 1L
                if (quote_pos <= n) {
                    dashes_match <- TRUE
                    if (raw_dash_count > 0L) {
                        dash_positions <- seq.int(
                            i + 1L,
                            i + raw_dash_count
                        )
                        dashes_match <- all(
                            chars[dash_positions] == "-"
                        )
                    }

                    if (
                        dashes_match &&
                        chars[quote_pos] == raw_quote
                    ) {
                        end <- quote_pos + 1L
                        add_region(
                            LS_RAW_QUOTED,
                            state_start,
                            end
                        )
                        state <- LS_CODE
                        state_start <- NA_integer_
                        reset_raw()
                        i <- end
                        next
                    }
                }
            }

            # Everything else inside a raw string is opaque, including
            # quotes, backslashes, comments, delimiters and newlines.
            i <- i + 1L
            next
        }

        # Malformed raw-string prefix
        if (state == LS_RAW_INVALID) {
            if (ch %in% c("\n", "\r")) {
                # Completion-oriented recovery: consider the malformed raw
                # literal finished at the end of this line.
                #
                # The newline itself remains code/whitespace rather than
                # belonging to the invalid region.
                add_region(
                    LS_RAW_INVALID,
                    state_start,
                    i
                )
                state <- LS_CODE
                state_start <- NA_integer_
                reset_raw()
            }

            i <- i + 1L
            next
        }

        # Special infix operators
        #
        # Between two percent signs, R permits any printable character except
        # another percent sign. In particular, #, quotes, backslashes and
        # brackets have no special meaning here. Escape sequences do not
        # apply. A newline cannot occur inside the operator token.
        if (state == LS_SPECIAL_OPERATOR) {
            if (ch == "%") {
                add_region(
                    LS_SPECIAL_OPERATOR,
                    state_start,
                    i + 1L
                )
                state <- LS_CODE
                state_start <- NA_integer_
            } else if (ch %in% c("\n", "\r")) {
                # Completion-oriented recovery for an unfinished operator.
                # The newline itself remains code/whitespace.
                add_region(
                    LS_SPECIAL_OPERATOR,
                    state_start,
                    i
                )
                state <- LS_CODE
                state_start <- NA_integer_
            }

            i <- i + 1L
            next
        }

        # Comments
        if (state == LS_COMMENT) {
            if (ch %in% c("\n", "\r")) {
                # Do not include the newline itself in the comment region.
                add_region(
                    LS_COMMENT,
                    state_start,
                    i
                )
                state <- LS_CODE
                state_start <- NA_integer_
            }

            i <- i + 1L
            next
        }

        # Code
        stopifnot(state == LS_CODE)

        # Special infix operator. Its first following percent sign terminates
        # the token, so %% and %/% naturally use the same state as custom
        # operators.
        if (ch == "%") {
            state <- LS_SPECIAL_OPERATOR
            state_start <- i
            i <- i + 1L
            next
        }

        # Comment.
        if (ch == "#") {
            state <- LS_COMMENT
            state_start <- i
            i <- i + 1L
            next
        }

        # Potential raw-string prefix.
        #
        # Avoid recognizing the r/R in an ordinary identifier such as:
        #
        #   foobar"...
        #
        # This isn't intended to be a complete R identifier lexer; it just
        # prevents the common identifier case from being mistaken for a raw
        # string.
        if (
            ch %in% c("r", "R") &&
            i < n
        ) {
            previous_is_name_char <-
                i > 1L &&
                grepl(
                    "^[[:alnum:]_.]$",
                    chars[i - 1L]
                )

            next_ch <- chars[i + 1L]

            if (
                !previous_is_name_char &&
                next_ch %in% c("\"", "'")
            ) {
                state <- LS_RAW_PREFIX
                state_start <- i
                raw_quote <- next_ch
                raw_dash_count <- 0L
                raw_open <- NA_character_
                raw_close <- NA_character_
                raw_invalid_position <- NA_integer_
                # Consume r" / R" / r' / R'.
                i <- i + 2L
                next
            }
        }

        # Ordinary quoted strings.
        if (ch %in% names(STATE_BY_QUOTE)) {
            state <- STATE_BY_QUOTE[[ch]]
            state_start <- i
            i <- i + 1L
            next
        }

        i <- i + 1L
    }

    # If the cursor is inside an opaque region, record the unfinished region.
    #
    # Because the input ends exactly at the cursor, n + 1 is its exclusive
    # end position.
    if (state != LS_CODE) {
        add_region(
            state,
            state_start,
            n + 1L
        )
    }

    # Raw-string metadata is only relevant if the cursor is currently in one
    # of the raw-string states.
    raw_string_info <- NULL

    if (state %in% c(
        LS_RAW_PREFIX,
        LS_RAW_QUOTED,
        LS_RAW_INVALID
    )) {
        terminator <- NULL

        if (state == LS_RAW_QUOTED) {
            terminator <- paste0(
                raw_close,
                strrep("-", raw_dash_count),
                raw_quote
            )
        }

        raw_string_info <- list(
            quote = raw_quote,
            dash_count = raw_dash_count,
            opening_delimiter = raw_open,
            closing_delimiter = raw_close,
            terminator = terminator,
            invalid_position = if (
                state == LS_RAW_INVALID
            ) {
                raw_invalid_position
            } else {
                NA_integer_
            }
        )
    }

    list(
        state = state,
        state_start = if (state == LS_CODE) {
            NA_integer_
        } else {
            state_start
        },
        regions = regions,
        raw = raw_string_info
    )
}
