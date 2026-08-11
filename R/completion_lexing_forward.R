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
.quote_symbols <- c("'", "\"", "`")
.quote_by_state <- setNames(as.list(.quote_symbols), QUOTED_STATES)
.state_by_quote <- setNames(as.list(QUOTED_STATES), .quote_symbols)

.raw_opening_delimiters <- c("(", "[", "{", "|")
.raw_closing_delimiters <- c(")", "]", "}", "|")
.raw_opening_to_closing_delimiters <- setNames(
    .raw_closing_delimiters,
    .raw_opening_delimiters
)

.completion_is_name_char <- function(ch){
    grepl("^[[:alnum:]_.]$", ch)
}

lex_forward <- function(text){
    # Split text into chars to iterate over
    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)

    # Opaque regions include strings, comments, and complete special operators.
    # Their characters are not reinterpreted by the rest of this scanner.
    # Positions are 1-based and half-open: [start, end).
    # For an unfinished region extending to the cursor, end == n + 1.
    regions <- list()
    add_region <- function(state, start, end){
        regions[[length(regions) + 1L]] <<- list(
            state = state,
            start = start,
            end = end
        )
    }

    # Initial lexical state
    state <- LS_CODE
    state_start <- NA_integer_

    # Metadata used by the raw-string states.
    raw_quote <- NA_character_
    raw_dash_count <- 0L
    raw_close <- NA_character_
    reset_raw <- function(){
        raw_quote <<- NA_character_
        raw_dash_count <<- 0L
        raw_close <<- NA_character_
    }

    i <- 1L

    while(i <= n){
        ch <- chars[i]

        # Inside a quoted string/name ("'`)
        if(state %in% QUOTED_STATES){
            quote <- .quote_by_state[[as.character(state)]]
            # Consume escape sequences
            if(ch == "\\"){
                if(i < n){
                    i <- i + 2L
                } else{
                    i <- i + 1L
                }
                next
            }

            # Handle closing quote
            if(ch == quote){
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
        if(state == LS_RAW_PREFIX){
            # Only dashes and opening delimiters are valid here
            if(ch == "-"){
                raw_dash_count <- raw_dash_count + 1L
                i <- i + 1L
                next
            }
            if(ch %in% .raw_opening_delimiters){
                raw_close <- .raw_opening_to_closing_delimiters[[ch]]
                state <- LS_RAW_QUOTED
                # The opening delimiter itself is part of the raw literal;
                # scanning raw contents starts after it.
                i <- i + 1L
                next
            }

            # r" is not followed by a valid opening delimiter -> invalid state
            state <- LS_RAW_INVALID
            next
        }

        # Valid raw-string contents
        if(state == LS_RAW_QUOTED){
            # Check for closing delimiter
            if(ch == raw_close){
                quote_pos <- i + raw_dash_count + 1L
                if(quote_pos <= n){
                    dashes_match <- TRUE
                    if(raw_dash_count > 0L){
                        dash_positions <- seq.int(
                            i + 1L,
                            i + raw_dash_count
                        )
                        dashes_match <- all(
                            chars[dash_positions] == "-"
                        )
                    }

                    if(
                        dashes_match &&
                        chars[quote_pos] == raw_quote
                    ){
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
        # Consider the rest of the line as invalid, then reset to code
        if(state == LS_RAW_INVALID){
            if(ch %in% c("\n", "\r")){
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

        # Special infix %operators%
        # Anything (but linebreaks) between two percent signs is allowed
        # We recover to code state on unexpected linebreaks
        if(state == LS_SPECIAL_OPERATOR){
            if(ch == "%"){
                add_region(
                    LS_SPECIAL_OPERATOR,
                    state_start,
                    i + 1L
                )
                state <- LS_CODE
                state_start <- NA_integer_
            } else if(ch %in% c("\n", "\r")){
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
        if(state == LS_COMMENT){
            if(ch %in% c("\n", "\r")){
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
        # (Should only throw if new states are introduced and not handled above.)
        stopifnot(state == LS_CODE)

        # Special infix operator. Its first following percent sign terminates
        # the token, so %% and %/% naturally use the same state as custom
        # operators.
        if(ch == "%"){
            state <- LS_SPECIAL_OPERATOR
            state_start <- i
            i <- i + 1L
            next
        }

        # Comment.
        if(ch == "#"){
            state <- LS_COMMENT
            state_start <- i
            i <- i + 1L
            next
        }

        # Potential raw-string prefix.
        if(
            ch %in% c("r", "R") &&
            i < n
        ){
            # Avoid recognizing the r/R in an ordinary identifier (e.g. foobar")
            previous_is_name_char <- (
                i > 1L
                && .completion_is_name_char(chars[i - 1L])
            )

            next_ch <- chars[i + 1L]
            next_is_quote <- next_ch %in% c("\"", "'")

            if(!previous_is_name_char && next_is_quote){
                state <- LS_RAW_PREFIX
                state_start <- i
                raw_quote <- next_ch
                raw_dash_count <- 0L
                raw_close <- NA_character_
                # Consume r" / R" / r' / R'.
                i <- i + 2L
                next
            }
        }

        # Ordinary quoted strings.
        if(ch %in% names(.state_by_quote)){
            state <- .state_by_quote[[ch]]
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
    if(state != LS_CODE){
        add_region(
            state,
            state_start,
            n + 1L
        )
    }

    list(
        state = state,
        regions = regions
    )
}
