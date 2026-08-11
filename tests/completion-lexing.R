source(file.path("R", "completion_lexing_forward.R"))
source(file.path("R", "completion_lexing_backward.R"))

STATE_NAME_BY_VALUE <- setNames(
    c(
        "LS_CODE",
        "LS_SINGLE_QUOTED",
        "LS_DOUBLE_QUOTED",
        "LS_BACKTICK",
        "LS_RAW_PREFIX",
        "LS_RAW_QUOTED",
        "LS_RAW_INVALID",
        "LS_COMMENT",
        "LS_SPECIAL_OPERATOR"
    ),
    c(
        LS_CODE,
        LS_SINGLE_QUOTED,
        LS_DOUBLE_QUOTED,
        LS_BACKTICK,
        LS_RAW_PREFIX,
        LS_RAW_QUOTED,
        LS_RAW_INVALID,
        LS_COMMENT,
        LS_SPECIAL_OPERATOR
    )
)

REGION_SYMBOL_BY_STATE <- setNames(
    c("'", "\"", "`", "r", "r", "r", "#", "%"),
    c(
        LS_SINGLE_QUOTED,
        LS_DOUBLE_QUOTED,
        LS_BACKTICK,
        LS_RAW_PREFIX,
        LS_RAW_QUOTED,
        LS_RAW_INVALID,
        LS_COMMENT,
        LS_SPECIAL_OPERATOR
    )
)

split_visual_lines <- function(chars, markers) {
    text_lines <- list(character())
    marker_lines <- list(character())
    line <- 1L
    i <- 1L

    while (i <= length(chars)) {
        ch <- chars[i]

        if (ch %in% c("\n", "\r")) {
            if (ch == "\r" && i < length(chars) && chars[i + 1L] == "\n") {
                i <- i + 1L
            }
            line <- line + 1L
            text_lines[[line]] <- character()
            marker_lines[[line]] <- character()
        } else {
            text_lines[[line]] <- c(text_lines[[line]], ch)
            marker_lines[[line]] <- c(marker_lines[[line]], markers[i])
        }

        i <- i + 1L
    }

    list(
        text = vapply(text_lines, paste0, character(1L), collapse = ""),
        markers = vapply(marker_lines, paste0, character(1L), collapse = "")
    )
}

format_status_value <- function(value) {
    if (is.null(value)) {
        return("<none>")
    }
    if (length(value) == 0L) {
        return("<empty>")
    }
    if (length(value) == 1L && is.na(value)) {
        return("NA")
    }
    if (is.character(value)) {
        return(encodeString(value, quote = "\""))
    }
    as.character(value)
}

completion_suffix <- function(backward) {
    paste0(
        backward$context,
        if (is.null(backward$accessor)) "" else backward$accessor,
        backward$partial_child
    )
}

visualize_lex_forward <- function(
    text,
    lexed = lex_forward(text),
    backward = lex_backward(text, lexed)
) {
    stopifnot(
        is.character(text),
        length(text) == 1L,
        !is.na(text),
        is.list(lexed)
    )

    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    region_markers <- rep(" ", length(chars))
    context_markers <- rep(" ", length(chars))

    for (region in lexed$regions) {
        symbol <- REGION_SYMBOL_BY_STATE[[as.character(region$state)]]
        positions <- seq.int(region$start, region$end - 1L)
        positions <- positions[positions >= 1L & positions <= length(chars)]
        region_markers[positions] <- symbol
    }

    if (backward$status == "candidate") {
        candidate_start <- length(chars) - nchar(completion_suffix(backward)) + 1L
        if (candidate_start <= length(chars)) {
            positions <- seq.int(candidate_start, length(chars))
            positions <- positions[positions >= 1L & positions <= length(chars)]
            context_markers[positions] <- "^"
        }
    }

    # Preserve tab alignment instead of replacing a tab with one marker.
    region_markers[chars == "\t"] <- "\t"
    context_markers[chars == "\t"] <- "\t"
    region_lines <- split_visual_lines(chars, region_markers)
    context_lines <- split_visual_lines(chars, context_markers)
    line_number_width <- nchar(as.character(length(region_lines$text)))

    for (line in seq_along(region_lines$text)) {
        cat(sprintf(
            "%*d | %s\n",
            line_number_width,
            line,
            region_lines$text[line]
        ))
        cat(sprintf(
            "%*s | %s\n",
            line_number_width,
            "R",
            region_lines$markers[line]
        ))
        cat(sprintf(
            "%*s | %s\n",
            line_number_width,
            "C",
            context_lines$markers[line]
        ))
    }

    state_name <- STATE_NAME_BY_VALUE[[as.character(lexed$state)]]
    state_start <- NA_integer_
    if (lexed$state != LS_CODE && length(lexed$regions)) {
        ending_region <- lexed$regions[[length(lexed$regions)]]
        if (ending_region$end == length(chars) + 1L) {
            state_start <- ending_region$start
        }
    }
    cat(
        "state: ", state_name, " (", lexed$state, ")",
        ", start: ", format_status_value(state_start),
        "\n",
        sep = ""
    )

    invisible(lexed)
}

print_lex_backward <- function(text, backward = lex_backward(text)) {
    if (backward$status != "candidate") {
        cat("completion suffix: ", backward$status, sep = "")
        if (!is.null(backward$reason)) {
            cat(": ", backward$reason, sep = "")
        }
        cat("\n")
        return(invisible(backward))
    }

    suffix <- completion_suffix(backward)
    start <- nchar(text) - nchar(suffix) + 1L
    end <- nchar(text) + 1L
    cat(
        "completion suffix: ", format_status_value(suffix),
        " [", start, ", ", end, ")",
        "\n",
        sep = ""
    )
    invisible(backward)
}

examples <- list(
    "empty input" = "",
    "only whitespace" = " ",
    "plain completion" = "foo",
    "plain completion ending in space" = "foo ",
    "separated names ending in space" = "asdf qwer ",
    "else name ending in space" = "else qwer ",
    "member completion" = "x$a$b$fo",
    "space before accessor" = "my_list $child_name",
    "space after accessor" = "my_list$ child_name",
    "member completion ending in space" = "x$a$b$fo ",
    "accessor ending in space" = "x$a$b$ ",
    "namespace completion" = "pkg:::fu",
    "escaped string in accessor" = paste0(
        'foo[["a',
        "\\",
        '"b"]]$bar$ba'
    ),
    "multiline comment and unfinished string" = paste0(
        "my_list[[ # comment\n",
        '    "item_na'
    ),
    "unfinished single-quoted string" = "x <- 'single",
    "unfinished double-quoted string" = 'x <- "double',
    "unfinished quoted string ending in space" = 'x[["item ',
    "unfinished quoted string containing a closing bracket" = 'x[["item]',
    "unfinished backtick containing a closing brace" = "x$`item}",
    "completed string" = 'x[["item"',
    "unfinished backtick name" = "x <- `backtick",
    "complete raw string" = 'r"---[contents]---" + (x)',
    "unfinished raw prefix" = 'r"---',
    "unfinished raw contents" = 'r"---[contents',
    "malformed raw recovery" = paste0('r"--not-an-opener', "\n", "(x"),
    "opaque special operator" = "something %#% complicated + x$a$fo",
    "local chain after surrounding code" = paste0(
        "something %custom% complicated |> code + ",
        'x$a[["b"]]$fo'
    ),
    "unfinished special operator recovery" = paste0(
        "x %unfinished",
        "\n",
        "(obj"
    ),
    "quoted string after delimiter mismatch" = '([)] + "still a string"',
    "delimiter mismatch before completion" = "([)]$foo",
    "unmatched call parenthesis" = "some_function(variable_na",
    "empty unmatched call parenthesis" = "some_function(",
    "bracket without receiver" = "[[foo",
    "empty single-bracket accessor" = "my_list[",
    "empty double-bracket accessor" = "my_list[[",
    "index expression" = "my_list[[foo",
    "completed call" = "get_object()",
    "completed closing brace" = "value}",
    "fresh expression after whitespace" = "value ",
    "fresh expression after comma" = "value,",
    "fresh expression after semicolon" = "value;",
    "fresh expression after opening parenthesis" = "some_function(",
    "fresh expression after opening brace" = "if (TRUE) {",
    "fresh expression after operator" = "value +",
    "fresh expression after special operator" = "value %in%",
    "fresh expression after newline" = "value\n",
    "completed index" = 'x[["item"]]',
    "function call receiver is rejected" = "get_object()$fo",
    "parenthesized receiver stops at closing parenthesis" = "(my_list)$fo",
    "computed index candidate" = "x[[name]]$fo",
    "incomplete %operator%" = "x %incomplete",
    "multidimensional index" = "array[a,b]$fo",
    "function call in index is rejected" = "array[f(a,b)]$fo",
    "multidimensional index stops at comma" = 'matrix[, "co',
    "ellipses" = "99 + list(...)$"
)

for (name in names(examples)) {
    cat("\n", strrep("=", 72L), "\n", name, "\n", sep = "")
    text <- examples[[name]]
    forward <- lex_forward(text)
    backward <- lex_backward(text, forward)
    visualize_lex_forward(text, forward, backward)
    print_lex_backward(text, backward)
}
