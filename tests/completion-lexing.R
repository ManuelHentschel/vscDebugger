source(file.path("R", "completion_lexing.R"))

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

visualize_lex_forward <- function(text, lexed = lex_forward(text)) {
    stopifnot(
        is.character(text),
        length(text) == 1L,
        !is.na(text),
        is.list(lexed)
    )

    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    markers <- rep(" ", length(chars))

    for (region in lexed$regions) {
        symbol <- REGION_SYMBOL_BY_STATE[[as.character(region$state)]]
        positions <- seq.int(region$start, region$end - 1L)
        positions <- positions[positions >= 1L & positions <= length(chars)]
        markers[positions] <- symbol
    }

    # Preserve tab alignment instead of replacing a tab with one marker.
    markers[chars == "\t"] <- "\t"
    lines <- split_visual_lines(chars, markers)
    line_number_width <- nchar(as.character(length(lines$text)))

    for (line in seq_along(lines$text)) {
        cat(sprintf("%*d | %s\n", line_number_width, line, lines$text[line]))
        cat(sprintf("%*s | %s\n", line_number_width, "", lines$markers[line]))
    }

    state_name <- STATE_NAME_BY_VALUE[[as.character(lexed$state)]]
    cat(
        "state: ", state_name, " (", lexed$state, ")",
        ", start: ", format_status_value(lexed$state_start),
        "\n",
        sep = ""
    )

    if (!lexed$delimiters_valid) {
        delimiter_status <- "invalid; stack discarded"
    } else if (length(lexed$delimiter_stack$kind) == 0L) {
        delimiter_status <- "valid; stack empty"
    } else {
        entries <- paste0(
            lexed$delimiter_stack$kind,
            "@",
            lexed$delimiter_stack$start
        )
        delimiter_status <- paste0("valid; stack: ", paste(entries, collapse = " "))
    }
    cat("delimiters: ", delimiter_status, "\n", sep = "")

    if (!is.null(lexed$raw)) {
        raw_status <- vapply(
            names(lexed$raw),
            function(name) {
                paste0(name, "=", format_status_value(lexed$raw[[name]]))
            },
            character(1L)
        )
        cat("raw: ", paste(raw_status, collapse = ", "), "\n", sep = "")
    }

    invisible(lexed)
}

examples <- list(
    "plain completion" = "foo",
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
    "unfinished backtick name" = "x <- `backtick",
    "complete raw string" = 'r"---[contents]---" + (x)',
    "unfinished raw prefix" = 'r"---',
    "unfinished raw contents" = 'r"---[contents',
    "malformed raw recovery" = paste0('r"--not-an-opener', "\n", "(x"),
    "opaque special operator" = "something %#% complicated + x$a$fo",
    "unfinished special operator recovery" = paste0(
        "x %unfinished",
        "\n",
        "(obj"
    ),
    "delimiter mismatch" = '([)] + "still a string"'
)

for (name in names(examples)) {
    cat("\n", strrep("=", 72L), "\n", name, "\n", sep = "")
    visualize_lex_forward(examples[[name]])
}
