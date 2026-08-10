source(file.path("R", "completion_lexing_forward.R"))
source(file.path("R", "completion_lexing_backward.R"))
source(file.path("R", "completion_context_parsing.R"))

format_ast <- function(ast) {
    paste0(deparse(ast, width.cutoff = 500L), collapse = "")
}

show_context_parsing <- function(text) {
    backward <- lex_backward(text)
    cat("text:          ", dQuote(text), "\n", sep = "")
    if (backward$status != "candidate") {
        cat("status:        ", backward$status, sep = "")
        if (!is.null(backward$reason)) {
            cat(": ", backward$reason, sep = "")
        }
        cat("\n")
        return(invisible())
    }

    accessor <- backward$accessor
    cat("context:       ", dQuote(backward$context), "\n", sep = "")
    cat(
        "accessor:      ",
        if (is.null(accessor)) "<none>" else accessor,
        "\n",
        sep = ""
    )
    cat("partial child: ", dQuote(backward$partial_child), "\n", sep = "")

    if (!is.null(accessor)) {
        parsed <- parse_completion_context(backward$context)
        if (parsed$status == "success") {
            cat("AST:           ", format_ast(parsed$ast), "\n", sep = "")
        } else {
            cat(
                "AST:           <invalid context> ", parsed$reason,
                "\n",
                sep = ""
            )
        }
    }
}

examples <- list(
    "empty text" = "",
    "entire incomplete string" = '"foo',
    "plain name" = "foo",
    "accessor without context" = "$foo",
    "quoted accessor without context" = '[["foo',
    "dollar accessor" = "my_list$chi",
    "accessor with empty partial" = "my_list$",
    "nested dollar normalization" = "my_list$child$al",
    "at accessor" = "my_object@slo",
    "quoted double bracket" = 'my_list[["chi',
    "backtick index expression" = "my_list[[`mea",
    "nested incomplete index" = "my_list[[index_list$na",
    "quoted accessor character is ignored" = 'my_list[["a$b"]]$chi',
    "namespace accessor" = "stats::l",
    "quoted namespace accessor" = '"stats"::l',
    "triple namespace accessor" = "stats:::l",
    "nested indices" = "my_list[indices[1], indices[2]]$chi",
    "parenthesized context is rejected" = "(my_list)$chi",
    "function call is rejected" = "get_object()$chi",
    "operator call is rejected" = "my_list[index + 1]$chi"
)

for (name in names(examples)) {
    cat("\n", strrep("=", 72L), "\n", name, "\n", sep = "")
    show_context_parsing(examples[[name]])
}

normalized <- parse_completion_context("my_list$child")
stopifnot(normalized$status == "success")
cat(
    "\nnormalized $ child type: ",
    typeof(normalized$ast[[3L]]),
    "\n",
    sep = ""
)
