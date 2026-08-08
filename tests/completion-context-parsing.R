source(file.path("R", "completion_lexing_forward.R"))
source(file.path("R", "completion_context_parsing.R"))

format_ast <- function(ast) {
    paste0(deparse(ast, width.cutoff = 500L), collapse = "")
}

show_context_parsing <- function(text) {
    split <- split_completion_context(text)
    cat("text:          ", dQuote(text), "\n", sep = "")
    cat("context:       ", dQuote(split$context), "\n", sep = "")
    cat(
        "accessor:      ",
        if (is.null(split$accessor)) "<none>" else split$accessor,
        "\n",
        sep = ""
    )
    cat("partial child: ", dQuote(split$partial_child), "\n", sep = "")

    if (!is.null(split$accessor)) {
        parsed <- tryCatch(
            parse_completion_context(split$context),
            error = identity
        )
        if (!inherits(parsed, "error")) {
            cat("AST:           ", format_ast(parsed), "\n", sep = "")
        } else {
            cat(
                "AST:           <invalid context> ", conditionMessage(parsed),
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
    "quoted accessor character is ignored" = 'my_list[["a$b"]]$chi',
    "namespace accessor" = "stats::l",
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
cat(
    "\nnormalized $ child type: ",
    typeof(normalized[[3L]]),
    "\n",
    sep = ""
)
