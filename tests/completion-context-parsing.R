source(file.path("R", "completion_forward_lexing.R"))
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
        parsed <- parse_completion_context(split$context)
        if (parsed$ok) {
            cat("AST:           ", format_ast(parsed$ast), "\n", sep = "")
        } else {
            cat(
                "AST:           <", parsed$reason, "> ", parsed$message,
                "\n",
                sep = ""
            )
        }
    }
}

examples <- list(
    "plain name" = "foo",
    "dollar accessor" = "my_list$chi",
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

normalized <- parse_completion_context("my_list$child")$ast
cat(
    "\nnormalized $ child type: ",
    typeof(normalized[[3L]]),
    "\n",
    sep = ""
)
