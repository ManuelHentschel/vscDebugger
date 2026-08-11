source(file.path("R", "completion_lexing_forward.R"))
source(file.path("R", "completion_lexing_backward.R"))
source(file.path("R", "completion_context_parsing.R"))
source(file.path("R", "stackTreeHelpers.R"))
source(file.path("R", "completion_context_resolution.R"))
source(file.path("R", "completion_candidates.R"))
source(file.path("R", "completion.R"))

# Keep these examples independent of the installed package and browser stack.
isPromise <- function(name, environment) FALSE
isCalledFromBrowser <- function() FALSE
logCat <- function(...) invisible()

firstenv <- new.env(parent = emptyenv())
firstenv$my_list <- list(
    child = 1L,
    'a"b' = 2L,
    "a b" = 3L,
    "aaa" = 4L,
    "aab" = 5L,
    "my item" = 6L
)
firstenv$mean_global <- 4L
firstenv$x <- 99
firstenv$unnamed <- unname(list(1L, 2L))
firstenv$overlap_list <- list(child = 1L, chili = 2L)
firstenv$my_function <- function(alpha, beta = 1L, ...) NULL

format_source <- function(text) {
    encodeString(text, quote = "\"")
}

print_items <- function(items) {
    if (!length(items)) {
        cat("  <none>\n")
        return(invisible())
    }
    for (item in items) {
        cat(
            "  ",
            format_source(item$label),
            " -> insert ",
            format_source(item$text),
            " [start=", item$start,
            ", length=", item$length,
            "]\n",
            sep = ""
        )
    }
    invisible()
}

show_items <- function(description, text, text_after_cursor = "") {
    items <- .completion_items_from_text(
        text,
        firstenv = firstenv,
        lastenv = firstenv,
        text_after_cursor = text_after_cursor
    )
    cat("\n", description, "\n", sep = "")
    cat(
        "  cursor: ",
        format_source(paste0(text, "|", text_after_cursor)),
        "\n",
        sep = ""
    )
    print_items(items)
    invisible(items)
}

cat("\nempty input\n")
items <- show_items("global suggestions for empty input", "")
stopifnot("mean_global" %in% vapply(items, `[[`, "", "label"))

cat("quoted and code completion items\n")
show_items("quoted child without a closing quote", 'my_list[["ch')
show_items("quoted child with an existing closing quote", 'my_list[["ch', '"')
show_items("single-quoted child", "my_list[['a")
show_items("escaped quote in a child name", 'my_list[["a\\"')
show_items("backtick-quoted global name", "`mea")
show_items("UTF-16 offset after an astral character", "😀 + mea")

cat("\nright-side reuse\n")
show_items("reuse remaining name characters", "my_list$ch", "ild")
show_items(
    "do not reuse a space inside a quoted name",
    'my_list[["my',
    ' item"]]'
)

cat("\nDebug from UI\n")
show_items("ex1", "my_list[[\"a")
show_items("ex2", "my_list[[\"a", "\"")

cat("\nempty index expressions\n")
stopifnot(!length(show_items("bracket without a receiver", "[")))
stopifnot(!length(show_items("double bracket without a receiver", "[[")))

items <- show_items("named single-bracket index", "my_list[")
labels <- vapply(items, `[[`, "", "label")
child <- items[[which(labels == '"child"')]]
stopifnot(child$text == '"child"')
stopifnot("mean_global" %in% labels)

items <- show_items("named double-bracket index", "my_list[[")
child <- items[[which(vapply(items, `[[`, "", "label") == '"child"')]]
stopifnot(child$text == '"child"')

items <- show_items("unnamed index starts a new expression", "unnamed[")
stopifnot("mean_global" %in% vapply(items, `[[`, "", "label"))

items <- show_items("nonempty index stays an expression", "my_list[[mea")
stopifnot("mean_global" %in% vapply(items, `[[`, "", "label"))

cat("\nfunction arguments\n")
items <- show_items("empty first argument", "my_function(")
labels <- vapply(items, `[[`, "", "label")
stopifnot(all(c("alpha=", "beta=", "...", "mean_global") %in% labels))

items <- show_items("partial first argument", "my_function(be")
stopifnot("beta=" %in% vapply(items, `[[`, "", "label"))

items <- show_items("unnamed argument expression", "my_function(mea")
stopifnot("mean_global" %in% vapply(items, `[[`, "", "label"))

items <- show_items("attached-package function", "install.packages(")
stopifnot("pkgs=" %in% vapply(items, `[[`, "", "label"))

old_options <- options(vsc.completionsFunctionArgumentSpaces = TRUE)
items <- show_items("spaced argument insertion", "my_function(al")
options(old_options)
argument_item <- Filter(function(item) item$label == "alpha=", items)[[1L]]
stopifnot(argument_item$text == "alpha = ")

cat("\nrequest cursor handling\n")
my_list <- firstenv$my_list
overlap_list <- firstenv$overlap_list
request_text <- 'first\r\nmy_list[["ch"]]'
cursor <- .completion_split_request_text(request_text, 2L, 13L)
items <- .completion_items_from_request(
    0L,
    request_text,
    13L,
    2L
)
cat(
    "  cursor: ",
    format_source(paste0(
        cursor$before_cursor,
        "|",
        cursor$after_cursor
    )),
    "\n",
    sep = ""
)
print_items(items)
stopifnot(
    items[[1L]]$label == '"child"',
    items[[1L]]$text == '"child',
    items[[1L]]$start == 16L,
    items[[1L]]$length == 3L
)

# UTF-16 cursor positions must not split a surrogate pair.
stopifnot(
    is.null(.completion_split_request_text("😀", 1L, 2L)),
    .completion_split_request_text("😀", 1L, 3L)$before_cursor == "😀"
)

# Reuse the part of a completion that is already present after the cursor.
mean_global <- firstenv$mean_global
items <- .completion_items_from_request(0L, "mean_global", 4L, 1L)
stopifnot(items[[1L]]$text == "mea")

items <- .completion_items_from_request(0L, "my_list$child", 11L, 1L)
stopifnot(items[[1L]]$text == "ch")

# Reuse safe overlaps and keep the full text for other candidates.
items <- .completion_items_from_request(
    0L,
    "overlap_list$child",
    nchar("overlap_list$ch") + 1L,
    1L
)
labels <- vapply(items, `[[`, "", "label")
stopifnot(
    identical(labels, c("child", "chili")),
    items[[which(labels == "child")]]$text == "ch",
    items[[which(labels == "chili")]]$text == "chili"
)

# DAP treats an empty insertion as the label, so omit only the exact overlap.
items <- .completion_items_from_request(
    0L,
    "overlap_list$child",
    nchar("overlap_list$") + 1L,
    1L
)
stopifnot(
    identical(vapply(items, `[[`, "", "label"), "chili"),
    items[[1L]]$text == "chili"
)

items <- .completion_items_from_request(0L, 'my_list[["child"]]', 13L, 1L)
stopifnot(items[[1L]]$text == '"ch')

# Do not reuse spaces or other expression separators on the right.
items <- .completion_items_from_request(0L, 'my_list[["my item"]]', 13L, 1L)
item <- items[[which(vapply(items, `[[`, "", "label") == '"my item"')]]
stopifnot(item$text == '"my item"')
