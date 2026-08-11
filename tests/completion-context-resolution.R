source(file.path("R", "completion_lexing_forward.R"))
source(file.path("R", "completion_lexing_backward.R"))
source(file.path("R", "completion_context_parsing.R"))
source(file.path("R", "stackTreeHelpers.R"))
source(file.path("R", "completion_context_resolution.R"))
source(file.path("R", "completion_candidates.R"))
source(file.path("R", "completion_main.R"))

# Load the package's native promise inspection without installing the package.
if (!("vscDebugger" %in% names(getLoadedDLLs()))) {
    dyn.load(file.path("src", paste0("vscDebugger", .Platform$dynlib.ext)))
}
isPromise <- function(name, environment) {
    .Call(
        "c_is_promise",
        as.name(name),
        environment,
        PACKAGE = "vscDebugger"
    )
}
getPromiseInfo <- function(name, environment) {
    .Call(
        "c_promise_info",
        as.name(name),
        environment,
        PACKAGE = "vscDebugger"
    )
}

show_candidates <- function(
    text,
    firstenv,
    lastenv,
    preview_promises = FALSE
) {
    old_options <- options(vsc.previewPromises = preview_promises)
    on.exit(options(old_options))
    forward <- lex_forward(text)
    backward <- lex_backward(text, forward)
    items <- completion_main(
        text,
        firstenv = firstenv,
        lastenv = lastenv
    )
    labels <- vapply(items, `[[`, "", "label")
    cat("text:    ", dQuote(text), "\n", sep = "")
    cat("status:  ", backward$status, sep = "")
    if (!is.null(backward$reason)) {
        cat(": ", backward$reason, sep = "")
    }
    cat("\n")
    cat(
        "matches: ",
        if (length(labels)) {
            paste(labels, collapse = ", ")
        } else {
            "<none>"
        },
        "\n",
        sep = ""
    )
    invisible(items)
}

lastenv <- new.env(parent = baseenv())
firstenv <- new.env(parent = lastenv)
lastenv$parent_list <- list(parent_child = 1L)
firstenv$my_list <- list(
    child = list(alpha = 1L, alphabet = 2L, beta = 3L),
    "a$b" = list(alpha = 1L),
    other = 4L
)
firstenv$indices <- c(2L, 1L)
firstenv$grid <- matrix(1:4, nrow = 2L)
firstenv$foo_index <- 3L
firstenv$mean_global <- 5L
firstenv$`[[` <- "not a function"
delayedAssign(
    "promised_list",
    my_list,
    eval.env = firstenv,
    assign.env = firstenv
)
delayedAssign(
    "promised_indices",
    indices,
    eval.env = firstenv,
    assign.env = firstenv
)
firstenv$promise_was_evaluated <- FALSE
delayedAssign(
    "unsafe_promise",
    {
        promise_was_evaluated <- TRUE
        list(alpha = 1L)
    },
    eval.env = firstenv,
    assign.env = firstenv
)
makeActiveBinding(
    "active_list",
    function(value) {
        stop("active binding was evaluated")
    },
    firstenv
)

cat("\n", strrep("=", 72L), "\nedge paths\n", sep = "")
show_candidates("", firstenv, lastenv)
show_candidates(" ", firstenv, lastenv)
show_candidates("my_list ", firstenv, lastenv)
show_candidates("my_list)", firstenv, lastenv)
show_candidates("my_list}", firstenv, lastenv)
show_candidates('"done"', firstenv, lastenv)
show_candidates('r"---[done]---"', firstenv, lastenv)
show_candidates('"mea', firstenv, lastenv)
show_candidates("'mea", firstenv, lastenv)
show_candidates("`mea", firstenv, lastenv)
show_candidates('r"---', firstenv, lastenv)
show_candidates('r"---[done', firstenv, lastenv)
show_candidates("$foo", firstenv, lastenv)
show_candidates("@foo", firstenv, lastenv)
show_candidates("::foo", firstenv, lastenv)
show_candidates('[["foo', firstenv, lastenv)
show_candidates("[foo", firstenv, lastenv)
show_candidates("[[foo", firstenv, lastenv)
show_candidates("my_list[[foo", firstenv, lastenv)
show_candidates("my_list$", firstenv, lastenv)
show_candidates("value+", firstenv, lastenv)
show_candidates("value %in%", firstenv, lastenv)
show_candidates("value\n", firstenv, lastenv)
show_candidates("some_function(", firstenv, lastenv)
show_candidates("some_function(my", firstenv, lastenv)

cat("\n", strrep("=", 72L), "\nplain and nested access\n", sep = "")
show_candidates("my_list$child$al", firstenv, lastenv)
show_candidates("something + my_list$child$al", firstenv, lastenv)
show_candidates('something + my_list[["a$b"]]$al', firstenv, lastenv)
show_candidates('my_list[["ch', firstenv, lastenv)
show_candidates('something + my_list[["ch', firstenv, lastenv)
show_candidates('my_list[["child"]]$be', firstenv, lastenv)
show_candidates("parent_list$par", firstenv, lastenv)

cat("\n", strrep("=", 72L), "\nnested index AST resolution\n", sep = "")
parsed <- parse_completion_context("grid[indices[1], indices[2]]")
stopifnot(parsed$status == "success")
resolved <- resolve_completion_context(
    parsed$ast,
    firstenv = firstenv,
    lastenv = lastenv
)
cat("AST:      ", paste0(deparse(parsed$ast), collapse = ""), "\n", sep = "")
cat(
    "resolved: ",
    if (resolved$status == "success") resolved$value else resolved$reason,
    "\n",
    sep = ""
)

parsed <- parse_completion_context(
    "grid[promised_indices[1], indices[2]]"
)
stopifnot(parsed$status == "success")
for (preview in c(FALSE, TRUE)) {
    old_options <- options(vsc.previewPromises = preview)
    resolved <- resolve_completion_context(
        parsed$ast,
        firstenv = firstenv,
        lastenv = lastenv
    )
    options(old_options)
    cat(
        "promised index, preview=", preview, ": ",
        if (resolved$status == "success") resolved$value else resolved$reason,
        "\n",
        sep = ""
    )
}

cat("\n", strrep("=", 72L), "\npromise handling\n", sep = "")
show_candidates("promised_list$ch", firstenv, lastenv)
show_candidates("promised_list$ch", firstenv, lastenv, TRUE)
show_candidates("unsafe_promise$al", firstenv, lastenv, TRUE)
cat(
    "unsafe promise code evaluated: ",
    firstenv$promise_was_evaluated,
    "\n",
    sep = ""
)
show_candidates("active_list$al", firstenv, lastenv)

cat("\n", strrep("=", 72L), "\nconfigurable environment boundary\n", sep = "")
show_candidates("mea", .GlobalEnv, .GlobalEnv)
show_candidates(
    "mea",
    .GlobalEnv,
    .GlobalEnv
)

cat("\n", strrep("=", 72L), "\nnamespace completion\n", sep = "")
show_candidates("stats::l", firstenv, lastenv)
show_candidates('"stats"::l', firstenv, lastenv)
