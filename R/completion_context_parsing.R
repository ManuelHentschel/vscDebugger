# Splits a lexed completion suffix and validates its context as a safe AST.
COMPLETION_ACCESSORS <- c(":::", "::", "[[", "$", "@", "[")
COMPLETION_AST_ACCESSORS <- c("$", "@", "[", "[[", "::", ":::")

completion_context_slice <- function(chars, start, end) {
    if (start >= end) {
        return("")
    }
    paste0(chars[seq.int(start, end - 1L)], collapse = "")
}

split_completion_context <- function(
    text,
    forward = lex_forward(text),
    start = 1L,
    end = NULL
) {
    stopifnot(
        is.character(text),
        length(text) == 1L,
        !is.na(text),
        is.list(forward)
    )

    chars <- strsplit(text, "", fixed = TRUE)[[1L]]
    n <- length(chars)
    if (is.null(end)) {
        end <- n + 1L
    }
    stopifnot(
        length(start) == 1L,
        length(end) == 1L,
        !is.na(start),
        !is.na(end),
        start >= 1L,
        start <= end,
        end <= n + 1L
    )
    if (start == end) {
        return(list(
            context = "",
            accessor = NULL,
            partial_child = "",
            accessor_start = NA_integer_,
            accessor_end = NA_integer_,
            partial_start = start,
            selection_start = start,
            selection_end = end
        ))
    }

    opaque <- rep(FALSE, n)

    # Ignore accessor-like characters inside lexical regions.
    for (region in forward$regions) {
        positions <- seq.int(region$start, region$end - 1L)
        positions <- positions[positions >= 1L & positions <= n]
        opaque[positions] <- TRUE
    }

    accessor <- NULL
    accessor_start <- NA_integer_
    accessor_end <- NA_integer_

    # Find the right-most accessor, preferring longer tokens at each endpoint.
    for (candidate_end in rev(seq.int(start + 1L, end))) {
        if (opaque[candidate_end - 1L]) {
            next
        }
        for (candidate in COMPLETION_ACCESSORS) {
            candidate_start <- candidate_end - nchar(candidate)
            if (
                candidate_start >= start &&
                completion_context_slice(
                    chars,
                    candidate_start,
                    candidate_end
                ) == candidate
            ) {
                accessor <- candidate
                accessor_start <- candidate_start
                accessor_end <- candidate_end
                break
            }
        }
        if (!is.null(accessor)) {
            break
        }
    }

    if (is.null(accessor)) {
        context <- ""
        partial_child <- completion_context_slice(chars, start, end)
        partial_start <- start
    } else {
        context <- completion_context_slice(chars, start, accessor_start)
        partial_child <- completion_context_slice(
            chars,
            accessor_end,
            end
        )
        partial_start <- accessor_end
    }

    list(
        context = context,
        accessor = accessor,
        partial_child = partial_child,
        accessor_start = accessor_start,
        accessor_end = accessor_end,
        partial_start = partial_start,
        selection_start = start,
        selection_end = end
    )
}

completion_ast_name <- function(node, role) {
    if (is.name(node)) {
        return(as.character(node))
    }
    if (is.character(node) && length(node) == 1L && !is.na(node)) {
        return(node)
    }
    stop(role, " must be a name or string")
}

completion_normalize_ast <- function(node) {
    if (is.null(node) || is.atomic(node) || is.name(node)) {
        return(node)
    }
    if (!is.call(node) || !is.name(node[[1L]])) {
        stop("The context contains an unsupported expression")
    }

    operator <- as.character(node[[1L]])
    if (!(operator %in% COMPLETION_AST_ACCESSORS)) {
        stop("Function or operator call is not allowed: ", operator)
    }

    if (operator %in% c("$", "@")) {
        if (length(node) != 3L) {
            stop(operator, " must have exactly two operands")
        }
        return(as.call(list(
            as.name(operator),
            completion_normalize_ast(node[[2L]]),
            completion_ast_name(node[[3L]], "Accessor child")
        )))
    }

    if (operator %in% c("::", ":::")) {
        if (length(node) != 3L) {
            stop(operator, " must have exactly two operands")
        }
        return(as.call(list(
            as.name(operator),
            completion_ast_name(node[[2L]], "Package"),
            completion_ast_name(node[[3L]], "Namespace child")
        )))
    }

    parts <- as.list(node)
    if (length(parts) < 2L || (operator == "[[" && length(parts) < 3L)) {
        stop("Invalid ", operator, " expression")
    }
    argument_names <- names(parts)
    if (
        !is.null(argument_names) &&
        length(argument_names) > 2L &&
        any(nzchar(argument_names[-seq_len(2L)]))
    ) {
        stop("Named indexing arguments are not supported")
    }

    parent <- completion_normalize_ast(parts[[2L]])
    indices <- lapply(parts[-seq_len(2L)], completion_normalize_ast)
    as.call(c(list(as.name(operator), parent), indices))
}

parse_completion_context <- function(context) {
    stopifnot(
        is.character(context),
        length(context) == 1L,
        !is.na(context)
    )

    tryCatch({
        parsed <- parse(text = context, keep.source = FALSE)
        if (length(parsed) != 1L) {
            stop("The context must contain exactly one expression")
        }
        list(
            ok = TRUE,
            ast = completion_normalize_ast(parsed[[1L]]),
            reason = NULL,
            message = NULL
        )
    }, error = function(error) {
        list(
            ok = FALSE,
            ast = NULL,
            reason = "invalid_context",
            message = error$message
        )
    })
}
