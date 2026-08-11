# Keep only text before the first whitespace to the right.
.completion_available_right_text <- function(text_after_cursor) {
    whitespace <- regexpr("[[:space:]]", text_after_cursor)
    if (whitespace == -1L) {
        return(text_after_cursor)
    }
    substr(text_after_cursor, 1L, whitespace - 1L)
}

# Reuse a matching right-hand suffix when the full name fragment matches.
.completion_trim_right_overlap <- function(insertion, available_right_text) {
    max_overlap <- min(nchar(insertion), nchar(available_right_text))
    if (max_overlap == 0L) {
        return(insertion)
    }

    matched_overlap <- 0L
    for (overlap in rev(seq_len(max_overlap))) {
        insertion_suffix <- substr(
            insertion,
            nchar(insertion) - overlap + 1L,
            nchar(insertion)
        )
        right_prefix <- substr(available_right_text, 1L, overlap)
        if (insertion_suffix == right_prefix) {
            matched_overlap <- overlap
            break
        }
    }

    # Reuse only complete name fragments; otherwise insert the full text.
    right_name <- sub("[^[:alnum:]_.].*$", "", available_right_text)
    if (matched_overlap < nchar(right_name)) {
        return(insertion)
    }
    substr(insertion, 1L, nchar(insertion) - matched_overlap)
}

# Spell a candidate as valid R code using the partial child's quote style.
.completion_candidate_text <- function(
    child_name,
    accessor,
    quote
) {
    if (!is.null(quote)) {
        return(encodeString(child_name, quote = quote))
    } else if (!is.null(accessor) && accessor %in% c("[", "[[")) {
        return(encodeString(child_name, quote = "\""))
    } else if (make.names(child_name) == child_name) {
        return(child_name)
    }
    encodeString(child_name, quote = "`")
}

# Build typed candidates from bindings in one environment.
.completion_environment_candidates <- function(
    environment,
    names = NULL,
    compute_types = TRUE
) {
    if (is.null(names)) {
        names <- ls(environment, all.names = TRUE, sorted = FALSE)
    }

    lapply(names, function(name) {
        candidate <- list(name = name)
        if (!compute_types) {
            return(candidate)
        }

        # Do not force promises or active bindings just to choose an icon.
        if (bindingIsActive(name, environment) || isPromise(name, environment)) {
            candidate$type <- "event"
        } else if (is.function(get(name, envir = environment))) {
            candidate$type <- "function"
        } else {
            candidate$type <- "variable"
        }
        candidate
    })
}

# Build uniform candidates from known names.
.completion_candidates_from_names <- function(
    names,
    type = 'variable'
) {
    lapply(names, function(name) {
        list(name = name, type = type)
    })
}

# Generate items for installed packages
.completion_generate_namespace_candidates <- function() {
    pkgs <- .packages(all.available = TRUE)
    lapply(pkgs, function(pkg) {
        list(
            name = pkg,
            type = "module"
        )
    })
}

# Generate DAP completion items from an already resolved context.
completion_candidates <- function(
    context,
    accessor,
    partial_name,
    quote,
    replacement_start,
    replacement_length,
    text_after_cursor,
    compute_types = TRUE
) {
    candidates <- NULL

    if (is.null(accessor)) {
        # Top-level expression, context is list of available environments.
        candidates <- unlist(lapply(
            context,
            .completion_environment_candidates,
            compute_types = compute_types
        ), recursive = FALSE, use.names = FALSE)
        candidates <- c(candidates, .completion_generate_namespace_candidates())
    } else if (accessor == "::") {
        candidates <- .completion_environment_candidates(
            context,
            getNamespaceExports(context),
            compute_types
        )
    } else if (accessor == ":::") {
        candidates <- .completion_environment_candidates(
            context,
            compute_types = compute_types
        )
    } else if (is.environment(context)) {
        if (accessor == "[") {
            candidates <- list()
        } else if(accessor == "$") {
            candidates <- .completion_environment_candidates(
                context,
                compute_types = compute_types
            )
        } else if(accessor == "[[") {
            candidates <- .completion_candidates_from_names(
                ls(context, all.names = TRUE, sorted = FALSE),
                type = "value"
            )
        }
    } else if (accessor == "@") {
        candidates <- .completion_candidates_from_names(
            utils::.Atnames(context),
            type = "field"
        )
    } else if (accessor == "$") {
        candidates <- .completion_candidates_from_names(
            utils::.DollarNames(context),
            type = "field"
        )
    } else if (accessor %in% c("[", "[[")) {
        candidates <- .completion_candidates_from_names(
            attr(context, "names", exact = TRUE),
            type = "value"
        )
    }

    # Any unsupported accessor/context combination
    if (is.null(candidates)) {
        return(list())
    }

    # Keep usable candidates matching the typed prefix, once per name/type pair.
    candidates <- Filter(function(candidate) {
        !is.na(candidate$name) &&
        nzchar(candidate$name) &&
        startsWith(candidate$name, partial_name)
    }, candidates)
    candidate_keys <- vapply(candidates, function(candidate) {
        paste(c(candidate$type, candidate$name), collapse = "\r")
    }, "")
    candidates <- candidates[!duplicated(candidate_keys)]

    # Consider right-hand text up to first whitespace
    available_right_text <- .completion_available_right_text(text_after_cursor)

    # Build DAP items, omitting only exact no-op overlaps.
    items <- lapply(candidates, function(candidate) {
        child_name <- candidate$name
        escaped_text <- .completion_candidate_text(child_name, accessor, quote)
        label_text <- escaped_text
        sort_text <- label_text
        # If a candidate starts with a non-letter move it to the end of the list
        if (!grepl("^[[:alpha:]]", label_text)) {
            sort_text <- paste0("zzz", label_text)
        }
        trimmed_text <- .completion_trim_right_overlap(
            escaped_text,
            available_right_text
        )
        if (nchar(trimmed_text) == 0L) {
            return(NULL)
        }
        item <- list(
            label = label_text,
            text = trimmed_text,
            sortText = sort_text,
            type = candidate$type,
            # DAP says 1-based, but vscode interprets 0-based for `start`
            # (Temporary?) fix by converting to 0-based
            start = replacement_start - 1L,
            length = replacement_length
        )
        item
    })
    Filter(Negate(is.null), items)
}
