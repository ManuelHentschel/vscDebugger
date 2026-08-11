
# Keep only text before the first whitespace to the right.
.completion_available_right_text <- function(text_after_cursor){
  whitespace <- regexpr("[[:space:]]", text_after_cursor)
  if(whitespace == -1L){
    return(text_after_cursor)
  }
  substr(text_after_cursor, 1L, whitespace - 1L)
}

# Reuse a matching right-hand suffix when the full name fragment matches.
.completion_trim_right_overlap <- function(insertion, available_right_text){
  max_overlap <- min(nchar(insertion), nchar(available_right_text))
  if(max_overlap == 0L){
    return(insertion)
  }

  matched_overlap <- 0L
  for(overlap in rev(seq_len(max_overlap))){
    insertion_suffix <- substr(
      insertion,
      nchar(insertion) - overlap + 1L,
      nchar(insertion)
    )
    right_prefix <- substr(available_right_text, 1L, overlap)
    if(insertion_suffix == right_prefix){
      matched_overlap <- overlap
      break
    }
  }

  # Reuse only complete name fragments; otherwise insert the full text.
  right_name <- sub("[^[:alnum:]_.].*$", "", available_right_text)
  if(matched_overlap < nchar(right_name)){
    return(insertion)
  }
  substr(insertion, 1L, nchar(insertion) - matched_overlap)
}

# Spell a candidate as valid R code using the partial child's quote style.
.completion_candidate_text <- function(
  child_name,
  accessor,
  quote
){
  if(identical(accessor, "(")){
    name_text <- if(make.names(child_name) == child_name){
      child_name
    } else{
      encodeString(child_name, quote = "`")
    }
    equals <- if(child_name == "..."){
      ""
    } else if(isTRUE(getOption("vsc.completionsFunctionArgumentSpaces", FALSE))){
      " = "
    } else{
      "="
    }
    return(paste0(name_text, equals))
  }
  if(!is.null(quote)){
    return(encodeString(child_name, quote = quote))
  } else if(!is.null(accessor) && accessor %in% c("[", "[[")){
    return(encodeString(child_name, quote = "\""))
  } else if(make.names(child_name) == child_name){
    return(child_name)
  }
  encodeString(child_name, quote = "`")
}

# Build one typed candidate without bypassing the resolver's binding guards.
.completion_candidate_from_binding <- function(
  name,
  environment,
  is_namespace
){
  candidate <- list(name = name)
  value <- tryCatch(
    list(value = .completion_read_binding(
      name,
      environment,
      is_namespace
    )),
    error = function(error) NULL
  )
  if(is.null(value)){
    # "event" not officially supported but renders a lightning bolt
    candidate$type <- "event"
  } else if(is.function(value$value)){
    candidate$type <- "function"
  } else{
    candidate$type <- "variable"
  }
  candidate
}

# Build typed candidates from bindings in one environment.
.completion_environment_candidates <- function(
  environment,
  names = NULL,
  partial_name = "",
  is_namespace = FALSE
){
  if(is.null(names)){
    names <- ls(environment, all.names = TRUE, sorted = FALSE)
  }
  names <- as.character(names)
  names <- names[!is.na(names) & startsWith(names, partial_name)]
  lapply(
    names,
    .completion_candidate_from_binding,
    environment = environment,
    is_namespace = is_namespace
  )
}

# Build uniform candidates from known names.
.completion_candidates_from_names <- function(
  names,
  type = "variable",
  partial_name = ""
){
  names <- as.character(names)
  names <- names[!is.na(names) & startsWith(names, partial_name)]
  lapply(names, function(name){
    list(name = name, type = type)
  })
}

# Generate module candidates for installed package namespaces.
.completion_namespace_candidates <- function(partial_name){
  pkgs <- .packages(all.available = TRUE)
  .completion_candidates_from_names(pkgs, "module", partial_name)
}

# Generate candidates from a namespace's lazy-loaded datasets.
.completion_lazy_data_candidates <- function(namespace, partial_name){
  lazy_data <- tryCatch(
    getNamespaceInfo(namespace, "lazydata"),
    error = function(error) list()
  )
  .completion_candidates_from_names(
    ls(lazy_data, all.names = TRUE, sorted = FALSE),
    "variable",
    partial_name
  )
}

# Generate candidates from the formal arguments of a function.
.completion_function_argument_candidates <- function(function_, partial_name){
  .completion_candidates_from_names(
    names(formals(function_)),
    "property",
    partial_name
  )
}

# Generate candidates from attached search-path environments.
.completion_search_path_candidates <- function(partial_name){
  namespace_environments <- lapply(
    setdiff(search(), ".GlobalEnv"),
    as.environment
  )
  candidates <- lapply(namespace_environments, function(environment){
    .completion_environment_candidates(
      environment,
      partial_name = partial_name,
      is_namespace = TRUE
    )
  })
  unlist(candidates, recursive = FALSE, use.names = FALSE)
}

# Generate candidates for language wide constants
.completion_constant_candidates <- function(partial_name){
  constant_items <- .completion_candidates_from_names(
    c(
      "TRUE", "FALSE", "NULL", "NA", "NA_integer_", "NA_real_",
      "NA_complex_", "NA_character_", "Inf", "NaN"
    ),
    "constant",
    partial_name
  )
  keyword_items <- .completion_candidates_from_names(
    c("if", "else", "repeat", "while", "function", "for", "in", "next", "break"),
    "keyword",
    partial_name
  )
  # Mark them as constants so they don't get escaped with backticks
  lapply(c(constant_items, keyword_items), function(item){
    item$is_constant <- TRUE
    item
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
  text_after_cursor
){
  candidates <- NULL

  if(is.null(accessor)){
    # Top-level expressions use frame/global bindings and the search path.
    candidates <- unlist(lapply(
      context,
      .completion_environment_candidates,
      partial_name = partial_name
    ), recursive = FALSE, use.names = FALSE)
    candidates <- c(
      candidates,
      .completion_search_path_candidates(partial_name),
      .completion_namespace_candidates(partial_name),
      .completion_constant_candidates(partial_name)
    )
  } else if(accessor == "::"){
    candidates <- .completion_environment_candidates(
      context,
      getNamespaceExports(context),
      partial_name = partial_name,
      is_namespace = TRUE
    )
    # Add lazy data (accessible through `::`, but not in namespace)
    candidates <- c(
      candidates,
      .completion_lazy_data_candidates(context, partial_name)
    )
  } else if(accessor == ":::"){
    candidates <- .completion_environment_candidates(
      context,
      partial_name = partial_name,
      is_namespace = TRUE
    )
  } else if(accessor == "(" && is.function(context)){
    candidates <- .completion_function_argument_candidates(
      context,
      partial_name
    )
  } else if(is.environment(context)){
    if(accessor == "["){
      candidates <- list()
    } else if(accessor == "$"){
      candidates <- .completion_environment_candidates(
        context,
        partial_name = partial_name
      )
    } else if(accessor == "[["){
      candidates <- .completion_candidates_from_names(
        ls(context, all.names = TRUE, sorted = FALSE),
        type = "value",
        partial_name = partial_name
      )
    }
  } else if(accessor == "@"){
    candidates <- .completion_candidates_from_names(
      utils::.Atnames(context),
      type = "field",
      partial_name = partial_name
    )
  } else if(accessor == "$"){
    candidates <- .completion_candidates_from_names(
      utils::.DollarNames(context),
      type = "field",
      partial_name = partial_name
    )
  } else if(accessor %in% c("[", "[[")){
    candidates <- .completion_candidates_from_names(
      attr(context, "names", exact = TRUE),
      type = "value",
      partial_name = partial_name
    )
  }

  # Any unsupported accessor/context combination
  if(is.null(candidates)){
    return(list())
  }

  # Discard unusable candidate names before spelling them as R code.
  candidates <- Filter(function(candidate){
    !is.na(candidate$name) &&
    nzchar(candidate$name)
  }, candidates)

  # Consider right-hand text up to first whitespace
  available_right_text <- .completion_available_right_text(text_after_cursor)

  # Build DAP items, omitting only exact no-op overlaps.
  items <- lapply(candidates, function(candidate){
    child_name <- candidate$name
    if(isTRUE(candidate$is_constant)){
      escaped_text <- child_name
    } else{
      escaped_text <- .completion_candidate_text(child_name, accessor, quote)
    }
    label_text <- escaped_text
    sort_text <- label_text
    # If a candidate starts with a non-letter move it to the end of the list
    if(!grepl("^[[:alpha:]]", label_text)){
      sort_text <- paste0("zzz", label_text)
    }
    trimmed_text <- .completion_trim_right_overlap(
      escaped_text,
      available_right_text
    )
    if(nchar(trimmed_text) == 0L){
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
  items <- Filter(Negate(is.null), items)

  # Keep each fully constructed DAP item only once.
  items[!duplicated(items)]
}
