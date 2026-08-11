# Count positions in the UTF-16 units used by DAP.
.completion_utf16_length <- function(text) {
  codepoints <- utf8ToInt(enc2utf8(text))
  as.integer(length(codepoints) + sum(codepoints > 0xffffL))
}

# Convert a UTF-16 cursor index to an R character count.
# Both offsets are 1-based. Allows also the first index after the last character.
# NA if index is not a valid UTF-16 offset in the text.
.completion_utf16_to_char_index <- function(text, index) {
  codepoints <- utf8ToInt(enc2utf8(text))
  ends <- cumsum(1L + (codepoints > 0xffffL))
  starts <- c(1L, ends + 1L) # includes start of next codepoints
  match(index, starts)
}

# Split the request at the UTF-16 cursor.
# Returns all text up to the cursor, and the rest of the same line after the cursor.
# Returns NULL if the line or column is invalid.
.completion_split_request_text <- function(text, line, column) {
  # First, split into lines
  breaks <- gregexpr("\r\n|\r|\n", text, perl = TRUE, useBytes = FALSE)[[1L]]
  break_lengths <- attr(breaks, "match.length")
  if (length(breaks) == 1L && breaks[1L] == -1L) {
    # No line breaks -> single line, no breaks
    breaks <- integer()
    break_lengths <- integer()
  }

  # Compute start and end of each line (excluding line break characters)
  # 1-based, excludes linebreaks characters, inclusive start and end
  starts <- c(1L, breaks + break_lengths)
  ends <- c(breaks - 1L, nchar(text))

  # Validate line number
  if (line < 1L || line > length(starts)) {
    return(NULL)
  }

  # Extract line text (empty string for end<start)
  line_text <- substr(text, starts[line], ends[line])

  column_chars <- .completion_utf16_to_char_index(
    line_text,
    column
  )
  if (is.na(column_chars)) {
    return(NULL)
  }

  # Convert the line-relative cursor index to an absolute one.
  cursor_index <- starts[line] + column_chars - 1L
  text_before_cursor <- substr(text, 1L, cursor_index - 1L)
  text_after_cursor <- substr(line_text, column_chars, nchar(line_text))

  list(before_cursor = text_before_cursor, after_cursor = text_after_cursor)
}

# Main function to run the completion stages and return DAP completion items.
.completion_items_from_text <- function(
  text,
  firstenv = parent.frame(),
  lastenv = .GlobalEnv,
  text_after_cursor = ""
) {
  # Select and split the expression suffix before the cursor.
  forward <- lex_forward(text)
  backward <- lex_backward(text, forward)
  if (backward$status != "candidate") {
    return(list())
  }

  # If partial child is quoted, parse it to validate and resolve escapes
  partial <- .completion_parse_partial_child(backward$partial_child)
  if (is.null(partial)) {
    return(list())
  }

  # Provide the range of the complete partial child as replacement range
  # (including quotes)
  replacement_length <- .completion_utf16_length(backward$partial_child)
  replacement_start <- .completion_utf16_length(text) - replacement_length + 1L

  accessor <- backward$accessor
  items <- list()
  if (is.null(accessor)) {
    # Expressions without an accessor must be code or backtick names.
    if (!forward$state %in% c(LS_CODE, LS_BACKTICK)) {
      return(list())
    }
    include_top_level <- TRUE
  } else {
    # Call arguments and empty brackets can contain top-level expressions.
    include_top_level <- (
      (accessor %in% c("[", "[[") && is.null(partial$quote))
      || accessor == "("
    )

    # Try to parse and resolve the accessor context
    parsed <- parse_completion_context(backward$context)
    if (parsed$status == "success") {
      resolved_context <- resolve_completion_context(
        parsed$ast,
        accessor,
        firstenv = firstenv,
        lastenv = lastenv
      )
      if (resolved_context$status == "success") {
        context <- resolved_context$value
        # Get candidates based on context and accessor
        items <- c(items, completion_candidates(
          context,
          accessor,
          partial$name,
          partial$quote,
          replacement_start,
          replacement_length,
          text_after_cursor
        ))
      }
    }
  }

  # Add top-level names when requested by the accessor branch above.
  if (include_top_level) {
    specific_items <- lapply(items, function(item) {
      item$sortText <- paste0("000", item$sortText)
      item
    })
    top_level_items <- completion_candidates(
      getScopeEnvs(firstenv, .GlobalEnv),
      NULL,
      partial$name,
      partial$quote,
      replacement_start,
      replacement_length,
      text_after_cursor
    )
    items <- c(specific_items, top_level_items)
  }

  items
}

.completion_items_for_request <- function(
  frame_id,
  text,
  column = 1L,
  line = 1L
) {
  text_parts <- .completion_split_request_text(text, line, column)
  if (is.null(text_parts)) {
    return(list())
  }

  # Resolve names from the selected debug frame or the global environment.
  if (!isCalledFromBrowser()) {
    firstenv <- globalenv()
  } else {
    frame_id <- convertFrameId(vsc = frame_id)
    if (is.null(frame_id)) {
      frame_id <- 0
    }
    firstenv <- sys.frame(frame_id)
  }

  .completion_items_from_text(
    text_parts$before_cursor,
    firstenv = firstenv,
    lastenv = globalenv(),
    text_after_cursor = text_parts$after_cursor
  )
}

# Handle a DAP completions request.
completionsRequest <- function(response, args, request) {
  frame_id <- lget(args, "frameId", 0)
  text <- lget(args, "text", "")
  column <- lget(args, "column", 0)
  line <- lget(args, "line", 1)

  response$body <- list(
    targets = .completion_items_for_request(frame_id, text, column, line)
  )
  sendResponse(response)
}
