completionsRequest <- function(response, args, request) {
  # Read DAP request arguments.
  frameIdVsc <- lget(args, 'frameId', 0)
  text <- lget(args, 'text', '')
  column <- lget(args, 'column', 0)
  line <- lget(args, 'line', 1)

  # Run the new completion pipeline when enabled.
  targets <- list()
  targets <- c(targets, .vsc.getCompletionNew(frameIdVsc, text, column, line))

  response$body <- list(
    targets = targets
  )
  sendResponse(response)
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
.completion_request_cursor <- function(text, line, column) {
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

  # Convert column to 1-based if needed (TEMP: VS Code is 1 based)
  column <- column + 0L

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

  list(
    text = text_before_cursor,
    text_after_cursor = text_after_cursor
  )
}

.vsc.getCompletionNew <- function(frameIdVsc, text, column = 1L, line = 1L) {
  cursor <- .completion_request_cursor(text, line, column)
  if (is.null(cursor)) {
    return(list())
  }
  # Resolve names from the selected debug frame or the global environment.
  if (!isCalledFromBrowser()) {
    firstenv <- globalenv()
  } else {
    frameId <- convertFrameId(vsc = frameIdVsc)
    if (is.null(frameId)) frameId <- 0
    firstenv <- sys.frame(frameId)
  }

  completion_main(
    cursor$text,
    firstenv = firstenv,
    lastenv = globalenv(),
    text_after_cursor = cursor$text_after_cursor
  )
}
