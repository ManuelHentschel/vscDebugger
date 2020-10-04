


# Can be used to dispatch requests via tcp socket instead of stdin
#' @export
.vsc.listenOnPort <- function(timeout=0){
  logPrint('start listening on port. timeout:')
  logPrint(timeout)
  session$stopListeningOnPort <- FALSE
  registerEntryFrame()
  if(!lget(session, 'useJsonServer', FALSE)){
    return(NULL)
  }
  t <- as.numeric(Sys.time())
  repeat{
    char <- readChar(session$jsonServerConnection, nchars=1)
    if(length(char)==0){
      if(timeout == 0 || (timeout > 0 && as.numeric(Sys.time()) - t > timeout)){
        break
      } else{
        Sys.sleep(0.01)
      }
    } else {
      if(char == '\n'){
        json <- session$restOfLine
        session$restOfLine <- ''
        logCat('Received json: ', json, '\n', sep='')
        .vsc.handleJson(json)
        if(session$stopListeningOnPort){
          break
        }
      } else{
        session$restOfLine <- paste0(session$restOfLine, char)
      }
      t <- as.numeric(Sys.time())
    }
  }
  logPrint('stop listening on port')
  unregisterEntryFrame()
}



#' Sends a json to vsc
#'
#' Sends a json to vsc
#'
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
sendToVsc <- function(body = "") {
  if(session$useJsonServer){
    j <- getJson(body)
    base::cat(j, '\n', sep='', file=session$jsonServerConnection)
  } else {
    # Deprecated!
    s <- makeStringForVsc(body)
    base::cat(s)
  }
}

getJson <- function(body){
  body <- removeNonJsonElements(body)
  s <- jsonlite::toJSON(body, auto_unbox = TRUE, force = TRUE)
}

removeNonJsonElements <- function(v){
  if(is.list(v)){
    lapply(v, removeNonJsonElements)
  } else{
    if(is.vector(v)){
      v
    } else{
      ''
    }
  }
}


#' Prepare a message as string for vsc
#'
#' Prepare a message as string for vsc. DEPRECATED!
#'
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @return A (json) string that can be interpreted by vsc
makeStringForVsc <- function(body = "") {
  body <- removeNonJsonElements(body)
  s <- jsonlite::toJSON(body, auto_unbox = TRUE, force = TRUE)
  r <- paste0(
    session$rStrings$delimiter0,
    s,
    session$rStrings$delimiter1,
    '\n'
  )
  return(r)
}



#' Modified version of `base::cat()` for vsc
#'
#' Captures the output of `base::cat(...)` and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to base::cat()
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @return NULL (invisible)
.vsc.cat <- function(..., skipCalls=0) {
  # TODO: consider correct environment for base::print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$state$isEvaluatingSilent() || (!identical(list(...)$file, "") && !is.null(list(...)$file))) {
    return(base::cat(...))
  }
  ret <- capture.output({base::cat(...);base::cat("\n")})
  printToVsc(ret, skipCalls+1)
  invisible(NULL)
}

#' Modified version of `base::print()` for vsc
#'
#' Captures the output of `base::print(...)` and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to `base::cat()`
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @return `NULL` (invisible)
.vsc.print <- function(x, ..., skipCalls=0) {
  # TODO: consider correct environment for base::print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$state$isEvaluatingSilent()) {
    return(base::print(x, ...))
  }
  ret <- capture.output(base::print(x, ...))
  ret <- c(ret, "")
  printToVsc(ret, skipCalls+1)
  invisible(x)
}

printToVsc <- function(ret, skipCalls=0){
  output <- paste0(ret, collapse = "\n")

  source <- getSource(sys.call(-skipCalls))
  line <- lget(source, 'line', 0)

  sendOutputEvent(category="stdout", output = output, line=line, source=source)
}


# used for debugging
logPrint <- function(...){
  # base::print(...)
}

# used for debugging
logCat <- function(...){
  # base::cat(...)
}

