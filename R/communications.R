


# Can be used to dispatch requests via tcp socket instead of stdin
#' @export
.vsc.listenOnPort <- function(timeout=0){
  logPrint('start listening on port. timeout:')
  logPrint(timeout)
  session$stopListeningOnPort <- FALSE
  registerEntryFrame()
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
  j <- getJson(body)
  if(!is.null(session$jsonServerConnection)){
    base::cat(j, '\n', sep='', file=session$jsonServerConnection)
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

  if (session$state$isEvaluatingSilent()) {
    return(base::cat(...))
  }

  args <- list(...)
  if(identical(args$file, stderr())){
    args['file'] <- NULL
    category <- "stderr"
  } else if(is.null(args$file) || identical(args$file, '')){
    category <- "stdout"
  } else{
    return(base::cat(...))
  }

  ret <- capture.output({do.call(base::cat, args);base::cat("\n")})
  printToVsc(ret, skipCalls+1, category)
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

#' @export
.vsc.message <- function(..., domain = NULL, appendLF = TRUE){
  args <- list(...)
  cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    if (nargs() > 1L) {
      warning("additional arguments ignored in message()")
    }
    args[[1L]]
  } else {
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF) # changed
    call <- sys.call()
    simpleMessage(msg, call)
  }
  defaultHandler <- function(c) {
    .vsc.cat(conditionMessage(c), file = stderr(), sep = "", skipCalls=5) # changed
  }
  withRestarts(
    {
      signalCondition(cond)
      defaultHandler(cond)
    },
    muffleMessage = function() NULL
  )
  invisible()
}



printToVsc <- function(ret, skipCalls=0, category="stdout"){
  output <- paste0(ret, collapse = "\n")

  source <- getSource(sys.call(-skipCalls))
  line <- lget(source, 'line', 0)

  sendOutputEvent(category, output = output, line=line, source=source)
}


# used for debugging
logPrint <- function(...){
  # base::print(...)
}

# used for debugging
logCat <- function(...){
  # base::cat(...)
}


