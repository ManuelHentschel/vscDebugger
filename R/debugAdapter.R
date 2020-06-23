




#' @export
.vsc.listenOnPort <- function(timeout=0){
  registerEntryFrame()
  if(!lget(session, 'useServer', FALSE)){
    return(NULL)
  }
  t <- as.numeric(Sys.time())
  repeat{
    char <- readChar(session$serverConnection, nchars=1)
    if(length(char)==0){
      if(timeout == 0 || (timeout > 0 && as.numeric(Sys.time()) - t > timeout)){
        break
      } else{
        Sys.sleep(0.01)
      }
    } else {
      if(char == '\n'){
        json <- session$restOfLine
        cat('Received json: ', json, '\n', sep='')
        .vsc.handleJson(json)
        session$restOfLine <- ''
      } else{
        session$restOfLine <- paste0(session$restOfLine, char)
      }
      t <- as.numeric(Sys.time())
    }
  }
  session$ignoreNextCallback <- FALSE
  unregisterEntryFrame()
}







sendResponse <- function(response){
  .vsc.sendToVsc(message = 'response', body = response)
}

prepareResponse <- function(request){
  response <- list(
    seq = 0,
    type = 'response',
    request_seq = request$seq,
    command = request$command,
    success = TRUE
  )
  return(response)
}

#' @export
.vsc.handleJson <- function(json){
  registerEntryFrame()
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if(lget(obj, 'type', '') == 'request'){
    .vsc.dispatchRequest(obj)
    success <- TRUE
  } else{
    cat('Unknown json: ', json, '\n')
    success <- FALSE
  }
  unregisterEntryFrame()
  invisible(success)
}

#' @export
.vsc.dispatchRequest <- function(request){
  registerEntryFrame()
  session$ignoreNextCallback <- TRUE
  response <- prepareResponse(request)
  command <- lget(request, 'command', '')
  args <- lget(request, 'arguments', list())
  success <- TRUE
  if(command == 'stackTrace'){
    stackTraceRequest(response, args, request)
  } else if(command == 'scopes'){
    scopesRequest(response, args, request)
  } else if(command == 'variables'){
    variablesRequest(response, args, request)
  } else if(command == 'setVariable'){
    setVariableRequest(response, args, request)
  } else if(command == 'setExpression'){
    setExpressionRequest(response, args, request)
  } else if(command == 'evaluate'){
    evaluateRequest(response, args, request)
  } else if(command == 'setExceptionBreakpoints'){
    setExceptionBreakPointsRequest(response, args, request)
  } else if(command == 'completions'){
    completionsRequest(response, args, request)
  } else if(command == 'threads'){
    threadsRequest(response, args, request)
  } else if(command == 'setBreakpoints'){
    setBreakpointsRequest(response, args, request)
  } else if(command == 'initialize'){
    initializeRequest(response, args, request)
  } else if(command == 'configurationDone'){
    configurationDoneRequest(response, args, request)
  } else if(command == 'launch'){
    launchRequest(response, args, request)
  } else if(command == 'continue'){
    continueRequest(response, args, request)
  } else if(command == 'terminate'){
    terminateRequest(response, args, request)
  } else {
    sendResponse(response)
    success <- FALSE
  }
  unregisterEntryFrame()
  invisible(success)
}


makeEvent <- function(eventType, body=NULL){
  list(
    seq = 0,
    type = "event",
    event = eventType,
    body = body
  )
}

makeOutputEvent <- function(
  output,
  category='console',
  group=NULL,
  variablesReference=0,
  source=NULL,
  line=NULL,
  column=NULL,
  data=NULL
){
  event <- makeEvent('output')
  body <- list(
    category = category,
    output = output
  )
  body$group <- group
  body$variablesReference <- variablesReference
  body$source <- source
  body$line <- line
  body$column <- column
  body$data <- data

  event$body <- body
  event
}
sendOutputEvent <- function(
  output,
  category='console',
  group=NULL,
  variablesReference=0,
  source=NULL,
  line=NULL,
  column=NULL,
  data=NULL
){
  sendEvent(makeOutputEvent(
    output,
    category,
    group,
    variablesReference,
    source,
    line,
    column,
    data
  ))
}


makeStoppedEvent <- function(reason="breakpoint", description=NULL, text=NULL){
  event <- makeEvent("stopped")
  event$body <- list(
    reason = reason,
    allThreadsStopped = TRUE,
    preserveFocusHint = FALSE
  )
  event$body$description <- description
  event$body$text <- text
  event$body$threadId <- session$threadId
  event
}
sendStoppedEvent <- function(reason="breakpoint", description=NULL, text=NULL){
  sendEvent(makeStoppedEvent(reason, description, text))
}

makeContinuedEvent <- function(){
  event <- makeEvent("continued")
  event$body <- list(
    threadId = session$threadId,
    allThreadsContinued = TRUE
  )
  event
}
sendContinuedEvent <- function(){
  sendEvent(makeContinuedEvent())
}


makeBreakpointEvent <- function(reason, breakpoint){
  event <- makeEvent("breakpoint")
  event$body <- list(
    reason = reason,
    breakpoint = breakpoint
  )
  event
}
sendBreakpointEvent <- function(reason, breakpoint){
  sendEvent(makeBreakpointEvent(reason, breakpoint))
}

sendEvent <- function(event){
  .vsc.sendToVsc(message='event', body=event, id=0)
}

makeAndSendEvent <- function(eventType, body){
  sendEvent(
    makeEvent(
      eventType = eventType,
      body = body
    )
  )
}

makeExitEvent <- function(exitCode=0){
  event <- makeEvent("exited")
  event$body <- list(
    exitCode = exitCode
  )
  event
}
sendExitedEvent <- function(exitCode=0){
  sendEvent(makeExitEvent(exitCode))
}

makeTerminatedEvent <- function(restart=NULL){
  event <- makeEvent("terminated")
  event$body <- list()
  event$body$restart <- restart
  event
}
sendTerminatedEvent <- function(restart=NULL){
  sendEvent(makeTerminatedEvent(restart))
}

setExceptionBreakPointsRequest <- function(response, args, request){
  filters <- lget(args, 'filters', list())
  session$breakOnErrorFromConsole <- ('fromEval' %in% filters)
  session$breakOnErrorFromFile <- ('fromFile' %in% filters)
  sendResponse(response)
}



threadsRequest <- function(response, args, request){
  response$body <- list(
    threads = list(
      list(
        id = session$threadId,
        name = paste0("Thread ", session$threadId)
      )
    )
  )
  sendResponse(response)
}

continueRequest <- function(response, args, request){
  setErrorHandler(session$breakOnErrorFromFile)
  callDebugSource <- lget(args, 'callDebugSource', FALSE)
  path <- lget(args$source, 'path', '')
  if(callDebugSource && !isCalledFromBrowser()){
    .vsc.debugSource(path)
  }
}

setVariableRequest <- function(response, args, request){
    
}

setExpressionRequest <- function(response, args, request){
    
}

terminateRequest <- function(response, args, request){
  if(lget(session, 'useServer', FALSE)){
    close(session$serverConnection)
  }
  quit(save = 'no')
}

