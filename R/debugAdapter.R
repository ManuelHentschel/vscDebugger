

### REQUESTS


#' @export
.vsc.handleJson <- function(json){
  registerEntryFrame()
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if(lget(obj, 'type', '') == 'request'){
    .vsc.dispatchRequest(obj)
    success <- TRUE
  } else{
    logCat('Unknown json: ', json, '\n')
    success <- FALSE
  }
  unregisterEntryFrame()
  invisible(success)
}

#' @export
.vsc.dispatchRequest <- function(request){
  registerEntryFrame()
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
  } else if(command == 'source'){
    sourceRequest(response, args, request)
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
  } else if(command == 'next'){
    nextRequest(response, args, request)
  } else if(command == 'stepIn'){
    stepInRequest(response, args, request)
  } else if(command == 'stepOut'){
    stepOutRequest(response, args, request)
  } else if(command == 'restart'){
    restartRequest(response, args, request)
  } else if(command == 'terminate'){
    terminateRequest(response, args, request)
  } else if(command == 'disconnect'){
    disconnectRequest(response, args, request) # different form terminate?
  } else if(command == 'custom'){
    customRequest(response, args, request)
  } else {
    success <- FALSE
    response$success <- FALSE
    sendResponse(response)
  }
  unregisterEntryFrame()
  invisible(success)
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

sendResponse <- function(response){
  sendToVsc(body = response)
}

customRequest <- function(response, args, request){
  if(lget(args, 'reason', '') == 'showingPrompt'){
    showingPromptRequest(response, args, request)
  }
}

threadsRequest <- function(response, args, request){
  # trheadId is currently just a dummy
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



### EVENTS

makeEvent <- function(eventType, body=NULL){
  list(
    seq = 0,
    type = "event",
    event = eventType,
    body = body
  )
}
sendEvent <- function(event){
  sendToVsc(body=event)
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

makeCustomEvent <- function(reason=NULL, body=list()){
  event <- makeEvent("custom")
  if(!is.null(reason)){
    body$reason <- reason
  }
  event$body <- body
  event
}
sendCustomEvent <- function(reason=NULL, body=list()){
  sendEvent(makeCustomEvent(reason, body))
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

makeWriteToStdinEvent <- function(text='', when='now', addNewLine=TRUE, expectPrompt=NULL, count=1, stack=FALSE){
  event <- makeCustomEvent('writeToStdin', list(
    text = text,
    when = when,
    addNewLine = addNewLine,
    count = count,
    stack = stack
  ))
}
sendWriteToStdinEvent <- function(text='', when='now', addNewLine=TRUE, expectPrompt=NULL, count=1, stack=FALSE){
  sendEvent(makeWriteToStdinEvent(text, when, addNewLine, expectPrompt, count, stack))
}
