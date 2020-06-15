

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
.vsc.dispatchRequest <- function(request){
  session$ignoreNextCallback <- TRUE
  response <- prepareResponse(request)
  command <- lget(request, 'command', '')
  args <- lget(request, 'arguments', list())
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
  } else {
    # ignore
  }
  invisible(NULL)
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
  event
}
sendStoppedEvent <- function(reason="breakpoint", description=NULL, text=NULL){
  sendEvent(makeStoppedEvent(reason, description, text))
}

makeContinuedEvent <- function(){
  event <- makeEvent("continued")
  body <- list(
    threadId = session$threadId,
    allThreadsContinued = TRUE
  )
  event
}
sendContinuedEvent <- function(){
  sendEvent(makeContinuedEvent())
}

globalStepCallback <- function(...){
  if(lget(session, 'ignoreNextCallback', FALSE)){
    session$ignoreNextCallback <- FALSE
  } else{
    sendContinuedEvent()
    sendStoppedEvent(reason="step")
  }
  TRUE
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

launchRequest <- function(response, args, request){
  print("Handling launch request!")
  # args
  debugMode <- lget(args, 'debugMode', '')
  allowGlobalDebugging <- lget(args, 'allowGlobalDebugging', TRUE)
  wd <- lget(args, 'workingDirectory', '.')
  file <- lget(args, 'file', '')
  mainFunction <- lget(args, 'mainFunction', 'main')
  includePackages <- lget(args, 'includePackages', FALSE)

  ## do stuff
  # assign to session
  if(debugMode %in% c('function', 'file', 'workspace')){
    session$debugMode <- debugMode
  } else{
    stop(paste0("Invalid debugMode:", format(debugMode), collapse=''))
  }
  session$allowGlobalDebugging <- allowGlobalDebugging
  session$workingDirectory <- wd
  session$file <- suppressWarnings(normalizePathInWd(file, wd=wd))
  session$mainFunction <- mainFunction
  session$includePackages <- includePackages

  setwd(wd)
  if (debugMode == 'function'){
    base::source(file)
  }

  sendResponse(response)
}


configurationDoneRequest <- function(response, args, request){
  # no args
  session$isConfigurationDone <- TRUE

  attachList <- list()

  if (session$overwritePrint) {
    attachList$print <- .vsc.print
  }

  if (session$overwriteCat) {
    attachList$cat <- .vsc.cat
  }

  if (session$overwriteSource) {
    attachList$source <- .vsc.debugSource
  }

  attach(attachList, name = "tools:vscDebugger", warn.conflicts = FALSE)

  # options(error = .vsc.onError)
  options(error = recover)

  if(session$debugMode == 'function'){
    # set breakpoints
    .vsc.setStoredBreakpoints()
  }

  # send response before launching main/debugSource!
  sendResponse(response)

  # do stuff
  if(session$debugMode == 'file'){
    .vsc.debugSource(session[['file']])
  } else if (session$debugMode == 'function'){
    eval(call(session$mainFunction), globalenv())
  } else{
    # do nothing/send stack?
    # sendStoppedEvent("entry")
  }

  if(session$allowGlobalDebugging){
    addTaskCallback(globalStepCallback)
    session$ignoreNextCallback <- FALSE
  } else{
    print("No global debugging :(")
    sendTerminatedEvent()
    sendExitedEvent()
  }
}


setVariableRequest <- function(response, args, request){
    
}

setExpressionRequest <- function(response, args, request){
    
}

initializeRequest <- function(response, args, request){
  body <- list()
  # support restart
  body$supportsRestartRequest <- TRUE

  # the adapter implements the configurationDoneRequest.
  body$supportsConfigurationDoneRequest <- TRUE

  # make VS Code to use 'evaluate' when hovering over source
  body$supportsEvaluateForHovers <- FALSE

  # make VS Code to show a 'step back' button
  body$supportsStepBack <- FALSE

  # make VS Code to support data breakpoints
  body$supportsDataBreakpoints <- FALSE

  # make VS Code to support completion in REPL
  body$supportsCompletionsRequest <- TRUE
  body$completionTriggerCharacters <- list("[", "$", ":", "@" )

  # make VS Code to send cancelRequests
  body$supportsCancelRequest <- FALSE

  # make VS Code send the breakpointLocations request
  body$supportsBreakpointLocationsRequest <- FALSE

  # enable exception-info (not working???)
  body$supportsExceptionInfoRequest <- FALSE
  body$supportsExceptionOptions <- TRUE
  exceptionBreakpointFilters <- list(
    list(
      filter = 'fromFile',
      label = 'Errors from R file',
      default = TRUE
    ),
    list(
      filter = 'fromEval',
      label = 'Errors from debug console',
      default = FALSE
    )
  )
  body$exceptionBreakpointFilters <- exceptionBreakpointFilters
  
  # 
  body$supportsClipboardContext <- TRUE
  body$supportsSetVariable <- FALSE
  body$supportsSetExpression <- FALSE

  # assign to session
  session$isInitialized <- TRUE
  session$isConfigurationDone <- FALSE

  rStrings <- lget(args, 'rStrings', list())
  lapply(names(rStrings), function(name){
    session$rStrings[[name]] <- rStrings[[name]]
  })

  options(prompt = paste0(session$rStrings$prompt, '\n'))
  options(continue = paste0(session$rStrings$continue, '\n'))
  options(browserNLdisabled = TRUE)



  response$body <- body
  sendResponse(response)

  initializedEvent <- makeEvent("initialized")
  sendEvent(initializedEvent)

}