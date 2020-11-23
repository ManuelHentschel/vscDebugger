
#' Error handler
#' 
#' Error handler used by vsc. Set with `options(error = .vsc.onError)`
#' 
#' @export
#' @param err The message to be sent to vsc. Defaults to `geterrmessage()`
.vsc.onError <- function(err=NULL) {
  logPrint('entering .vsc.onError()')
  registerEntryFrame()
  pauseOnError <- (
    (session$state$isRunningFileOrMain() && session$breakOnErrorFromFile) ||
    (session$state$isEvaluating() && session$breakOnErrorFromConsole)
  )
  pauseOnError <- pauseOnError && !session$state$isError()

  for(response in session$pendingEvalResponses){
    response$success <- FALSE
    sendResponse(response)
  }
  session$pendingEvalResponses <- list()

  if(pauseOnError){
    logPrint('starting error mode!!')
    session$state$startPaused('error')
    if(is.null(err)){
      message <- geterrmessage()
    } else{
      attributes(err) <- list()
      message <- err
    }
    body <- list(message=message)
    sendWriteToStdinEvent('', when='browserPrompt', count=0)
    session$clearStackTree <- TRUE
    sendStoppedEvent('exception', description = 'Stopped on Exception', text = message)
    # unregisterEntryFrame()
    browser() # must be last command!
  } else {
    logPrint('showing traceback!!!')
    traceback()
    unregisterEntryFrame()
  }
}

showingPromptRequest <- function(response, args, request){
  whichPrompt <- lget(args, 'which', '')
  if(session$state$baseState %in% c('starting', 'loadLib', 'quitting')){
    logPrint('ignoring callback...')
  } else if(whichPrompt == 'topLevel'){
    logPrint('is showing toplevel prompt!!!')
    if(session$allowGlobalDebugging){
      logPrint('breakpoint on toplevel')
      session$state$changeBaseState('workspace', startPaused=TRUE)
      sendStoppedEvent(reason='step')
    } else{
      logPrint('quit from toplevel')
      session$state$changeBaseState('quitting')
      terminateSessionFromTopLevel()
      # session$stopListeningOnPort <- TRUE
    }
  } else if(session$state$isPausedOnError()){
    # ignore
  } else if(session$state$isPausedOnBreakpoint()){
    logPrint('starting paused on breakpoint!!!')
    sendStoppedEvent(reason='breakpoint')
  } else{
    logPrint('starting paused!!!')
    session$state$startPaused('browser')
    sendStoppedEvent(reason='step')
  }
}

# also used by breakpoints!
sendWriteToStdinForFlowControl <- function(text){
  if(session$supportsStdoutReading){
    # request text on browser prompt
    # .vsc.listenOnPort is called automatically
    logCat('Request text on browserPrompt: ', text, '\n')
    sendWriteToStdinEvent(text, when = 'browserPrompt')
  } else{
    # request text immediately
    logCat('Request text now: ', text, '\n')
    ret <- sendWriteToStdinEvent(text, when = 'now')

    # request new listen call
    if(session$useDapSocket){
      listenFunction <- format(quote(.vsc.listenForDAP))
      callListenFunction <- TRUE
    } else if(session$useJsonSocket){
      listenFunction <- format(quote(.vsc.listenForJSON))
      callListenFunction <- TRUE
    } else{
      callListenFunction <- FALSE
    }
    if(callListenFunction){
      listenCall <- paste0(session$rStrings$packageName, '::', listenFunction, '()')
      sendWriteToStdinEvent(listenCall, when = 'now')
    } else{
      ret # return success of previous sendWriteToSTdinEvent()
    }
  }
}

continueRequest <- function(response, args, request){
  if(session$state$isPaused() && session$state$pausedOn == "toplevel" && lget(args, 'callDebugSource', FALSE)){
    path <- lget(args$source, 'path', '')
    if(!identical(path, '')){
      logPrint('starting debugSource()...')
      msg <- paste0('.vsc.debugSource("', path, '")')
      sendOutputEvent(msg, group='startCollapsed')
      sendOutputEvent('', group='end')
      prevState <- session$state$startRunning('file')
      .vsc.debugSource(path)
      session$state$revert(prevState)
    } else{
      response$success <- FALSE
    }
    session$stopListeningOnPort <- response$success
  } else if(session$state$isPaused()){
    if(session$state$pausedOn != "toplevel"){
      sendWriteToStdinEvent('c', when='browserPrompt', fallBackToNow = TRUE)
    }
    # always treat as successful -> return control to stdin in attached mode
    success <- TRUE
    response$success <- success
    session$stopListeningOnPort <- success
    if(success){
      session$state$startRunning()
    }
  } else {
    logPrint('case not handled...')
    logPrint(session$state$export())
    response$success <- FALSE
  }
  if(response$success){
    session$clearStackTree <- TRUE
  }
  sendResponse(response)
}

genericStepRequest <- function(response, textToStdin){
  if(isCalledFromBrowser()){
    success <- sendWriteToStdinForFlowControl(textToStdin)
    response$success <- success
    session$stopListeningOnPort <- success
    if(success){
      session$clearStackTree <- TRUE
      session$state$startRunning()
    }
  } else{
    logCat('Not called from browser!\n')
    response$success <- FALSE
  }
  sendResponse(response)
}

nextRequest <- function(response, args, request){
  genericStepRequest(response, 'n')
}

stepInRequest <- function(response, args, request){
  genericStepRequest(response, 's')
}

stepOutRequest <- function(response, args, request){
  genericStepRequest(response, 'f')
}

disconnectRequest <- function(response, args, request){
  doQuit <- session$state$baseState != 'attached'
  session$state$changeBaseState('quitting')
  options(error = NULL)

  if(!doQuit){
    logPrint('disconnect from attached session')
    sendResponse(response)
    closeConnections()
    try(
      detach(session$rStrings$attachName, character.only = TRUE),
      silent = TRUE
    )
    session$state$changeBaseState('detached')
  } else if(isCalledFromBrowser()){
    logPrint('disconnect from browser')
    sendWriteToStdinEvent('Q', when = "browserPrompt", fallBackToNow = TRUE)
    sendWriteToStdinEvent(
      format(quote(
        quit(save='no')
      )),
      stack = TRUE,
      when = "topLevelPrompt"
    )
    sendResponse(response)
    for(evalResponse in session$pendingEvalResponses){
      evalResponse$success <- FALSE
      sendResponse(evalResponse)
    }
    closeConnections()
  } else{
    logPrint('disconnect from toplevel')
    sendResponse(response)
    closeConnections()
    quit(save = 'no')
  }
}

terminateRequest <- function(response, args, request){
  if(isCalledFromBrowser()){
    sendWriteToStdinEvent('Q', when = "browserPrompt", fallBackToNow = TRUE)
    session$stopListeningOnPort <- TRUE
    sendResponse(response)
    sendContinuedEvent()
    sendStoppedEvent('step')
  } else{
    session$clearStackTree <- TRUE
    sendResponse(response)
    sendTerminatedEvent()
  }
}


reverseContinueRequest <- function(response, args, request){
  if(isCalledFromBrowser()){
    response$success <- sendWriteToStdinForFlowControl('Q')
  } else{
    response$success <- FALSE
  }
  if(response$success){
    session$stopListeningOnPort <- TRUE
    session$clearStackTree <- TRUE
  }
  sendResponse(response)
}

terminateSessionFromTopLevel <- function(){
  session$state$changeBaseState('quitting')
  sendTerminatedEvent()
  sendExitedEvent()
}

sessionFinalizer <- function(...){
  logPrint('finalizing session!!!!')
  logPrint(session$state$baseState)
  if(session$state$baseState != 'quitting'){
    options(error = NULL)
    if(session$supportsHelpViewer && !is.null(session$print_help_files_with_topic_0)){
      try(suppressWarnings(.S3method(
        "print",
        "help_files_with_topic",
        session$print_help_files_with_topic_0
      )))
    }
    try(detach(session$rStrings$attachName, character.only = TRUE), silent = TRUE)
    try(sendExitedEvent(), silent = TRUE)
    try(sendTerminatedEvent(), silent = TRUE)
    Sys.sleep(0.05)
    try(closeConnections(), silent = TRUE)
  }
}
