
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
  } else{
    logPrint('starting paused!!!')
    session$state$startPaused('browser')
    sendStoppedEvent(reason='breakpoint')
  }
}


continueRequest <- function(response, args, request){
  # setErrorHandler(session$breakOnErrorFromFile)
  logPrint(session$state$isPaused())
  logPrint(session$state$pausedOn)
  if(session$state$isPaused() && session$state$pausedOn == "toplevel"){
    path <- lget(args$source, 'path', '')
    if(!identical(path, '')){
      logPrint('starting debugSource()...')
      msg <- paste0('.vsc.debugSource("', path, '")')
      sendOutputEvent(msg, group='startCollapsed')
      sendOutputEvent('', group='end')
      prevState <- session$state$startRunning('file')
      .vsc.debugSource(path)
      session$state$revert(prevState)
      session$stopListeningOnPort <- TRUE
    } else{
      logPrint('invalid path for debugSource()')
    }
  } else if(session$state$isPaused()){
    logPrint('continuing execution...')
    session$state$startRunning()
    sendWriteToStdinEvent('c', expectPrompt = FALSE, when = "browserPrompt")
    session$stopListeningOnPort <- TRUE
  } else {
    logPrint('case not handled...')
    logPrint(session$state$export())
    response$success <- FALSE
  }
  sendResponse(response)
}

nextRequest <- function(response, args, request){
  if(isCalledFromBrowser()){
    session$state$startRunning()
    sendWriteToStdinEvent('n', when = "browserPrompt")
    session$stopListeningOnPort <- TRUE
  } else{
    logPrint('not called from browser!')
    response$success <- FALSE
  }
  sendResponse(response)
}

stepInRequest <- function(response, args, request){
  if(isCalledFromBrowser()){
    session$state$startRunning()
    sendWriteToStdinEvent('s', when = "browserPrompt")
    session$stopListeningOnPort <- TRUE
  } else{
    response$success <- FALSE
  }
  sendResponse(response)
}

stepOutRequest <- function(response, args, request){
  if(isCalledFromBrowser()){
    session$state$startRunning()
    sendWriteToStdinEvent('f', when = "browserPrompt")
    session$stopListeningOnPort <- TRUE
  } else{
    response$success <- FALSE
  }
  sendResponse(response)
}

disconnectRequest <- function(response, args, request){
  doQuit <- session$state$baseState != 'attached'
  session$state$changeBaseState('quitting')

  if(!doQuit){
    logPrint('disconnect from attached session')
    sendResponse(response)
    closeConnections()
    session$state$changeBaseState('detached')
  } else if(isCalledFromBrowser()){
    logPrint('disconnect from browser')
    sendWriteToStdinEvent('Q', when = "browserPrompt")
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
    sendWriteToStdinEvent('Q', when = "browserPrompt")
    session$stopListeningOnPort <- TRUE
    sendResponse(response)
    sendContinuedEvent()
    sendStoppedEvent('step')
  } else{
    sendResponse(response)
    sendTerminatedEvent()
  }
}


terminateSessionFromTopLevel <- function(){
  session$state$changeBaseState('quitting')
  sendTerminatedEvent()
  sendExitedEvent()
}


closeConnections <- function(){
  session$stopListeningOnPort <- TRUE
  if(!is.null(session$sinkSocketConnection)){
    while(sink.number() > session$sinkNumber){
      sink(NULL)
    }
    try(close(session$sinkSocketConnection), silent=TRUE)
  }
  if(!is.null(session$jsonSocketConnection)){
    try(close(session$jsonSocketConnection), silent=TRUE)
  }
  if(!is.null(session$dapSocketConnection)){
    try(close(session$dapSocketConnection), silent=TRUE)
  }
}

