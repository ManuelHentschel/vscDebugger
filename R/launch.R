

launchRequest <- function(response, args, request){
  ## args
  session$debugMode <- lget(
    args,
    'debugMode',
    getOption('vsc.defaultDebugMode')
  )
  session$allowGlobalDebugging <- lget(
    args,
    'allowGlobalDebugging',
    getOption('vsc.defaultAllowGlobalDebugging')
  )
  session$includePackageScopes <- lget(
    args,
    'includePackageScopes',
    getOption('vsc.includePackageScopes')
  )
  session$setBreakpointsInPackages <- lget(
    args,
    'includePackageScopes',
    getOption('vsc.setBreakpointsInPackages')
  )
  session$overwriteCat <- lget(
    args,
    'overwriteCat',
    getOption('vsc.overwriteCat')
  )
  session$overwritePrint <- lget(
    args,
    'overwritePrint',
    getOption('vsc.overwritePrint')
  )
  session$overwriteSource <- lget(
    args,
    'overwriteSource',
    getOption('vsc.overwriteSource')
  )

  session$mainFunction <- lget(args, 'mainFunction', 'main')

  session$workingDirectory <- lget(args, 'workingDirectory', '.')
  setwd(session$workingDirectory)

  file <- lget(args, 'file', 'main.R')
  file <- normalizePath(file) # make sure to setwd() first!
  session$file <- file

  ## do stuff
  # check debugMode
  if(!(session[['debugMode']] %in% c('function', 'file', 'workspace'))){
    stop(paste0("Invalid debugMode:", format(session[['debugMode']]), collapse=''))
  }

  # source file
  if (session[['debugMode']] == 'function'){
    base::source(session[['file']])
  }

  ## ret
  sendResponse(response)
}


configurationDoneRequest <- function(response, args, request){
  # no args

  # overwrite requested functions
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

  # attach functions
  if(length(attachList)>0){
    attach(attachList, name = "tools:vscDebugger", warn.conflicts = FALSE)
  }

  # set breakpoints
  if(session$debugMode == 'function'){
    .vsc.setStoredBreakpoints()
  }


  # send response before launching main/debugSource!
  session$isConfigurationDone <- TRUE
  sendResponse(response)

  # do stuff
  if(session$debugMode == 'file'){
    setErrorHandler(session$breakOnErrorFromFile)
    .vsc.debugSource(session[['file']])
  } else if (session$debugMode == 'function'){
    setErrorHandler(session$breakOnErrorFromFile)
    eval(call(session$mainFunction), globalenv())
  } else{
    setErrorHandler(session$breakOnErrorFromConsole)
  }

  if(session$allowGlobalDebugging){
    addTaskCallback(globalStepCallback)
    setErrorHandler(session$breakOnErrorFromConsole)
    session$ignoreNextCallback <- FALSE
  } else{
    sendTerminatedEvent()
    sendExitedEvent()
  }
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

  session$threadId <- lget(args, 'threadId', 1)

  response$body <- body
  sendResponse(response)

  initializedEvent <- makeEvent("initialized")
  sendEvent(initializedEvent)

}