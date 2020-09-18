# Sent as very first request
# Contains info about the debug session itself, not the specific file/function etc.
initializeRequest <- function(response, args, request){
  body <- list()
  # don't support restart -> automatically termiantes + starts again
  body$supportsRestartRequest <- FALSE

  # suppoert terminate: can be used to exit function without terminating R session
  # only works ONCE (!)
  body$supportsTerminateRequest <- TRUE

  # the adapter implements the configurationDoneRequest.
  body$supportsConfigurationDoneRequest <- TRUE

  # make VS Code NOT use 'evaluate' when hovering over source
  body$supportsEvaluateForHovers <- FALSE

  # make VS Code NOT show a 'step back' button
  body$supportsStepBack <- FALSE

  # make VS Code NOT support data breakpoints
  body$supportsDataBreakpoints <- FALSE

  # make VS Code to support completion in REPL
  body$supportsCompletionsRequest <- TRUE
  body$completionTriggerCharacters <- list("[", "$", ":", "@", "(", ")")

  # make VS Code to send cancelRequests
  body$supportsCancelRequest <- FALSE

  # make VS Code send the breakpointLocations request
  body$supportsBreakpointLocationsRequest <- FALSE

  # enable exception-info (not working???)
  body$supportsExceptionInfoRequest <- FALSE
  body$supportsExceptionOptions <- TRUE
  body$exceptionBreakpointFilters <- list(
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
  
  # 
  body$supportsClipboardContext <- TRUE
  body$supportsSetVariable <- getOption('vsc.supportSetVariable', TRUE)

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

  session$useJsonServer <- lget(args, 'useJsonServer', FALSE)
  if(session$useJsonServer){
    session$jsonPort <- lget(args, 'jsonPort', 0)
    session$jsonHost <- lget(args, 'jsonHost', '127.0.0.1')
    session$jsonServerConnection <- socketConnection(
      host = session$jsonHost,
      port = session$jsonPort,
      server = FALSE,
      blocking = FALSE,
      open = "r+b"
    )
  }

  session$useSinkServer <- lget(args, 'useSinkServer', FALSE)
  if(session$useSinkServer){
    session$sinkPort <- lget(args, 'sinkPort', 0)
    session$sinkHost <- lget(args, 'sinkHost', 'localhost')
    session$sinkServerConnection <- socketConnection(
      host = session$sinkHost,
      port = session$sinkPort,
      server = FALSE,
      blocking = FALSE,
      open = "r+b"
    )
    sink(session$sinkServerConnection)
  }

  session$threadId <- lget(args, 'threadId', 1)

  response$body <- body
  response$packageInfo <- packageDescription('vscDebugger')
  sendResponse(response)

  initializedEvent <- makeEvent("initialized")
  sendEvent(initializedEvent)

}


# Sent as second request
# Contains info about the file/function debugged and file-specifig settings
launchRequest <- function(response, args, request){
  ## args
  session$debugMode <- lget(
    args,
    'debugMode',
    getOption('vsc.defaultDebugMode', 'workspace')
  )
  session$allowGlobalDebugging <- lget(
    args,
    'allowGlobalDebugging',
    getOption('vsc.defaultAllowGlobalDebugging', TRUE)
  )
  session$includePackageScopes <- lget(
    args,
    'includePackageScopes',
    getOption('vsc.includePackageScopes', FALSE)
  )
  session$setBreakpointsInPackages <- lget(
    args,
    'setBreakpointsInPackages',
    getOption('vsc.defaultSetBreakpointsInPackages', FALSE)
  )
  session$overwriteCat <- lget(
    args,
    'overwriteCat',
    getOption('vsc.defaultOverwriteCat', TRUE)
  )
  session$overwritePrint <- lget(
    args,
    'overwritePrint',
    getOption('vsc.defaultOverwritePrint', TRUE)
  )
  session$overwriteSource <- lget(
    args,
    'overwriteSource',
    getOption('vsc.defaultOverwriteSource', TRUE)
  )
  session$packagesBeforeLaunch <- lget(
    args,
    'packagesBeforeLaunch',
    character(0)
  )


  session$mainFunction <- lget(args, 'mainFunction', 'main')

  session$workingDirectory <- lget(args, 'workingDirectory', '.')
  setwd(session$workingDirectory)

  file <- lget(args, 'file', 'main.R')
  file <- normalizePath(file, mustWork=FALSE) # make sure to setwd() first!
  session$file <- file

  if(!file.exists(file) && session[['debugMode']] %in% c('function', 'file')){
    response$success <- FALSE
    response$message <- paste0("The file ", file, " could not be found!")
  }

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


# Sent at the end of the launch sequence
# Indicates that all configuration is done and that debugging can start
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

  # load packages
  if(length(session$packagesBeforeLaunch)>0){
    for(pkg in session$packagesBeforeLaunch){
      try(
        library(package=pkg, character.only=TRUE)
      )
    }
  }

  # set breakpoints
  if(
    session$debugMode == 'function' ||
    (session$setBreakpointsInPackages && length(session$packagesBeforeLaunch)>0)
  ){
    .vsc.setStoredBreakpoints()
  }


  # send response before launching main/debugSource!
  session$isConfigurationDone <- TRUE
  sendResponse(response)

  launchFromStdin <- FALSE

  # do stuff
  if(session$debugMode == 'file'){
    registerLaunchFrame()
    setErrorHandler(session$breakOnErrorFromFile)
    .vsc.debugSource(session[['file']])
    unregisterLaunchFrame()
  } else if (session$debugMode == 'function'){
    registerLaunchFrame(skipCalls=2)
    setErrorHandler(session$breakOnErrorFromFile)
    # eval(call(session$mainFunction), globalenv())
    sendWriteToStdinEvent(format(call(session$mainFunction)))
    launchFromStdin <- TRUE
    unregisterLaunchFrame()
  } else{
    setErrorHandler(session$breakOnErrorFromConsole)
  }

  if(session$allowGlobalDebugging){
    addTaskCallback(globalStepCallback)
    setErrorHandler(session$breakOnErrorFromConsole)
    session$ignoreNextCallback <- FALSE
  } else{
    addTaskCallback(terminateSessionCallBack)
    session$ignoreNextCallback <- launchFromStdin
  }
}