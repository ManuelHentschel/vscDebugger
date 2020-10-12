# Sent as very first request
# Contains info about the debug session itself, not the specific file/function etc.
initializeRequest <- function(response, args, request){
  session$state$changeBaseState('starting')
  body <- list()
  # don't support restart -> automatically termiantes + starts again
  body$supportsRestartRequest <- FALSE

  # suppoert terminate: can be used to exit function without terminating R session
  # only works ONCE (!)
  body$supportsTerminateRequest <- getOption('vsc.supportTerminateRequest', TRUE)

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

  rStrings <- lget(args, 'rStrings', list())
  lapply(names(rStrings), function(name){
    session$rStrings[[name]] <- rStrings[[name]]
  })

  options(prompt = paste0(session$rStrings$prompt, '\n'))
  options(continue = paste0(session$rStrings$continue, '\n'))
  options(browserNLdisabled = TRUE)

  session$jsonPort <- lget(args, 'jsonPort', 0)
  session$jsonHost <- lget(args, 'jsonHost', '127.0.0.1')
  session$jsonServerConnection <- socketConnection(
    host = session$jsonHost,
    port = session$jsonPort,
    server = FALSE,
    blocking = FALSE,
    open = "r+b"
  )

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

  session$supportsInvalidatedEvent <- lget(args, 'supportsInvalidatedEvent', FALSE)

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
  session$overwriteMessage <- lget(
    args,
    'overwriteMessage',
    getOption('vsc.defaultOverwriteMessage', TRUE)
  )
  session$overwriteStr <- lget(
    args,
    'overwriteStr',
    getOption('vsc.defaultOverwriteStr', TRUE)
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
  session$debuggedPackages <- lget(
    args,
    'debuggedPackages',
    character(0)
  )
  session$noDebug <- lget(
    args,
    'noDebug',
    FALSE
  )

  if(session$noDebug){
    session$allowGlobalDebugging <- FALSE
  }


  session$mainFunction <- lget(args, 'mainFunction', 'main')

  session$workingDirectory <- lget(args, 'workingDirectory', '.')
  setwd(session$workingDirectory)

  file <- lget(args, 'file', 'main.R')
  file <- normalizePath(file, mustWork=FALSE) # make sure to setwd() first!
  session$file <- file

  if(!file.exists(file) && session[['debugMode']] %in% c('function', 'file')){
    # abort if file doesn't exist
    response$success <- FALSE
    response$message <- paste0("The file ", file, " could not be found!")
  } else if(!(session[['debugMode']] %in% c('function', 'file', 'workspace'))){
    # abort if debugmode is invalid
    response$success <- FALSE
    response$message <- paste0("Invalid debugMode: ", format(session[['debugMode']]), collapse='')
  } 
  
  
  if(response$success && length(session$debuggedPackages)>0){
    # load debugged packages
    session$state$changeBaseState('loadLib', startRunning=TRUE)
    for(pkg in session$debuggedPackages){
      ret <- try(
        library(package=pkg, character.only=TRUE)
      )
      if(inherits(ret, 'try-error')){
        response$success <- FALSE
        response$message <- paste0("Package not found: ", pkg)
        break
      }

      # overwrite print/cat in packages
      ns <- getNamespace(pkg)
      if(session$overwritePrint){
        try(
          assignOverBinding('print', .vsc.print, ns, FALSE),
          silent = TRUE
        )
      }
      if(session$overwriteCat){
        try(
          assignOverBinding('cat', .vsc.cat, ns, FALSE),
          silent = TRUE
        )
      }
      if(session$overwriteStr){
        try(
          assignOverBinding('str', .vsc.str, ns, FALSE),
          silent = TRUE
        )
      }
      if(session$overwriteMessage){
        try(
          assignOverBinding('message', .vsc.message, ns, FALSE),
          silent = TRUE
        )
      }
    }
    session$state$changeBaseState('starting', startPaused=TRUE)
  }

  if(response$success && session[['debugMode']] == 'function'){
    # source file if debugmode is function
    base::source(session[['file']])
    if(!exists(session$mainFunction, mode='function')){
      response$success <- FALSE
      response$message <- paste0("Could not find function: ", session$mainFunction, "()")
    }
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

  if (session$overwriteStr) {
    attachList$str <- .vsc.str
  }

  if (session$overwriteMessage) {
    attachList$message <- .vsc.message
  }

  if (session$overwriteSource) {
    attachList$source <- .vsc.debugSource
  }

  # attach functions
  if(length(attachList)>0){
    attach(attachList, name = "tools:vscDebugger", warn.conflicts = FALSE)
  }

  # set breakpoints
  if(
    session$debugMode == 'function' ||
    (session$setBreakpointsInPackages && length(session$debuggedPackages)>0)
  ){
    setStoredBreakpoints()
  }


  # send response before launching main/debugSource!
  sendResponse(response)

  options(error = .vsc.onError)

  # do stuff
  if(session$debugMode == 'file'){
    registerLaunchFrame()
    session$state$changeBaseState('runFile', startRunning=TRUE)
    .vsc.debugSource(session[['file']])
    unregisterLaunchFrame()
    session$stopListeningOnPort <- TRUE
  } else if (session$debugMode == 'function'){
    session$state$changeBaseState('runMain', startRunning=TRUE)
    sendWriteToStdinEvent(format(call(session$mainFunction)), when = "topLevelPrompt")
    session$stopListeningOnPort <- TRUE
  } else{ # debugMode == 'workspace'
    session$state$changeBaseState('workspace')
    session$stopListeningOnPort <- TRUE
  }

  # response sent already!
}
