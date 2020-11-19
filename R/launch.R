# Sent as very first request
# Contains info about the debug session itself, not the specific file/function etc.
initializeRequest <- function(response, args, request){
  session$state$changeBaseState('starting')
  body <- list()
  # don't support restart -> automatically termiantes + starts again
  body$supportsRestartRequest <- FALSE

  # support delayed stackTraceResponse
  body$supportsDelayedStackTraceLoading <- TRUE

  # support terminate: can be used to exit function without terminating R session
  # only works ONCE (!)
  body$supportsTerminateRequest <- getOption('vsc.supportTerminateRequest', FALSE)
  body$supportsStepBack <- getOption('vsc.repurposeReverseContinue', FALSE)

  # the adapter implements the configurationDoneRequest.
  body$supportsConfigurationDoneRequest <- TRUE

  # make VS Code NOT use 'evaluate' when hovering over source
  body$supportsEvaluateForHovers <- FALSE

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
      label = 'Errors from R Files',
      default = TRUE
    ),
    list(
      filter = 'fromEval',
      label = 'Errors from the Debug Console',
      default = FALSE
    )
  )
  
  # support clipboard context 
  # is always answered with success=false -> uses known variable.value
  body$supportsClipboardContext <- TRUE

  # support setVariable. Is implemented for most basic variable types
  body$supportsSetVariable <- getOption('vsc.supportSetVariable', TRUE)

  # save R strings
  # mostly deprecated, only packagename still relevant
  rStrings <- lget(args, 'rStrings', list())
  lapply(names(rStrings), function(name){
    session$rStrings[[name]] <- rStrings[[name]]
  })

  # save and apply options that are reverted upon disconnect
  internalOptions <- list(browserNLdisabled = TRUE)
  if(!is.null(rStrings$prompt)){
    internalOptions$prompt <- paste0(rStrings$prompt, '\n')
  }
  if(!is.null(rStrings$continue)){
    internalOptions$continue <- paste0(rStrings$continue, '\n')
  }
  session$previousOptions <- options(internalOptions)
  session$internalOptions <- internalOptions

  # connect to json socket, if specified
  session$useJsonSocket <- lget(args, 'useJsonSocket', FALSE)
  session$jsonPort <- lget(args, 'jsonPort', 0)
  session$jsonHost <- lget(args, 'jsonHost', '127.0.0.1')
  if(session$useJsonSocket){
    session$jsonSocketConnection <- socketConnection(
      host = session$jsonHost,
      port = session$jsonPort,
      server = FALSE,
      blocking = FALSE,
      encoding = 'UTF-8',
      open = "r+b"
    )
  }

  # connect to sink socket if specified
  session$useSinkSocket <- lget(args, 'useSinkSocket', FALSE)
  session$sinkPort <- lget(args, 'sinkPort', 0)
  session$sinkHost <- lget(args, 'sinkHost', 'localhost')
  if(session$useSinkSocket){
    session$sinkSocketConnection <- socketConnection(
      host = session$sinkHost,
      port = session$sinkPort,
      server = FALSE,
      blocking = FALSE,
      encoding = 'UTF-8',
      open = "r+b"
    )
    sink(session$sinkSocketConnection)
    session$sinkNumber <- sink.number()
  }

  # save session info that was supplied
  session$supportsInvalidatedEvent <- lget(args, 'supportsInvalidatedEvent', FALSE)
  session$threadId <- lget(args, 'threadId', 1)

  # this info is used by VS Code to identify the terminal corresponding to this debug session
  session$pid <- Sys.getpid()
  session$ppid <- getPpid()
  session$terminalId <- Sys.getenv('VSCODE_R_DEBUGGER_TERMINAL_ID')

  # prepare and send response
  response$body <- body
  response$packageInfo <- packageDescription('vscDebugger')
  sendResponse(response)

  # send initialized event
  initializedEvent <- makeEvent("initialized")
  sendEvent(initializedEvent)

}


# Sent as second request
# Contains info about the file/function debugged and file-specifig settings
launchRequest <- function(response, args, request){

  # handle generic config entries
  handleDebugConfig(args)

  # handle launch specific config entries
  if(session$noDebug){
    session$allowGlobalDebugging <- FALSE
  }
  session$mainFunction <- lget(args, 'mainFunction', 'main')
  session$workingDirectory <- lget(args, 'workingDirectory', '.')
  if(nchar(session$workingDirectory)>0){
    setwd(session$workingDirectory)
  }

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
    session$state$changeBaseState('loadLib', startRunning=TRUE)
    # only load packages from installation that are not loaded from source
    allPackages <- setdiff(session$debuggedPackages, session$loadPackages)
    for(pkg in allPackages){
      # try to load package
      ret <- try(
        library(package=pkg, character.only=TRUE)
      )

      # abort launch if package not found
      if(inherits(ret, 'try-error')){
        response$success <- FALSE
        response$message <- paste0("Package not found: ", pkg)
        break
      }

      # get() every item from package to avoid strange behaviour with lazyLoading
      avoidLazyLoading(pkg)

      # overwrite print, str, etc. in package
      ns <- getNamespace(pkg)
      attachList <- makeAttachList(session, overwriteLoadAll = FALSE)
      for(fncName in names(attachList)){
        try(
          assignOverBinding(fncName, attachList[[fncName]], ns, FALSE),
          silent = TRUE
        )
      }
    }
    session$state$changeBaseState('starting', startPaused=TRUE)
  }

  if(response$success && length(session$loadPackages)>0){
    session$state$changeBaseState('loadLib', startRunning=TRUE)
    # pkgload is 
    if(!requireNamespace('pkgload', quietly = TRUE)){
      response$message <- paste(
        'Could not load packages!',
        'Please install the package "pkgload" or',
        'remove "loadPackages" from the launch configuration!',
        sep = '\n'
      )
      response$success <- FALSE
    } else{
      for(pkg in session$loadPackages){
        ret <- try({
          pkgInfo <- internalLoadAll(
            path=pkg,
            refreshBreakpoints = FALSE,
            loadSilently = session$loadSilently
          )
          ns <- pkgInfo$env
        })
        if(inherits(ret, 'try-error')){
          response$success <- FALSE
          response$message <- paste0('Failed to load package: ', pkg)
          break
        }
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

makeAttachList <- function(args, ...){
  # use ... to overwrite individual entries of args
  overwriteArgs <- list(...)

  getArg <- function(name){
    if(name %in% names(overwriteArgs)){
      isTRUE(overwriteArgs[[name]])
    } else{
      isTRUE(args[[name]])
    }
  }

  # make list containing the requested functions
  attachList <- list()

  if (getArg("overwritePrint")) {
    attachList$print <- .vsc.print
  }

  if (getArg("overwriteCat")) {
    attachList$cat <- .vsc.cat
  }

  if (getArg("overwriteStr")) {
    attachList$str <- .vsc.str
  }

  if (getArg("overwriteMessage")) {
    attachList$message <- .vsc.message
  }

  if (getArg("overwriteSource")) {
    attachList$source <- .vsc.debugSource
  }

  if (getArg("overwriteLoadAll")) {
    attachList$load_all <- .vsc.load_all
  }

  return(attachList)
}


# Sent at the end of the launch sequence
# Indicates that all configuration is done and that debugging can start
configurationDoneRequest <- function(response, args, request){
  # no args

  # update capabilities that might have been changed by loaded user code
  capabilities <- list()
  capabilities$supportsTerminateRequest <- getOption('vsc.supportTerminateRequest', FALSE)
  capabilities$supportsStepBack <- getOption('vsc.repurposeReverseContinue', FALSE)
  capabilities$supportsSetVariable <- getOption('vsc.supportSetVariable', TRUE)
  sendCapabilitesEvent(capabilities)

  # attach functions
  attachList <- makeAttachList(session)

  if(length(attachList)>0){
    attach(attachList, name = session$rStrings$attachName, warn.conflicts = FALSE)
  }

  # set breakpoints
  if(session$debugMode == 'function' || length(session$debuggedPackages)>0 || length(session$loadPackages)>0){
    setStoredBreakpoints()
  }

  # register finalizer to send disconnect event on quit()
  reg.finalizer(
    topenv(),
    sessionFinalizer,
    onexit = TRUE
  )

  # disable just-in-time compilation (messes with source info etc.)
  compiler::enableJIT(getOption('vsc.enableJIT', 0))

  # send response before launching main/debugSource!
  ret <- sendResponse(response)

  errorOption <- list(error=.vsc.onError)
  previousErrorOption <- options(errorOption)

  session$internalOptions <- c(session$internalOptions, errorOption)
  session$previousOptions <- c(session$previousOptions, previousErrorOption)

  # do stuff
  if(session$debugMode == 'file'){
    session$state$changeBaseState('runFile', startRunning=TRUE)
    .vsc.debugSource(session[['file']])
    session$stopListeningOnPort <- TRUE
  } else if (session$debugMode == 'function'){
    session$state$changeBaseState('runMain', startRunning=TRUE)
    sendWriteToStdinEvent(format(call(session$mainFunction)), when = "topLevelPrompt")
    session$stopListeningOnPort <- TRUE
  } else if(session$debugMode == 'workspace'){
    session$state$changeBaseState('workspace')
    session$stopListeningOnPort <- TRUE
  } else{ # attached
    if(isCalledFromBrowser()){
      session$state$startPaused('entry')
    } else{
      session$state$startPaused('toplevel')
    }
    sendStoppedEvent('entry')
  }

  # response sent already!
  invisible(ret)
}


attachRequest <- function(response, args, request){
  handleDebugConfig(args)

  if(session$useCustomSocket){
    session$customSocketConnection <- socketConnection(
      host = session$customHost,
      port = session$customPort,
      server = FALSE,
      open = 'r+b'
    )
  }
  
  session$debugMode <- 'attached'
  session$state$changeBaseState('attached', TRUE)
  sendResponse(response)
}

handleDebugConfig <- function(args){
  ## args
  session$debugMode <- lget(args, 'debugMode', getOption('vsc.defaultDebugMode', 'workspace'))
  session$allowGlobalDebugging <- lget(args, 'allowGlobalDebugging', getOption('vsc.defaultAllowGlobalDebugging', TRUE))
  session$includePackageScopes <- lget(args, 'includePackageScopes', getOption('vsc.includePackageScopes', FALSE))
  session$setBreakpointsInPackages <- lget(args, 'setBreakpointsInPackages', getOption('vsc.defaultSetBreakpointsInPackages', FALSE))
  session$overwriteCat <- lget(args, 'overwriteCat', getOption('vsc.defaultOverwriteCat', TRUE))
  session$overwriteMessage <- lget(args, 'overwriteMessage', getOption('vsc.defaultOverwriteMessage', TRUE))
  session$overwriteStr <- lget(args, 'overwriteStr', getOption('vsc.defaultOverwriteStr', TRUE))
  session$overwritePrint <- lget(args, 'overwritePrint', getOption('vsc.defaultOverwritePrint', TRUE))
  session$overwriteSource <- lget(args, 'overwriteSource', getOption('vsc.defaultOverwriteSource', TRUE))
  session$overwriteLoadAll <- lget(args, 'overwriteLoadAll', getOption('vsc.defaultOverwriteLoadAll', TRUE))
  session$splitOverwrittenOutput <- lget(args, 'splitOverwrittenOutput', FALSE)
  session$debuggedPackages <- lget(args, 'debuggedPackages', character(0))
  session$loadPackages <- lget(args, 'loadPackages', character(0))
  session$loadSilently <- lget(args, 'loadSilently', FALSE)
  session$noDebug <- lget(args, 'noDebug', FALSE)
  session$supportsWriteToStdinEvent <- lget(args, 'supportsWriteToStdinEvent', FALSE)
  session$supportsShowingPromptRequest <- lget(args, 'supportsShowingPromptRequest', FALSE)
  session$supportsStdoutReading <- lget(args, 'supportsStdoutReading', FALSE)
  session$useCustomSocket <- lget(args, 'useCustomSocket', FALSE)
  session$customPort <- lget(args, 'customPort', 0)
  session$customHost <- lget(args, 'customHost', 'localhost')
  session$assignToAns <- lget(args, 'assignToAns', TRUE)
  return(invisible(NULL))
}
