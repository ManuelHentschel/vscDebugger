



#' vscDebugger
#' 
#' Implementation of the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) for R
#' 
#' Provides support for the vscode extension [vscode-R-Debugger](https://github.com/ManuelHentschel/VSCode-R-Debugger).
#' 
#' 
#' @docType package
#' @name vscDebugger-package
#' 
NULL


# make placeholder session object
session <- new.env()

# create environment for global data used by package functions
initSession <- function() {
  eval(quote({
    # settings:
    # (usually changed globally, persisting across debug sessions)
    varInfos <- list()

    # debugSession:
    # (set for this debug session)
    allowGlobalDebugging <- FALSE
    overwritePrint <- TRUE
    overwriteCat <- TRUE
    overwriteMessage <- TRUE
    overwriteStr <- TRUE
    overwriteSource <- TRUE
    overwriteLoadAll <- TRUE
    overwriteHelp <- TRUE
    splitOverwrittenOutput <- FALSE

    supportsInvalidatedEvent <- FALSE
    noDebug <- FALSE
    debugMode <- ''
    workingDirectory <- ''
    file <- ''
    mainFunction <- 'main'
    includePackageScopes <- FALSE
    setBreakpointsInPackages <- FALSE
    debuggedPackages <- character(0)
    loadPackages <- character(0)
    loadSilently <- FALSE
    assignToAns <- TRUE

    previousOptions <- list()
    internalOptions <- list()
    taskCallback <- 0

    pid <- 0
    ppid <- 0
    terminalId <- ''

    # server/communication:
    # (set for this debug session)
    # (should not influence the behaviour of the "R facing part" of the debugger)

    useDapSocket <- FALSE
    dapPort <- 18721
    dapHost <- 'localhost'
    dapSocketConnection <- NULL

    useJsonSocket <- FALSE
    jsonPort <- 0
    jsonHost <- 'localhost'
    jsonSocketConnection <- NULL

    useSinkSocket <- FALSE
    sinkPort <- 0
    sinkHost <- 'localhost'
    sinkSocketConnection <- NULL
    sinkNumber <- 0

    useCustomSocket <- FALSE
    customPort <- 18720
    customHost <- 'localhost'
    customSocketConnection <- NULL

    socketServer <- FALSE
    svName <- ''
    restOfWs <- ''

    threadId <- 1

    rStrings <- list(
      packageName = 'vscDebugger',
      attachName = 'tools:vscDebugger'
    )

    # custom events/requests:
    supportsWriteToStdinEvent <- FALSE
    supportsShowingPromptRequest <- FALSE
    supportsStdoutReading <- FALSE
    supportsHelpViewer <- FALSE

    # state:
    # (is managed by the debugger itself and might change frequently)
    breakOnErrorFromConsole <- FALSE
    breakOnErrorFromFile <- TRUE
    entryFrames <- c()
    launchFrames <- c()
    breakpointId <- 1
    stopListeningOnPort <- FALSE
    clearStackTree <- FALSE
    restOfLine <- ''

    state <- NULL
    pendingEvalResponses <- list()


    # data:
    # (like 'state', but contains longer lists etc.)
    rootNode <- NULL
    sourceBreakpointsList <- list()
    sources <- list()
    print_help_files_with_topic_0 <- NULL
    print_hsearch_0 <- NULL
    breakpointEnvironments <- list()


    # lock and return the environment:
    lockEnvironment(environment())
  }), envir = session)

  # initialize dynamic entries:
  session$varInfos <- getDefaultVarInfos()
  session$rootNode <- RootNode$new()
  session$state <- State$new()
  invisible(NULL)
}

.onLoad <- function(...) {
  options(error = traceback)
  initSession()
}

#' @export
.vsc.getSession <- function(entry=NULL, default=NULL){
  if(is.null(entry)){
    ret <- as.list(session)
    ret$state <- as.list(ret$state)
    return(ret)
  } else{
    lget(session, entry, default)
  }
}



State <- R6::R6Class(
  classname = "State",
  public = list(
    baseState = '',
    running = FALSE,
    runningWhat = NULL,
    evalSilent = FALSE,
    pausedOn = '',
    hasHitError = FALSE,
    update = function(
      changeBaseState = FALSE,
      baseState = '', # only for changeBaseState==TRUE
      startRunning = FALSE,
      runningWhat = '', # only for startRunning==TRUE
      evalSilent = FALSE, # only for runningWhat=='eval'
      startPaused = FALSE,
      pausedOn = '' # only for startPaused==TRUE
    ){
      prevState <- self$export()
      if(changeBaseState){
        self$baseState <- baseState
        self$running<- FALSE
        self$hasHitError <- FALSE
      }
      if(startRunning){
        self$running<- TRUE
        if(!is.null(runningWhat)){
          self$runningWhat <- runningWhat
        }
        if(!is.null(evalSilent)){
          self$evalSilent <- evalSilent
        }
      } else if(startPaused){
        self$running<- FALSE
        if(!is.null(pausedOn)){
          self$pausedOn <- pausedOn
          if(pausedOn == "error"){
            self$hasHitError <- TRUE
          }
        }
        logCat('State: starting paused on', toString(pausedOn), '\n')
      }
      return(prevState)
    },
    changeBaseState = function(baseState, startRunning=FALSE, startPaused=FALSE){
      prevState <- self$update(changeBaseState=TRUE, baseState=baseState)
      if(startRunning){
        runningWhat <- switch(
          baseState,
          starting = "",
          loadLib = "loadLib",
          sourceMain = "sourceMain",
          runMain = "main",
          runFile = "file",
          workspace = "eval",
          attached = "attachedCode",
          quitting = "",
          ""
        )
        self$startRunning(runningWhat=runningWhat)
      } else if(startPaused){
        pausedOn <- switch(
          baseState,
          workspace = "toplevel",
          ""
        )
        self$startPaused(pausedOn=pausedOn)
      }
      return(prevState)
    },
    startRunning = function(runningWhat=NULL, evalSilent=FALSE){
      self$update(startRunning=TRUE, runningWhat=runningWhat, evalSilent=evalSilent)
    },
    startPaused = function(pausedOn=NULL){
      self$update(startPaused=TRUE, pausedOn=pausedOn)
    },
    export = function(){
      list(
        baseState = self$baseState,
        running= self$running,
        runningWhat = self$runningWhat,
        evalSilent = self$evalSilent,
        pausedOn = self$pausedOn,
        hasHitError = self$hasHitError
      )
    },
    revert = function(state){
      prevState <- self$export()
      self$baseState <- state$baseState
      self$running<- state$running
      self$runningWhat <- state$runningWhat
      self$evalSilent <- state$evalSilent
      self$pausedOn <- state$pausedOn
      self$hasHitError <- state$hasHitError
      return(prevState)
    },
    isRunning = function(){
      self$running
    },
    isError = function(){
      self$hasHitError
    },
    isRunningFile = function(){
      self$running && (self$runningWhat == "file")
    },
    isSourcingMain = function(){
      self$running && (self$runningWhat == "sourceMain")
    },
    isRunningMain = function(){
      self$running && (self$runningWhat == "main")
    },
    isRunningFileOrMain = function(){
      self$running && ((self$runningWhat == "file") || (self$runningWhat == "main"))
    },
    isEvaluating = function(){
      self$running && (self$runningWhat == "eval")
    },
    isEvaluatingSilent = function(){
      self$running && (self$runningWhat == "eval") && self$evalSilent
    },
    isPaused = function(){
      !self$running
    },
    isPausedOnBreakpoint = function(){
      !self$running && (self$pausedOn == 'breakpoint')
    },
    isPausedOnError = function(){
      !self$running && (self$pausedOn == "error")
    },
    isPausedAfterError = function(){
      !self$running && self$hasHitError
    },
    isStarted = function(){
      !(self$baseState %in% c("starting", "loadLib", "sourceMain"))
    }
  )
)
