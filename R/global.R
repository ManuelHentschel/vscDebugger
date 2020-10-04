# create environment for global data used by package functions
session <- local({
  # settings:
  # (usually changed globally, persisting across debug sessions)
  varInfos <- NULL

  # debugSession:
  # (set for this debug session)
  allowGlobalDebugging <- FALSE
  overwritePrint <- TRUE
  overwriteCat <- TRUE
  overwriteSource <- TRUE

  noDebug <- FALSE # currently not used
  debugMode <- NULL
  workingDirectory <- NULL
  file <- NULL
  mainFunction <- NULL
  includePackageScopes <- NULL
  setBreakpointsInPackages <- FALSE
  packagesBeforeLaunch <- character(0)

  # server/communication:
  # (set for this debug session)
  # (should not influence the behaviour of the "R facing part" of the debugger)
  useJsonServer <- FALSE
  jsonPort <- 0
  jsonHost <- 'localhost'
  jsonServerConnection <- NULL

  useSinkServer <- FALSE
  sinkPort <- 0
  sinkHost <- 'localhost'
  sinkServerConnection <- NULL

  threadId <- 1

  rStrings <- list(
    delimiter0 = '<v\\s\\c>',
    delimiter1 = '</v\\s\\c>',
    prompt = '<#v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
    continue = '<##v\\s\\c>' #actual prompt is followed by a newline to make easier to identify
  )

  # state:
  # (is managed by the debugger itself and might change frequently)
  breakOnErrorFromConsole <- FALSE
  breakOnErrorFromFile <- TRUE
  entryFrames <- c()
  launchFrames <- c()
  breakpointId <- 1
  stopListeningOnPort <- FALSE
  restOfLine <- ''

  state <- NULL
  pendingEvalResponses <- list()


  # data:
  # (like 'state', but contains longer lists etc.)
  rootNode <- NULL
  fileBreakpoints <- list()
  sources <- list()


  # lock and return the environment:
  lockEnvironment(environment())
  environment()
})

.onLoad <- function(...) {
  options(error = traceback)
  session$varInfos <- getDefaultVarInfos()
  session$rootNode <- RootNode$new()
  session$state <- State$new()
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
    isPausedOnError = function(){
      !self$running&& (self$pausedOn == "error")
    },
    isPausedAfterError = function(){
      !self$running&& self$hasHitError
    }
  )
)
