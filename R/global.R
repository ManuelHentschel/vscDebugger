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

  rStrings <- list(
    delimiter0 = '<v\\s\\c>',
    delimiter1 = '</v\\s\\c>',
    prompt = '<#v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
    continue = '<##v\\s\\c>' #actual prompt is followed by a newline to make easier to identify
  )
  threadId <- 1

  # state:
  # (is managed by the debugger itself and might change frequently)
  breakOnErrorFromConsole <- FALSE
  breakOnErrorFromFile <- TRUE
  isInitialized <- FALSE
  isConfigurationDone <- FALSE
  isEvaluating <- FALSE
  isError <- FALSE;
  entryFrames <- c()
  launchFrames <- c()
  ignoreNextCallback <- FALSE
  breakpointId <- 1

  # data:
  # (like 'state', but contains longer lists etc.)
  rootNode <- NULL
  fileBreakpoints <- list()


  # lock and return the environment:
  lockEnvironment(environment())
  environment()
})

.onLoad <- function(...) {
  options(error = traceback)
  session$varInfos <- getDefaultVarInfos()
  session$rootNode <- RootNode$new()
}

#' @export
.vsc.getSession <- function(){
  return(session)
}
