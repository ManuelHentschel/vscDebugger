# create environment for global data used by package functions
session <- local({
  allowGlobalDebugging <- FALSE
  breakOnErrorFromConsole <- FALSE
  breakOnErrorFromFile <- TRUE
  assignToAns <- TRUE

  overwritePrint <- TRUE
  overwriteCat <- TRUE
  overwriteSource <- TRUE

  rStrings <- list(
    delimiter0 = '<v\\s\\c>',
    delimiter1 = '</v\\s\\c>',
    prompt = '<#v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
    continue = '<##v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
    append = ' ### <v\\s\\c\\COMMAND>'
  )

  threadId <- 1

  varInfos <- NULL

  useJsonServer <- FALSE
  jsonPort <- 0
  jsonHost <- 'localhost'
  jsonServerConnection <- NULL

  useSinkServer <- FALSE
  sinkPort <- 0
  sinkHost <- 'localhost'
  sinkServerConnection <- NULL

  noDebug <- FALSE # currently not used
  debugMode <- NULL
  workingDirectory <- NULL
  file <- NULL
  mainFunction <- NULL
  includePackageScopes <- NULL
  setBreakpointsInPackages <- FALSE

  isInitialized <- FALSE
  isConfigurationDone <- FALSE
  isEvaluating <- FALSE
  isError <- FALSE;
  entryFrames <- c()
  launchFrames <- c()
  ignoreNextCallback <- FALSE

  # tree <- NULL
  rootNode <- NULL
  # stackNodeId <- 0
  # frameIds <- list(
  #   vsc = list(),
  #   R = list(),
  #   node = list()
  # )
  # varRefs <- list(
  #   varRef = list(),
  #   node = list()
  # )
  # varRef <- 1
  breakpointId <- 1
  fileBreakpoints <- list()

  lockEnvironment(environment())
  environment()
})

.onLoad <- function(...) {
  options(error = traceback)
  setOptionIfNull('vsc.trySilent', TRUE)

  setOptionIfNull('vsc.evaluateActiveBindings', FALSE)
  setOptionIfNull('vsc.previewPromises', FALSE)
  setOptionIfNull('vsc.matricesByRow', TRUE)
  setOptionIfNull('vsc.dataFramesByRow', FALSE)
  setOptionIfNull('vsc.convertFactorEntries', FALSE)
  setOptionIfNull('vsc.showAttributes', TRUE)
  setOptionIfNull('vsc.showCustomAttributes', TRUE)


  setOptionIfNull('vsc.defaultIncludePackageScopes', FALSE)
  setOptionIfNull('vsc.includePackageScopes', FALSE)
  setOptionIfNull('vsc.setBreakpointsInPackages', FALSE)
  setOptionIfNull('vsc.assignToAns', TRUE)
  setOptionIfNull('vsc.overwritePrint', TRUE)
  setOptionIfNull('vsc.overwriteCat', TRUE)
  setOptionIfNull('vsc.overwriteSource', TRUE)

  setOptionIfNull('vsc.defaultDebugMode', 'file')
  setOptionIfNull('vsc.defaultAllowGlobalDebugging', FALSE)
  setOptionIfNull('vsc.defaultFile', 'main.R')

  session$varInfos <- getDefaultVarInfos()
  session$rootNode <- RootNode$new()
}

#' @export
.vsc.getSession <- function(){
  return(session)
}
