# create environment for global data used by package functions
session <- local({
  stackNode <- 0
  frameIds <- list(
    vsc = list(),
    R = list(),
    node = list()
  )
  varRefs <- list(
    varRef = list(),
    node = list()
  )
  varRef <- 1
  tree <- NULL
  setBreakpointsInPackages <- FALSE
  breakpointId <- 1
  fileBreakpoints <- list()
  threadId <- 1

  overwritePrint <- TRUE
  overwriteCat <- TRUE
  overwriteSource <- TRUE

  ignoreNextCallback <- FALSE
  isError <- FALSE

  noDebug <- FALSE

  useServer <- FALSE
  port <- 0
  host <- '127.0.0.1'
  serverConnection <- NULL

  varLists <- list()
  varListArgs <- list()
  varListPersistent <- list()
  isRunningDebugSession <- FALSE
  isEvaluating <- FALSE
  frameIdsR <- list()
  frameIdsVsc <- list()
  breakpoints <- list()
  varInfos <- list()
  allowGlobalDebugging <- FALSE
  srcBreakpoints <- list()
  breakOnErrorFromConsole <- FALSE
  breakOnErrorFromFile <- TRUE
  assignToAns <- TRUE

  rStrings <- list(
    delimiter0 = '<v\\s\\c>',
    delimiter1 = '</v\\s\\c>',
    prompt = '<#v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
    continue = '<##v\\s\\c>', #actual prompt is followed by a newline to make easier to identify
    append = ' ### <v\\s\\c\\COMMAND>'
  )

  environment()
})

.onLoad <- function(...) {
  options(error = traceback)
  setOptionIfNull('vsc.previewPromises', FALSE)
  setOptionIfNull('vsc.trySilent', TRUE)
  setOptionIfNull('vsc.matricesByRow', TRUE)
  setOptionIfNull('vsc.evaluateActiveBindings', FALSE)
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

  session$tree <- LazyTree(
    childrenFunction = childrenFunction,
    contentFunction = contentFunction,
    defaultContentProducesChildren = TRUE
  )
  session$rootNode <- session$tree$getNewNodeId()
}

