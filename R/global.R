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

  noDebug <- FALSE

  varLists <- list()
  varListArgs <- list()
  varListPersistent <- list()
  isRunningDebugSession <- FALSE
  isEvaluating <- FALSE
  frameIdsR <- list()
  frameIdsVsc <- list()
  breakpoints <- list()
  varInfos <- list()
  debugGlobal <- FALSE
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

  time <- Sys.time()

  environment()
})

.onLoad <- function(...) {
  options(error = traceback)
  if(is.null(getOption('vsc.previewPromises'))){
    options(vsc.previewPromises = FALSE)
  }
  if(is.null(getOption('vsc.trySilent'))){
    options(vsc.trySilent = TRUE)
  }
  if(is.null(getOption('vsc.matricesByRow'))){
    options(vsc.matricesByRow = TRUE)
  }
  if(is.null(getOption('vsc.evaluateActiveBindings'))){
    options(vsc.evaluateActiveBindings = FALSE)
  }
  session$varInfos <- getDefaultVarInfos()

  session$tree <- LazyTree(
    childrenFunction = childrenFunction,
    contentFunction = contentFunction,
    defaultContentProducesChildren = TRUE
  )
  session$rootNode <- session$tree$getNewNodeId()
}
