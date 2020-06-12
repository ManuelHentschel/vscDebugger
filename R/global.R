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
}
