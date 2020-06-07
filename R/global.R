# create environment for global data used by package functions
session <- local({
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
  session$varInfos <- getDefaultVarInfos()
}
