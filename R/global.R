# create environment for global data used by package functions
session <- local({
  varLists <- list()
  varListArgs <- list()
  varListPersistent <- list()
  isEvaluating <- FALSE
  frameIdsR <- list()
  frameIdsVsc <- list()
  breakpoints <- list()
  varInfo <- NULL
  debugGlobal <- FALSE
  environment()
})

.onLoad <- function(...) {
  options(error = traceback)
  options(vsc.previewPromises = FALSE)
  options(vsc.trySilent = TRUE)
  session$varInfo <- defaultVarInfo
}
