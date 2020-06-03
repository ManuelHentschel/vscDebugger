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
