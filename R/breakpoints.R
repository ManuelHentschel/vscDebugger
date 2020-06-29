
#' Set breakpoints in a file
#'
#' Set breakpoints in a file, and sends a confirmation message to vsc
#'
#' @param srcfile The file in which to set breakpoints
#' @param lines A list of lines in which to set breakpoints
#' @param ids A list of numbers, specifying the id of each breakpoint. Same length as `lines`
#' @param includePackageScopes Whether to set breakpoints in packages
#' @param id The id of the answer sent to vsc
#'
.vsc.setBreakpoints <- function(file, bps = NULL, includePackageScopes = NULL, id = 0) {
  # breakpoints: bp[]
  # bp: {id: number; line: number; verified: boolean}

  # make sure includePackageScopes is bool:
  if (is.null(includePackageScopes)) {
    includePackageScopes <- FALSE
  }

  if (includePackageScopes) {
    lastenv <- emptyenv() # searches through package-envs as well
  } else {
    lastenv <- .GlobalEnv # searches only through 'user'-envs
  }

  linesInFile <- length(readLines(file))

  refList <- list()
  for (i in seq_along(bps)) {
    bp <- bps[[i]]
    line <- bp$requestedLine
    maxOffset <- lget(bp, 'maxOffset', 0)
    maxLine <- min(line + maxOffset, linesInFile)

    # find line numbers in functions
    # might return multiple refs
    refs <- findLineNum2(file, line, maxLine, lastenv = lastenv)

    # store occurences of line (for R)
    refList <- append(refList, refs)

    # store info about bp (for vsc)
    if (length(refs) > 0) {
      bp$verified <- TRUE
      bp$line <- refs[[1]]$line
      bp$rAt <- refs[[1]]$at
    } else {
      bp$verified <- FALSE
      bp$line <- 0
      bp$rAt <- NULL
    }
    bp$attempted <- TRUE
    bps[[i]] <- bp
  }

  # summarize refs: all bps in the same function need to be set with one call to trace()
  summarizedRefs <- summarizeRefs(refList)

  # set breakpoints
  for (sRef in summarizedRefs) {
    # use generic trace function -> does not preserve source info
    trace(
      what = sRef$name,
      tracer = browser,
      at = sRef$at,
      where = sRef$env
    )
    # add source info to lines overwritten by trace():
    fixSrcrefOnTracedFunction(
      what = sRef$name,
      at = sRef$at,
      where = sRef$env
    )
  }

  # send breakpoints to vsc
  sendBreakpoints(bps, id = id)

  return(bps)
}

fixSrcref <- function(f, at){
  at0 <- at[ -length(at) ]
  at1 <- at[ length(at) ]
  if(length(at)==0){
    return(f)
  } else if(length(at)==1){
    b <- body(f)
  } else{
    b <- body(f)[[at0]]
  }
  sr <- attr(b, 'srcref')[[at1]]
  srNew <- lapply(body(f)[[at]], function(...) sr)
  attr(body(f)[[at]], 'srcref') <- srNew
  return(f)
}

fixSrcrefOnTracedFunction <- function(what, at, where){
  f <- get(what, envir=where)
  if(!is.list(at)){
    at <- list(at)
  }
  f2 <- f@.Data
  for(atEntry in at){
    f2 <- fixSrcref(f2, atEntry)
  }
  f@.Data <- f2
  methods:::.assignOverBinding(what, f, where, FALSE)
}

sendBreakpoints <- function(bps = list(), acknowledge = TRUE, id = 0) {
  for (bp in bps) {
    sendBreakpointEvent("changed", bp)
  }
}


findLineNum2 <- function(srcfile, firstLine, lastLine = firstLine, ...) {
  # same as findLineNum, but continues down the lines until a valid lines is found
  # maxLine should be the number of lines in the file or e.b. line+<maxOffset>
  refs <- NULL
  for (line in seq2(firstLine, lastLine)) {
    refs <- findLineNum(srcfile = srcfile, line = line, ...)
    if (length(refs) > 0) {
      break
    }
  }
  return(refs)
}


summarizeRefs <- function(refList) {
  summarizedRefs <- list()
  for (ref in refList) {
    found <- FALSE
    for (j in seq_along(summarizedRefs)) {
      sRef <- summarizedRefs[[j]]
      if (identical(ref$name, sRef$name) && identical(ref$env, sRef$env)) {
        found <- TRUE
        # avoid adding the same breakpoint twice:
        if (!any(sapply(sRef$at, identical, ref$at))){
          sRef$at <- appendToList(sRef$at, ref$at)
          sRef$line <- appendToList(sRef$line, ref$line)
          sRef$requestedLine <- appendToList(sRef$requestedLine, ref$requestedLine)
          sRef$timediff <- appendToList(sRef$timediff, ref$timediff)
          summarizedRefs[[j]] <- sRef
        }
        break
      }
    }
    if (!found) {
      sRef <- ref
      sRef$at <- list(ref$at)
      sRef$line <- list(ref$line)
      sRef$requestedLine <- list(ref$requestedLine)
      sRef$timediff <- list(ref$timediff)
      summarizedRefs <- appendToList(summarizedRefs, sRef)
    }
  }
  return(summarizedRefs)
}


appendToList <- function(oldList, ...) {
  if (!is.list(oldList)) {
    oldList <- list(oldList)
  }
  newList <- base::append(oldList, list(...))
  return(newList)
}
