

setBreakpoints <- function(
  sourceBreakpoints,
  unsetBreakpoints=FALSE,
  envs=list(),
  inNormalEnvs=TRUE
){
  path <- sourceBreakpoints$source$path
  bps <- sourceBreakpoints$breakpoints
  refList <- list()

  for (i in seq_along(bps)) {
    bp <- bps[[i]]
    line <- bp$requestedLine

    # find line number in additional envs (= debugged packages)
    refs <- list()
    for(env in envs){
      newRefs <- try(
        findLineNum(path, line, nameonly = FALSE, envir=env, lastenv=env),
        silent = TRUE
      )
      if(!inherits(newRefs, 'try-error')){
        refs <- c(refs, newRefs)
      }
    }

    # store occurences of line (for R)
    refList <- c(refList, refs)

    # store info about bp (for vsc)
    if (length(refs) > 0) {
      bp$verified <- !unsetBreakpoints
      bp$line <- refs[[1]]$line
      bp$changed <- TRUE
    } else {
      # bp$verified <- FALSE
      # bp$line <- 0
    }
    bp$attempted <- TRUE
    bps[[i]] <- bp
  }

  # summarize refs: all bps in the same function need to be set with one call to trace()
  summarizedRefs <- summarizeRefs(refList)

  # set breakpoints
  for(sRef in summarizedRefs){
    if(unsetBreakpoints){
      # remove breakpoints
      suppressMessages(try(
        untrace(
          what = sRef$name,
          where = sRef$env
        ),
        silent = TRUE
      ))
    } else{
      # use generic trace function -> does not preserve source info
      suppressMessages(try(
        trace(
          what = sRef$name,
          tracer = quote({vscDebugger::.vsc.preBreakpoint(); browser()}),
          at = sRef$at,
          where = sRef$env
        ),
        silent = TRUE
      ))
      # add source info to lines overwritten by trace():
      fixSrcrefOnTracedFunction(
        what = sRef$name,
        at = sRef$at,
        where = sRef$env
      )
    }
  }

  # send breakpoints to vsc
  bps <- sendBreakpoints(bps)

  sourceBreakpoints$breakpoints <- bps

  return(sourceBreakpoints)
}

sendBreakpoints <- function(bps) {
  for (bp in bps) {
    if(!is.null(bp$changed) && bp$changed){
      sendBreakpointEvent("changed", bp)
    }
    bp$changed <- FALSE
  }
  return(bps)
}

summarizeRefs <- function(refList){
  summarizedRefs <- list()
  for(ref in refList){
    found <- FALSE
    for(j in seq_along(summarizedRefs)){
      sRef <- summarizedRefs[[j]]
      if(identical(ref$name, sRef$name) && identical(ref$env, sRef$env)){
        # update sRef
        found <- TRUE
        # avoid adding the same breakpoint twice: (necessary?)
        if (!any(sapply(sRef$at, identical, ref$at))){
          sRef$at <- c(sRef$at, list(ref$at))
          sRef$line <- c(sRef$line, list(ref$line))
          sRef$timediff <- c(sRef$timediff, list(ref$timediff))
          summarizedRefs[[j]] <- sRef
        }
        break
      }
    }
    if(!found){
      # make new sRef entry
      sRef <- list(
        name = ref$name,
        env = ref$env,
        at = list(ref$at),
        line = list(ref$line),
        timediff = list(ref$timediff)
      )
      summarizedRefs <- c(summarizedRefs, list(sRef))
    }
  }
  return(summarizedRefs)
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
  assignOverBinding(what, f, where, FALSE)
}

