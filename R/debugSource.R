
# TODO: make sure browser-output is correctly parsed by vsc
# TODO: make sure call stack is correctly returned to vsc (skipped frames etc.)
# TODO: function definitions:
#       - do not modify function body directly
#       - instead: define function normally, then trace function (-> bp can be easily unset)
# TODO: recursive debugSource() -> simply replace source() in .GlobalEnv
# TODO: enable breakpoints in specific columns?

#' @export
.vsc.debugSource <- function(file, lines = list(), envir = parent.frame(), encoding = "unknown", applyInternalBreakpoints = TRUE, recursive = TRUE, ...) {
  # parse file:
  file <- normalizePath(file)
  body <- parse(file, encoding = encoding, keep.source = TRUE)

  # apply breakpoints stored in .packageEnv$breakpoints
  if (applyInternalBreakpoints) {
    bps <- .vsc.getBreakpoints(file)
    lines <- .vsc.getBreakpointLines(file)
  }

  # find steps/expressions corresponding to the requested lines:
  ats <- lapply(lines, lineFind, body)

  # check if bps were found and confirm breakpoints to vsc
  for (i in seq2(bps)) {
    bps[[i]]$verified <- length(ats[[i]]) > 0
  }
  sendBreakpoints(bps)


  # set breakpoints:
  body <- mySetBreakpoints(body, ats)

  # store debugState
  tmpDebugGlobal <- .packageEnv$debugGlobal
  .packageEnv$debugGlobal <- FALSE

  # actually run the code:
  enclos <- baseenv()
  .Internal(eval(body, envir, enclos))
  # is the same as eval(body, envir=envir), but without the extra stack frame inbetween


  # restore debugState
  .packageEnv$debugGlobal <- tmpDebugGlobal
}




mySetBreakpoints <- function(body, ats = list(), finalize = TRUE) {
  # iteratively set breakpoints:
  for (at in ats) {
    body <- mySetBreakpoint(body, at = at, finalize = FALSE)
  }

  # enclose entire body in {}:
  if (finalize) {
    body <- encloseBody(body)
  }

  return(body)
}

mySetBreakpoint <- function(body, at, finalize = FALSE) {
  if (length(at) == 0) {
    # do nothing
  } else if (length(at) == 1) {
    # innermost step: replace expression expr with {browser(), expr}
    atr <- attributes(body)
    srcref <- list(atr$srcref[[at]], atr$srcref[[at]], atr$srcref[[at]], atr$srcref[[at]])
    # cat() a dummy tracing statement to indicate to vsc that this breakpoint is set by the debugger (-> sends 1x 'n' immediately)
    b2 <- call('{', quote(base::cat('Tracing debugSourceBreakpoint step\n')), quote(.doTrace(browser())), body[[at]])
    b2 <- structure(
      b2,
      srcref = srcref,
      srcfile = atr$srcfile,
      wholeSrcref = atr$wholeSrcref
    )

    body[[at]] <- b2
  } else {
    # outer steps: continue recursively
    body[[at[1]]] <- mySetBreakpoint(body[[at[1]]], at[-1], finalize = FALSE)
  }

  if (finalize) {
    body <- encloseBody(body)
  }

  return(body)
}

encloseBody <- function(body) {
  # Below is a complicated way of turning this:
  #     doStuff(1)
  #     ...
  #     doStuff(2)
  # into this:
  #     {
  #         doStuff(1)
  #         ...
  #         doStuff(2)
  #     }
  # while preserving srcrefs.
  # (Is necessary to enable correct browser()-behaviour.)

  # surround body with '{', '}'
  b2 <- as.call(append(as.list(call('{')), body))

  # make new srcref that contains a dummy for the call to '{'
  newSrcref <- prependDummySrcref(attr(body, 'srcref'))

  # add srcref to new body
  b2 <- structure(
    b2,
    srcref = newSrcref,
    srcfile = attr(body, 'srcfile'),
    wholeSrcref = attr(body, 'wholeSrcref')
  )

  # convert body to expression
  ex <- as.expression(b2)

  # add attributes to expression
  b2 <- structure(
    ex,
    srcref = attr(body, 'wholeSrcref'),
    srcfile = attr(body, 'srcfile'),
    wholeSrcref = attr(body, 'wholeSrcref')
  )

  # assign to b
  return(b2)
}

prependDummySrcref <- function(srcref) {
  dummySrcref <- c(1, 1, 1, 1, 1, 1, 1, 1)
  attributes(dummySrcref) <- attributes(srcref[[1]])
  newSrcref <- append(list(dummySrcref), srcref)
  attributes(newSrcref) <- attributes(srcref)
  return(newSrcref)
}

findLine <- function(b, line, at = c()) {
  if (hasSrcref(b)) {
    at <- findLineWithSrcref(b, line, at)
  } else {
    at <- findLineWithoutSrcref(b, line, at)
  }
  return(at)
}
lineFind <- function(line, b, at = c()) {
  # swapped arguments for easier use in lapply()
  findLine(b, line, at)
}

findLineWithoutSrcref <- function(b, line, at) {
  try({
    # fails if b is a symbol -> try()
    for (i in seq(length(b))) {
      # try all sub-steps of b
      bb <- b[[i]]
      if (length(bb) > 1) {
        # assumption: only calls to '{' have sourceref -> length>1

        # (provisionally) append step to at
        at2 <- c(at, i)

        # continue search
        at3 <- findLine(bb, line, at2)

        # if length increased, the line was found down the parse-tree
        if (length(at3) > length(at2)) {
          at <- at3
          break
        }
      }
    }
  }, silent = TRUE)
  return(at)
}

findLineWithSrcref <- function(b, line, at) {
  # get source info about each step
  sr <- attr(b, 'srcref')

  # get interval of lines covered by each step
  mms <- lapply(sr, getMinLineMaxLine)

  # find the step/interval that contains the requested line
  ind <- findInterval(mms, line)

  if (ind == 0) {
    # not found
    return(at)
  } else {
    # get the content of the corresponding step (might be a long block itself)
    bb <- b[[ind]]

    # add index of step to at
    at <- c(at, ind)

    # continue search
    at <- findLine(bb, line, at)
    return(at)
  }
}

getMinLineMaxLine <- function(sr) {
  # srcref is an integer vector containing info about lines, columns, and bytes
  # simply select the entries corresponding to minLine, maxLine:
  sr[c(1, 3)]
}

findInterval <- function(mms, line) {
  # checks only lines -> improve by also allowing breakpoint location by col?
  # only accepts precise hits -> return 'closest' hit to auto-correct bps?
  hits <- sapply(mms, intervalContains, line)
  ind <- which(hits)[1]
  if (is.na(ind)) {
    return(0)
  } else {
    return(ind)
  }
}

intervalContains <- function(interval, x) {
  return(x >= interval[1] && x <= interval[2])
}

hasSrcref <- function(x) {
  'srcref' %in% names(attributes(x))
}
