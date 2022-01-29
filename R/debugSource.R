
# TODO: function definitions:
#       - do not modify function body directly
#       - instead: define function normally, then trace function (-> bp can be easily unset)
# TODO: enable breakpoints in specific columns?


#' Modified version of `base::source`
#' 
#' Modified version of `base::source` that honors breakpoints set by the debugger.
#' 
#' @param file String giving the name of the file to be sourced. Connections etc. are not supported!
#' @param local Same as in [base::source()], can be overwritten by specifying an environment in `envir`.
#' @param envir The environment in which to evaluate the sourced code. Overwrites `local`, if specified.
#' @param chdir Whether to temporarily change the working directory to the location of `file`.
#' @param encoding The encoding to be used by [base::parse()].
#' @param ... Further arguments are ignored but allowed for compatibility with [base::source()].
#' 
#' @family overwrites
#' 
#' @export
.vsc.debugSource <- function(
  file,
  local = FALSE,
  envir = NULL,
  chdir = FALSE,
  print.eval = NULL,
  encoding = "unknown",
  isRmd = NULL,
  ...
) {
  # determine envir
  if(!is.null(envir)){
    # do nothing
  } else if(is.environment(local)){
    envir <- local
  } else if(local){
    envir <- parent.frame()
  } else{
    envir <- globalenv()
  }
  
  # check print.eval
  if(is.null(print.eval)){
    print.eval <- getOption('vsc.defaultPrintEval', 0)
  }

  # parse file:
  path <- normalizePath(file)
  usePurl <- identical(isRmd, TRUE) || (is.null(isRmd) && endsWith(tolower(path), '.rmd'))
  if(usePurl){
    body0 <- purlAndParse(path)
  } else{
    body0 <- parse(path, encoding = encoding, keep.source = TRUE)
  }

  # store state
  if(chdir){
    tmpwd <- setwd(dirname(path))
  }

  registerLaunchFrame(2)
  ret <- NULL
  # actually run the code:
  for(i in seq_along(body0)){
    body <- setAndUpdateBreakpoints(body0, path)
    expr <- body[i]
    # expr <- encloseBody(expr)
    # ret <- eval(expr, envir=envir)
    valueAndVisible <- withVisible(eval(expr, envir=envir))
    ret <- valueAndVisible$value
    if(print.eval>=2 || (valueAndVisible$visible && print.eval>=1)){
      cl <- substitute(.vsc.print(ret))
      attributes(cl) <- attributes(body[i])
      cl <- encloseBody(cl)
      eval(cl)
    }
  }
  # is the same as eval(body, envir=envir), but without the extra stack frame inbetween
  unregisterLaunchFrame()


  # restore state
  if(chdir){
    setwd(tmpwd)
  }

  invisible(ret)
}


setAndUpdateBreakpoints <- function(body, path, line0=1){
  sourceBreakpoints <- getSourceBreakpoints(path)
  bps <- sourceBreakpoints$breakpoints
  lines <- lapply(bps, '[[', 'line')

  # find steps/expressions corresponding to the requested lines:
  ats <- lapply(lines, lineFind, body)

  # check if bps were found and confirm breakpoints to vsc
  for (i in seq_along(bps)) {
    if(lines[[i]] < line0){
      next
    }
    verifiedBefore <- bps[[i]]$verified
    verifiedNow <- (length(ats[[i]]) > 0)
    bps[[i]]$verified <- verifiedNow
    bps[[i]]$changed <- (verifiedNow != verifiedBefore)
  }
  bps <- sendBreakpoints(bps)

  sourceBreakpoints$breakpoints <- bps
  storeSourceBreakpoints(sourceBreakpoints)

  # set breakpoints:
  body <- mySetBreakpoints(body, ats)  
  
  return(body)
}


mySetBreakpoints <- function(body, ats = list(), finalize = TRUE) {
  # iteratively set breakpoints:
  for (at in ats) {
    body <- mySetBreakpoint(body, at = at, finalize = FALSE)
  }

  # enclose entire body in {}:
  if (finalize) {
    # body <- encloseBody(body)
    for(i in seq_along(body)){
      expr <- body[i]
      if(tryCatch(body[[i]][[1]] != as.name('{'), error = function(...) FALSE)){
        body[i] <- encloseBody(body[i])
      }
    }
  }

  return(body)
}

mySetBreakpoint <- function(body, at, finalize = FALSE) {
  if (length(at) == 0) {
    # do nothing
  } else if (length(at) == 1) {
    # innermost step: replace expression expr with {browser(), expr}
    atr <- attributes(body)
    # srcref <- list(atr$srcref[[at]], atr$srcref[[at]], atr$srcref[[at]], atr$srcref[[at]], atr$srcref[[at]])
    # base::cat() a dummy tracing statement to indicate to vsc that this breakpoint is set by the debugger (-> sends 1x 'n' immediately)
    # b2 <- call('{', quote(base::cat('Tracing debugSourceBreakpoint step XXX\n')), quote(.doTrace(browser())), body[[at]])
    callAsList <- list(
      as.name('{'),
      quote(base::cat('Tracing debugSourceBreakpoint step XXX\n')),
      quote(vscDebugger::.vsc.preDebugSourceBreakpoint()),
      quote(.doTrace(browser())),
      body[[at]]
    )
    srcref <- replicate(length(callAsList), atr$srcref[[at]], simplify = FALSE)
    b2 <- as.call(callAsList)
    # b2 <- call('{', quote(base::cat('Tracing debugSourceBreakpoint step XXX\n')), quote(.vsc.preBreakpoint()), quote(.doTrace(browser())), body[[at]])
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
  if(length(srcref) == 0){
    return(NULL)
  }
  # used to make a new srcref that contains a dummy for the call to '{'
  dummySrcref <- c(1, 1, 1, 1, 1, 1, 1, 1)
  attributes(dummySrcref) <- attributes(srcref[[1]])
  newSrcref <- append(list(dummySrcref), srcref)
  attributes(newSrcref) <- attributes(srcref)
  return(newSrcref)
}

findLine <- function(b, line, at = c()) {
  # recursively find a given line in a body `b` 
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
    for (i in seq_along(b)) {
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

purlAndParse <- function(fileName){
  txt <- purlAndRead(fileName)
  body0 <- parse(text = txt, keep.source = TRUE)
  attr(body0, 'srcfile')[['filename']] <- fileName
  return(body0)
}

purlAndRead <- function(fileName){
  ret <- NULL
  file <- textConnection('ret', open = 'w', local = TRUE)
  knitr::purl(fileName, output = file, quiet = TRUE, documentation = 2)
  close(file)
  ret <- paste0(ret, collapse = '\n')
  return(ret)
}

