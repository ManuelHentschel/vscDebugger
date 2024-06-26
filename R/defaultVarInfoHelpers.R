
# helper functions for the default varInfos:

#' Get variable from environment
#' 
#' Is basically a wrapper for `get(name, envir=env)`, but does not force promises.
#' 
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return Returns the value of the variable or a representation of a promise as returned by `getPromiseVar`
#' 
#' @keywords internal
getVarInEnv <- function(name, env) {
  # get Info about a variable in an environment
  # separate from getVariable(), since environments might contain promises
  tryCatch({
    if (name == '...') {
      getDotVars(env)
    } else if (isPromise(name, env, strict = FALSE)) {
      prom <- getPromiseVar(name, env)
      if (isTRUE(prom$evaluated)) prom$value else prom
    } else if (bindingIsActive(name, env) && !getOption('vsc.evaluateActiveBindings', FALSE)) {
      getActiveBinding(name, env)
    } else {
      get(name, envir = env)
    }
  }, error = function(e) {
    .text <- 
      if (missingInEnv(name, env)) {
        '<MISSING>'
      } else{
        '<ERROR>'
      }
    getInfoVar(.text)
  })
}

getActiveBinding <- function(name, env){
  ret <- if (getRversion() >= "4.0.0") {
    activeBindingFunction(name, env)
  } else {
    getInfoVar("R version >= 4.0.0 required to show active binding function!")
  }
  structure(list(bindingFunction = ret), class = c('.vsc.activeBinding', '.vsc.internalClass'))
}

getVarsInEnv <- function(env, ind=NULL, all.names = TRUE) {
  names <- ls(env, all.names = all.names)
  if(!is.null(ind)){
    names <- names[ind]
  }
  lapply(names, function(name) {
    list(
      value = getVarInEnv(name, env),
      name = name
    )
  })
}

missingInEnv <- function(name, env){
  ret <- tryCatch(
    do.call('missing', list(name), envir=env),
    error = function(e) FALSE
  )
  return(ret)
}

getInfoVar <- function(text, type='Warning: the variable value is just an info from the debugger!'){
  var <- list( text = text, type = type)
  class(var) <- c('.vsc.infoVar', '.vsc.internalClass')
  return(var)
}


#' Get unevaluated variables passed as ellipses
#' 
#' Get variables which are passed as ellipses without evaluating them.
#' @param env The environment where the "..." variable is present
#' @return A named list for each element of "..." containing the 
#'   expression that will be evaluated, 
#'   the environment where it will be evaluated, and the string 
#'   representation of the expression.
#' 
#' @keywords internal
getDotVars <- function(env) {
  ## Note: substitute(...()) is officially not supported, but it 
  ## works as intended for all relevant R versions (at least from R 3.0.0)
  dots <- substitute(...(), env = env)
  if(is.null(dots)) dots <- list()
  structure(dots, class = c(".vsc.ellipsis", ".vsc.internalClass"))
} 

#' Get a representation of a promise
#' 
#' Gets a representation of a promise without forcing the promise
#' 
#' @aliases .vsc.promise
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return
#' An object of class `.vsc.promise` or `.vc.promiseDetails`,
#' depending on the option `vsc.showPromiseDetails`.
#' The two classes are functionally identical and only rendered differently.
#' A named list containing the expression that will be evaluated,
#' the status whether the promise has already been evaluated,
#' the value if it has already been evaluated,
#' and the environment in which the unevaluated promise will be evaluated. 
#' 
#' @examples 
#' ## create a promise
#' e <- new.env()
#' delayedAssign("x", {message("evaluating promise..."); 1L}, assign.env = e)
#' vscDebugger:::getPromiseVar("x", e)
#' 
#' ## evaluate it...
#' base::print(e$x)
#' 
#' ## is it still a promise? (It depends...)
#' stopifnot(vscDebugger:::isPromise("x", e, strict = FALSE))
#' stopifnot(!vscDebugger:::isPromise("x", e, strict = TRUE))
#' 
#' ## get info again
#' vscDebugger:::getPromiseVar("x", e)
#' 
#' @keywords internal
getPromiseVar <- function(name, env) {
  if(getOption('vsc.showPromiseDetails', FALSE)){
    promiseClass <- '.vsc.promiseDetails'
  } else{
    promiseClass <- '.vsc.promise'
  }
  structure(
    getPromiseInfo(name, env),
    class = c(promiseClass, ".vsc.internalClass")
  )
}

#' Get information about a promise
#' 
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return A named list: 
#' * `code`: the expression that will be evaluated
#' * `environment`: the environment where the promise is evaluated
#' * `evaluated`: logical flag if the promise has been already evaluated
#' * `value`: optional node; the value of the evaluated promise
#' 
#' @keywords internal
#' @useDynLib vscDebugger c_promise_info 
getPromiseInfo <- function(name, env) {
  sym <- as.name(name)
  .Call(c_promise_info, sym, env)
}

#' Test if an object is a promise
#' 
#' @param name `character(1L)` the name of the object
#' @param env [`environment`] the environment of the object 
#' @param strict `logical(1L)` if \code{strict} is \code{TRUE} 
#'   (the default), evaluated promises return with \code{FALSE}
#' @return `TRUE` or `FALSE`
#' 
#' @keywords internal
#' @useDynLib vscDebugger c_is_promise
isPromise <- function(name, env, strict = TRUE) {
  sym <- as.name(name)
  .Call(c_is_promise, sym, env, strict)
}

# Used to deparse object, unless it's too large
deparseUnlessTooLarge <- function(v) {
  bytes <- object.size(v)
  maxBytes <- getOption('vsc.deparseMaxBytes', 1e5)
  if(bytes <= maxBytes){
    ret <- paste0(deparse(v), collapse = '\n')
  } else{
    ret <- '<Object too large>'
  }
  return(ret)
}



unsummarizeLists <- function(items, repeatItems = list(), names = NULL) {
  if (length(items) == 0) {
    return(list())
  }
  if (1 != length(unique(lapply(items, length)))) {
    stop('Not all item-lists of the same size.')
  }
  makeLists <- function(...) mapply(list, ..., MoreArgs = repeatItems, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  ret <- do.call(makeLists, args = items)
  names(ret) <- names
  return(ret)
}




getDimOrder <- function(nDims=2, dimOrder=NULL, isDataFrame=FALSE){
  if(is.null(dimOrder)){
    if(isDataFrame){
      dimOrder <- getOption('vsc.dataFrameDimOrder', NULL)
    }
    if(!isDataFrame || is.null(dimOrder)){
      dimOrder <- getOption('vsc.arrayDimOrder', c(3,1,2))
    }
  }
  if(is.list(dimOrder)){
    lens <- sapply(dimOrder, length)
    if(nDims %in% lens){
      dimOrder <- dimOrder[[match(nDims, lens)]]
    } else if(nDims > max(lens)){
      dimOrder <- dimOrder[[which.max(lens)]]
    } else{
      dimOrder <- dimOrder[nDims < lens]
      lens <- lapply(dimOrder, length)
      dimOrder <- dimOrder[[which.min(lens)]]
    }
  }
  len <- length(dimOrder)
  if(len>nDims){
    dimOrder <- dimOrder[dimOrder <= nDims]
  } else if(len<nDims){
    missing <- (len+1):nDims
    if(dimOrder[1]==length(dimOrder)){
      dimOrder <- c(rev(missing), dimOrder)
    } else{
      dimOrder <- c(dimOrder, missing)
    }
  }
  return(dimOrder)
}

getNextDim <- function(dimOrder, prevIndices, dims=NULL){
  dimOrder[match(0,prevIndices[dimOrder])]
}


arrayDimToList <- function(
  arr,
  dimension=NULL,
  ind=NULL,
  onlyNChildVars=FALSE
){
  if(inherits(arr, '.vsc.subArray')){
    arr0 <- arr
    arr <- attr(arr, '.vsc.undroppedArray')
    nDim <- length(dim(arr))
  } else{
    nDim <- length(dim(arr))
    arr0 <- arr
    attr(arr, '.vsc.dimOrder') <- getDimOrder(nDim, isDataFrame=is.data.frame(arr))
    prevIndices <-  rep_len(0, nDim)
    if(getOption('vsc.dropArrays',TRUE)){
      prevIndices[dim(arr)==1] <- 1
    }
    attr(arr, '.vsc.prevIndices') <- prevIndices
  }
  prevIndices <- attr(arr, '.vsc.prevIndices')
  if(is.null(dimension)){
    dimension <- getNextDim(attr(arr, '.vsc.dimOrder'), prevIndices)
  }
  if(onlyNChildVars){
    ret <- dim(arr)[dimension]
    if(length(ret)==0 || is.na(ret)){
      ret <- 0
    }
    return(ret)
  }
  if(is.null(ind)){
    ind <- seq_len(dim(arr)[dimension])
  }
  r <- replicate(nDim, substitute(), simplify=FALSE) # used to get all entries using `[`
  names <- dimnames(arr)[[dimension]]
  subArrays <- lapply(
    ind,
    function(k){
      # extract subArray
      r[[dimension]] <- k
      subArray <- do.call('[', c(list(arr, drop=FALSE), r))
      attr(subArray, '.vsc.dimOrder') <- attr(arr, '.vsc.dimOrder')

      # update previous indices of subArray
      newIndices <- prevIndices
      newIndices[dimension] <- k
      attr(subArray, '.vsc.prevIndices') <- newIndices

      # find name of subArray
      if(is.null(names)){
        name <- makeNameFromIndex(newIndices)
      } else{
        name <- names[k]
      }
      attr(subArray, '.vsc.name') <- name

      # construct setter-call
      r <- r[prevIndices==0]
      setCall <- as.call(c(
        list(
          as.symbol('['),
          quote(parent)
        ),
        r
      ))
      attr(subArray, '.vsc.setter') <- setCall

      # save undropped array
      attr(subArray, '.vsc.undroppedArray') <- subArray
      # then drop dimensions that are already handled
      prevDims <- which(prevIndices>0)
      newDim <-  dim(subArray)[-c(prevDims, dimension)]
      if(length(newDim)==0){
        newDim <- NULL
      }
      dim(subArray) <- newDim

      # assign class
      class(subArray) <- c('.vsc.subArray', '.vsc.internalClass')
      return(subArray)
    }
  )
  return(subArrays)
}

makeNameFromIndex <- function(ind){
  ind <- gsub('^0', '', ind)
  name <- paste(ind, collapse=',', sep='')
  name <- paste('[', name, ']', collapse=',', sep='')
}

`.vsc.unclass<-` <- function(x, value){
    do.call(structure, c(list(value), attributes(x)))
}
