
# helper functions for the default varInfos:

getRow <- function(v, i) {
  row <- v[i, ]
  names(row) <- colnames(v)
  if (is.null(names(row))) {
    names(row) <- getIndices(v, row = i)
  }
  class(row) <- c('.vsc.matrixRow', '.vsc.internalClass')
  return(row)
}
getCol <- function(v, j) {
  col <- v[, j]
  names(col) <- rownames(v)
  if (is.null(names(col))) {
    names(col) <- getIndices(v, col = j)
  }
  # matrixRow can be used here as well!
  class(col) <- c('.vsc.matrixRow', '.vsc.internalClass')
  return(col)
}
getIndices <- function(v, row = NULL, col = NULL) {
  if (is.null(row) && is.null(col)) {
    m <- expand.grid(nrow(v), ncol(v))
    names <- mapply(function(i, j) paste0('[', i, ',', j, ']'), m[[1]], m[[2]])
  } else if (is.null(row)) {
    names <- lapply(seq_len(nrow(v)), function(i) paste0('[', i, ',', col, ']'))
  } else if (is.null(col)) {
    names <- lapply(seq_len(ncol(v)), function(j) paste0('[', row, ',', j, ']'))
  } else {
    names <- list(paste0('[', row, ',', col, ']'))
  }
}




#' Get variable from environment
#' 
#' Is basically a wrapper for `get(name, envir=env)`, but does not forces promises.
#' 
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return Returns the value of the variable or a representation of a promise as returned by `getPromiseVar`
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

getVarsInEnv <- function(env, all.names = TRUE) {
  names <- ls(env, all.names = all.names)
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
getDotVars <- function(env) {
  ## Note: substitute(...()) is officially not supported, but it 
  ## works as intended for all relevant R versions (at least from R 3.0.0)
  dots <- substitute(...(), env = env)
  structure(dots, class = c(".vsc.ellipsis", ".vsc.internalClass"))
} 

#' Get a representation of a promise
#' 
#' Gets a representation of a promise without forcing the promise
#' 
#' @aliases .vsc.promise
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return An object of class \code{".vsc.promise"}; a named list containing the expression that will be evaluated, the status whether the promise has already been evaluated, the value if it has already been evaluated, and the environment in which the unevaluated promise will be evaluated. 
#' @examples 
#' ## create a promise
#' e <- new.env()
#' delayedAssign("x", {message("evaluating promise..."); 1L}, assign.env = e)
#' vscDebugger:::getPromiseVar("x", e)
#' 
#' ## evaluate it...
#' print(e$x)
#' 
#' ## is it still a promise? (It depends...)
#' stopifnot(vscDebugger:::isPromise("x", e, strict = FALSE))
#' stopifnot(!vscDebugger:::isPromise("x", e, strict = TRUE))
#' 
#' ## get info again
#' vscDebugger:::getPromiseVar("x", e)
#' 
getPromiseVar <- function(name, env) {
  structure(
    getPromiseInfo(name, env),
    class = c(".vsc.promise", ".vsc.InternalClass")
  )
}

#' Get information about a promise
#' 
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return A named list: 
#' \itemize{
#'   \item{\code{code}: }{the expression that will be evaluated}
#'   \item{\code{environment}: }{the environment where the promise is evaluated}
#'   \item{\code{evaluated}: }{logical flag if the promise has been already evaluated}
#'   \item{\code{value}: }{optional node; the value of the evaluated promise}
#' }
#' @keywords internal
#' @useDynLib vscDebugger c_promise_info 
getPromiseInfo <- function(name, env) {
  sym <- as.name(name)
  .Call(c_promise_info, sym, env)
}

#' Test if in object is a promise
#' 
#' @param name [character(1L)] the name of the object
#' @param env [environment] the environment of the object 
#' @param strict [logical(1L)] if \code{strict} is \code{TRUE} 
#'   (the default), evaluated promises return with \code{FALSE}
#' @useDynLib vscDebugger c_is_promise
#' @keywords internal
#' @seealso \code{\link{getPromiseVar}}
#' @return \code{TRUE} or \code{FALSE}
isPromise <- function(name, env, strict = TRUE) {
  sym <- as.name(name)
  .Call(c_is_promise, sym, env, strict)
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
