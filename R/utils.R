#' Modified version of base::seq
#'
#' Modified version of `base::seq`
#' @usage seq2(from, to=NULL, by=1)
#'
#' @param from Can be the starting value of the sequence, or the end value of the sequence, or a vector of length>1, or a list
#' @param to=NULL The ending value of the sequence
#' @param by=1 The step size (as in `base::seq`)
#' @return A vector containing a sequence of numbers
#'
#' @details
#' Basically the same as `base::seq`, but returns an empty vector if `(to-from)*by<0`.
seq2 <- function(from, to = NULL, by = 1) {
  if (is.null(to)) {
    to <- from
    from <- 1
  }
  if((to - from) * by < 0) {
    return(integer(0))
  } else {
    return(seq(from, to, by))
  }
}


lgetSafe <- function(list, entry, default=NULL){
  suppressWarnings(
    tryCatch(
      lget(list, entry, default),
      error = function(e) default
    )
  )
}

lget <- function(list, entry, default=NULL){
  ret <- list[[entry]]
  if(missing(ret)){
    substitute()
  } else if(is.null(ret)){
    default
  } else{
    ret
  }
}

lset <- function(env, entry, value, defaultReturn=NULL){
  ret <- lget(env, entry, default)
  env[[entry]] <- value
  invisible(ret)
}


isCalledFromBrowser <- function(){
  # Does not work when called via `addTaskCallback()`
  tryCatch(
    {
      browserText()
      TRUE
    },
    error = function(e) FALSE
  )
}

assignOverBinding <- function(what, value, where, verbose = TRUE){
  methods:::.assignOverBinding(what, value, where, verbose)
}

avoidLazyLoading <- function(package){
  ns <- getNamespace(package)
  for(name in ls(ns)){
    get(name, envir=ns)
  }
  invisible(NULL)
}
