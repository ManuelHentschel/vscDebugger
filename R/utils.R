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


isCalledFromBrowser <- function(){
  tryCatch(
    {
      browserText()
      TRUE
    },
    error = function(e) FALSE
  )
}

# Is used to avoid showing internal frames in the stack tree
registerEntryFrame <- function(skipCalls=0, entryFrames = NULL){
  if(is.null(entryFrames)){
    parentFrame <- sys.nframe()-1
    session$entryFrames <- c(session$entryFrames, parentFrame - skipCalls)
  } else{
    session$entryFrames <- entryFrames
  }
  invisible(session$entryFrames)
}

# Is used to avoid showing internal frames in the stack tree
unregisterEntryFrame <- function(all=FALSE){
  ret <- session$entryFrames
  n <- sys.nframe() - 1
  unregisterFrame(n, all)
  invisible(ret)
}

registerLaunchFrame <- function(skipCalls=0, launchFrames = NULL){
  if(is.null(launchFrames)){
    parentFrame <- sys.nframe()-1
    session$launchFrames <- c(session$launchFrames, parentFrame + skipCalls)
  } else{
    session$launchFrames <- launchFrames
  }
  invisible(session$launchFrames)
}

unregisterLaunchFrame <- function(all=FALSE){
  ret <- session$launchFrames
  n <- sys.nframe() - 1
  unregisterFrame(n, all)
  invisible(ret)
}

unregisterFrame <- function(upto=sys.nframe()-1, all=FALSE){
  lf <- session$launchFrames
  ef <- session$entryFrames
  if(all){
    lf <- c()
    ef <- c()
  } else{
    lf <- lf[lf < upto]
    ef <- ef[ef < upto]
  }
  session$launchFrames <- lf
  session$entryFrames <- ef
}

getSkipFromBottom <- function(){
  suppressWarnings({
    lf <- min(session$launchFrames)
    ef <- min(session$entryFrames)
  })
  if(ef == 1 && lf < Inf){
    return(lf)
  } else{
    return(0)
  }
}

getTopFrameId <- function(){
  suppressWarnings({
    lf <- max(session$launchFrames)
    efs <- session$entryFrames
    ef <- max(efs)
  })
  if(ef > lf){
    while((ef - 1) %in% efs && ef > lf){
      ef <- ef - 1
    }
    if(ef>0){
      ef <- ef - 1
    }
  } else{
    ef <- sys.nframe() - 1
  }
  return(ef)
}


setOptionIfNull <- function(option, value){
  if(is.null(getOption(option))){
    options(structure(list(value), names=option))
  }
}