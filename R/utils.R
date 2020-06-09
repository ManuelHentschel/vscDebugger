


lGroupBy <- function(X, FUN = NULL, item = NULL, ...) {
  if (is.null(FUN)) {
    FUN <- function(x) x
  }
  if (is.null(item) || identical(item, '')) {
    X2 <- X
  } else {
    X2 <- lapply(X, function(x) x[[item]])
  }
  values <- lapply(X2, FUN)
  uniqueValues <- unique(values)
  grouped <- lapply(uniqueValues, function(v) X[lIdentical(values, v)])
  return(grouped)
}

lIdentical <- function(xList, xElement) {
  ret <- lapply(xList, function(x) identical(x, xElement))
  ret <- unlist(ret)
}

########################################################################
# Helper

#' Modified version of base::seq
#'
#' Modified version of `base::seq`
#' @usage seq2(from, to, by=1)
#' @usage seq2(from)
#'
#' @param from Can be the starting value of the sequence, or the end value of the sequence, or a vector of length>1, or a list
#' @param to The ending value of the sequence
#' @param by The step size (as in `base::seq`)
#' @return A vector containing a sequence of numbers
#'
#' @details
#' If `from, to, by} are supplied, the function returns the same as \code{base::seq`.
#' If `from, to} are supplied the function returns \code{NULL} if \code{from>to`,
#' else the same as `base::seq`.
#' If `from} is a number, the same as \code{seq2(1,from)` is returned.
#' If `from} is a vector of length>1 or a list, \code{seq2(length(from))` is returned.
seq2 <- function(from, to = NULL, by = 1) {
  if (is.null(from) || is.list(from) || (is.vector(from) && length(from) > 1)) {
    return(seq2(1, length(from), by))
  } else if (is.null(to)) {
    to <- from
    from <- 1
    return(seq2(from, to, by))
  } else if ((to - from) * by < 0) {
    return(NULL)
  } else {
    return(seq(from, to, by))
  }
}

#' Returns a list of 0s
#'
#' Returns a list of 0s, of the same length as the input list
#'
#' @param list0 Any oobject that can be passed to lapply as first argument
#' @return A list of the same length as list0, filled with 0s
zeroList <- function(list0) {
  return(lapply(list0, function(x) 0))
}


appendNested <- function(l0, l1){
  names <- as.list(unique(c(names(l0), names(l1))))
  ret <- list()
  for (name in names) {
    ret[[name]] <- append(l0[[name]], l1[[name]])
  }
  return(ret)
}


summarizeLists <- function(lists) {
  names <- as.list(unique(unlist(lapply(lists, names))))
  ret <- list()
  for (name in names) {
    ret[[name]] <- lapply(lists, function(l) l[[name]])
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
