
# TODO:
# User facing functions in this file need to be updated to match the new entries in VarInfo!


#' Get info about a variable
#'
#' Get info about a variable by applying the relevant `varInfo` entries.
#'
#' @param v The variable, any R value.
#' @param infos Character vector of field names in `varInfo`. Retrieve first match.
#' @param stackingInfos Character vector of field names in `varInfo`. Retrieve all matches.
#' @param verbose Whether to print debug info using `logCat`, `logPrint`.
#' @param ind The indices to retrieve if child variables are retrieved.
#' 
#' @details
#' The allowed `varInfo` entries and their types are:
#' - childVars: MinimalVariable[]
#' - nChildVars: number
#' - customAttributes: MinmalVariable[]
#' - internalAttributes: MinmalVariable[]
#' - toString: string
#' - type: string
#' - evaluateName: string
#' - printFunc: function | boolean
#' 
#' Where `MinimalVariable[]` refers to a list of named lists with entries:
#' - name: string
#' - rValue: any R value
#' - setter: (optional, undocumented)
#' - setInfo: (optional, undocumented)
#'
#' @seealso [`varInfos`]
#' @return A named list, containing the corresponding `varInfo` results.
.vsc.applyVarInfos <- function(
  v,
  infos = character(0),
  stackingInfos = character(0),
  verbose = getOption('vsc.verboseVarInfos', FALSE),
  ind = NULL
) {
  # check args
  if(is.null(stackingInfos)) {
    stackingInfos <- c()
  } else if(is.list(stackingInfos)) {
    stackingInfos <- unlist(stackingInfos)
  }
  if(is.null(infos)) {
    infos <- c()
  } else if(is.list(infos)) {
    infos <- unlist(infos)
  }
  force(v)

  missingInfos <- c(infos, stackingInfos)
  names(missingInfos) <- missingInfos
  isStacking <- c(logical(length(infos)), !logical(length(stackingInfos)))
  names(isStacking) <- missingInfos
  ret <- list()

  if(verbose){
    logCat('\n\n\nApplying VarInfos to:\n')
    logPrint(v)
    logCat("\nMissing Infos:\n")
    logPrint(missingInfos)
    logCat("IsStacking:\n")
    logPrint(isStacking)
  }

  for (varInfo in session$varInfos) {
    # find missing infos that are supplied by this varInfo:
    matching <- intersect(missingInfos, names(varInfo))
    if(verbose) logCat("Checking ", varInfo$name, "...\n", sep="")
    applies <- toAtomicBoolean(varInfo$doesApply(v)) # safe conversion to atomic boolean
    if(applies){
      if(verbose) logCat("applies!\n")
      for(info in matching){
        # get and (if function) apply info:
        tmp <- varInfo[[info]]
        if(is.function(tmp)){
          valueAndError <- tryCatch(
            if(info=='childVars'){
              list(
                value = tmp(v, ind),
                isError = FALSE
              )
            } else{
              list(
                value = tmp(v),
                isError = FALSE
              )
            },
            error = function(e) {
              if(verbose){
                logPrint('error:')
                logPrint(e)
              }
              list(
                value = NULL,
                isError = TRUE
              )
            }
          )
          if(valueAndError$isError){
            next
          } else{
            tmp <- valueAndError$value
          }
        }
        if(verbose){
          logCat(info, " gives: ", sep="")
          logPrint(tmp)
        }
        # append or store result:
        if(is.null(tmp)){
          # ignore result
        } else if(isStacking[info]){
          ret[[info]][[varInfo$name]] <- tmp
          # keep looking...
        } else{
          ret[[info]] <- tmp
          # remove from missing infos:
          ind <- which(missingInfos == info)
          missingInfos <- missingInfos[-ind]
          isStacking <- isStacking[-ind]
        }
      }
      if(length(missingInfos) == 0){
        break
      }
    }
  }
  ret
}

toAtomicBoolean <- function(v, ...){
  # outputs TRUE or FALSE
  # for nice input equivalent to `as.logical(v)[[1]]`
  # catches all errors, suppresses all warnings
  # able to handle any number of inputs, only considers 1st, returns FALSE if no arguments
  # able to handle e.g. NULL, c(), integer(0), list(), NA, NaN (all of these return FALSE)
  # if input throws an error and is lazy -> evaluated withing tryCatch -> returns FALSE
  suppressWarnings(
    tryCatch(
      if(as.logical(v)[[1]]){
        TRUE
      } else{
        FALSE
      },
      error = function(e) FALSE
    )
  )
}

#' Tools to modify/debug varInfos
#' 
#' Tools to check the varInfos computed for variables and modify
#' the list of varInfos used.
#' 
#' @param varInfos List of varInfos
#' 
#' @seealso [`.vsc.applyVarInfos`]
#' @rdname varInfos
.vsc.getAllVarInfoFields <- function(varInfos = session$varInfos){
  allFields <- c()
  for(vi in varInfos){
    allFields <- union(allFields, names(vi))
  }
  return(allFields)
}

#' @param v Variable, any R object
#' @param verbose passed to `.vsc.applyVarInfos`
#' 
#' @rdname varInfos
.vsc.applyAllVarInfos <- function(v, verbose = TRUE){
  stackingInfos <- .vsc.getAllVarInfoFields()
  .vsc.applyVarInfos(v, stackingInfos = stackingInfos, verbose = verbose)
}

#' @rdname varInfos
.vsc.resetVarInfos <- function() {
  session$varInfos <- getDefaultVarInfos()
}

#' @param varInfo A varInfo, i.e. named list.
#' @param overwrite Boolean, whether to overwrite the entry in that position
#' 
#' @rdname varInfos
.vsc.addVarInfo <- function(
  varInfo,
  overwrite = FALSE
) {
  if(!.vsc.checkVarInfos(list(varInfo))){
    stop('Not a valid varInfo.')
  }
  position <- lget(varInfo, 'position', 1)
  if (position < 0) {
    # negative positions count from the end, -1 = last position
    position <- length(session$varInfos) + 1 + position
  }

  if(overwrite){
    session$varInfos[[position]] <- varInfo
  } else{
    session$varInfos <- append(session$varInfos, list(varInfo), position - 1)
  }
}

#' @param positions Numeric or character vector, the entries to retrieve/remove
#' 
#' @rdname varInfos
.vsc.removeVarInfos <- function(positions) {
  session$varInfos[getVarInfoInds(positions, FALSE)] <- NULL
}

getVarInfoInds <- function(positions = NULL, defaultToAll = FALSE){
  varInfos <- getVarInfosWithPositions()
  inds <- logical(length(varInfos))
  names(inds) <- names(varInfos)
  if(is.null(positions)){
    positions <- defaultToAll
  } else if(!is.numeric(positions) && !is.character(positions)){
    stop('Argument positions must be character or numeric vector')
  }
  inds[positions] <- TRUE
  inds
}

#' @rdname varInfos
.vsc.getVarInfos <- function(positions = NULL){
  varInfos <- getVarInfosWithPositions()
  varInfos[getVarInfoInds(positions, TRUE)]
}

getVarInfosWithPositions <- function(){
  varInfos <- session$varInfos
  varInfos <- lapply(seq_along(varInfos), function(i){
    vI <- varInfos[[i]]
    vI$position <- i
    vI
  })
  names(varInfos) <- lapply(varInfos, function(vI){
    if(is.null(vI$name)){
      paste0('varInfo', vI$pos)
    } else{
      vI$name
    }
  })
  return(varInfos)
}

#' @rdname varInfos
.vsc.checkVarInfos <- function(varInfos = session$varInfos){
  if(!is.list(varInfos)){
    stop('`varInfos` has to be a list of varInfos (i.e., list of lists).')
  }
  allOk <- TRUE
  allNames <- c()
  for(i in seq_along(varInfos)){
    vi <- varInfos[[i]]
    alert <- function(...){
      allOk <<- FALSE
      base::cat('varInfo #', i, ': ', ..., '\n', sep = '')
    }
    # Check that it's a named list
    if(!is.list(vi)){
      alert('Is not a list.')
      next
    }
    if(is.null(names(vi))){
      alert('Has no names.')
      next
    }
    # Check name (must be a string)
    viName <- lget(vi, 'name', NULL)
    if(is.null(viName)){
      alert('Has no name!')
    } else if(!is.string(viName)){
      alert('Has invalid name (must be a string)!')
    } else{
      allNames <- c(allNames, viName)
    }
    # Check doesApply (must be a function taking >=1 args)
    doesApply <- lget(vi, 'doesApply', NULL)
    if(is.null(doesApply)){
      alert('Has no doesApply().')
    } else if(!is.function.with.args(doesApply, 1)){
      alert('doesApply needs to be a function with 1 argument.')
    }
    # Check others (if function, must take >= 1 args)
    entryNames <- setdiff(names(vi), c('name', 'doesApply'))
    if(length(entryNames) == 0){
      alert('Has no entries.')
    }
    for(e in entryNames){
      tmp <- vi[[e]]
      if(viName == 'childVars' && !is.function.with.args(tmp, 2)){
        alert('childVars() needs to take 2 arguments (second one optional).')
      }
      if(viName %in% c('toString', 'type', 'evaluateName')){
        if(!(is.string(tmp) || is.function.with.args(tmp))){
          alert(e, ' needs to be a string or function with 1 argument.')
        }
      } else if(viName %in% c('nChildVars')){
        if(!(is.number(tmp) || is.function.with.args(tmp))){
          alert(e, ' needs to be a number or function with 1 argument.')
        }
      } else if(is.function.with.args(tmp, 0, 'equal')){
        alert(e, ' is a function but takes no arguments.')
      }
    }
  }
  return(allOk)
}

is.string <- function(v){
  !is.null(v) && is.atomic(v) && is.character(v) && length(v) == 1
}

is.number <- function(v){
  !is.null(v) && is.atomic(v) && is.numeric(v) && length(v) == 1
}

is.function.with.args <- function(f, nArgs = 1, match = c('atLeast', 'equal', 'atMost')[1]){
  if(!is.function(f)){
    return(FALSE)
  }
  nArgsf <- length(formals(args(f)))
  if(match == 'atLeast'){
    nArgsf >= nArgs
  } else if(match == 'equal'){
    nArgsf = nArgs
  } else if(match == 'atMost'){
    nArgsf <= nArgs
  } else{
    stop('Invalid argument `match`!')
  }
}
