



#' Modified Output Functions
#' 
#' @description 
#' Modified versions of core R functions that add debugger specific functionality.
#' With the default debug configuration, the normal R functions are overwritten with these
#' (using [`attach()`]).
#' 
#' The modified functions are designed to be mostly interchangeable with their counterparts.
#' If the overwrites are not required, they can be toggled using the launch config entries
#' `overwriteCat`, `overwritePrint`, `overwriteStr`, and `overwriteMessage`.
#' 
#' 
#' @name outputOverwrites
#' @rdname outputOverwrites
#' 
#' @param ... Further arguments that are passed to the original function
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be useful e.g. inside a log function.
#' @param showSource Whether to show the calling source file and line
#' 
#' @return These functions return the same value as the overwritten functions themselves.
#' 
#' @family overwrites
NULL


#' @rdname outputOverwrites
#' @seealso [base::cat()]
#' @export
.vsc.cat <- function(..., skipCalls=0, showSource=TRUE) {
  # TODO: consider correct environment for base::print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$state$isEvaluatingSilent()) {
    return(base::cat(...))
  }

  split <- session$splitOverwrittenOutput
  args <- list(...)

  if(identical(args$file, stderr())){
    if(split){
      # cannot split message connection -> cat() twice
      base::cat(...)
    }
    category <- 'stderr'
    args['file'] <- NULL
    ret <- capture.output({do.call(base::cat, args);base::cat("\n")})
  } else if(is.null(args$file) || identical(args$file, '')){
    type <- 'output'
    category <- 'stdout'
    ret <- capture.output({do.call(base::cat, args);base::cat("\n")}, type=type, split=split)
  } else{
    return(base::cat(...))
  }

  printToVsc(ret, skipCalls+1, category, showSource = showSource)
  invisible(NULL)
}


#' @rdname outputOverwrites
#' @param x Same as in [base::print()]
#' @seealso [base::print()]
#' @export
.vsc.print <- function(x, ..., skipCalls=0, showSource=TRUE) {
  # TODO: consider correct environment for base::print(...)?
  # env <- sys.frame(-1)
  # ret <- capture.output(base::print(...), envir=env)

  if (session$state$isEvaluatingSilent()) {
    return(base::print(x, ...))
  }
  split <- session$splitOverwrittenOutput
  ret <- capture.output(base::print(x, ...), split=split)
  ret <- c(ret, "")
  printToVsc(ret, skipCalls+1, showSource = showSource)
  invisible(x)
}

#' @rdname outputOverwrites
#' @param domain Same as in [base::message()]
#' @param appendLF Same as in [base::message()]
#' @seealso [base::message()]
#' @export
.vsc.message <- function(..., domain = NULL, appendLF = TRUE, showSource=TRUE, skipCalls=0){
  args <- list(...)
  cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
    if (nargs() > 1L) {
      warning("additional arguments ignored in message()")
    }
    args[[1L]]
  } else {
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
    call <- sys.call()
    simpleMessage(msg, call)
  }
  defaultHandler <- function(c) {
    .vsc.cat(conditionMessage(c), file = stderr(), sep = "", skipCalls=skipCalls+5, showSource = showSource) # changed
  }
  withRestarts(
    {
      signalCondition(cond)
      defaultHandler(cond)
    },
    muffleMessage = function() NULL
  )
  invisible()
}


#' @rdname outputOverwrites
#' @param object Same as in [utils::str()]
#' @seealso [utils::str()]
#' @export
.vsc.str <- function(object, ..., skipCalls=0, showSource=TRUE){
  args <- list(
    name = 'vscStrResult',
    rValue = list(object)
  )
  node <- session$rootNode$getEvalRootNode()$addChild(args)
  variable <- node$getContent()

  if(showSource){
    source <- getSource(sys.call(-skipCalls))
    line <- lget(source, 'line', 0)
  } else{
    source <- NULL
    line <- NULL
  }

  sendOutputEvent(
    output = "",
    category = "stdout",
    variablesReference = variable$variablesReference,
    source = source,
    line = line
  )

  if(session$splitOverwrittenOutput){
    utils::str(object, ...)
  }

  invisible(NULL)
}

#' Internal function to print to vsc
#' 
#' Sends text to vsc, together with source information.
#' Is used by e.g. [.vsc.print], [.vsc.cat]
#' 
#' @param ret The text to be sent
#' @param skipCalls The number of calls to skip when reporting the calling file and line. Can be used e.g. inside log functions.
#' @param category The output category ("stdout", "stderr", ...)
#' @param showSource Whether to show the calling source file and line.
#' 
#' @keywords internal
printToVsc <- function(ret, skipCalls=0, category="stdout", showSource=TRUE){
  output <- paste0(ret, collapse = "\n")
  if(showSource){
    source <- getSource(sys.call(-skipCalls))
    line <- lget(source, 'line', 0)
  } else{
    source <- NULL
    line <- NULL
  }
  sendOutputEvent(category, output = output, line=line, source=source)
}


#' @export
.vsc.print.help_files_with_topic <- function(h, ...) {
  viewer <- getOption("vsc.helpPanel", "Two")
  if (!identical(FALSE, viewer) && length(h) >= 1 && is.character(h)) {
    file <- h[1]
    path <- dirname(file)
    dirpath <- dirname(path)
    pkgname <- basename(dirpath)
    requestPath <- paste0(
      "/library/",
      pkgname,
      "/html/",
      basename(file),
      ".html"
    )
    success <- sendCustomEvent('viewHelp', list(requestPath = requestPath, viewer = viewer))
  } else{
    utils:::print.help_files_with_topic(h, ...)
  }
  invisible(h)
}


#' @export
.vsc.print.hsearch <- function(x, ...){
  viewer <- getOption("vsc.helpPanel", "Two")
  if (!identical(FALSE, viewer) && length(x) >= 1) {
    requestPath <- paste0(
      "/doc/html/Search?pattern=",
      tools:::escapeAmpersand(x$pattern),
      paste0("&fields.", x$fields, "=1",
        collapse = ""
      ),
      if (!is.null(x$agrep)) paste0("&agrep=", x$agrep),
      if (!x$ignore.case) "&ignore.case=0",
      if (!identical(
        x$types,
        getOption("help.search.types")
      )) {
        paste0("&types.", x$types, "=1",
          collapse = ""
        )
      },
      if (!is.null(x$package)) {
        paste0(
          "&package=",
          paste(x$package, collapse = ";")
        )
      },
      if (!identical(x$lib.loc, .libPaths())) {
        paste0(
          "&lib.loc=",
          paste(x$lib.loc, collapse = ";")
        )
      }
    )
    success <- sendCustomEvent('viewHelp', list(requestPath = requestPath, viewer = viewer))
  } else{
    utils:::print.hsearch(x, ...)
  }
  invisible(x)
}

#' Refresh Breakpoints
#' 
#' Refresh breakpoints known to the debugger
#' Can be used if breakpoints were invalidated by e.g. `load_all()` or `source()`
#' @export
.vsc.refreshBreakpoints <- function(envs=NULL){
  setStoredBreakpoints(envs)
}


#' Modified version of `pkgload::load_all()`
#' @family overwrites
#' @export
.vsc.load_all <- function(...){
  internalLoadAll(..., refreshBreakpoints = TRUE)
}

internalLoadAll <- function(..., refreshBreakpoints=FALSE, loadSilently=FALSE){
  if(!requireNamespace('pkgload', quietly = TRUE)){
    stop('Package pkgload must be installed to use load_all!')
  }

  # normal load_all
  if(loadSilently){
    suppressMessages(ret <- pkgload::load_all(...))
  } else{
    ret <- pkgload::load_all(...)
  }
  ns <- ret$env

  # attach overwritten print/cat etc.
  attachList <- makeAttachList(list(
    overwritePrint = session$overwritePrint,
    overwriteCat = session$overwriteCat,
    overwriteMessage = session$overwriteMessage
  ))
  
  # Insert overwrites into parent-env of package-namespace
  # As of pkgload v1.3.1 this environment contains some pkgload-specific shims
  # and is writeable.
  if(length(attachList)>0){
    shimEnv <- parent.env(ns)
    for(fName in names(attachList)){
      assign(fName, attachList[[fName]], envir = shimEnv)
    }
  }

  # store pkgname
  s <- format(ns)
  pkgName <- sub('^<environment: (?:package|namespace):(.*)>$', '\\1', s)
  session$debuggedPackages <- unique(c(session$debuggedPackages, pkgName))

  # refresh breakpoints (used if called curing session, not launch)
  if(refreshBreakpoints){
    exports <- as.environment(paste0('package:', pkgName))
    .vsc.refreshBreakpoints(list(ns, exports))
  }

  # return output from normal load_all
  invisible(ret)
}

