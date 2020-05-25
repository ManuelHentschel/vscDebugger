########################################################################
# Prep


.packageEnv$varLists <- list()
.packageEnv$varListCalls <- list()
.packageEnv$varListArgs <- list()
.packageEnv$isEvaluating <- FALSE
.packageEnv$frameIdsR <- list()
.packageEnv$frameIdsVsc <- list()
.packageEnv$varInfo <- defaultVarInfo
.packageEnv$debugGlobal <- FALSE


.onLoad <- function(...){
    options(error=traceback)
}

#' Evaluate an expression and send result to vsc
#' 
#' Evaluates an expression in a given frameId and sends the result to vsc
#' 
#' @export
#' @param expr The espression to be evaluated
#' @param frameId The Id of the frame (as given by vsc)
#' @param id The Id of the message sent to vsc
.vsc.evalInFrame <- function(expr, frameId, silent=TRUE, id=0){
    if(.packageEnv$debugGlobal && calledFromGlobal()){
        env <- .GlobalEnv
    } else{
        frameIdR <- convertFrameId(vsc=frameId)
        env <- sys.frame(frameIdR)
    }
    .packageEnv$isEvaluating <- TRUE
    options(error=traceback)
    ret <- try(
        capture.output(eval(parse(text=expr), envir=env))
        , silent=silent
    )
    if(class(ret)=='try-error'){
        ret <- 'ERROR'
    }
    options(error=.vsc.onError)
    .packageEnv$isEvaluating <- FALSE
    ret <- paste(ret, sep="", collapse="\n")
    .vsc.sendToVsc('eval', ret, id=id)
}

calledFromGlobal <- function(){
    # can be used inside other functions from this package

    # Make sure not to nest this line: (!!!)
    thisPackageEnv <- parent.env(environment())

    # check if the first call (stack frame) is to a funcion from this package:
    if(identical(parent.env(sys.frame(1)), thisPackageEnv)){
        return(TRUE)
    } else{
        return(FALSE)
    }
}


isPackageFrame <- function(env=parent.frame()){
    while(!identical(env, emptyenv())){
        if(identical(env, globalenv())){
            return(FALSE)
        }
        env <- parent.env(env)
    }
    return(TRUE)
}

#' Modified version of \code{cat()} for vsc
#' 
#' Captures the output of \code{cat(...)} and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to base::cat()
#' @return NULL (invisible)
.vsc.cat <- function(...){
    # TODO: consider correct environment for print(...)?
    # env <- sys.frame(-1)
    # ret <- capture.output(base::print(...), envir=env)

    if(.packageEnv$isEvaluating){
        # return(base::cat(...))
        return(base::cat(...))
    }
    ret <- capture.output(base::cat(...))
    output <- paste(ret, sep="", collapse="\n")

    line <- .vsc.getLineNumber(sys.call())
    frame <- parent.frame()
    call <- sys.call(-1)
    file <- .vsc.getFileName(call, frame)
    # output <- capture.output(base::print(...))
    .vsc.sendToVsc('print', list(output=output, file=file, line=line))
    invisible(NULL)
}

#' Modified version of \code{print()} for vsc
#' 
#' Captures the output of \code{print(...)} and sends it to vsc together with information about the sourcefile and line
#' @export
#' @param ... Arguments passed to \code{base::cat()}
#' @return \code{NULL} (invisible)
.vsc.print <- function(x, ...){
    # TODO: consider correct environment for print(...)?
    # env <- sys.frame(-1)
    # ret <- capture.output(base::print(...), envir=env)

    if(.packageEnv$isEvaluating){
        return(base::print(x, ...))
    }
    ret <- capture.output(base::print(x, ...))
    output <- paste(ret, sep="", collapse="\n")

    line <- .vsc.getLineNumber(sys.call())
    frame <- parent.frame()
    call <- sys.call(-1)
    file <- .vsc.getFileName(call, frame)
    # output <- capture.output(base::print(...))
    .vsc.sendToVsc('print', list(output=output, file=file, line=line))
    invisible(x)
}




#' Send info about some vars to vsc
#' 
#' Gathers info about the specified variablesReferences and sends them to vsc
#' 
#' @export
#' @param refs A list of variableReferences, as specified in the scopes/previous variables
#' @param id The id of the message sent to vsc
#' @return None (The variable info, formatted as a nested named list is sent to vsc)
#' 
.vsc.getVarLists <- function(refs, id=0){
    varLists <- makeVarLists(refs)
    .vsc.sendToVsc('variables', varLists, id)
}


#' Get Filename corresponding to a call in a given frame
#'
#' Get Filename corresponding to a call in a given frame
#'
#' @param call A function call (usually from the stack)
#' @param frame The corresponding frame
#' @return The filename
.vsc.getFileName <- function(call, frame){
    fullpath <- try({
        fileName <- getSrcFilename(eval(call[[1]], envir=frame))
        dirName <- getSrcDirectory(eval(call[[1]], envir=frame))
        dirName <- suppressWarnings(normalizePath(dirName, winslash = '/'))
        fullPath <- file.path(dirName, fileName)
        fullPath <- suppressWarnings(normalizePath(fullPath, winslash = '\\'))
        fullPath <- toString(fullPath)
    }, silent=TRUE)
    if(class(fullPath)=='try-error'){
        fullPath <- ''
    }
    return(fullPath)
}


#' Get Line-number corresponding to a call in a given frame
#'
#' Get Line-number corresponding to a call in a given frame
#'
#' @param call A function call (usually from the stack)
#' @param frame The corresponding frame
#' @return The line-numer
.vsc.getLineNumber <- function(call){
    ref <- try(attr(call, 'srcref')[1], silent=TRUE)
    if(class(ref)=='try-error'){
        ref <- 0
    }
    return(ref)
}

#' @export
.vsc.getLineAtBreakpoint <- function(id=0){
    line <- getCallingLineAtTracingBreakpoint(1)
    .vsc.sendToVsc(message='lineAtBreakpoint', body=line, id=id)
}

getCallingLineAtTracingBreakpoint <- function(skipCalls=0){
    ret <- try({
        argList <- as.list(sys.call(-4-skipCalls))
        stepInfo <- argList[[3]]
        stepNumber <- as.integer(substring(stepInfo, 6))

        functionBody <- body(sys.function(-5-skipCalls))
        srcref <- attr(functionBody, 'srcref')[[stepNumber]]
        srcfile <- attr(srcref, 'srcfile')
        ret <- list(
            wd = srcfile$wd,
            filename = srcfile$filename,
            line = srcref[1]
        )
    }, silent=TRUE)
    if(class(ret)=='try-error'){
        ret = list(
            wd = '',
            filename = '',
            line = 0
        )
    }
    return(ret)
}

#' @export
.vsc.getLineAtBrowser <- function(id=0){
    line <- getCallingLine(1)
    .vsc.sendToVsc(message='lineAtBreakpoint', body=line, id=id)
}

getCallingLine <- function(skipCalls=0){
    srcref <- attr(sys.call(-skipCalls), 'srcref')
    srcfile <- attr(srcref, 'srcfile')
    ret <- list(
        wd = srcfile$wd,
        filename = srcfile$filename,
        line = srcref[1]
    )
    return(ret)
}




#' Send a message to vsc
#' 
#' Sends a message (text) together with body and id to vsc
#'
#' @param message A string identifying the type of message
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @param id The message id. Is usually provided in the function call from vsc.
.vsc.sendToVsc <- function(message, body="", id=0){
    s <- .vsc.makeStringForVsc(message, body, id)
    # base::cat(s)
    base::cat(s)
}



#' Prepare a message as string for vsc
#' 
#' Prepare a message as string for vsc
#' 
#' @param message A string identifying the type of message
#' @param body The body of the message. Must be convertible to JSON. Usually named lists.
#' @param id The message id. Is usually provided in the function call from vsc.
#' @return A (json) string that can be interpreted by vsc

.vsc.makeStringForVsc <- function(message, body="", id=0){
    .vsc.delimiter0 <- '<v\\s\\c>' # hardcoded to avoid triggering vsc when e.g. stack is sent
    .vsc.delimiter1 <- '</v\\s\\c>' # should probably be solved more elegantly
    l <- list(message=message, body=body, id=id)
    s <- jsonlite::toJSON(l, auto_unbox = TRUE, force=TRUE)
    r <- paste0(
        .vsc.delimiter0,
        s,
        .vsc.delimiter1,
        '\n'
    )
    return(r)
}





#' Runs the \code{main()} function
#' 
#' @description 
#' HAS CHANGED! Following info not up to date!
#' Looks for a function \code{main()} in the global environment and runs it
#' Runs main() inside this function, hence \code{parent.frame()} and \code{parent.env()} etc. will behave differently
#' 
#' @export
#' @param overWritePrint Whether to overwrite \code{base::print} with a version that sends output to vsc
#' @param overWriteCat Whether to overwrite \code{base::cat} with a version that sends output to vsc
.vsc.prepGlobalEnv <- function(overwritePrint=TRUE, overwriteCat=TRUE, overwriteSource=TRUE, findMain=TRUE, mainFunction='main', debugGlobal=FALSE) {

    .packageEnv$debugGlobal <- debugGlobal

    options(prompt = "<#v\\s\\c>\n")
    options(browserNLdisabled = TRUE)

    require(pryr, quietly = TRUE, warn.conflicts = FALSE)

    if(overwritePrint){
        .GlobalEnv$print <- .vsc.print
    }

    if(overwriteCat){
        .GlobalEnv$cat <- .vsc.cat
    }

    if(overwriteSource){
        .GlobalEnv$source <- .vsc.debugSource
    }
    
    .packageEnv$isEvaluating <- FALSE

    ignoreMain <- (
        !findMain ||
        is.null(mainFunction) || length(mainFunction)==0 ||
        nchar(mainFunction)==0 || isFALSE(mainFunction)
    )

    if(ignoreMain){
        options(error = .vsc.onError)
        .vsc.sendToVsc('go')
    } else if(!(mainFunction %in% ls(globalenv()))){
        .vsc.sendToVsc('noMain')
    } else if(typeof(get(mainFunction, envir=globalenv())) != 'closure'){
        # is there no short-circuit evaluation???
        .vsc.sendToVsc('noMain')
    } else{
        options(error = .vsc.onError)
        .vsc.sendToVsc('callMain')
    }
}

.vsc.onError <- function(){
    .vsc.sendToVsc('error')
    browser()
}


#' Check if debugger is evaluating
#' 
#' Returns \code{TRUE} iff an expression is being evaluated by the debugger during a breakpoint
#' 
#' @export
#' @return Boolean indicating whether an expression is being evaluated
.vsc.isEvaluating <- function(){
    return(.packageEnv$isEvaluating)
}


#' Check if R should stop on breakpoint
#' 
#' Returns \code{FALSE} iff an expression is being evaluated by the debugger during a (different) breakpoint, else TRUE
#' 
#' @export
#' @return Boolean indicating whether R should stop on breakpoints
.vsc.stopOnBreakpoint <- function(){
    return(!.packageEnv$isEvaluating)
}

#' Same as base::cat
#'
#' Same as base::cat
#' @export
.vsc.baseCat <- base::cat

#' Same as base::print
#'
#' Same as base::print
#' @export
.vsc.basePrint <- base::print


