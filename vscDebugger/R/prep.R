########################################################################
# Prep


# create environment for global data used by package functions
.packageEnv <- new.env()
.packageEnv$varLists <- list()
.packageEnv$varListCalls <- list()
.packageEnv$varListArgs <- list()
.packageEnv$isEvaluating <- FALSE
.packageEnv$frameIdsR <- list()
.packageEnv$frameIdsVsc <- list()
.packageEnv$varInfo <- defaultVarInfo




#' Evaluate an expression and send result to vsc
#' 
#' Evaluates an expression in a given frameId and sends the result to vsc
#' 
#' @export
#' @param expr The espression to be evaluated
#' @param frameId The Id of the frame (as given by vsc)
#' @param id The Id of the message sent to vsc
.vsc.evalInFrame <- function(expr, frameId, silent=TRUE, id=0){
    frameIdR <- convertFrameId(vsc=frameId)
    env <- sys.frame(frameIdR)
    .packageEnv$isEvaluating <- TRUE
    options(error=NULL)
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


#' Build current stack and send to vsc
#' 
#' Gives info about the current stack and sends it to vsc
#' 
#' @export
#' @param topFrame The first stack frame to consider (= the current function call)
#' @param id The id of the message sent to vsc
#' @return None (The current stack, formatted as a nested named list is sent to vsc)
#' 
.vsc.getStack <- function(topFrame=parent.frame(),id=0, isError=0){
    stack <- .vsc.buildStack(topFrame = topFrame, isError = isError)
    .vsc.sendToVsc('stack', stack, id)
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
.vsc.getLineNumberAtBreakpoint <- function(id=0){
    line <- getCallingLine(1)
    .vsc.sendToVsc(message='lineAtBreakpoint', body=line, id=id)
}
getCallingLine <- function(skipCalls=0){
    ret <- try({
        argList <- as.list(sys.call(-4-skipCalls))
        stepInfo <- argList[[3]]
        stepNumber <- as.integer(substring(stepInfo, 6))

        functionBody <- body(sys.function(-5-skipCalls))
        srcref <- attr(functionBody, 'srcref')[[stepNumber]]
        lineNumber <- srcref[1]
    }, silent=TRUE)
    if(class(ret)=='try-error'){
        ret = 0
    }
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
#' Runs the function \code{main()} from the global environment
#' 
#' @export
#' @param overWritePrint Whether to overwrite \code{base::print} with a version that sends output to vsc
#' @param overWriteCat Whether to overwrite \code{base::cat} with a version that sends output to vsc
.vsc.runMain <- function(overwritePrint=TRUE, overwriteCat=TRUE) {

    options(prompt = "<#v\\s\\c>\n")
    options(error = .vsc.onError)
    options(browserNLdisabled = TRUE)

    require(pryr, quietly = TRUE, warn.conflicts = FALSE)

    if(overwritePrint){
        .GlobalEnv$print <- .vsc.print
    }

    if(overwriteCat){
        .GlobalEnv$cat <- .vsc.cat
    }
    
    .packageEnv$isEvaluating <- FALSE

    
    if('main' %in% ls(.GlobalEnv) && typeof(main)=='closure'){
        .vsc.sendToVsc('go')
        main()
        .vsc.sendToVsc('end')
    } else{
        .vsc.sendToVsc('noMain')
    }
}

.vsc.onError <- function(){
    .vsc.sendToVsc('error')
    browser()
}


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# GET file-name of current function:
# attr(attr(eval.parent(sys.call()[[1]]), 'srcref'), 'srcfile')
# attr(attr(attributes(eval.parent(sys.call()[[1]]))$original, 'srcref'), 'srcfile')


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
#' Same as base::cat. Used by custom breakpoints
#' @export
.vsc.baseCat <- base::cat

#' Same as base::print
#'
#' Same as base::print. Used by custom breakpoints
#' @export
.vsc.basePrint <- base::print

# #' Sets a breakpoint
# #' 
# #' @description
# #' Sets breakpoints in the given file and line.
# #' Uses \code{findLineNum()} and \code{trace()}
# #' 
# #' @export
# #' @param srcfile The file in which to set the breakpoint
# #' @param line The line in which to set the breakpoint
# #' @param incdluePackages Whether to set breakpoints in package files
# .vsc.mySetBreakpoint <- function(srcfile, lines, includePackages=TRUE){
#     # find steps, that correspond to the given line numbers
#     stepList <- list()
#     for(i in seq2(length(lines))){
#         if(includePackages){
#             lastenv <- emptyenv() # searches through package-envs as well
#         } else {
#             lastenv <- .GlobalEnv # searches only through 'user'-envs
#         }
#         refs <- findLineNum(srcfile, lines[i], lastenv=lastenv)
#         if(length(refs)>0){
#             step <- refs[[1]] # Ignore other refs?? In what cases are there >1 refs??
#             found <- FALSE

#             # check if the same function already has breakpoints:
#             for(j in seq2(length(stepList))){
#                 #check if env and function are identical:
#                 if(identical(stepList[[j]]$name, step$name) && identical(stepList[[j]]$env, step$env)){ 
#                     #append step$at to steplist[[j]]$at, etc.
#                     stepList[[j]]$at[[length(stepList[[j]]$at)+1]] <- step$at 
#                     stepList[[j]]$line[[length(stepList[[j]]$line)+1]] <- step$line 
#                     stepList[[j]]$timediff[[length(stepList[[j]]$timediff)+1]] <- step$timediff 
#                     found <- TRUE
#                     break
#                 }
#             }
#             # add new functions to stepList
#             if(!found){
#                 step$at <- list(step$at)
#                 step$line <- list(step$line)
#                 step$timediff <- list(step$timediff)
#                 stepList[[length(stepList)+1]] <- step
#             }
#         }
#     }

#     # loop through functions found above
#     for(i in seq2(1, length(stepList))){
#         step <- stepList[[i]]

#         # use trace instead of custom version:
#         trace(
#             what = step$name,
#             tracer = browser,
#             at = unlist(step$at),
#             where = step$env
#         )
#     }
# }


########################################################################
# Helper

seq2 <- function(from, to=NULL, by=1){
    if(is.null(from) || is.list(from) || (is.vector(from) && length(from)>1)){
        return(seq2(1, length(from), by))
    } else if(is.null(to)){
        to <- from
        from <- 1
        return(seq2(from, to, by))
    } else if((to-from)*by<0){
        return(NULL)
    } else{
        return(seq(from, to, by))
    }
}

zeroList <- function(list0){
    # returns a list of 0s, as long as the input list
    return(lapply(list0, function(x) 0))
}