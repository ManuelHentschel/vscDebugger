########################################################################
# Prep

.packageEnv <- new.env()
.packageEnv$varLists <- list()
.packageEnv$varListCalls <- list()
.packageEnv$isEvaluating <- FALSE



#' Evaluate an expression and send result to vsc
#' 
#' Evaluates an expression in a given frameId and sends the result to vsc
#' 
#' @export
#' @param expr The espression to be evaluated
#' @param frameId The Id of the frame (as given by vsc)
#' @param id The Id of the message sent to vsc
.vsc.evalInFrame <- function(expr, frameId, id=0){
    env <- sys.frame(frameId + 1)
    .packageEnv$isEvaluating <- TRUE
    ret <- capture.output(eval(parse(text=expr), envir=env))
    .packageEnv$isEvaluating <- FALSE
    ret <- paste(ret, sep="", collapse="\n")
    .vsc.sendToVsc('eval', ret, id=id)
}

#' @export
.vsc.basePrint <- base::print
#' @export
.vsc.baseCat <- base::cat

#' @export
.vsc.cat <- function(...){
    # TODO: consider correct environment for print(...)?
    # env <- sys.frame(-1)
    # ret <- capture.output(base::print(...), envir=env)

    if(.packageEnv$isEvaluating){
        # return(base::cat(...))
        return(.vsc.baseCat(...))
    }
    ret <- capture.output(.vsc.baseCat(...))
    output <- paste(ret, sep="", collapse="\n")

    line <- .vsc.getLineNumber(sys.call())
    frame <- parent.frame()
    call <- sys.call(-1)
    file <- .vsc.getFileName(call, frame)
    # output <- capture.output(base::print(...))
    .vsc.sendToVsc('print', list(output=output, file=file, line=line))
}

#' @export
.vsc.print <- function(...){
    # TODO: consider correct environment for print(...)?
    # env <- sys.frame(-1)
    # ret <- capture.output(base::print(...), envir=env)

    if(.packageEnv$isEvaluating){
        return(.vsc.basePrint(...))
    }
    ret <- capture.output(.vsc.basePrint(...))
    output <- paste(ret, sep="", collapse="\n")

    line <- .vsc.getLineNumber(sys.call())
    frame <- parent.frame()
    call <- sys.call(-1)
    file <- .vsc.getFileName(call, frame)
    # output <- capture.output(base::print(...))
    .vsc.sendToVsc('print', list(output=output, file=file, line=line))
}

# #' @export
# print <- .vsc.print
# #' @export
# cat <- .vsc.cat


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

.vsc.getLineNumber <- function(call){
    ref <- attr(call, 'srcref')
    return(ref[1])
}


.vsc.sendToVsc <- function(message, body="", id=0){
    s <- .vsc.makeStringForVsc(message, body, id)
    # base::cat(s)
    .vsc.baseCat(s)
}

.vsc.makeStringForVsc <- function(message, body="", id=0, args=list()){
    .vsc.delimiter0 <- '<v\\s\\c>'
    .vsc.delimiter1 <- '</v\\s\\c>'
    l <- list(message=message, body=body, id=id, args=args)
    s <- jsonlite::toJSON(l, auto_unbox = TRUE, force=TRUE)
    r <- paste0(
        .vsc.delimiter0,
        s,
        .vsc.delimiter1,
        '\n'
    )
    return(r)
}


#' Runs the main() function
#' 
#' Runs the function main() from the global environment
#' 
#' @export
#' @param overWritePrint Whether to overwrite base::print with a version that sends output to vsc
.vsc.runMain <- function(overWritePrint=1) {

    options(prompt = "<#v\\s\\c>\n")
    options(error = recover)
    options(browserNLdisabled = TRUE)


    if(overWritePrint>=2){
        # super hacky and not recommended!!!
        methods:::.assignOverBinding(what = 'print', value = .vsc.print, where = baseenv())
        methods:::.assignOverBinding(what = 'cat', value = .vsc.cat, where = baseenv())
    } else if(overWritePrint>=1){
        .GlobalEnv$print <- .vsc.print
        .GlobalEnv$cat <- .vsc.cat
    }
    .packageEnv$isEvaluating <- FALSE

    .vsc.sendToVsc('go')
    main()
    .vsc.sendToVsc('end')
}


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# GET file-name of current function:
# attr(attr(eval.parent(sys.call()[[1]]), 'srcref'), 'srcfile')
# attr(attr(attributes(eval.parent(sys.call()[[1]]))$original, 'srcref'), 'srcfile')


#' Check if debugger is evaluating
#' 
#' Returns TRUE iff an expression is being evaluated by the debugger during a breakpoint
#' 
#' @export
#' @return Boolean indicating whether an expression is being evaluated
.vsc.isEvaluating <- function(){
    return(.packageEnv$isEvaluating)
}


#' Check if R should stop on breakpoint
#' 
#' Returns FALSE iff an expression is being evaluated by the debugger during a (different) breakpoint, else TRUE
#' 
#' @export
#' @return Boolean indicating whether R should stop on breakpoints
.vsc.stopOnBreakpoint <- function(){
    return(!.packageEnv$isEvaluating)
}


#' Sets a breakpoint
#' 
#' @description
#' Sets a breakpoint in the given file and line.
#' Is a very hacky alternative to setBreakpoint()/trace().
#' Better alternatives would be:
#'  - trace(..., at=stp$at) => Problem: trace does not print the full filename upon hitting a breakpoint (needed for vsc)
#'  - setBreakpoint() => Problem: does not print the full filename and does not support multiple breakpoints per method
#' 
#' @export
#' @param srcfile The file in which to set the breakpoint
#' @param line The line in which to set the breakpoint
#' @param incdluePackages Whether to set breakpoints in package files
.vsc.mySetBreakpoint <- function(srcfile, lines, includePackages=TRUE){
    # helper function. used to loop through (potentially empty) lists
    seq2 <- function(from, to){
        if(from>to) return(c())
        return(seq(from, to))
    }

    # find steps, that correspond to the given line numbers
    stepList <- list()
    for(i in seq2(1,length(lines))){
        if(includePackages){
            lastenv <- emptyenv() # searches through package-envs as well
        } else {
            lastenv <- .GlobalEnv # searches only through 'user'-envs
        }
        refs <- findLineNum(srcfile, lines[i], lastenv=lastenv)
        if(length(refs)>0){
            step <- refs[[1]] # Ignore other refs?? In what cases are there >1 refs??
            found <- FALSE

            # check if the same function already has breakpoints:
            for(j in seq2(1,length(stepList))){
                #check if env and function are identical:
                if(identical(stepList[[j]]$name, step$name) && identical(stepList[[j]]$env, step$env)){ 
                    #append step$at to steplist[[j]]$at, etc.
                    stepList[[j]]$at[[length(stepList[[j]]$at)+1]] <- step$at 
                    stepList[[j]]$line[[length(stepList[[j]]$line)+1]] <- step$line 
                    stepList[[j]]$timediff[[length(stepList[[j]]$timediff)+1]] <- step$timediff 
                    found <- TRUE
                    break
                }
            }
            # add new functions to stepList
            if(!found){
                step$at <- list(step$at)
                step$line <- list(step$line)
                step$timediff <- list(step$timediff)
                stepList[[length(stepList)+1]] <- step
            }
        }
    }

    # loop through functions found above
    for(i in seq2(1, length(stepList))){
        step <- stepList[[i]]
        if(FALSE){
            tracer <- bquote({
                cat(paste0(.(step$filename), "#", .(step$line), "\n"))
                browser(skipCalls = 4L)
            })
        } else {
            func <- eval(parse(text=step$name), envir = step$env)

            # loop through breakpoints for each function
            for(j in seq2(1, length(step$at))){
                loc <- step$at[[j]]
                # Make string that is cat() upon hitting the breakpoint
                # tells vsc that a breakpoint was hit and gives info about file and line
                catString <- paste0(
                    .vsc.makeStringForVsc('breakpoint'),
                    "debug at ", step$filename, '#', step$line[[j]], ": ?\n"
                )
                # only stop, if vsc is not running an eval statment from the debug console
                body(func)[[loc]] <- call('{',
                    call('if',
                        quote(.vsc.stopOnBreakpoint()),
                        call('{',
                            call('.vsc.baseCat', catString),
                            quote(.doTrace(browser()))
                        )
                    ),
                    body(func)[[loc]]
                )
            }
            # assign modified function to original environment
            # assign(step$name, func, envir = step$env)
            global <- identical(step$env, .GlobalEnv)
            methods:::.assignOverBinding(step$name, func, step$env, FALSE)
        }
    }
}

