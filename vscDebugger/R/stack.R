
# # buildStack() gathers information about current stackframes/scopes
# # structured as follows (using nested named lists):

# interface stack{
#     frames: Stackframe[];
#     varLists: Variable[];
# }
#
# interface StackFrame{
#     env: R-environment;
#     id: number;
#     name: string;
#     source: Source;
#     line: number;
#     scopes: Scope[];
# }
#
# interface Source{
#     name: string;
#     path: string;
# }
#
# interface Scope{
#     name: string;
#     variablesReference: number;
# }
# 
# interface Variable{
#     name: string;
#     value: string;
#     type: string;
#     variablesReference: number;
# }
#
# # VarLists are internally stored as:
# interface Variable2{
#     reference: number;
#     isReady: boolean;
#     variables: Variable[];
# }



########################################################################
# Stack

#' Build current stack
#' 
#' Gives info about the current stack, formatted to be used in a vsc-debugger.
#' 
#' @export
#' @param topFrame The first stack frame to consider (= the current function call)
#' @param skipFromTop Number of frames to skip from the top. Can be used to skip e.g. the frame of \code{browser()} itself.
#' @param skipFromBottom Number of frames to skip from the bottom. Can be used to skip e.g. the frame of \code{.vsc.runMain()}
#' @param isError Boolean indicating whether the function is called from an error state. Adds 1 to \code{skipFromTop}
#' @return The current stack, formatted as a nested named list
#' 
.vsc.buildStack <- function(topFrame = parent.frame(), skipFromTop=0, skipFromBottom=1, isError=FALSE){
    .packageEnv$varLists <- list()
    .packageEnv$varListCalls <- list()
    if(isError){
        skipFromTop = skipFromTop + 1
    }
    nFrames <- getNFrames(topFrame)
    frameIdsR <- seq2((nFrames-skipFromTop), (skipFromBottom+1), -1) # vsc considers frames in the opposite order!
    frameIdsVsc <- seq2(length(frameIdsR))-1
    # frameIds <- 1:nFrames
    # frames <- lapply(frameIdsR, getStackFrame, nFrames)
    frames <- mapply(getStackFrame, frameIdsR, frameIdsVsc, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    stack <- list(
        frames=frames,
        varLists=.packageEnv$varLists
    )
    .packageEnv$frameIdsR <- frameIdsR
    .packageEnv$frameIdsVsc <- frameIdsVsc
    return(stack)
}

#' Converts the frame id
#' 
#' Converts the frame id form R to vsc or viceversa
#' 
#' @param vsc The frame id as used by vsc
#' @param R The frame id as used by R
#' @return The frame id as used by the other program
convertFrameId <- function(vsc=NULL, R=NULL){
    if(is.null(vsc) && is.null(R)){
        return(NULL)
    } else if(is.null(vsc)){
        ind <- which(R == .packageEnv$frameIdsR)
        if(length(ind)>0){
            return(.packageEnv$frameIdsVsc[ind])
        } else{
            return(NULL)
        }
    } else{
        ind <- which(vsc == .packageEnv$frameIdsVsc)
        if(length(ind)>0){
            return(.packageEnv$frameIdsR[ind])
        } else{
            return(NULL)
        }
    }
}

#' Get varLists for a list of given varRefs
#' 
#' Get varLists for a list of given varRefs
#' 
#' @param refs List of varRefs
makeVarLists <- function(refs){
    varLists <- lapply(refs, getVarListsEntry)
    return(varLists)
}

#' Get the number of frames
#' 
#' Get the number of frames
#'
#' @param topFrame Consider only frames below this frame
getNFrames <- function(topFrame){
    nFrames <- sys.nframe()
    while(!identical(sys.frame(nFrames), topFrame) && !identical(sys.frame(nFrames), .GlobalEnv)){
        nFrames <- nFrames - 1
    }
    return(nFrames)
}


########################################################################
# StackFrames

#' Gather info about a stack frame
#' 
#' Gathers info about a stack frame identified by frameIdR
#'
#' @param frameIdR Frame Id as used by R. Used to identify the frame
#' @param frameIdVsc Frame Id as used by vsc. Is only added as info
getStackFrame <- function(frameIdR, frameIdVsc){
    env <- sys.frame(frameIdR)
    # id <- nFrames + 1 - frameIdR # vsc considers frames in the opposite order!
    id <- frameIdVsc
    call <- sys.call(frameIdR)
    name <- getFrameName(call)
    source <- getSource(env, call)
    line <- getLine(frameIdR)
    column <- 0
    frame <- list(
        env=env,
        id=id,
        index=id,
        name=name,
        source=source,
        line=line,
        column=column
    )
    scopes <- getScopes(frame)
    frame$scopes <- scopes
    return(frame)
}

#' Get the frame name of a given call
#' 
#' Get the frame name of a given call
getFrameName <- function(call){
    name <- varToStringWithCaptureOutput(call)
    # name <- substr(name, 1, 16)
    name <- substr(name, 1, 50)
    return(name)
}

#' Get Source Info about a call in a given environment
#' 
#' Get Source Info about a call in a given environment
getSource <- function(env, call){
    source <- try({
        fileName <- getSrcFilename(eval(call[[1]], envir=env))
        dirName <- getSrcDirectory(eval(call[[1]], envir=env))
        dirName <- suppressWarnings(normalizePath(dirName, winslash = '/'))
        fullPath <- file.path(dirName, fileName)
        fullPath <- suppressWarnings(normalizePath(fullPath, winslash = '\\'))
        fullPath <- toString(fullPath)
        fileName <- toString(fileName)

        source <- list(
            name = fileName,
            path = fullPath,
            sourceReference = 0
        )
    }, silent=TRUE)
    if(class(source)=='try-error'){
        source <- list(name='???', path='')
    }
    return(source)
}

#' Get the source line of the function call corresponding to a frameId
#' 
#' Get the source line of the function call corresponding to a frameId
getLine <- function(frameId){
    if(frameId>=sys.nframe()){
        return(1)
    }
    call <- sys.call(frameId+1)
    ref <- attr(call, 'srcref')
    if(is.null(ref)){
        ref <- 0
    }
    return(ref[1])
    # return(1)
}


########################################################################
# Scopes

getScopes <- function(frame){
    envs <- getScopeEnvs(frame$env)
    scopes <- lapply(envs, getScope)
    return(scopes)
}

getScope <- function(env){
    name <- capture.output(str(env))[1]
    varRef <- getVarRefForEnv(env)
    scope <- list(
        name=name,
        variablesReference=varRef
    )
    return(scope)
}

getScopeEnvs <- function(firstenv=parent.frame(), lastenv=.GlobalEnv){
    env <- firstenv
    scopes <- list(env)
    while (!identical(env, lastenv) && !identical(env, emptyenv())) {
        env <- parent.env(env)
        scopes[[length(scopes) + 1]] <- env
    }
    return(scopes)
}

getVarRefForEnv <- function(env, maxVars=1000){
    varnames <- ls(env)

    if(length(varnames)>maxVars && maxVars>0){
        varnames <- varnames[1:maxVars]
    }

    # varList <- getVarListForEnv(varnames, env)
    varListCall <- call('getVarListForEnv', varnames, env)
    varRef <- getVarRef(varListCall)
    return(varRef)
}

getDummyVarRef <- function(call){
    varRef <- 
    .packageEnv$varListCalls

}

getVarRef <- function(varListCall=NULL, evalCall=FALSE, varRef=NULL){
    if(is.null(varRef)){
        varRef <- length(.packageEnv$varListCalls) + 1
    }
    .packageEnv$varListCalls[[varRef]] <- varListCall
    if(evalCall){
        varList <- eval(varListCall)
        varList <- list(
            reference = varRef,
            isReady = TRUE,
            variables = varList
        )
        .packageEnv$varLists[[varRef]] <- varList
    } else{
        .packageEnv$varLists[[varRef]] <- list(
            reference = 0,
            isReady = FALSE,
            variables = list()
        )
    }
    return(varRef)
}

getVarListsEntry <- function(varRef){
    if(varRef>length(.packageEnv$varLists)){
        return(list(
            reference = varRef,
            isReady = TRUE,
            variables = list()
        ))
    }
    varList <- .packageEnv$varLists[[varRef]]
    if(!varList$isReady){
        variables <- eval(.packageEnv$varListCalls[[varRef]])
        varList <- list(
            reference = varRef,
            isReady = TRUE,
            variables = variables
        )
        .packageEnv$varLists[[varRef]] <- varList
    }
    return(varList)
}


########################################################################
# Variables

getVarListForEnv <- function(names, scope){
    varList <- lapply(names, getVariableInScope, scope)
    return(varList)
}

getVariableInScope <- function(name, scope){
    if(isPromise(name, scope)){
        variable <- getPromiseVariable(name, scope)
    } else{
        variable <- try({
            valueR <- getValueR(name, scope)
            getVariable(valueR, name)
        }, silent=TRUE)
    }
    if(class(variable)=='try-error'){
        variable <- getDummyVariable(name)
    }
    return(variable)
}

isPromise <- function(name, env){
    if(pryr:::is_promise2(name, env)){
        return(!pryr:::promise_evaled(name, env))
    }
    return(FALSE)
}

getPromiseVariable <- function(name, env){
    promiseCode <- varToStringWithCaptureOutput(
        pryr:::promise_code(name,env)
    )
    variable <- list(
        name=name,
        value=promiseCode,
        type='Promise',
        variablesReference=0
    )
}

getDummyVariable <- function(name){
    variable <- list(
        name=name,
        value='???',
        type='???',
        variablesReference=0
    )
}

getVariable<- function(valueR, name, depth=20){
    value <- getValue(valueR)
    type <- getType(valueR)

    variablesReference <- getVarRefForVar(valueR, depth)

    variable <- list(
        name=name,
        value=value,
        type=type,
        variablesReference=variablesReference,
        depth=depth
    )
    return(variable)
}

getValueR <- function(name, scope){
    # valueR <- eval(parse(text=name), envir=scope)
    valueR <- get(name, envir = scope)
    return(valueR)
}

getValue <- function(valueR){
    value <- varToString(valueR)
    return(value) # as string
}

varToString <- function(v){
    # special case: v is NULL
    if(is.null(v)){
        ret <- 'NULL'
        return(ret)
    }

    # check for type of v
    type <- ''
    if(is.list(v)){
        type <- 'list'
    } else if(is.matrix(v)){
        type <- 'matrix'
    } else if(is.vector(v) && length(v)>1){
        type <- 'vector'
    }

    # get value-representation of v
    ret <- ''
    if(is.function(v)){
        ret <- varToStringWithCaptureOutput(v)
    } else{
        ret <- try(toString(v), silent = TRUE)
    }

    # handle errors
    if(class(ret) == 'try-error'){
        ret <- '???'
    }

    # join type+value
    if(type != ''){
        ret <- paste0(type, '(', ret, ')')
    } else{
        ret <- ret
    }

    # return
    return(ret)
}

varToStringWithCaptureOutput <- function(v){
    ret <- try({
        paste0(capture.output(v), collapse = '\n')
    }, silent = TRUE)
    if(class(ret) == 'try-error'){
        ret <- '???'
    }
    return(ret)
}

getType <- function(valueR){
    if(is.list(valueR)){
        return('list')
    } else if(is.vector(valueR) && length(valueR)>1){
        return('vector')
    } else if(is.matrix(valueR)){
        return('matrix')
    } else{
        return(typeof(valueR))
    }
}

getVarRefForVar <- function(valueR, depth) {
    if(depth>0 && (is.list(valueR) || (is.vector(valueR) && length(valueR)>1))){
        varListCall <- call('getVarListForVar', valueR, depth)
        varRef <- getVarRef(varListCall)
    } else{
        varRef <- 0
    }
    return(varRef)
}

getVarListForVar <- function(valueR, depth, maxVars=1000) {
    if(depth>0 && (is.list(valueR) || (is.vector(valueR) && length(valueR)>1))){
        valuesR <- valueR
        if(length(valuesR)>maxVars && maxVars>0){
            valuesR <- valuesR[1:maxVars]
        }
        names <- names(valuesR)
        if(is.null(names)){
            names <- seq2(valuesR)
        }
        names <- vapply(names, toString, '0')
        varList <- mapply(getVariable, valuesR, names, depth-1, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(varList)
    } else{
        # TODO: handle matrix
        return(list())
    }
}


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