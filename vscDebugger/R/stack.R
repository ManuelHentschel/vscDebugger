
# # buildStack() gathers information about current stackframes/scopes
# # structured as follows:

# interface stack{
#     frames: Stackframe[];
#     varLists: Variable[][];
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




########################################################################
# Stack

#' Build current stack
#' 
#' Gives info about the current stack, formatted to be used in a vsc-debugger.
#' 
#' @export
#' @param topFrame The first stack frame to consider (= the current function call)
#' @return The current stack, formatted as a nested named list
#' 
.vsc.buildStack <- function(topFrame = parent.frame()){
    assign('varLists', list(), envir = .packageEnv) # used in getVarRef()
    nFrames <- getNFrames(topFrame)
    frameIds <- nFrames:1 # vsc considers frames in the opposite order!
    frames <- lapply(frameIds, getStackFrame, nFrames)
    stack <- list(
        frames=frames,
        varLists=.packageEnv$varLists
    )
    return(stack)
}

getNFrames <- function(topFrame){
    nFrames <- sys.nframe()
    while(!identical(sys.frame(nFrames), topFrame) && !identical(sys.frame(nFrames), .GlobalEnv)){
        nFrames <- nFrames - 1
    }
    return(nFrames)
}


########################################################################
# StackFrames

getStackFrame <- function(frameId, nFrames){
    env <- sys.frame(frameId)
    id <- nFrames + 1 - frameId # vsc considers frames in the opposite order!
    call <- sys.call(frameId)
    name <- getFrameName(call)
    source <- getSource(env, call)
    line <- getLine(frameId, nFrames)
    column <- 1
    frame <- list(
        env=env,
        id=id,
        name=name,
        source=source,
        line=line,
        column=column
    )
    scopes <- getScopes(frame)
    frame$scopes <- scopes
    return(frame)
}

getFrameName <- function(call){
    name <- capture.output(base::print(call))[1]
    return(name)
}

getSource <- function(env, call){
    source <- try({
        fileName <- getSrcFilename(eval(call[[1]], envir=env))
        dirName <- getSrcDirectory(eval(call[[1]], envir=env))
        dirName <- suppressWarnings(normalizePath(dirName, winslash = '/'))
        fullPath <- file.path(dirName, fileName)
        fullPath <- suppressWarnings(normalizePath(fullPath, winslash = '\\'))
        fullPath <- toString(fullPath)

        source <- list(
            name = fileName,
            path = fullPath
        )
    }, silent=TRUE)
    if(class(source)=='try-error'){
        source <- list(name='', path='')
    }
    return(source)
}

# getLine <- function(env, call){
getLine <- function(frameId, nFrames){
    if(frameId==nFrames){
        return(1)
    }
    call <- sys.call(frameId+1)
    ref <- attr(call, 'srcref')
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

getVarRefForEnv <- function(env, maxVars=100){
    varnames <- ls(env)

    if(length(varnames)>maxVars && maxVars>0){
        varnames <- varnames[1:maxVars]
    }

    varList <- getVarList(varnames, env)
    varRef <- getVarRef(varList)
    return(varRef)
}

getVarRef <- function(varList){
    if(length(varList)==0){
        varRef <- 0
    } else{
        varRef <- length(.packageEnv$varLists) + 1 #.packageEnv$varLists is created in buildStack()
        .packageEnv$varLists[[varRef]] <- varList
    }
    return(varRef)
}


########################################################################
# Variables

getVarList <- function(names, scope){
    varList <- lapply(names, getVariableInScope, scope)
    return(varList)
}

getVariableInScope <- function(name, scope){
    variable <- try({
        valueR <- getValueR(name, scope)
        getVariable(valueR, name)
    }, silent=TRUE)
    if(class(variable)=='try-error'){
        variable <- getDummyVariable(name)
    }
    return(variable)
}


getDummyVariable <- function(name){
    variable <- list(
        name=name,
        value='???',
        type='???',
        variablesReference=0
    )
}

getVariable<- function(valueR, name, depth=5){
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
    valueR <- eval(parse(text=name), envir=scope)
    return(valueR)
}

getValue <- function(valueR){
    value <- varToString(valueR)
    return(value) # as string
}

varToString <- function(v){
    ret <- try(toString(v), silent = TRUE)
    if(class(ret) != 'try-error') return(ret)
    ret <- try({
        paste0(capture.output(v), collapse = ';\n')
    }, silent = TRUE)
    if(class(ret) != 'try-error') return(ret)
    return('???')
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
    varList <- getVarListForVar(valueR, depth)
    varRef <- getVarRef(varList)
    return(varRef)
}

getVarListForVar <- function(valueR, depth) {
    if(depth>0 && (is.list(valueR) || (is.vector(valueR) && length(valueR)>1))){
        valuesR <- valueR
        names <- names(valuesR)
        if(is.null(names)){
            names <- seq2(valuesR)
        }
        names <- vapply(names, toString, '0')
        varList <- mapply(getVariable, valuesR, names, depth-1, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(varList)
    } else{
        return(list())
    }
}


########################################################################
# Helper

seq2 <- function(from, to=NULL){
    if(is.null(from) || is.list(from) || (is.vector(from) && length(from)>1)){
        return(seq2(length(from)))
    } else if(is.null(to)){
        if(from==0){
            return(NULL)
        } else {
            return(seq(from))
        }
    } else if(from>to){
        return(NULL)
    } else{
        return(seq(from, to))
    }
}