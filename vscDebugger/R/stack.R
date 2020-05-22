
# # buildStack() gathers information about current stackframes/scopes
# # structured as follows (using nested named lists):

# interface stack{
#     frames: Stackframe[];
#     varLists: Variable2[];
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
    .packageEnv$varListArgs <- list()
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
    varRef <- getVarRefForVar(env)
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

getVarRefForVarListArgs <- function(varListArgs=NULL, evalCall=FALSE, varRef=NULL){
    if(is.null(varRef)){
        varRef <- length(.packageEnv$varListArgs) + 1
    }
    .packageEnv$varListArgs[[varRef]] <- varListArgs
    if(evalCall){
        # use arglist instead of call/eval/do.call to avoid evaluating the content of variables that contain expressions
        v <- varListArgs$v
        depth <- varListArgs$depth
        maxVars <- varListArgs$maxVars
        includeAttributes <- varListArgs$includeAttributes

        variables <- getVarList(v=v, depth=depth, maxVars=maxVars, includeAttributes=includeAttributes)
        varList <- list(
            reference = varRef,
            isReady = TRUE,
            variables = variables
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
    # basically is a wrapper for ".packageEnv$varLists[[varRef]]"
    # to avoid excessive nested calls, the varList is only computed once requested
    # before the varList is requested, only the arguments for getVarList() that will return it is stored in .packageEnv$varListArgs

    # return dummy entries for invalid requests:
    if(varRef>length(.packageEnv$varLists)){
        return(list(
            reference = varRef,
            isReady = TRUE,
            variables = list()
        ))
    }

    # retrieve varList
    varList <- .packageEnv$varLists[[varRef]]

    # compute varList if necessary
    if(!varList$isReady){
        varListArgs <- .packageEnv$varListArgs[[varRef]]
        v <- varListArgs$v
        depth <- varListArgs$depth
        maxVars <- varListArgs$maxVars
        includeAttributes <- varListArgs$includeAttributes
        variables <- getVarList(v=v, depth=depth, maxVars=maxVars, includeAttributes=includeAttributes)


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

getVariableInEnv <- function(name, env){
    # get Info about a variable in an environment
    # separate from getVariable(), since environments might contain promises
    if(isPromise(name, env)){
        variable <- getPromiseVariable(name, env)
    } else{
        variable <- try({
            valueR <- get(name, envir = env)
            getVariable(valueR, name)
        }, silent=FALSE)
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
    value <- varToString(valueR)
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


varToString <- function(v){
    ret <- getCustomInfo(v, 'toString')
    if(!is.null(ret)){
        # do nothing
    } else if(is.null(v)){
        ret <- 'NULL'
    } else if(is.environment(v)){
        ret <- format(v)
    } else if(is.function(v)){
        ret <- paste(format(v), collapse='\n')
    } else if(is.language(v) && length(as.list(v))>1){
        ret <- paste0(format(v), collapse = '\n')
    } else{
        # generic case
        # TODO: if v is a list, return e.g.: 'list(c(1,2,3), c("a","b","c"))'
        # check for type of v
        type <- getType(v, TRUE)

        # get value-representation of v
        ret <- try(toString2(v), silent = TRUE)

        # handle errors
        if(class(ret) == 'try-error'){
            ret <- '???'
        }

        # join type+value
        if(type != ''){
            ret <- paste0(type, '(', ret, ')')
        }
    }

    return(ret)
}

toString2 <- function(v, quoteStrings=FALSE){
    # recursive version of toString()
    # TODO: modify to return e.g.: 'list(c(1,2,3), c("a","b","c"))'
    if(is.factor(v)){
        v <- format(v)
    } else if(is.data.frame(v)){
        v <- as.list(v)
    }
    if(is.list(v) || is.vector(v) && length(v)>1){
        l <- lapply(v, toString2, quoteStrings=TRUE)
        s <- paste0(l, collapse = ',')
    } else{
        s <- toString(v)
        if(quoteStrings && is.character(v)){
            s <- paste0('"', s, '"')
        }
    }
    return(s)
}

varToStringWithCaptureOutput <- function(v){
    # TODO: replace with proper use of format(...)?
    ret <- try({
        paste0(capture.output(v), collapse = '\n')
    }, silent = TRUE)
    if(class(ret) == 'try-error'){
        ret <- '???'
    }
    return(ret)
}


getType <- function(valueR, short=FALSE){
    # returns one of two possible values (depending on argument short):
    # long: full typename, returned as variable type
    #       default: typeof(v)
    #
    # short: used in varToString to differentiate e.g. list(1,2,3) and c(1,2,3)
    #        default: ''
    
    # default case:

    if(short){
        ret <- getCustomInfo(valueR, 'shortType')
    } else{
        ret <- getCustomInfo(valueR, 'longType')
    }
    if(!is.null(ret)){
        return(ret)
    }

    ret <- list(long=typeof(valueR), short='')    

    # overwrite default for some types:
    if(is.data.frame(valueR)){
        ret$short <- ret$long <- 'data.frame'
    } else if(is.factor(valueR)){
        ret$short <- ret$long <- 'factor'
    } else if(is.list(valueR)){
        ret$short <- ret$long <- 'list'
    } else if(is.vector(valueR) && length(valueR)>1){
        ret$long <- 'vector'
        ret$short <- 'c'
    } else if(is.matrix(valueR)){
        ret$long <- 'matrix'
    } else if(is.character(valueR)){
        ret$long <- 'string'
    }

    # return the requested entry:
    if(short){
        return(ret$short)
    } else{
        return(ret$long)
    }
}



getVarRefForVar <- function(valueR, depth=10, maxVars=1000, includeAttributes=TRUE) {
    hasAttributes <- (!is.null(attributes(valueR)) || !is.null(getCustomInfo(valueR, 'customAttributes'))) && includeAttributes
    if(depth>0 && (is.environment(valueR) || hasAttributes || is.list(valueR) || (is.vector(valueR) && length(valueR)>1) || is.factor(valueR) || is.language(valueR) && length(as.list(valueR))>1)){
        varListArgs <- list(v=valueR, depth=depth, maxVars=maxVars, includeAttributes=includeAttributes)
        varRef <- getVarRefForVarListArgs(varListArgs)
    } else{
        varRef <- 0
    }
    return(varRef)
}



getCustomInfo <- function(v, info){
    ret <- NULL
    try({
        for(varInfo in .packageEnv$varInfo){
            if(is.function(varInfo[[info]])){
                if(varInfo$doesApply(v)){
                    ret <- varInfo[[info]](v)
                    break
                }
            }
        }
    }, silent=FALSE)
    if(class(ret) == 'try-error'){
        return(NULL)
    } else{
        return(ret)
    }
}


getVarList <- function(v, depth=10, maxVars=1000, includeAttributes=TRUE){
    # TODO: accept argList containing all args
    # TODO: make customizable with 'groups' of functions:
    #       - function that returns a boolean (e.g. is.XXX()), if this 'group' applies
    #       - function that returns the corresponding children-variables (e.g. as.list())
    #       - function that returns a string representation
    #       - further functions for e.g. numbering ([[1]] vs [1]), custom 'attributes' (e.g. body(f) for functions)
    #       - define priority by order of groups (e.g. check is.data.frame() first , then is.list())
    if(is.environment(v)){
        # special case environment: might contain promises that should not be evaluated
        varNames <- ls(v)
        varList <- lapply(varNames, getVariableInEnv, v)
    } else{
        # normal case:
        childVars <- getCustomInfo(v, 'childVars')
        vars <- childVars$values
        varNames <- childVars$names
        if(is.list(vars)){
            # do nothing
        } else if(is.data.frame(v)){
            vars <- as.list(v)
        } else if(is.factor(v)){
            vars <- format(v)
        } else if(is.vector(v) && length(v)>1){
            vars <- as.list(v)
        } else if(is.language(v)){
            vars <- as.list(v)
        } else if(is.list(v)){
            vars <- v
        } else if(is.matrix(v)){
            vars <- lapply(seq2(nrow(v)), function(i) v[i,])
            names(vars) <- rownames(v)
        } else{
            # normal (atomic) variable does not have children
            vars <- list()
        }

        if(!is.null(varNames)){
            # do nothing
        } else if(!is.null(names(vars))){
            varNames <- as.list(names(vars))
        } else{
            varNames <- lapply(seq2(vars), toString, '0')
        }
        

        varList <- mapply(getVariable, vars, varNames, MoreArgs=list(depth=depth-1), SIMPLIFY=FALSE, USE.NAMES=FALSE)
        
    }
    

    # get variable info about attributes
    # separate, since environments might have attributes as well
    if(includeAttributes){
        atr <- attributes(v)
        atrNames <- lapply(names(atr), function(s){paste0('_',s)})

        customAttributes <- getCustomInfo(v, 'customAttributes')
        cAtr <- customAttributes$values
        cAtrNames <- customAttributes$names

        atr <- append(atr, cAtr)
        atrNames <- append(atrNames, cAtrNames)

        atrList <- mapply(getVariable, atr, atrNames, MoreArgs=list(depth-1), SIMPLIFY=FALSE, USE.NAMES=FALSE)

        varList <- append(varList, atrList)
    }

    return(varList)
}
