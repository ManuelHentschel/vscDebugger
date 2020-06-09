
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


#' Build current stack and send to vsc
#'
#' Gives info about the current stack and sends it to vsc
#'
#' @export
#' @param topFrame The first stack frame to consider (= the current function call)
#' @param id The id of the message sent to vsc
#' @return None (The current stack, formatted as a nested named list is sent to vsc)
#'
.vsc.getStack <- function(topFrame = parent.frame(), id = 0, isError = 0, dummyFile = NULL, forceDummyStack = FALSE, includePackages = FALSE) {
  if(includePackages){
    lastenv <- emptyenv()
  } else{
    lastenv <- globalenv()
  }

  if (forceDummyStack || session$debugGlobal && calledFromGlobal()) {
    stack <- .vsc.getDummyStack(dummyFile = dummyFile, lastenv = lastenv)
  } else {
    stack <- .vsc.buildStack(topFrame = topFrame, isError = isError, skipFromBottom = 0, lastenv = lastenv)
  }
  .vsc.sendToVsc('stack', stack, id)
}



#' Build current stack
#'
#' Gives info about the current stack, formatted to be used in a vsc-debugger.
#'
#' @export
#' @param topFrame The first stack frame to consider (= the current function call)
#' @param skipFromTop Number of frames to skip from the top. Can be used to skip e.g. the frame of `browser()` itself.
#' @param skipFromBottom Number of frames to skip from the bottom. Can be used to skip e.g. the frame of `.vsc.runMain()`
#' @param isError Boolean indicating whether the function is called from an error state. Adds 1 to `skipFromTop`
#' @return The current stack, formatted as a nested named list
#'
.vsc.buildStack <- function(topFrame = parent.frame(), skipFromTop = 0, skipFromBottom = 1, isError = FALSE, lastenv = .GlobalEnv) {
  clearVarLists()
  if (isError) {
    skipFromTop = skipFromTop + 1
  }

  nFrames <- getNFrames(topFrame)
  frameIdsR <- seq2((nFrames - skipFromTop), (skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
  frameIdsVsc <- seq2(length(frameIdsR)) - 1
  frames <- mapply(
    getStackFrame, frameIdsR, frameIdsVsc,
    MoreArgs = list(lastenv = lastenv),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
  stack <- list(
    frames = frames,
    varLists = session$varLists
  )
  session$frameIdsR <- frameIdsR
  session$frameIdsVsc <- frameIdsVsc
  return(stack)
}


clearVarLists <- function(deleteAll = FALSE){
  for(i in seq2(session$varLists)){
    if(deleteAll || !session$varListPersistent[[i]]){
      session$varLists[[i]] <- list()
      session$varListArgs[[i]] <- list()
    }
  }
}


#' Build a dummy stack
#' 
#' Build a dummy stack that contains only one frame with the .GlobalEnv as only scope.
#' 
#' @param dummyFile The file that is returned as the source file
#' @return A stack with the same structure as `.vsc.buildStack`
#' 
.vsc.getDummyStack <- function(dummyFile = NULL, lastenv = .GlobalEnv) {
  clearVarLists()
  nFrames <- 1
  frameIdsR <- seq(nFrames, 1, -1)
  frameIdsVsc <- seq2(length(frameIdsR)) - 1
  frames <- mapply(
    getDummyFrame,
    frameIdsR,
    frameIdsVsc,
    MoreArgs = list(
      dummyFile = dummyFile,
      lastenv = lastenv
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  stack <- list(
    frames = frames,
    varLists = session$varLists
  )
  session$frameIdsR <- frameIdsR
  session$frameIdsVsc <- frameIdsVsc
  return(stack)
}

#' Get a dummy frame
#' 
#' Returns a dummy frame used in `.vsc.getDummyStack`.
#' 
#' @param frameIdR The internal id of the frame
#' @param frameIdVsc The id of the frame that is reported to vsc
#' @param dummyFile The name of the file used as source file
#' 
getDummyFrame <- function(frameIdR, frameIdVsc, dummyFile = NULL, lastenv = .GlobalEnv) {
  env <- .GlobalEnv
  id <- frameIdVsc
  name <- 'Global Workspace (click here to see variables)'
  if (is.null(dummyFile)) {
    source <- NULL
  } else {
    # source <- list(
    #     path=dummyFile
    # )
    source <- NULL
  }
  line <- 0
  column <- 0
  frame <- list(
    env = env,
    id = id,
    index = id,
    name = name,
    line = line,
    column = column
  )
  if (!is.null(source) && length(source) > 0) {
    frame$source <- source
  }
  scopes <- getScopes(frame, lastenv)
  frame$scopes <- scopes
  # only for dummy:
  frame$presentationHint <- 'label'
  return(frame)
}

#' Converts the frame id
#'
#' Converts the frame id form R to vsc or viceversa
#'
#' @param vsc The frame id as used by vsc
#' @param R The frame id as used by R
#' @return The frame id as used by the other program
convertFrameId <- function(vsc = NULL, R = NULL) {
  if (is.null(vsc) && is.null(R)) {
    return(NULL)
  } else if (is.null(vsc)) {
    ind <- which(R == session$frameIdsR)
    if (length(ind) > 0) {
      return(session$frameIdsVsc[ind])
    } else {
      return(NULL)
    }
  } else {
    ind <- which(vsc == session$frameIdsVsc)
    if (length(ind) > 0) {
      return(session$frameIdsR[ind])
    } else {
      return(NULL)
    }
  }
}

#' Get varLists for a list of given varRefs
#'
#' Get varLists for a list of given varRefs
#'
#' @param refs List of varRefs
makeVarLists <- function(refs) {
  varLists <- lapply(refs, getVarListsEntry)
  return(varLists)
}

#' Get the number of frames
#'
#' Get the number of frames
#'
#' @param topFrame Consider only frames below this frame
getNFrames <- function(topFrame) {
  nFrames <- sys.nframe()
  while (!identical(sys.frame(nFrames), topFrame) && !identical(sys.frame(nFrames), .GlobalEnv)) {
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
getStackFrame <- function(frameIdR, frameIdVsc, lastenv=.GlobalEnv) {
  env <- sys.frame(frameIdR)
  # id <- nFrames + 1 - frameIdR # vsc considers frames in the opposite order!
  id <- frameIdVsc
  call <- sys.call(frameIdR)
  name <- getFrameName(call)
  # source <- getSource(env, call)
  source <- getSource(frameIdR)
  lineAndFile <- .vsc.getCallingLineAndFile(frameId=frameIdR, skipCalls=-1)
  line <- lineAndFile$line
  endLine <- lineAndFile$endLine
  # column <- 0
  column <- lineAndFile$column
  endColumn <- lineAndFile$endColumn+1 # vsc does not include the last column
  if(is.null(line)) line <- 0
  frame <- list(
    env = env,
    id = id,
    index = id,
    name = name,
    line = line,
    endLine = endLine,
    column = column,
    endColumn = endColumn
  )
  if (!is.null(source) && length(source) > 0) {
    frame$source <- source
  }
  scopes <- getScopes(frame, lastenv)
  frame$scopes <- scopes
  return(frame)
}

#' Get the frame name of a given call
#'
#' Get the frame name of a given call
getFrameName <- function(call) {
  name <- varToStringWithCaptureOutput(call)
  # name <- substr(name, 1, 16)
  maxChars <- 300
  if (nchar(name) > maxChars) {
    name <- paste0(substr(name, 1, maxChars - 3), '...')
  }
  return(name)
}


#' Gather info about a frame's source code
#' 
#' Gathers and returns a named list containing info about the name, path, and content of the source file of a frame
#' 
#' @param frameId A frame id (as used by R) that can be passed to sys.call
#' @return A named list containing info about the source file
getSource <- function(frameId) {
  if (frameId >= sys.nframe()) {
    return(NULL)
  }
  ret <- tryCatch({
    call <- sys.call(frameId + 1)
    lineAndFile <- .vsc.getLineAndFile(call)
    filename <- lineAndFile$filename

    if(is.null(lineAndFile$isFile)){
      lineAndFile$isFile <- is.character(filename) && !identical(filename, '')
    }

    if(lineAndFile$isFile){
      sourceReference = 0
    } else{
      sourceReference = frameId + 1
    }


    ret <- list(
      name = basename(filename),
      path = filename,
      sourceReference = sourceReference,
      isFile = lineAndFile$isFile,
      srcbody = lineAndFile$srcbody
    )
  }, error = function(e) NULL)
  
  return(ret)
}

########################################################################
# Scopes

#' Gather info about scopes in a frame
#' 
#' Gathers and returns info about the scopes in a frame
#' 
#' @param frame A frame as constructed by `getStackFrame`
#' @return A list of scopes as constructed by `getScope`
getScopes <- function(frame, lastenv = .GlobalEnv) {
  envs <- getScopeEnvs(frame$env, lastenv)
  scopes <- lapply(envs, getScope)
  return(scopes)
}

#' Gather info about a scope
#' 
#' Gathers and returns info about the variables in a scope
#' 
#' @param env The environment (=scope) that is to be analyzed
#' @return A named list containing the name of the scope an a variablesReference
getScope <- function(env) {
  name <- capture.output(str(env))[1]
  varRef <- getVarRefForVar(env)
  scope <- list(
    name = name,
    variablesReference = varRef
  )
  return(scope)
}

#' Get the scopes corresponding to a frame
#' 
#' Gets the scopes corresponding to a frame
#' 
#' @param firstenv The top environment of the current frame
#' @param lastenv The last environment to be considered. By default .GlobalEnv, use emptyenv() to consider package environments
getScopeEnvs <- function(firstenv = parent.frame(), lastenv = .GlobalEnv) {
  env <- firstenv
  scopes <- list(env)
  while (!identical(env, lastenv) && !identical(env, emptyenv())) {
    env <- parent.env(env)
    scopes[[length(scopes) + 1]] <- env
  }
  return(scopes)
}

#' Get VariablesReference for varListArgs
#' 
#' @description 
#' Gets a variablesReference for a list of arguments meant for `getVarList`.
#' Unless specified the actual call to `getVarList` is not evaluated.
#' Instead the arguments are stored and evaluated when the variablesReference is requested.
#' 
#' @param varListArgs A named list containing the arguments that are passed to `getVarList`
#' @param evalCall A boolean indicating whether to evaluate the call already (defaults to `FALSE`)
#' @param varRef If specified, this varRef is used instead of incrementing the last varRef by 1
#' 
getVarRefForVarListArgs <- function(varListArgs = NULL, evalCall = FALSE, varRef = NULL, persistent=FALSE) {
  if (is.null(varRef)) {
    varRef <- length(session$varListArgs) + 1
  }
  session$varListArgs[[varRef]] <- varListArgs
  session$varListPersistent[[varRef]] <- persistent
  if (evalCall) {
    # use arglist instead of call/eval/do.call to avoid evaluating the content of variables that contain expressions
    v <- varListArgs$v
    depth <- varListArgs$depth
    maxVars <- varListArgs$maxVars
    includeAttributes <- varListArgs$includeAttributes

    variables <- getVarList(v = v, depth = depth, maxVars = maxVars, includeAttributes = includeAttributes)
    varList <- list(
      reference = varRef,
      isReady = TRUE,
      variables = variables
    )
    session$varLists[[varRef]] <- varList
  } else {
    session$varLists[[varRef]] <- list(
      reference = 0,
      isReady = FALSE,
      variables = list()
    )
  }
  return(varRef)
}

#' Get the variable corresponding to a variablesReference
#' 
#' Is basically is a wrapper for `session$varLists[[varRef]]`.
#' If necessary evaluates the call for a given varRef and returns the result.
#' 
#' @param varRef The variablesReference as returned by `getVarRefForVarListArgs} or \code{getVarRefForVar`
#' @return A list of variables corresponding to the varRef
getVarListsEntry <- function(varRef) {
  # 
  # to avoid excessive nested calls, the varList is only computed once requested
  # before the varList is requested, only the arguments for getVarList() that will return it is stored in session$varListArgs


  # retrieve varList (if exists)
  if(varRef <= length(session$varLists)){
    varList <- session$varLists[[varRef]]
    returnDummy <- is.null(varList$isReady)
  } else{
    returnDummy <- TRUE
  }

  # return dummy variable if no varList entry found
  if (returnDummy) {
    return(list(
      reference = varRef,
      isReady = TRUE,
      variables = list()
    ))
  }

  # compute varList if necessary
  if (!varList$isReady) {
    varListArgs <- session$varListArgs[[varRef]]
    v <- varListArgs$v
    depth <- varListArgs$depth
    maxVars <- varListArgs$maxVars
    includeAttributes <- varListArgs$includeAttributes
    variables <- getVarList(v = v, depth = depth, maxVars = maxVars, includeAttributes = includeAttributes)


    varList <- list(
      reference = varRef,
      isReady = TRUE,
      variables = variables
    )
    session$varLists[[varRef]] <- varList
  }

  return(varList)
}


########################################################################
# Variables


#' Get variable from environment
#' 
#' Is basically a wrapper for `get(name, envir=env)`, but does not forces promises.
#' 
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return Returns the value of the variable or a representation of a promise as returned by `getPromiseVar`
getVarInEnv <- function(name, env) {
  # get Info about a variable in an environment
  # separate from getVariable(), since environments might contain promises
  tryCatch({
    if (name == '...') {
      getDotVars(env)
    } else if (isPromise(name, env)) {
      getPromiseVar(name, env)
    } else if (bindingIsActive(name, env) && !getOption('vsc.evaluateActiveBindings', FALSE)) {
      getActiveBinding(name, env)
    } else {
      get(name, envir = env)
    }
  }, error = function(e) {
    .text <- 
      if (missingInEnv(name, env)) {
        '<MISSING>'
      } else{
        '<ERROR>'
      }
    getInfoVar(.text)
  })
}

getActiveBinding <- function(name, env){
  ret <- if (getRversion() >= "4.0.0") {
    activeBindingFunction(name, env)
  } else {
    # as.list.environment(env)[[name]]
    getInfoVar("R version >= 4.0.0 required to show active binding function!")
  }
  structure(list(bindingFunction = ret), class = '.vsc.activeBinding')
}

getVarsInEnv <- function(env, all.names = TRUE) {
  names <- ls(env, all.names = all.names)
  vars <- lapply(names, getVarInEnv, env)
  return(list(
    values = vars,
    names = names
  ))
}

missingInEnv <- function(name, env){
  ret <- tryCatch(
    do.call('missing', list(name), envir=env),
    error = function(e) FALSE
  )
  return(ret)
}

getInfoVar <- function(text, type='Warning: the variable value is just an info from the debugger!'){
  var <- list( text = text, type = type)
  class(var) <- '.vsc.infoVar'
  return(var)
}

#' Get unevaluated variables passed as ellipses
#' 
#' Get variables which are passed as ellipses without evaluating them.
#' @param env The environment where the "..." variable is present
#' @return A named list for each element of "..." containing the 
#'   expression that will be evaluated, 
#'   the environment where it will be evaluated, and the string 
#'   representation of the expression.
getDotVars <- function(env) {
  ## Note: substitute(...()) is officially not supported, but it 
  ## works as intended for all relevant R versions (at least from R 3.0.0)
  dots <- substitute(...(), env = env)
  ret <- lapply(
    dots, 
    function(x) {
      list(
        dotExpr = x,
        dotCode = tryCatch(
          ##TODO: use proper formatting function
          paste0(toString(x), collapse = ";"), 
          error = function(e) {
            if (isTRUE(getOption('vsc.trySilent', default=TRUE))) {
              "???"
            } else {
              stop(e, call. = FALSE)
            }
          }
        ),
        ##TODO: which environment do we want to display here?
        dotEnv = env
      )
    }
  )
  structure(ret, class = ".vsc.ellipses")
} 

#' Get a representation of a promise
#' 
#' Gets a representation of a promise without forcing the promise
#' 
#' @param name The name of the variable
#' @param env The environment in which to evaluate
#' @return A named list containing the expression that will be evaluated, the environment in which it will be evaluated and a string representation of the expression
#' 
getPromiseVar <- function(name, env) {
  promiseExpr <- pryr:::promise_code(name, env)
  promiseCode <- try(paste0(toString(promiseExpr), collapse = ';'), silent=getOption('vsc.trySilent', default=TRUE))
  if (class(promiseCode) == 'try-error') promiseCode <- '???'
  promiseEnv <- pryr:::promise_env(name, env)
  var <- list(
    promiseCode = promiseCode,
    promiseEnv = promiseEnv,
    promiseExpr = promiseExpr
  )
  class(var) <- '.vsc.promise'
  return(var)
}


isPromise <- function(name, env) {
  if (pryr:::is_promise2(name, env)) {
    return(!pryr:::promise_evaled(name, env))
  }
  return(FALSE)
}

getDummyVariable <- function(name) {
  variable <- list(
    name = name,
    value = '???',
    type = '???',
    variablesReference = 0
  )
}

getVariable <- function(valueR, name, depth = 20) {
  value <- varToString(valueR)
  type <- getType(valueR)
  variablesReference <- getVarRefForVar(valueR, depth)
  evaluateName <- .vsc.getCustomInfo(valueR, 'evaluateName', '', '')

  variable <- list(
    name = name,
    value = value,
    type = type,
    variablesReference = variablesReference,
    depth = depth,
    evaluateName = evaluateName
  )
  return(variable)
}

getVariableForEval <- function(valueR, name, depth = 20){
  value <- varToString(valueR)
  type <- getType(valueR)
  variablesReference <- getVarRefForVar(valueR, depth, persistent=TRUE)

  variable <- list(
    result = value,
    type = type,
    variablesReference = variablesReference
  )
}

getEmptyVariableForEval <- function(){
  variable <- list(
    result = '',
    variablesReference = 0
  )
}


varToString <- function(v) {
  ret <- .vsc.getCustomInfo(v, 'toString', NULL, NULL)
  try({
    if (is.null(ret)) {
      ret <- toString2(v)
    }
  }, silent = getOption('vsc.trySilent', default=TRUE))
  if(is.null(ret) || inherits(ret, 'try-error')){
    ret <- '???'
  }
  return(ret)
}

toString2 <- function(v, quoteStrings = FALSE) {
  # recursive version of toString()
  # WIP
  # TODO: modify to return e.g.: 'list(c(1,2,3), c("a","b","c"))'
  if (is.factor(v)) {
    v <- format(v)
  } else if (is.data.frame(v)) {
    v <- as.list(v)
  }
  if (is.list(v) || is.vector(v) && length(v) > 1 || is.matrix(v)) {
    l <- lapply(v, toString2, quoteStrings = TRUE)
    type <- getType(v, TRUE)
    s <- paste0(l, collapse = ',')
    if(!is.null(type) || nchar(type)>0){
      s <- paste0(type, '(', s, ')')
    }
  } else {
    s <- toString(v)
    if (quoteStrings && is.character(v)) {
      s <- paste0('"', s, '"')
    }
  }
  return(s)
}

varToStringWithCaptureOutput <- function(v) {
  # dirty way to convert anything to string
  # should be avoided!
  # TODO: replace with proper use of format(...)?
  ret <- try({
    paste0(capture.output(v), collapse = '\n')
  }, silent = getOption('vsc.trySilent', default=TRUE))
  if (inherits(ret, 'try-error')) {
    ret <- '???'
  }
  return(ret)
}


getType <- function(valueR, short = FALSE) {
  # returns one of two possible values (depending on argument short):
  # long: full typename, returned as variable type
  #       default: typeof(v)
  #
  # short: used in varToString to differentiate e.g. list(1,2,3) and c(1,2,3)
  #        default: ''

  # default case:

  if (short) {
    ret <- .vsc.getCustomInfo(valueR, 'shortType', '???', '???')
  } else {
    ret <- .vsc.getCustomInfo(valueR, 'longType', '???', '???')
  }
  return(ret)
}



getVarRefForVar <- function(valueR, depth = 10, maxVars = 1000, includeAttributes = TRUE, persistent=FALSE) {
  if (depth > 0 && .vsc.getCustomInfo(valueR, 'hasChildren', TRUE, TRUE)) {
    varListArgs <- list(v = valueR, depth = depth, maxVars = maxVars, includeAttributes = includeAttributes)
    varRef <- getVarRefForVarListArgs(varListArgs, persistent=persistent)
  } else {
    varRef <- 0
  }
  return(varRef)
}


getVarList <- function(v, depth = 10, maxVars = 1000, includeAttributes = TRUE) {
  # TODO: accept argList containing all args
  childVars <- .vsc.getCustomInfo(v, 'childVars')

  vars <- childVars$values
  varNames <- childVars$names
  if (is.null(varNames) || length(varNames) == 0) {
    varNames <- names(vars)
  }
  if (is.null(varNames) || length(varNames) == 0) {
    varNames <- lapply(seq2(vars), toString, '0')
  }

  varList <- mapply(getVariable, vars, varNames, MoreArgs = list(depth = depth - 1), SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # get variable info about attributes
  # separate, since environments might have attributes as well

  if (includeAttributes) {
    if (.vsc.getCustomInfo(v, 'includeAttributes', TRUE, TRUE)) {
      atr <- attributes(v)
      atrNames <- lapply(names(atr), function(s) {paste0('_', s)})
    } else {
      atr <- list()
      atrNames <- list()
    }

    customAttributes <- .vsc.getCustomInfo(v, 'customAttributes', list(), list(), appendNested = TRUE)
    internalAttributes <- .vsc.getCustomInfo(v, 'internalAttributes', list(), list())

    cAtr <- c(internalAttributes$values, customAttributes$values)
    cAtrNames <- c(internalAttributes$names, customAttributes$names)

    atr <- append(atr, cAtr)
    atrNames <- append(atrNames, cAtrNames)

    atrList <- mapply(getVariable, atr, atrNames, MoreArgs = list(depth - 1), SIMPLIFY = FALSE, USE.NAMES = FALSE)

    varList <- append(varList, atrList)
  }

  return(varList)
}
