


#' @export
.vsc.setVariable <- function(variablesReference, name, value){
  varList <- getVarListsEntry(variablesReference)
  setInfo <- NULL
  for(var in varList$variables){
    if(var$name == name){
      setInfo <- var$setInfo
    }
  }
  if(is.null(setInfo)){
    print('Setting not possible')
    return(FALSE)
  }

  setInfo <- makeSetExpression(setInfo)
  setEnvironment <- setInfo$environment
  setExpression <- setInfo$expression

  if(is.null(setEnvironment) || is.null(setExpression)){
    print('Error while setting')
    return(FALSE)
  }

  valueR <- eval(parse(text=value), envir = setEnvironment)
  
  cl <- call('<-', setExpression, valueR)
  eval(cl, setEnvironment)

  print(setExpression)
  print(cl)
  print(setEnvironment)
  print(valueR)

  invisible(TRUE)
}


makeSetExpression <- function(setInfo){
  print(setInfo)
  if(is.null(setInfo)){
    return(NULL)
  } else if(!is.null(setInfo$expression)){
    return(setInfo)
  } else{
    parentInfo <- makeSetExpression(setInfo$parent)
    
    if(is.null(setInfo$environment)) setInfo$environment <- parentInfo$environment
    parentExpression <- parentInfo$expression

    emptyIndex1 <- setInfo$emptyIndex1
    if(is.null(emptyIndex1)) emptyIndex1 <- FALSE

    hasIndex2 <- !is.null(setInfo$emptyIndex2) || !is.null(setInfo$index2)

    emptyIndex2 <- setInfo$emptyIndex2
    if(is.null(emptyIndex2)) emptyIndex2 <- FALSE

    if(!hasIndex2){
      setInfo$expression <- call(
        setInfo$func,
        parentExpression,
        setInfo$index,
      )
    } else if(emptyIndex1 && emptyIndex2){
      setInfo$expression <- call(
        setInfo$func,
        parentExpression,,
      )
    } else if(emptyIndex1){
      setInfo$expression <- call(
        setInfo$func,
        parentExpression,,
        setInfo$index2
      )
    } else if(emptyIndex2){
      setInfo$expression <- call(
        setInfo$func,
        parentExpression,
        setInfo$index,
      )
    } else{
      setInfo$expression <- call(
        setInfo$func,
        parentExpression,
        setInfo$index,
        setInfo$index2
      )
    }

    return(setInfo)
  }
}

