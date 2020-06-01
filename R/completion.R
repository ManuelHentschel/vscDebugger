getSymbolsFromAttachedPackages <- function(text) {
  pkgs <- search()
  pkgs <- pkgs[startsWith(pkgs, "package:")]
  pkgs <- gsub("package:", "", pkgs, fixed = TRUE)
  symbols <- lapply(pkgs, function(pkg) {
    ns <- getNamespace(pkg)
    exported <- getNamespaceExports(ns)
    exported[startsWith(exported, text)]
  })
  unlist(symbols, use.names = FALSE)
}

#' @export
.vsc.getCompletion <- function(frameIdVsc, text, column=0, line=1, id=0, onlyGlobalEnv=FALSE){
  if(column>1){
    text <- substring(text, 1, column-1)
  }
  if(onlyGlobalEnv){
    firstenv <- globalenv()
  } else{
    frameId <- convertFrameId(vsc = frameIdVsc)
    firstenv <- sys.frame(frameId)
  }
  lastenv <- globalenv()
  envs <- getScopeEnvs(firstenv=firstenv, lastenv=lastenv)

  pattern0 <- "(\\$|\\[\\[|\\[)$"
  ind <- regexpr(pattern0, text)
  if(ind!=-1){
    text1 <- substring(text = text, first = 1, last = ind-1)
    text2 <- substring(text = text, first = ind)
  } else{
    text1 <- text
    text2 <- ""
  }

  var <- getLastVar(text1)

  if(var=="" && text2!=""){
    # only "$", "[", or "[[" --> no matches
    matches <- list()
  } else if(text2==""){
    # find all matching variable names
    pattern = paste0("^", var)
    matches <- c(
      lapply(envs, ls, all.names = TRUE, pattern = pattern, sorted = FALSE),
      getSymbolsFromAttachedPackages(var)
    )
    matches <- unlist(matches)
  } else{
    # find all children of the last variable
    matches <- getNameList(var, envs)
    if(text2!="$"){
      matches <- lapply(matches, function(s) paste0('"', s, '"'))
    }
  }

  targets <- lapply(matches, function(s) list(label=s))

  .vsc.sendToVsc('completion', targets, id)
}

#' @export
getLastVar <- function(text){
  pattern1 <- "((?:[a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z\\._0-9]*|\\.)$" # matches the beggining of the last valid variable name
  ind <- regexpr(pattern1, text)
  if(ind == -1){
    return("")
  } else{
    return(substring(text, ind))
  }
}


#' @export
getNameList <- function(var, envs){
  names <- list()
  for(env in envs){
    if(var %in% ls(env, sorted=FALSE)){
      if(isPromise(var, env)){
        names <- getNameListForPromise(var,env)
      } else{
        names <- names(get(var, envir=env))
      }
      names <- as.list(names)
      break
    }
  }
  return(names)
}


#' @export
getNameListForPromise <- function(var, env){
  promise <- getPromiseVar(var, env)
  val <- eval(promise$promiseExpr, promise$promiseEnv)
  names <- names(val)
  return(names)
}
