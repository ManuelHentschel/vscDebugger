

storeFrameId <- function(node, R, vsc){
    session$frameIds <- list(
        node = append(session$frameIds$node, node),
        R = append(session$frameIds$R, R),
        vsc = append(session$frameIds$vsc, vsc)
    )
}

storeFrameIds <- function(frames, frameNodes, preserve=FALSE){
  if(!preserve){
    session$frameIds <- list(
      node = list(),
      R = list(),
      vsc = list()
    )
  }
  mapply(
    function(frameNode, frame){
      storeFrameId(
          node = frameNode,
          R = frame[['frameIdR']],
          vsc = frame[['id']]
      )
    },
    frameNodes,
    frames
  )

}

#' Converts the frame id
#'
#' Converts the frame id form R to vsc or viceversa
#'
#' @param vsc The frame id as used by vsc
#' @param R The frame id as used by R
#' @return The frame id as used by the other program
convertFrameId <- function(vsc = NULL, R = NULL) {
  frameIdsR <- session$frameIds$R
  frameIdsVsc <- session$frameIds$vsc

  if (is.null(vsc) && is.null(R)) {
    return(NULL)
  } else if (is.null(vsc)) {
    ind <- which(R == frameIdsR)
    if (length(ind) > 0) {
      return(frameIdsVsc[[ind[1]]])
    } else {
      return(NULL)
    }
  } else {
    ind <- which(vsc == frameIdsVsc)
    if (length(ind) > 0) {
      return(frameIdsR[[ind[1]]])
    } else {
      return(NULL)
    }
  }
}


storeVarRef <- function(varRef, node){
  session$varRefs <- list(
      varRef = append(session$varRefs$varRef, list(varRef)),
      node = append(session$varRefs$node, list(node))
  )
}

storeVarRefs <- function(variables, variableNodes){
  mapply(
    function(node, variable){
      storeVarRef(
        node = node,
        varRef = variable$variablesReference
      )
    },
    variableNodes,
    variables
  )
}


getNodeId <- function(R = NULL, vsc = NULL, varRef=NULL){
  if(!is.null(varRef)){
    ind <- which(varRef == session$varRefs$varRef)
    if(length(ind)>0){
      return(session$varRefs$node[[ind[1]]])
    } else{
      return(0)
    }
  } else if (is.null(vsc) && is.null(R)) {
    return(0)
  } else if (is.null(vsc)) {
    ind <- which(R == session$frameIds$R)
    if (length(ind) > 0) {
      return(session$frameIds$node[[ind[1]]])
    } else {
      return(0)
    }
  } else {
    ind <- which(vsc == session$frameIds$vsc)
    if (length(ind) > 0) {
      return(session$frameIds$node[[ind[1]]])
    } else {
      return(0)
    }
  }
}


getNodeIdByVarRefAndName <- function(varRef, name){
  parentId <- getNodeId(varRef = varRef)
  childrenIds <- session$tree$getChildrenIds(parentId)
  children <- session$tree$getContents(childrenIds)
  matches <- sapply(children, function(child) identical(child$name, name))
  if(length(matches>0)){
    ind <- which(matches)[1]
    nodeId <- childrenIds[ind]
  } else{
    nodeId <- 0
  }
  return(nodeId)
}


