


### Construction

makeNewTree <- function(
    childrenFunction=NULL,
    contentFunction=NULL,
    defaultContentProducesChildren=FALSE,
    defaultPreserve=FALSE,
    defaultLazy=TRUE
){
    session$treeArgs <- list(
        defaultContentProducesChildren=defaultContentProducesChildren,
        defaultPreserve=defaultPreserve,
        defaultLazy=defaultLazy
    )
    session$tree <- list(getEmptyNode(nodeId=1,parentId=0))
    session$childrenFunction <- childrenFunction
    session$contentFunction <- contentFunction
    return(1)
}

getNewNodeId <- function(parentId){
    id <- findFirstNull()
    session$tree[[id]] <- getEmptyNode(id, parentId)
    session$tree[[parentId]]$childrenIds <- c(session$tree[[parentId]]$childrenIds, id)
    return(id)
}

getEmptyNode <- function(nodeId=0, parentId=0){
    return(list(
        nodeId = nodeId,
        parentId = parentId,

        childrenIds = integer(0),
        childrenReady = TRUE,
        childrenArgs = NULL,

        contentContent = NULL,
        contentReady = TRUE,
        contentProducesChildren = FALSE,
        contentArgs = NULL
    ))
}


### Node assignment

storeToNode <- function(args, id){
    preserve <- lget(args, 'preserve', session$treeArgs$defaultPreserve)

    contentContent <- args$contentContent
    contentArgs <- args$contentArgs

    # content:
    if(is.null(contentContent) && is.null(contentArgs) && preserve){
        # do nothing
    } else if(is.null(contentContent) && is.null(contentArgs)){ # delete content
        session$tree[[id]]$contentReady <- TRUE
        session$tree[[id]]$contentContent <- NULL
        session$tree[[id]]$contentArgs <- NULL
        session$tree[[id]]$contentProducesChildren <- FALSE
    } else if(is.null(contentContent)){ # use contentArgs
        session$tree[[id]]$contentReady <- FALSE
        session$tree[[id]]$contentContent <- NULL
        session$tree[[id]]$contentArgs <- args$contentArgs

        # overwritten if children or childrenArgs provided:
        contentProducesChildren <- lget(contentArgs, 'contentProducesChildren', session$treeArgs$defaultContentProducesChildren)
        session$tree[[id]]$contentProducesChildren <- contentProducesChildren
        if(contentProducesChildren && !preserve){
            session$tree[[id]]$childrenReady <- FALSE
        }

        lazy <- lget(args$contentArgs, 'lazy', session$treeArgs$defaultLazy)
        if(!lazy){
            forceContent(id)
        }
    } else{ # store content
        session$tree[[id]]$contentReady <- TRUE
        session$tree[[id]]$contentContent <- contentContent
        session$tree[[id]]$contentArgs <- NULL
        session$tree[[id]]$contentProducesChildren <- FALSE # can only be TRUE for contentArgs
    }

    childrenIds <- args$childrenIds
    childrenArgs <- args$childrenArgs

    if(is.null(childrenIds) && is.null(childrenArgs) && preserve){
        # do nothing
    } else if(is.null(childrenIds) && is.null(childrenArgs)){ # delete children
        session$tree[[id]]$childrenArgs <- NULL
        session$tree[[id]]$childrenIds <- integer(0)
        session$tree[[id]]$childrenReady <- !session$tree[[id]]$contentProducesChildren
    } else if(is.null(childrenIds)){ # use childrenArgs
        session$tree[[id]]$childrenArgs <- args$childrenArgs
        session$tree[[id]]$childrenReady <- FALSE
        session$tree[[id]]$childrenIds <- integer(0)
        session$tree[[id]]$contentProducesChildren <- FALSE

        lazy <- lget(args$childrenArgs, 'lazy', session$treeArgs$defaultPreserve)
        if(!lazy){
            forceChildren(id)
        }
    } else{
        treeArgs <- args$childrenIds
        ids <- sapply(treeArgs, storeToNewNode, id)
        session$tree[[id]]$childrenIds <- ids
        session$tree[[id]]$childrenReady <- TRUE
        session$tree[[id]]$contentProducesChildren <- FALSE
    }
}

storeToNewNode <- function(args, parentId){
    id <- getNewNodeId(parentId)
    storeToNode(args, id)
    return(id)
}


### Tree Manipulation (non-lazy stuff)

orphanNode <- function(id){
    parentId <- session$tree[[id]]$parentId
    childrenIds <- session$tree[[parentId]]$childrenIds
    ind <- which(childrenIds == id)
    session$tree[[parentId]]$childrenIds <- childrenIds[-ind]
    session$tree[[id]]$parentId <- 0
    invisible(parentId)
}

deleteNode <- function(id){
    parentId <- orphanNode(id)
    internalDeleteNode(id)
    trimTree()
    invisible(parentId)
}
internalDeleteNode <- function(id){
    lapply(session$tree[[id]]$childrenIds, internalDeleteNode)
    session$tree[id] <- list(NULL)
}

trimTree <- function(){
    for(ind in rev(seq_along(session$tree))){
        if(is.null(session$tree[[ind]])){
            session$tree[[ind]] <- NULL
        } else{
            break
        }
    }
}

moveNode <- function(id0, id1){
    # returns new id (id1) if successful, else old id (id0)
    if(is.null(session$tree[[id1]])){
        session$tree[[id1]] <- session$tree[[id0]]
        for(child in session$tree[[id0]]$childrenIds){
            session$tree[[child]]$parentId <- id1
        }
        parentId <- session$tree[[id1]]$parentId
        siblings <- session$tree[[parentId]]$childrenIds
        siblings[siblings == id0] <- id1
        session$tree[[parentId]]$childrenIds <- siblings
        session$tree[id0] <- list(NULL)
        id1
    } else{
        id0
    }
}

shrinkTree <- function(){
    # invalidates externally stored node Ids (of moved nodes)!
    repeat{
        ind0 <- findFirstNull()
        ind1 <- findLastEntry()
        if(ind0 < ind1){
            moveNode(ind1, ind0)
        } else{
            break
        }
    }
    trimTree()
}

findFirstNull <- function(){
    for(ind in seq_along(session$tree)){
        if(is.null(session$tree[[ind]])){
            return(ind)
        }
    }
    return(ind+1)
}

findLastEntry <- function(){
    for(ind in rev(seq_along(session$tree))){
        if(!is.null(session$tree[[ind]])){
            return(ind)
        }
    }
    return(0)
}


### Tree manipulation (lazy stuff)

forceChildren <- function(id){
    if(lget(session$tree[[id]], 'childrenReady', TRUE)){
        # do nothing
    } else if(session$tree[[id]]$contentProducesChildren){
        forceContent(id)
        forceChildren(id)
    } else {
        childrenArgs <- session$tree[[id]]$childrenArgs
        treeArgs <- session$childrenFunction(childrenArgs)
        ids <- sapply(treeArgs, storeToNewNode, id)
        session$tree[[id]]$childrenIds <- ids
        session$tree[[id]]$childrenReady <- TRUE
        session$tree[[id]]$contentProducesChildren <- FALSE
    }
    invisible(session$tree[[id]]$childrenIds)
}

forceContent <- function(id){
    if(!lget(session$tree[[id]], 'contentReady', TRUE)){
        args <- session$tree[[id]]$contentArgs
        nodeArgs <- session$contentFunction(args)
        nodeArgs$preserve <- TRUE
        storeToNode(nodeArgs, id)
    }
    invisible(session$tree[[id]]$content)
}

forceTree <- function(id, depth=9, forceContent=TRUE, deleteArgs=FALSE){
    if(forceContent){
        forceContent(id)
    }
    if(depth>0){
        ids <- forceChildren(id)
        lapply(ids, forceTree, depth-1, forceContent, deleteArgs)
    }
    if(deleteArgs){
        session$tree[[id]]$contentReady <- NULL
        session$tree[[id]]$contentProducesChildren <- NULL
        session$tree[[id]]$contentArgs <- NULL
        session$tree[[id]]$childrenReady <- NULL
        session$tree[[id]]$childrenArgs <- NULL
    }
    invisible(id)
}


### Info retrieval
# these work 'lazy-agnostic', i.e. all lazy nodes are forced before returning

getContent <- function(id){
    forceContent(id)
    lget(session$tree[[id]], 'contentContent', NULL)
}

getChildrenIds <- function(id){
    forceChildren(id)
    lget(session$tree[[id]], 'childrenIds', integer(0))
}

getContents <- function(ids){
    contents <- lapply(ids, getContent)
}

getChildrenContents <- function(id){
    ids <- getChildrenIds(id)
    getContents(session$tree[[id]]$childrenIds)
}

treeToList <- function(id, depth=9){
    forceContent(id)
    if(depth<=0){
        list(
            contentContent = session$tree[[id]]$contentContent,
            children = list()
            # childrenArgs = session$tree[[id]]$childrenArgs # no! must be compatible with non-lazy tree
        )
    } else{
        forceChildren(id)
        list(
            contentContent = session$tree[[id]]$contentContent,
            children = lapply(session$tree[[id]]$childrenIds, treeToList, depth-1)
        )
    }
}




### Helpers:

lgetSafe <- function(list, entry, default=NULL){
    suppressWarnings(
        tryCatch(
            lget(list, entry, default),
            error = function(e) default
        )
    )
}

lget <- function(list, entry, default=NULL){
    if(!is.list(list)){
        return(default)
    }
    ret <- list[[entry]]
    if(is.null(ret)){
        return(default)
    }
    ret
}
