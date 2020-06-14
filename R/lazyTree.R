


LazyTree <- function(
    childrenFunction=function(args) list(),
    contentFunction=function(args) NULL,
    defaultContentProducesChildren=FALSE,
    defaultPreserve=FALSE,
    defaultLazy=TRUE,
    makeRootNode=FALSE
){
    tree <- local({

        # define 'this' to refer to the current tree (javascript/typescript convention)
        # can be ommitted when reading from variables, but is necessary when writing, e.g.: `this$nodes <- append(nodes, list(newNode))`
        this <- environment()

        # properties
        defaultArgs <- list(
            defaultContentProducesChildren=defaultContentProducesChildren,
            defaultPreserve=defaultPreserve,
            defaultLazy=defaultLazy
        )

        childrenFunction <- childrenFunction
        contentFunction <- contentFunction

        nodes <- list()

        # methods

        ### construction
        getNewNodeId <- function(parentId=0L){
            id <- findFirstNull()
            this$nodes[[id]] <- getEmptyNode(id, parentId)
            if(parentId>0){
                this$nodes[[parentId]]$childrenIds <- c(this$nodes[[parentId]]$childrenIds, id)
            }
            return(id)
        }

        getEmptyNode <- function(nodeId=0L, parentId=0L){
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
            preserve <- lget(args, 'preserve', defaultArgs$defaultPreserve)

            contentContent <- args$contentContent
            contentArgs <- args$contentArgs

            # content:
            if(is.null(contentContent) && is.null(contentArgs) && preserve){
                # do nothing
            } else if(is.null(contentContent) && is.null(contentArgs)){ # delete content
                this$nodes[[id]]$contentReady <- TRUE
                this$nodes[[id]]$contentContent <- NULL
                this$nodes[[id]]$contentArgs <- NULL
                this$nodes[[id]]$contentProducesChildren <- FALSE
            } else if(is.null(contentContent)){ # use contentArgs
                this$nodes[[id]]$contentReady <- FALSE
                this$nodes[[id]]$contentContent <- NULL
                this$nodes[[id]]$contentArgs <- args$contentArgs

                # overwritten if children or childrenArgs provided:
                contentProducesChildren <- lget(contentArgs, 'contentProducesChildren', defaultArgs$defaultContentProducesChildren)
                this$nodes[[id]]$contentProducesChildren <- contentProducesChildren
                if(contentProducesChildren && !preserve){
                    this$nodes[[id]]$childrenReady <- FALSE
                }

                lazy <- lget(args$contentArgs, 'lazy', defaultArgs$defaultLazy)
                if(!lazy){
                    forceContent(id)
                }
            } else{ # store content
                this$nodes[[id]]$contentReady <- TRUE
                this$nodes[[id]]$contentContent <- contentContent
                this$nodes[[id]]$contentArgs <- NULL
                this$nodes[[id]]$contentProducesChildren <- FALSE # can only be TRUE for contentArgs
            }

            childrenIds <- args$childrenIds
            childrenArgs <- args$childrenArgs

            if(is.null(childrenIds) && is.null(childrenArgs) && preserve){
                # do nothing
            } else if(is.null(childrenIds) && is.null(childrenArgs)){ # delete children
                this$nodes[[id]]$childrenArgs <- NULL
                this$nodes[[id]]$childrenIds <- integer(0)
                this$nodes[[id]]$childrenReady <- !this$nodes[[id]]$contentProducesChildren
            } else if(is.null(childrenIds)){ # use childrenArgs
                this$nodes[[id]]$childrenArgs <- args$childrenArgs
                this$nodes[[id]]$childrenReady <- FALSE
                this$nodes[[id]]$childrenIds <- integer(0)
                this$nodes[[id]]$contentProducesChildren <- FALSE

                lazy <- lget(args$childrenArgs, 'lazy', defaultArgs$defaultPreserve)
                if(!lazy){
                    forceChildren(id)
                }
            } else{
                defaultArgs <- args$childrenIds
                ids <- sapply(defaultArgs, storeToNewNode, id)
                this$nodes[[id]]$childrenIds <- ids
                this$nodes[[id]]$childrenReady <- TRUE
                this$nodes[[id]]$contentProducesChildren <- FALSE
            }
        }

        storeToNewNode <- function(args, parentId=0L){
            id <- getNewNodeId(parentId)
            storeToNode(args, id)
            return(id)
        }


        ### Tree Manipulation (non-lazy stuff)

        orphanNode <- function(id){
            parentId <- this$nodes[[id]]$parentId
            childrenIds <- this$nodes[[parentId]]$childrenIds
            ind <- which(childrenIds == id)
            this$nodes[[parentId]]$childrenIds <- childrenIds[-ind]
            this$nodes[[id]]$parentId <- 0L
            invisible(parentId)
        }

        deleteNode <- function(id){
            parentId <- orphanNode(id)
            deletedNodes <- internalDeleteNode(id)
            invisible(deletedNodes)
        }
        internalDeleteNode <- function(id){
            deletedNodes <- sapply(this$nodes[[id]]$childrenIds, internalDeleteNode)
            this$nodes[id] <- list(NULL)
            c(id, deletedNodes)
        }

        trimTree <- function(){
            for(ind in rev(seq_along(this$nodes))){
                if(is.null(this$nodes[[ind]])){
                    this$nodes[[ind]] <- NULL
                } else{
                    break
                }
            }
        }

        moveNode <- function(id0, id1){
            # returns new id (id1) if successful, else old id (id0)
            if(is.null(this$nodes[[id1]])){
                this$nodes[[id1]] <- this$nodes[[id0]]
                for(child in this$nodes[[id0]]$childrenIds){
                    this$nodes[[child]]$parentId <- id1
                }
                parentId <- this$nodes[[id1]]$parentId
                siblings <- this$nodes[[parentId]]$childrenIds
                siblings[siblings == id0] <- id1
                this$nodes[[parentId]]$childrenIds <- siblings
                this$nodes[id0] <- list(NULL)
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
            for(ind in seq_along(this$nodes)){
                if(is.null(this$nodes[[ind]])){
                    return(ind)
                }
            }
            return(length(this$nodes)+1)
        }

        findLastEntry <- function(){
            for(ind in rev(seq_along(this$nodes))){
                if(!is.null(this$nodes[[ind]])){
                    return(ind)
                }
            }
            return(0)
        }


        ### Tree manipulation (lazy stuff)

        forceChildren <- function(id){
            if(lget(this$nodes[[id]], 'childrenReady', TRUE)){
                # do nothing
            } else if(this$nodes[[id]]$contentProducesChildren){
                forceContent(id)
                forceChildren(id)
            } else {
                childrenArgs <- this$nodes[[id]]$childrenArgs
                defaultArgs <- childrenFunction(childrenArgs)
                ids <- sapply(defaultArgs, storeToNewNode, id)
                this$nodes[[id]]$childrenIds <- ids
                this$nodes[[id]]$childrenReady <- TRUE
                this$nodes[[id]]$contentProducesChildren <- FALSE
            }
            invisible(this$nodes[[id]]$childrenIds)
        }

        forceContent <- function(id){
            if(!lget(this$nodes[[id]], 'contentReady', TRUE)){
                args <- this$nodes[[id]]$contentArgs
                nodeArgs <- contentFunction(args)
                nodeArgs$preserve <- TRUE
                if(!is.null(nodeArgs$contentArgs)){
                    nodeArgs$contentArgs$lazy <- FALSE
                }
                storeToNode(nodeArgs, id)
            }
            invisible(this$nodes[[id]]$content)
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
                this$nodes[[id]]$contentReady <- NULL
                this$nodes[[id]]$contentProducesChildren <- NULL
                this$nodes[[id]]$contentArgs <- NULL
                this$nodes[[id]]$childrenReady <- NULL
                this$nodes[[id]]$childrenArgs <- NULL
            }
            invisible(id)
        }


        ### Info retrieval
        # these work 'lazy-agnostic', i.e. all lazy nodes are forced before returning

        getContent <- function(id){
            if(id>0){
                forceContent(id)
                lget(this$nodes[[id]], 'contentContent', NULL)
            } else{
                NULL
            }
        }

        getChildrenIds <- function(id){
            if(id>0){
                forceChildren(id)
                lget(this$nodes[[id]], 'childrenIds', integer(0))
            } else{
                integer(0)
            }
        }

        getParentId <- function(id){
            if(id>0){
                lget(this$nodes[[id]], 'parentId', 0L)
            } else{
                0L
            }
        }

        getAncestorIds <- function(id){
            if(id>0){
                parentId <- getParentId(id)
                c(parentId, getAncestorIds(parentId))
            } else{
                integer(0)
            }
        }


        getContents <- function(ids){
            contents <- lapply(ids, getContent)
        }

        getChildrenContents <- function(id){
            ids <- getChildrenIds(id)
            getContents(this$nodes[[id]]$childrenIds)
        }

        treeToList <- function(id, depth=9){
            if(id>0){
                forceContent(id)
                if(depth<=0){
                    list(
                        contentContent = this$nodes[[id]]$contentContent,
                        children = list()
                        # childrenArgs = this$nodes[[id]]$childrenArgs # no! must be compatible with non-lazy tree
                    )
                } else{
                    forceChildren(id)
                    list(
                        contentContent = this$nodes[[id]]$contentContent,
                        children = lapply(this$nodes[[id]]$childrenIds, treeToList, depth-1)
                    )
                }
            } else{
                list()
            }
        }

        lget <- function(list, entry, default=NULL){
            ret <- list[[entry]]
            if(is.null(ret)){
                default
            } else{
                ret
            }
        }

        class(this) <- 'LazyTree'
        environment()
    })

    if(makeRootNode){
        tree$getNewNodeId()
    }

    tree
}

