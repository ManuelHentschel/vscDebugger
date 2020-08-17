
library(R6)

options(vsc.showAttributes = FALSE)
options(vsc.showCustomAttributes = FALSE)


Node <- R6Class(
    classname = "Node",
    public = list(
        nodeType = "Node",
        getParent = function() private$parent,
        getAncestors = function(includeStartingNode = FALSE) {
            if(is.null(private$parent)){
                ancestors <- list()
            } else{
                ancestors <- private$parent$getAncestors(TRUE)
            }
            if(includeStartingNode){
                ancestors <- c(list(self), ancestors)
            }
            return(ancestors)
        },
        getChildren = function(args=list()) private$children,
        getContent = function(args=list()) private$content,
        initialize = function(args=list(), parent=NULL) {
            if(!is.null(parent)){
                private$parent <- parent
            }
        },
        findChildNode = function(args=list()) {
            findBy <- lget(args, 'findBy', '')
            if(findBy == 'varRef'){
                varRef <- lget(args, 'varRef', -1)
                if(varRef < 0){
                    return(NULL)
                }
                if(lget(self, 'variablesReference', 0) == varRef){
                    return(self)
                }
                if(varRef %in% private$childrenVarRefs){
                    for(child in private$children){
                        node <- child$findChildNode(args)
                        if(!is.null(node)){
                            return(node)
                        }
                    }
                }
            } else if(findBy == 'nameAndVarRef'){
                varRef <- lget(args, 'variablesReference', -1)
                name <- lget(args, 'name', NULL)
                if(varRef < 0 || is.null(name)){
                    return(NULL)
                }
                args2 <- list(
                    findBy = 'varRef',
                    varRef = varRef
                )
                node2 <- self$findChildNode(args2)
                if(is.null(node2)){
                    return(NULL)
                }
                childNodes <- node2$getChildren(list(lazy=TRUE))
                names <- lapply(childNodes, function(child) lget(child, 'name', ''))
                names <- unlist(names)
                ind <- which(names == name)[1]
                if(is.na(ind)){
                    return(NULL)
                } else{
                    return(childNodes[[ind]])
                }
            }
            return(NULL)
        },
        getNewVarRef = function(){
            if(is.null(private$parent)){
                private$newVarRef <- private$newVarRef + 1
                newVarRef <- private$newVarRef
            } else{
                newVarRef <- private$parent$getNewVarRef()
            }
            private$childrenVarRefs <- c(private$childrenVarRefs, newVarRef)
            return(newVarRef)
        }
    ),
    private = list(
        parent = NULL,
        children = NULL,
        content = NULL,
        newVarRef = 1,
        childrenVarRefs = c()
    )
)

MetaNode <- R6Class(
    classname = "MetaNode",
    public = list(
        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)
        }
    ),
    private = list(),
    inherit = Node
)

RootNode <- R6Class(
    classname = "RootNode",
    public = list(
        getStackNode = function(args=list()) {
            if(lget(args, 'refresh', FALSE)){
                private$children$stackNode <- StackNode$new(args, self)
            }
            return(private$children$stackNode)
        },
        getEvalRootNode = function(args=list()) {
            if(lget(args, 'refresh', FALSE) || is.null(private$children$evalRootNode)){
                private$children$evalRootNode <- EvalRootNode$new(args, self)
            }
            return(private$children$evalRootNode)
        },
        getChildren = function(args=list()){
            return(private$children)
        },

        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)
            private$children <- list()
        }
    ),
    private = list(),
    inherit = MetaNode
)

EvalRootNode <- R6Class(
    classname = "EvalRootNode",
    public = list(
        addChild = function(args=list()){
            node <- VariableNode$new(args, self)
            private$children <- c(private$children, list(node))
            return(node)
        },

        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)
        }
    ),
    private = list(),
    inherit = MetaNode
)

StackNode <- R6Class(
    classname = "StackNode",
    public = list(
        # from DebugProtocol.StackTraceResponse
        totalFrames = NULL,

        # internal
        topFrameId = NULL,
        skipFromTop = NULL,
        skipFromBottom = NULL,
        forceDummyStack = NULL,
        dummyFile = NULL,
        frameIdsR = NULL,
        frameIdsVsc = NULL,


        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)

            self$topFrameId <- lget(args, 'topFrameId', 0)
            self$skipFromTop <- lget(args, 'skipFromTop', 0)
            self$skipFromBottom <- lget(args, 'skipFromBottom', 0)
            self$forceDummyStack <- lget(args, 'forceDummyStack', FALSE)
            self$dummyFile <- lget(args, 'dummyFile', '')

            # do stuff
            if(self$topFrameId <= 0 || self$forceDummyStack){
                self$forceDummyStack <- TRUE
                self$frameIdsR <- c(0)
                self$frameIdsVsc <- c(0)
            } else{
                self$frameIdsR <- seq2((self$topFrameId - self$skipFromTop), (self$skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
                self$frameIdsVsc <- seq_along(self$frameIdsR) - 1
            }

            private$children <- mapply(
                function(frameIdR, frameIdVsc){
                    FrameNode$new(parent=self, args=list(
                        frameIdR = frameIdR,
                        frameIdVsc = frameIdVsc,
                        dummyFile = self$dummyFile,
                        isDummyFrame = self$forceDummyStack
                    ))
                },
                self$frameIdsR,
                self$frameIdsVsc,
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )

            self$totalFrames <- length(private$children)
        },

        getChildren = function(args=list()){
            frameId <- lget(args, 'frameIdR', -1)
            if(frameId>=0){
                frameIds <- self$frameIdsR
            } else{
                frameId <- lget(args, 'frameId', -1)
                frameIds <- self$frameIdsVsc
            }
            ind <- which(frameIds == frameId)[1]
            if(!is.na(ind)){
                return(private$children[[ind]])
            } else{
                return(private$children)
            }
        },

        getContent = function(args=list()) list(
            stackFrames = lapply(private$children, function(child) child$getContent()),
            totalFrames = self$totalFrames
        )

    ),
    private = list(),
    inherit = Node
)

FrameNode <- R6Class(
    classname = "FrameNode",
    public = list(
        # from DebugProtocol.StackFrame
        id = NULL,
        name = NULL,
        source = NULL,
        line = NULL,
        column = NULL,
        endLine = NULL,
        endColumn = NULL,
        instructionPointerReference = NULL,
        moduleId = NULL,
        presentationHint = NULL,

        # internal
        frameIdR = NULL,
        frameIdVsc = NULL,
        dummyFile = NULL,
        isDummyFrame = NULL,
        parent = NULL,
        firstenv = NULL,
        lastenv = NULL,
        call = NULL,


        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)

            # read args
            self$frameIdR <- lget(args, "frameIdR", list(0))
            self$frameIdVsc <- lget(args, "frameIdVsc", list(0))
            self$dummyFile <- lget(args, "dummyFile", "")
            self$isDummyFrame <- lget(args, "isDummyFrame", FALSE)
            
            # do stuff
            if (lget(session, "includePackageScopes", FALSE)) {
                self$lastenv <- emptyenv()
            } else {
                self$lastenv <- globalenv()
            }
            
            if (self$isDummyFrame) {
                self$firstenv <- globalenv()
                self$name <- "Global Workspace"
                self$presentationHint <- "label"
                source <- NULL
                self$line <- 0
                self$column <- 0
                self$endLine <- NULL
                self$endColumn <- NULL
            } else {
                self$call <- sys.call(self$frameIdR)
                self$firstenv <- sys.frame(self$frameIdR)
                self$name <- getFrameName(call)
                self$presentationHint <- "normal"
                source <- getSource(sys.call(self$frameIdR + 1), self$frameIdR + 1)
                self$line <- source$line
                self$column <- source$column
                self$endLine <- source$endLine
                self$endColumn <- source$endColumn + 1
            }
            self$id <- self$frameIdVsc
            
            if (lget(source, "isFile", FALSE)) {
                self$source <- source
            } else {
                self$source <- NULL
            }
        },
        getContent = function(args=list()) {
            list(
                id = self$id,
                name = self$name,
                source = self$source,
                line = self$line,
                column = self$column,
                endLine = self$endLine,
                endColumn = self$endColumn,
                presentationHint = self$presentationHint
            )
        },
        getChildren = function(args=list()) {
            if(is.null(private$children) || lget(args, 'refresh', FALSE)){
                scopeEnvs <- getScopeEnvs(self$firstenv, self$lastenv)
                nodes <- lapply(scopeEnvs, function(env) {
                    ScopeNode$new(
                        parent=self,
                        args=list(
                            name = format(env),
                            rValue = env
                        )
                    )
                })
                private$children <- nodes
            }
            return(private$children)
        }
    ),
    private = list(),
    inherit = Node
)

VariableNode <- R6Class(
    classname = "VariableNode",
    public = list(
        # from DebugProtocol.Variable
        name = '',
        value = NULL,
        type = NULL,
        presentationHint = NULL,
        evaluateName = NULL,
        variablesReference = NULL,
        namedVariables = NULL,
        indexedVariables = NULL,
        memoryReference = NULL, # not implemented

        # internal
        rValue = NULL,
        setter = NULL,
        setInfo = NULL,
        internalAttributes = NULL,
        customAttributes = NULL,
        attrVars = NULL,
        childVars = NULL,
        childIndex = NULL, # as used by parent
        childFilter = NULL, # 'indexed'|'named' as used by parent

        updateValue = function(newValue) {
            args <- list(
                name = self$name,
                rValue = newValue,
                setter = self$setter,
                setInfo = self$setInfo
            )
            self$initialize(args, parent=self$parent)
        },
        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)
            
            # read args
            self$name <- lget(args, "name", "")
            self$rValue <- lget(args, "rValue", NULL)
            self$setter <- lget(args, "setter", NULL)
            self$setInfo <- lget(args, "setInfo", NULL)
            self$childIndex <- lget(args, "childIndex", NULL)

            self$childVars <- NULL
            self$attrVars <- NULL
            private$children <- NULL

            # do stuff
            infos <- c(
                "toString",
                "type",
                "evaluateName",
                "nChildVars"
            )
            
            if (getOption("vsc.showAttributes", TRUE)) {
                infos <- c(infos, "internalAttributes")
            }
            
            stackingInfos <- c()
            if (getOption("vsc.showCustomAttributes", TRUE)) {
                stackingInfos <- c("customAttributes")
            }
            
            # get VarInfos
            infos <- .vsc.applyVarInfos(
                self$rValue,
                infos = infos,
                stackingInfos = stackingInfos
            )
            
            # handle attributes
            self$internalAttributes <- lget(infos, "internalAttributes", list())
            customAttributes <- lget(infos, "customAttributes", list(list()))
            self$customAttributes <- unlist(customAttributes, recursive = FALSE)
            self$attrVars <- c(self$internalAttributes, self$customAttributes)
            self$namedVariables <- length(self$attrVars)
            
            # handle other
            self$indexedVariables <- lget(infos, "nChildVars", NULL)
            self$value <- infos$toString
            self$type <- infos$type
            self$evaluateName <- infos$evaluateName
            
            if (self$indexedVariables + self$namedVariables > 0) {
                self$variablesReference <- self$getNewVarRef()
            } else {
                self$variablesReference <- 0
            }
        },
        getContent = function(args=list()){
            content <- list(
                name = self$name,
                value = self$value,
                type = self$type,
                evaluateName = self$evaluateName,
                variablesReference = self$variablesReference,
                namedVariables = self$namedVariables,
                indexedVariables = self$indexedVariables
            )
            if(lget(args, 'includeSetInfo', FALSE)){
                content$setter <- self$setter
                content$setInfo <- self$setInfo
            }
            return(content)
        },
        getChildren = function(args=list()){
            lazy <- lget(args, 'lazy', FALSE)
            filter <- lget(args, 'filter', '')
            start <- lget(args, 'start', 0)
            count <- lget(args, 'count', 0)

            if(lazy){
                return(private$children)
            }

            if(is.null(self$childVars)){
                infos <- .vsc.applyVarInfos(
                    self$rValue,
                    infos = c('childVars')
                )
                self$childVars <- lget(infos, 'childVars', list())

                allVars <- c(self$attrVars, self$childVars)
                allVars <- fixNames(allVars)
                nAttr <- length(self$attrVars)
                if(nAttr>0){
                    self$attrVars <- allVars[1:nAttr]
                }
                if(nAttr<length(allVars)){
                    self$childVars <- allVars[(nAttr+1):length(allVars)]
                }
            }

            if(filter == 'named'){
                children <- self$attrVars
            } else if(filter == 'indexed'){
                children <- self$childVars
                if(count > 0){
                    children <- children[(start+1):(start+count)]
                }
            } else{
                children <- c(self$attrVars, self$childVars)
            }

            newChildNodes <- lapply(children, function(child){
                VariableNode$new(parent=self, args=child)
            })
            private$children <- c(private$children, newChildNodes)
            return(newChildNodes)
        }
    ),
    private = list(),
    inherit = Node
)

ScopeNode <- R6Class(
    classname = "ScopeNode",
    public = list(
        # from DebugProtocol.Scope
        expensive = FALSE,
        source = NULL,
        line = NULL,
        column = NULL,
        endLine = NULL,
        endColumn = NULL,

        initialize = function(args=list(), parent=NULL){
            super$initialize(args, parent)
        }
    ),
    private = list(),
    inherit = VariableNode
)









