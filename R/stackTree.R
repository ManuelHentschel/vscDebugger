

# This file defines the R6 classes used to internally represent the stack tree
# The general structure of the tree is as follows:

#     RootNode
#     |
#     +-StackNode
#     | |
#     | +FrameNode1
#     | | |
#     | | +-ScopeNode1
#     | | | |
#     | | | +-VariableNode1
#     | | | |
#     | | | +-VariableNode2
#     | | |   |
#     | | |   +-VariableNode2a
#     | | |   +-VariableNode2b
#     | | |   +-VariableNode2...
#     | | |
#     | | +-ScopeNode2
#     | | +-ScopeNode3
#     | | +-ScopeNode...
#     | |
#     | |
#     | +-FrameNode...
#     |   |
#     |   +ScopeNode...
#     |
#     +-EvalRootNode
#       |
#       +-VariableNode1
#       | |
#       | +-VariableNode1a
#       | +-VariableNode1b
#       | +-VariableNode1...
#       |
#       +-VariableNode...

# The RootNode is produced once per debug session and only has two children

# The StackNode is computed anew after every step/breakpoint
# The FrameNodes correspond to active frames
# By default, 'internal' frames of the debugger are omitted
# Since Scopes are basically the same as environments (-> possible variables),
# they are not implemented separately
# Attributes are represented by VariableNodes (just like list entries etc.)

# The EvalRootNode is used to store the variables produced as the result of
# eval requests. These are persistent for the entire debug session.

# In general, nodes are only computed if/when they are requested
# The tree can be nested infinitely deeply (e.g. environments containing a self reference)
# Nodes that are no longer needed are deleted by the garbage collector



# Base class for all nodes
Node <- R6::R6Class(
  classname = "Node",
  public = list(
    nodeType = "Node",
    # Get the parent of a node
    getParent = function() private$parent,
    # Find the ancestors of a node. Should always terminate with the RootNode
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
    # Get the childNodes of a node
    # Meant to be overloaded
    # Should return all children if no arguments are supplied
    # Should not cause extra computation if args$lazy==TRUE
    getChildren = function(args=list()) private$children,
    # Delete childNodes of a node
    # Should return a boolean that is TRUE iff there were children to be cleared
    clearChildren = function(){
      ret <- !is.null(private$children)
      private$children <- NULL
      return(ret)
    },
    # Get the content of a node
    # Meant to be overloaded
    getContent = function(args=list()) private$content,
    # Initialize a node
    initialize = function(args=list(), parent=NULL) {
      if(!is.null(parent)){
        private$parent <- parent
      }
    },
    # Recursively find a child node, by different properties
    findChildNode = function(args=list()) {
      findBy <- lget(args, 'findBy', '')
      if(findBy == 'varRef'){
        varRef <- lget(args, 'varRef', -1)
        if(varRef < 0){
          return(NULL)
        }
        if(lgetSafe(self, 'variablesReference', 0) == varRef){
          return(self)
        }
        if(varRef %in% private$childrenVarRefs){
          for(child in self$getChildren(list(lazy=TRUE))){
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
        names <- lapply(childNodes, function(child) lgetSafe(child, 'name', ''))
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
    # Get a new VarRef, unique within the tree
    # (Recursively) delegates to the parent node if present (-> RootNode)
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

# Nodes that are used only for 'organization'
MetaNode <- R6::R6Class(
  classname = "MetaNode",
  public = list(
    initialize = function(args=list(), parent=NULL){
      super$initialize(args, parent)
    }
  ),
  private = list(),
  inherit = Node
)

# Unique root node of a tree
RootNode <- R6::R6Class(
  classname = "RootNode",
  public = list(
    getStackNode = function(args=list()) {
      if(lget(args, 'refresh', FALSE) || is.null(private$children$stackNode)){
        logPrint('Refreshing stack node')
        private$children$stackNode <- StackNode$new(args, self)
      }
      return(private$children$stackNode)
    },
    clearStackNode = function(){
      ret <- !is.null(private$children$stackNode)
      private$children$stackNode <- NULL
      return(ret)
    },
    clearVariables = function(){
      if(!is.null(private$children$stackNode)){
        private$children$stackNode$clearVariables()
      } else{
        FALSE
      }
    },
    getEvalRootNode = function(args=list()) {
      if(lget(args, 'refresh', FALSE) || is.null(private$children$evalRootNode)){
        private$children$evalRootNode <- EvalRootNode$new(args, self)
      }
      return(private$children$evalRootNode)
    },
    clearEvalRootNode = function() {
      ret <- !is.null(private$children$evalRootNode)
      private$children$evalRootNode <- NULL
      return(ret)
    },

    initialize = function(args=list(), parent=NULL){
      super$initialize(args, parent)
      private$children <- list()
    }
  ),
  private = list(),
  inherit = MetaNode
)

# Used as root for variable nodes produced by eval requests
EvalRootNode <- R6::R6Class(
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

# Unique root of the stack
StackNode <- R6::R6Class(
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

      self$frameIdsR <- lget(args, 'frameIdsR', integer(0))
      self$topFrameId <- lget(args, 'topFrameId', 0)
      self$skipFromTop <- lget(args, 'skipFromTop', 0)
      self$skipFromBottom <- lget(args, 'skipFromBottom', 0)
      self$forceDummyStack <- lget(args, 'forceDummyStack', FALSE)
      self$dummyFile <- lget(args, 'dummyFile', '')

      # do stuff
      # if(self$topFrameId <= 0 || self$forceDummyStack){
      if(length(self$frameIdsR)==0){
        self$forceDummyStack <- TRUE
        self$frameIdsR <- c(0)
        self$frameIdsVsc <- c(0)
      } else{
        # self$frameIdsR <- seq2((self$topFrameId - self$skipFromTop), (self$skipFromBottom + 1), -1) # vsc considers frames in the opposite order!
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
      frameIdsVsc <- unlist(lget(args, 'frameIdsVsc', NULL))
      frameIdR <- lget(args, 'frameIdR', -1)
      frameIdVsc <- lget(args, 'frameId', -1)
      if(!is.null(frameIdsVsc)){
        ind <- which(self$frameIdsVsc %in% frameIdsVsc)
      } else if(frameIdR>=0){
        ind <- which(self$frameIdsR == frameIdR)
      } else if(frameIdVsc>=0){
        ind <- which(self$frameIdsVsc == frameIdVsc)
      } else{
        ind <- seq_along(private$children)
      }
      return(private$children[ind])
    },

    clearVariables = function(){
      ret <- FALSE
      for(frame in private$children){
        for(scope in frame$children){
          if(!is.null(scope)){
            ret <- scope$clearChildren() || ret
          }
        }
      }
      return(ret)
    },

    getContent = function(args=list()) list(
      stackFrames = lapply(private$children, function(child) child$getContent()),
      totalFrames = self$totalFrames
    )

  ),
  private = list(),
  inherit = Node
)

FrameNode <- R6::R6Class(
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
    firstenv = NULL,
    lastenv = NULL,
    call = NULL,


    initialize = function(args=list(), parent=NULL){
      super$initialize(args, parent)

      # read args
      self$frameIdR <- lget(args, "frameIdR", 0)
      self$frameIdVsc <- lget(args, "frameIdVsc", 0)
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
        self$source <- NULL
        self$line <- NULL
        self$column <- NULL
        self$endLine <- NULL
        self$endColumn <- NULL
      } else {
        self$call <- sys.call(self$frameIdR)
        self$firstenv <- sys.frame(self$frameIdR)
        self$name <- getFrameName(self$call)
        self$presentationHint <- "normal"
        self$line <- NULL # to be overwritten by line from source
        self$column <- NULL # to be overwritten by column from source
        source <- getSource(sys.call(self$frameIdR + 1), self$frameIdR + 1)
        # source <- getSource(sys.call(self$frameIdR), self$frameIdR)
        if(!is.null(source)){
          self$source <- source
          self$line <- source$line
          self$column <- source$column
          self$endLine <- source$endLine
          self$endColumn <- source$endColumn + 1
        }
      }
      self$id <- self$frameIdVsc

      if(!getOption('vsc.includeFrameColumn', FALSE) && !is.null(self$source)){
        self$column <- 1
        self$endColumn <- NULL
        if(!is.null(self$endLine) && self$endLine > self$line){
          self$endLine <- self$endLine + 1
        }
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

VariableNode <- R6::R6Class(
  classname = "VariableNode",
  public = list(
    # from DebugProtocol.Variable
    name = '',
    value = '',
    type = '',
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
    attrVars = NULL,
    childVars = NULL,
    attrNodes = NULL,
    childNodes = NULL,
    storedAttrIndices = NULL,
    storedChildIndices = NULL,

    updateValue = function(newValue) {
      args <- list(
        name = self$name,
        rValue = newValue,
        setter = self$setter,
        setInfo = self$setInfo
      )
      self$initialize(args, parent=private$parent)
    },
    initialize = function(args=list(), parent=NULL){
      super$initialize(args, parent)
      
      # read args
      self$name <- lget(args, "name", "")
      self$rValue <- lget(args, "rValue", NULL)
      self$setter <- lget(args, "setter", NULL)
      self$setInfo <- lget(args, "setInfo", NULL)

      self$childVars <- list()
      self$attrVars <- list()
      private$children <- list()

      # do stuff
      infos <- c(
        "toString",
        "type",
        "nChildVars",
        "presentationHint"
      )

      if(getOption('vsc.showEvaluateName', TRUE)){
        infos <- c(infos, "evaluateName")
      }
      
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
      internalAttributes <- lget(infos, "internalAttributes", list())
      nestedCustomAttributes <- lget(infos, "customAttributes", list(list()))
      customAttributes <- unlist(nestedCustomAttributes, recursive = FALSE)
      allAttributes <- c(internalAttributes, customAttributes)
      self$attrVars <- lapply(seq_along(allAttributes), function(i){
        list(
          filter = 'named',
          index = i,
          minVar = allAttributes[[i]]
        )
      })
      self$namedVariables <- length(self$attrVars)
      
      # handle other
      self$indexedVariables <- lget(infos, "nChildVars", NULL)
      self$value <- infos$toString
      self$type <- infos$type
      self$evaluateName <- infos$evaluateName
      self$presentationHint <- infos$presentationHint
      
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
        presentationHint = self$presentationHint,
        namedVariables = self$namedVariables,
        indexedVariables = self$indexedVariables
      )
      if(lget(args, 'includeSetInfo', FALSE)){
        content$setter <- self$setter
        content$setInfo <- self$setInfo
      }
      return(content)
    },
    getIndices = function(indices, filter){
      if(length(indices)==0){
        return(list())
      }
      if(filter=='indexed'){
        vars <- self$childVars
      } else{
        vars <- self$attrVars
      }
      savedIndices <- lapply(vars, function(v) v$index)
      missingIndices <- setdiff(indices, savedIndices)


      # get childVars from VarInfos
      infos <- .vsc.applyVarInfos(
        self$rValue,
        infos = c('childVars'),
        ind = missingIndices
      )

      childVars <- infos[['childVars']]
      childVars <- fixNames(childVars)

      # combine with old vars
      newVars <- lapply(seq_along(missingIndices), function(i){
        if(i<=length(childVars)){
          list(
            index=missingIndices[i],
            minVar=childVars[[i]],
            node=NULL
          )
        } else{
          list(
            index=missingIndices[i],
            minVar=list(
              name="<WARNING>",
              rValue=getInfoVar("<Some child variables might be missing!>")
            ),
            node=NULL
          )
        }
      })
      vars <- c(vars, newVars)
      savedIndices <- c(savedIndices, missingIndices)

      indPositions <- match(indices, savedIndices)
      vars <- vars[indPositions]

      nodes <- replicate(length(indices), NULL)
      # compute/retrieve nodes
      for(i in seq_along(vars)){
        var <- vars[[i]]
        if(is.null(var$node)){
          var$node <- VariableNode$new(parent=self, args=var$minVar)
        }
        nodes[[i]] <- var$node
        vars[[i]] <- var
      }

      # save vars
      if(filter=='indexed'){
        self$childVars <- vars
      } else{
        self$attrVars <- vars
      }

      return(nodes)
    },
    getChildren = function(args=list()){
      lazy <- lget(args, 'lazy', FALSE)
      filter <- lget(args, 'filter', '')
      start <- lget(args, 'start', 0)
      count <- lget(args, 'count', 0)

      if(lazy){
        childNodes <- lapply(self$childVars, function(v) v$node)
        attrNodes <- lapply(self$attrVars, function(v) v$node)
        nodes <- c(childNodes, attrNodes)
        nullIndices <- sapply(nodes, is.null)
        nodes <- nodes[!nullIndices]
        return(nodes)
      }

      # determine requested indices
      if(filter=='indexed'){
        attrIndices <- integer(0)
        if(count==0){
          childIndices <- seq_len(self$indexedVariables)
        } else{
          childIndices <- (start+1):(start+count)
        }
      } else if(filter=='named'){
        childIndices <- integer(0)
        if(count==0){
          attrIndices <- seq_len(self$namedVariables)
        } else{
          attrIndices <- (start+1):(start+count)
        }
      } else {
        attrIndices <- seq_len(self$namedVariables)
        childIndices <- seq_len(self$indexedVariables)
      }

      nodes <- c(
        self$getIndices(attrIndices, 'named'),
        self$getIndices(childIndices, 'indexed')
      )

      return(nodes)
    }
  ),
  private = list(),
  inherit = Node
)

ScopeNode <- R6::R6Class(
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




