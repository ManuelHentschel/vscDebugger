
showDataViewerRequest <- function(response, args, request){
  args$findBy <- 'nameAndVarRef'
  node <- session$rootNode$findChildNode(args)
  if(is.null(node)){
    response$success <- FALSE
  } else{
    val <- node$rValue
    viewFunc <- getOption('vsc.dataViewer', NULL)
    if(is.null(viewFunc)){
      viewFunc <- function(val) utils::View(val, node$name)
    }
    try(viewFunc(val))
  }
  return(sendResponse(response))
}
