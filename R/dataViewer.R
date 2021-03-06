
showDataViewerRequest <- function(response, args, request){
  args$findBy <- 'nameAndVarRef'
  node <- session$rootNode$findChildNode(args)
  if(is.null(node)){
    response$success <- FALSE
  } else{
    val <- node$rValue
    viewFunc <- getOption('vsc.dataViewer', utils::View)
    viewFunc(val)
  }
  return(sendResponse(response))
}
