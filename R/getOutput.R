#'@title Get requested model output from an lp Model. 
#'
#'@param outputRequest A data frame, character vector, or expression vector. 
#'
#'@return A data frame or vector of requested model output for a single time step. 
#'
#'@export
getOutput <- function(outputRequest, rapper){
  modelVarNames <- dimnames(rapper$lpModel)[[2]]
  variableOutput <- lpSolveAPI::get.variables(rapper$lpModel)[order(modelVarNames)]
  names(variableOutput) <- modelVarNames[order(modelVarNames)]
  variableOutput <- data.frame(variableOutput)
  if(class(outputRequest) == "data.frame"){
    outputList <- lapply(1:ncol(outputRequest), 
                         function(i){
                           if(all(outputRequest[i] %in% modelVarNames)&class(outputRequest[i]) == "character"){
                             return(variableOutput[outputRequest[i],])
                           }else if(class(outputRequest[i]) == "expression"){
                             return(sapply(outputRequest[i], eval))
                           }else{
                             return(outputRequest[i])
                           }
                         }
    )
    outputDF <- data.frame(outputList)
    names(outputDF) <- names(outputRequest)
    row.names(outputDF) <- row.names(outputRequest)
  }else if(class(outputRequest) == "character"){
    #output <- variableOutput[outputRequest,]
    output <- sapply(outputRequest, eval)
    namesOutput <- outputRequest
    return(output)
  }else if(class(outputRequest) == "expression"){
    output <- sapply(outputRequest, eval)
    return(output)
  }else{
    stop("Output request must be a data frame, character vector, or expression vector")
  }
}