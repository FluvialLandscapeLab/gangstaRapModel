#'@title Get requested model output from an lp Model. 
#'
#'@param outputRequest A list, character vector, expression. Lists elements may be expressions, character vectors, or lists of expressions.
#'
#'@return A data frame or vector of requested model output for a single time step. 
#'
#'@export
getOutput <- function(outputRequest, rapper){
  if(typeof(outputRequest)=="list"){
    outputList <- lapply(outputRequest, 
                         function(i){
                           if(class(i) == "character"){
                             return(getCharVecOutput(charVec = i, rapper = rapper))
                           }else if(class(i)== "expression"){
                             return(eval(i, envir = rapper))
                           }else if(all(sapply(i, class)== "expression")){
                             return(getExprListOutput(exprList = i, rapper = rapper))
                           }else{
                             return(i)
                           }
                         }
    )
    if(class(outputRequest[[1]][[1]]) == "call"){
      output <- simplify2array(outputList)
      names(output) <- names(outputRequest)
    }else{
      output <- data.frame(outputList,
                           stringsAsFactors = FALSE)
      if(!is.null(rownames(outputRequest))){
        rownames(output) <- rownames(outputRequest)
      }
    }
    names(output) <- names(outputRequest)
    row.names(output) <- row.names(outputRequest)
  }else if(class(outputRequest) == "character"){
    output<- getCharVecOutput(charVec = outputRequest, rapper = rapper)
    names(output) <- names(outputRequest)
  }else if(class(outputRequest) == "expression"|| class(outputRequest) == "call"){
    output <- eval(outputRequest, envir=rapper)
    names(output) <- names(outputRequest)
  }else{
    stop("Output request must be a data frame, character vector, or expression vector")
  }
  return(output)
}

getCharVecOutput<- function(charVec, rapper){
  if(all(charVec %in% ls(rapper))){
    output <- simplify2array(mget(x = charVec, envir = rapper))
  }else{
    output <- charVec
  }
  return(output)
}

getExprListOutput<- function(exprList, rapper){
  output <- sapply(exprList, eval, envir = rapper)
  names(output)<-names(exprList)
  return(output)
}
