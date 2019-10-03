#'@title Make a default list of output requests.
#'
#'@param gangstas The list of gangstas in the model.
#'
#'@return A list of output requests that can be passed to \code{gangstaRapper()}
#'
#'@export
defaultOutput <- function(gangstas){
  # Make pool values data frame
  pools <-  subsetGangstas(gangstas, "class", gangsta:::gangstaClassName("pool"))
  poolNames <-  getGangstaAttribute(pools, gangsta:::gangstaAttributeName("name"))
  elementName <-  getGangstaAttribute(pools, gangsta:::gangstaAttributeName("element"))
  elementIdx <-  order(elementName)
  initPoolNames <-  gangsta:::makePoolStartMolVars(poolNames)
  finalPoolNames <-  gangsta:::makePoolEndMolVars(poolNames)
  poolValsDF <- data.frame(initial = initPoolNames, 
                           final = finalPoolNames,
                           row.names = poolNames,
                           stringsAsFactors = F)
  poolValsDF <- poolValsDF[elementIdx, ]
  
  # Make transfer values data frame
  transClassName <- gangsta:::gangstaClassName("trans")
  procAttrName <- gangsta:::gangstaAttributeName("procName")
  nameAttrName <- gangsta:::gangstaAttributeName("name")
  transfers <- subsetGangstas(gangstas, "class", transClassName)
  transferNames <- getGangstaAttribute(transfers, nameAttrName)
  processNames <- getGangstaAttribute(transfers, procAttrName)
  transferMolTransVars <- gangsta:::makeTransferMolTransVars(transferNames)
  molTransferFroms <- unlist(lapply(transfers, "[[", "from") )
  molTransferTos <- unlist(lapply(transfers, "[[", "to") )
  molTransferValsDFbyProcess <- data.frame(fromPool = molTransferFroms,
                                           toPool = molTransferTos,
                                           process = processNames,
                                           molTransfered = transferMolTransVars,
                                           stringsAsFactors = F)
  row.names(molTransferValsDFbyProcess) <- transferMolTransVars
  molTransferValsDF <- plyr::ddply(.data = molTransferValsDFbyProcess,
                                   .variables = c("fromPool", "toPool"),
                                   .fun = plyr::summarise,
                                   molsTransferred = paste(molTransfered, collapse = "+"))

  outputRequestList <- list(
    #objective = expression(lpSolveAPI::get.objective(rapper$lpModel)),
    #status = expression(rapper$lpStatusCode), 
    poolVals = poolValsDF,
    molTransferVals = molTransferValsDF,
    molTransferValsByProcess = molTransferValsDFbyProcess#,
    #leakInPoolVals = NULL,
    #leakInCompoundVals = NULL,
    #compoundVals = NULL,
    #processEnergyVals = NULL,
    #respirationEnergyVals = NULL
  )
  return(outputRequestList)
}