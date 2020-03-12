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
  
  # Make transfer values data frames
  transClassName <- gangsta:::gangstaClassName("trans")
  procAttrName <- gangsta:::gangstaAttributeName("procName")
  nameAttrName <- gangsta:::gangstaAttributeName("name")
  transfers <- subsetGangstas(gangstas, "class", transClassName)
  transferNames <- getGangstaAttribute(transfers, nameAttrName)
  processNames <- getGangstaAttribute(transfers, procAttrName)
  transferMolTransVars <- gangsta:::makeTransferMolTransVars(transferNames)
  molTransferFroms <- unlist(lapply(transfers, "[[", "from"))
  molTransferTos <- unlist(lapply(transfers, "[[", "to"))
  molTransferValsDFbyProcess <- data.frame(fromPool = molTransferFroms,
                                           toPool = molTransferTos,
                                           process = processNames,
                                           molTransfered = transferMolTransVars,
                                           stringsAsFactors = F)
  row.names(molTransferValsDFbyProcess) <- transferMolTransVars
  
  molTransferValsDF <-  molTransferValsDFbyProcess %>% 
    group_by(fromPool, toPool) %>% 
    summarise_at(.vars = vars(molTransfered), .funs = function(x){paste(x, collapse = "+")})
  molTransferVals <- list(fromPool = molTransferValsDF$fromPool, 
                          toPool = molTransferValsDF$toPool,
                          molTransfered = lapply(molTransferValsDF$molTransfered, function(x) parse(text = x)))
  
  # Make compound values data frame
  compounds <-  subsetGangstas(gangstas, "class", gangsta:::gangstaClassName("comp"))
  compoundNames <-  getGangstaAttribute(compounds, gangsta:::gangstaAttributeName("name"))
  initCompoundNames <-  gangsta:::makeCompoundStartMolVars(compoundNames)
  finalCompoundNames <-  gangsta:::makeCompoundEndMolVars(compoundNames)
  change <- paste0(finalCompoundNames, "-", initCompoundNames)
  change <- lapply(change, function(x) parse(text = x))
  compoundVals <- list(compoundName = compoundNames, 
                       change = change,
                       intitial = initCompoundNames,
                       final = finalCompoundNames)
  
  # Make lists for leakInCompoundVals, leakInPoolVals 
  compoundMolsAdded <- paste0("if(","\"add.to.", compoundNames, ".initialMolecules\" ","%in% ls()){add.to.", compoundNames,".initialMolecules}else{0}")
  compoundMolsAdded <- lapply(compoundMolsAdded, function(x) parse(text = x))
  names(compoundMolsAdded) <- compoundNames
  
  poolCompoundNames <-  getGangstaAttribute(pools, gangsta:::gangstaAttributeName("compName"))
  molarRatios <- getGangstaAttribute(pools, gangsta:::gangstaAttributeName("molRatio"))
  poolMolsAdded <- paste0("if(","\"add.to.", poolCompoundNames, ".initialMolecules\" ","%in% ls()){add.to.", poolCompoundNames,".initialMolecules*",molarRatios,"}else{0}")
  poolMolsAdded <- lapply(poolMolsAdded, function(x) parse(text = x))
  names(poolMolsAdded) <- names(pools)
  
  outputRequestList <- list(
    objective = parse(text= "lpSolveAPI::get.objective(lpModel)"),
    status = "lpStatus", 
    poolVals = poolValsDF,
    molTransferVals = molTransferVals,
    molTransferValsByProcess = molTransferValsDFbyProcess,
    leakInPoolVals = poolMolsAdded,
    leakInCompoundVals = compoundMolsAdded,
    compoundVals = compoundVals#,
    #processEnergyVals = NULL,
    #respirationEnergyVals = NULL
  )
  return(outputRequestList)
}