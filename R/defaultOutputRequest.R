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
  
  # Make transfer values request lists
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
  # Summarize by to pool and from pools to request total mols transferred from one pool to another by any process
  molTransferValsDF <-  molTransferValsDFbyProcess %>% 
    group_by(fromPool, toPool) %>% 
    summarise_at(.vars = vars(molTransfered), .funs = function(x){paste(x, collapse = "+")})
  molTransferVals <- list(fromPool = molTransferValsDF$fromPool, 
                          toPool = molTransferValsDF$toPool,
                          molTransfered = lapply(molTransferValsDF$molTransfered, function(x) parse(text = x)))
  
  # Make compound values request list
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
  
  # Make request list for process energy values
  processes <- subsetGangstas(gangstas, "class", gangsta:::gangstaClassName("proc"))
  processNames <- getGangstaAttribute(processes, gangsta:::gangstaAttributeName("name"))
  processEnergy <- getGangstaAttribute(processes, gangsta:::gangstaAttributeName("energy"))
  # Decay is neither catabolic or anabolic because it neither generates nor consumes energy, so we remove it:
  processEnergy <- processEnergy[-grep("Decay", names(processEnergy))]
  processNames <- processNames[-grep("Decay", processNames)]
  energyVarNames <- gangsta:::makeProcessEnergyVars(processNames)
  # Make a vector of 
  procType <- character(length(processEnergy))
  procType[processEnergy>0] <- "catabolic"
  procType[processEnergy<0] <- "anabolic"
  processEnergyVals <- data.frame(energy = energyVarNames,
                                  procType = procType,
                                  stringsAsFactors = FALSE,
                                  row.names = processNames)
  
  # Make request list for process energy values
  respEnergyVarName <- gangsta:::gangstaVarName("respEnergy")
  organismClassName <- gangsta:::gangstaClassName("org")
  organisms <- subsetGangstas(gangstas, "class", organismClassName)
  organismNames <- getGangstaAttribute(organisms, nameAttrName)
  
  respirationEnergyVals <- gangsta:::makeGenericVars(organismNames, "respEnergy")
  names(respirationEnergyVals) <- respirationEnergyVals
  
  outputRequestList <- list(
    objective = parse(text= "lpSolveAPI::get.objective(lpModel)"),
    status = "lpStatus", 
    poolVals = poolValsDF,
    molTransferVals = molTransferVals,
    molTransferValsByProcess = molTransferValsDFbyProcess,
    leakInPoolVals = poolMolsAdded,
    leakInCompoundVals = compoundMolsAdded,
    compoundVals = compoundVals,
    processEnergyVals = processEnergyVals,
    respirationEnergyVals = respirationEnergyVals
  )
  return(outputRequestList)
}