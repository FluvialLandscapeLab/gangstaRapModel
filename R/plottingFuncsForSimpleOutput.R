#### Plot Model Results Wrapper ####

# Type can be bar, line, or points
# Target may be an atomic vector of names of compounds, pools, or processes. 
# If the target vector contains compound names, the function returns a plot of moles of compound vs. time
# If the target vector contains one pool name, the function returns a plot of transfers into and out of the pool vs. time
# If the target vector contains process names, the function returns a plot of process energies vs. time. 
# Target may also be "allCompounds", "allProcesses", "allCatabProcesses", or "allAnabProcesses", 
#  which will plot moles of all compound, or energies for all processes, all catabolic processes, and all 
#  anabolic processes, respecfively
plotModelResults <- function(results, target , type, refactorDF = NULL){
  # Determine if the target is a compound, pool, or process, and call the 
  # corresponding plotting function
  # Make a list of all of the compound names by
  # finding all names that have ".initialMolecules" in them.
  modelVarNames = names(results[[1]]$variables)
  initialCompoundNames = modelVarNames[grep(".initialMolecules", modelVarNames)]
  # Ooops!  Except remove the Ox. and Hx. variables
  initialCompoundNames =  initialCompoundNames[-grep("x.", initialCompoundNames)]
  compoundNames = sub(".initialMolecules", "", initialCompoundNames)
  
  initialPoolNames = modelVarNames[grep(".initialAtoms", modelVarNames)]
  # Ooops!  Except remove the Ox. and Hx. variables
  initialPoolNames =  initialPoolNames[-grep("x_", initialPoolNames)]
  poolNames = sub(".initialAtoms", "", initialPoolNames)
  
  processEnergyNames = modelVarNames[grep(".netEnergy", modelVarNames)]
  processNames = sub(".netEnergy", "", processEnergyNames)
  #remove decay
  processNames = processNames[-grep("Decay", processNames)]
  
  if(all(target %in% compoundNames|target == "allCompounds")){
    plotCompounds(results, target, type)
  }else if(all(target %in% poolNames)){
    plotTransfers(results, target, type)
  }else if(all(target %in% processNames)|target %in% c("allProcesses", "allCatabProcesses", "allAnabProcesses")){
    plotEnergies(results, target, type, refactorDF)
  }else{stop("An element in the target vector doesn't exist in the model or elements in the vector are not of the same type")}
}

#### Plot Compound ####
getCompoundResults <- function(results, compoundName){
  initCompoundVal <- results[[1]]$variables[paste0(compoundName, ".initialMolecules")]
  compoundVals <- c(initCompoundVal,
                    sapply(results,
                           function(singleIterationResults){
                             singleIterationResults$variables[paste0(compoundName, ".finalMolecules")]
                           }
                    )
  )
  compoundValsDF <- data.frame(compoundName = compoundName, 
                               timeStep = 0:length(results), 
                               concentration = compoundVals)
  return(compoundValsDF)
}

plotCompounds <- function(results, target, type){
  if("allCompounds" %in% target){
    # make a list of all of the compound names by
    # finding all names that have ".initialMolecules" in them.
    modelVarNames = names(results[[1]]$variables)
    initialValueNames = modelVarNames[grep(".initialMolecules", modelVarNames)]
    # Ooops!  Except remove the Ox. and Hx. variables
    initialValueNames = initialValueNames[-grep("x.", initialValueNames)]
    compoundNames <- sub(".initialMolecules", "", initialValueNames)
  }else{
    compoundNames <- target
  }
  compoundValsDF <- do.call("rbind", lapply(compoundNames, getCompoundResults, results = results))
  
  compoundPlot <- ggplot(compoundValsDF, aes(x = timeStep, y = concentration, col = compoundName)) +
    ylab("Amount of Compound")+
    xlab("Time Step")+
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    compoundPlot + geom_line(size = 2) + labs(colour = "Compound \nName")
  }else if(type == "point"){
    compoundPlot + geom_point(size = 2) + labs(colour = "Compound \nName")
  }else if(type == "bar"){
    compoundPlot+ geom_bar(stat = "identity", aes(fill = compoundName))+labs(fill = "Compound \nName", colour = "Compound \nName")
  }
}

#### Plot Transfers ####
getTransferResults <- function(results, poolName, inOutOrAll){
  modelVarNames <- names(results[[1]]$variables)
  transfersInIndex <- grep(paste0("_", poolName,".atoms"), modelVarNames)
  transfersOutIndex <- grep(paste0("_", poolName,"_"), modelVarNames)
  transfersInProcesses <- sapply(modelVarNames[transfersInIndex], 
                                 function(transferName){
                                   #underScorePositions <- gregexpr(pattern = "_", text = transferName)[[1]]
                                   #processName <- substr(transferName, start = 1, stop = underScorePositions[1]-1)
                                   processName <-sub(".atoms", "", transferName)})
  transfersOutProcesses <- sapply(modelVarNames[transfersOutIndex], 
                                 function(transferName){processName <-sub(".atoms", "", transferName)})
  transfersIn <- lapply(1:length(results),
                        function(i){
                          data.frame(timeStep = i, 
                                     processName = transfersInProcesses,
                                     molsTransferred = results[[i]]$variables[transfersInIndex],
                                     stringsAsFactors = F)
                        })
  transfersInDF <- do.call("rbind", transfersIn)
  transfersOut <- lapply(1:length(results),
                        function(i){
                          data.frame(timeStep = i, 
                                     processName = transfersOutProcesses,
                                     molsTransferred = -1* results[[i]]$variables[transfersOutIndex],
                                     stringsAsFactors = F)
                        })
  transfersOutDF <- do.call("rbind", transfersOut)
  allTransfersDF <- rbind(transfersInDF, transfersOutDF)
  if(inOutOrAll == "in"){
    return(transfersInDF)
  }else if(inOutOrAll == "out"){
    return(transfersOutDF)
  }else if (inOutOrAll == "all"){
    return(allTransfersDF)
  }
}

plotTransfers <- function(results, target, type, refactorDF = NULL, inOutOrAll = "all"){
  transferDF <- getTransferResults(results, poolName = target, inOutOrAll = inOutOrAll)
  if(is.null(refactorDF)){
    refactorDF <- data.frame(oldNames = unique(transferDF$processName),
                             newNames = unique(transferDF$processName))
  }
  transferDF$newProcessName <- refactorDF[match(transferDF$processName, refactorDF$oldNames), ]$newNames
  refactoredTransfers <- tapply(X = transferDF$molsTransferred, INDEX = list(transferDF$newProcessName, transferDF$timeStep), FUN = sum)
  transferDF <- data.frame(newProcessName = rep(row.names(refactoredTransfers), times = ncol(refactoredTransfers)), 
                           timeStep = rep(as.numeric(colnames(refactoredTransfers)), each = nrow(refactoredTransfers)),
                           molsTransferred = as.vector(refactoredTransfers),
                           stringsAsFactors = F)
  
  transferPlot <- ggplot(transferDF, aes(x = timeStep, y = molsTransferred, col = newProcessName)) +
    ylab(paste0("Amount of ", substr(target, nchar(target), nchar(target)) ," transferred \n in to and out of ", substr(target, 1, nchar(target)-2)))+
    xlab("Time Step")+
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    transferPlot+ geom_line(size = 2) + labs(colour = "Process \nName")
  }else if(type == "point"){
    transferPlot + geom_point(size = 2) + labs(colour = "Process \nName")
  }else if(type == "bar"){
    transferPlot + geom_bar(stat = "identity", aes(fill = newProcessName))+labs(fill = "Process \nName", colour = "Process \nName")
  }
}
#### Plot Energies ####
# Should work for a vector of processNames

getEnergyResults <- function(results, processName){
  # Use the make energy names function - makeVarName
  processEnergyName <- paste0(processName, ".netEnergy")

  energies <- sapply(1:length(results), 
                     function(i){
                       results[[i]]$variables[processEnergyName]
                     }
  )
  energiesDF <- data.frame(timeStep = 1:length(results),
                           processName = processName,
                           energy = energies,
                           stringsAsFactors = F)
  return(energiesDF)
}


plotEnergies <- function(results, target, type, refactorDF = NULL){
  processEnergyNames = modelVarNames[grep(".netEnergy", modelVarNames)]
  allProcessNames <- sub(".netEnergy", "", processEnergyNames)
  #remove decay
  allProcessNames <- allProcessNames[-grep("Decay", allProcessNames)]
  allEnergiesDF <- do.call("rbind", lapply(allProcessNames, getEnergyResults, results = results))
  if("allProcesses" %in% target){
    nonZeroDF <- subset(allEnergiesDF, allEnergiesDF$energy != 0)
    processNames <- unique(nonZeroDF$processName)
  }else if ("allCatabProcesses" %in% target){
    catabolicDF <- subset(allEnergiesDF, allEnergiesDF$energy > 0)
    processNames <- unique(catabolicDF$processName)
  }else if ("allAnabProcesses" %in% target){
    anabolicDF <- subset(allEnergiesDF, allEnergiesDF$energy < 0)
    processNames <- unique(anabolicDF$processName)
  }else{
    processNames <- target
  }
  energiesDF <- do.call("rbind", lapply(processNames, getEnergyResults, results = results))
  
  if(is.null(refactorDF)){
    refactorDF <- data.frame(oldNames = allProcessNames,
                             newNames = allProcessNames)
  }
  energiesDF$newProcessName <- refactorDF[match(energiesDF$processName, refactorDF$oldNames), ]$newNames
  refactoredEnergies <- tapply(X = energiesDF$energy, INDEX = list(energiesDF$newProcessName, energiesDF$timeStep), FUN = sum)
  energiesDF <- data.frame(newProcessName = rep(row.names(refactoredEnergies), times = ncol(refactoredEnergies)), 
                           timeStep = rep(as.numeric(colnames(refactoredEnergies)), each = nrow(refactoredEnergies)),
                           energy = as.vector(refactoredEnergies),
                           stringsAsFactors = F)
  
  energyPlot <- ggplot(energiesDF, aes(x = as.numeric(timeStep), y = energy, col = newProcessName)) +
    ylab("Energy")+
    xlab("Time Step")+
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    energyPlot + geom_line(size = 2) + labs(colour = "Process \nName")
  }else if(type == "point"){
    energyPlot + geom_point(size = 2) + labs(colour = "Process \nName")
  }else if(type == "bar"){
    energyPlot + geom_bar(stat = "identity", aes(fill = newProcessName))+labs(fill = "Process \nName", colour = "Process \nName")
  }
  
}

# Plot process energy per unit biomass?

#### Plot Data ####
plotData <- function(observationsDF, xVarName, yVarName, colorVarName = NULL, add = F){
  x <- enquo(xVarName)
  y <- enquo(yVarName)
  colour <- enquo(colorVarName)
  if(add == T){
    return(geom_point(data = observationsDF, mapping = aes(x = !!x, y =  !!y, colour = !!colour), size = 2))
  }else{
    return(ggplot(data = observationsDF, mapping = aes(x =!!x, 
                                                       y =!!y,
                                                       colour = !!colour)) +
             geom_point(size = 2) +
             theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),  
                   panel.background = element_rect(fill = "white"),  
                   axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                   legend.key = element_rect(fill = "white")))
  }
}

#### Make color vector ####
getColors <- function(ggplotObject, skip = 0){
  n <- length(unique(ggplot_build(ggplotObject)$data[[1]]$colour))
  colors <- c("#0288fc", "#33ac24", "#8f44cb", "#a5d64b", "#ff459d", "#009f4f", "#bd0057", 
              "#00c8b3","#844a00", "#d0c974", "#ff814a", "#4c6200", '#cfbdf7', "#ff5b62","#87c5a0", 
              "#ff91b3", "#8e7100", "#cd9875","#a70b13","#017ea9")
  colors[(1+skip):(n+skip)]
}

#### Plot Model Results Wrapper ####

# Type can be bar, line, or points
# Target may be an atomic vector of names of compounds, pools, or processes. 
# If the target vector contains compound names, the function returns a plot of moles of compound vs. time
# If the target vector contains one pool name, the function returns a plot of transfers into and out of the pool vs. time
# If the target vector contains process names, the function returns a plot of process energies vs. time. 
# Target may also be "allCompounds", "allProcesses", "allCatabProcesses", or "allAnabProcesses", 
#  which will plot moles of all compound, or energies for all processes, all catabolic processes, and all 
#  anabolic processes, respecfively
plotModelResults <- function(results, target , type, refactorDF = NULL){
  # Determine if the target is a compound, pool, or process, and call the 
  # corresponding plotting function
  # Make a list of all of the compound names by
  # finding all names that have ".initialMolecules" in them.
  modelVarNames = names(results[[1]]$variables)
  initialCompoundNames = modelVarNames[grep(".initialMolecules", modelVarNames)]
  # Ooops!  Except remove the Ox. and Hx. variables
  initialCompoundNames =  initialCompoundNames[-grep("x.", initialCompoundNames)]
  compoundNames = sub(".initialMolecules", "", initialCompoundNames)
  
  initialPoolNames = modelVarNames[grep(".initialAtoms", modelVarNames)]
  # Ooops!  Except remove the Ox. and Hx. variables
  initialPoolNames =  initialPoolNames[-grep("x_", initialPoolNames)]
  poolNames = sub(".initialAtoms", "", initialPoolNames)
  
  processEnergyNames = modelVarNames[grep(".netEnergy", modelVarNames)]
  processNames = sub(".netEnergy", "", processEnergyNames)
  #remove decay
  processNames = processNames[-grep("Decay", processNames)]
  
  if(all(target %in% compoundNames|target == "allCompounds")){
    plotCompounds(results, target, type)
  }else if(all(target %in% poolNames)){
    plotTransfers(results, target, type)
  }else if(all(target %in% processNames)|target %in% c("allProcesses", "allCatabProcesses", "allAnabProcesses")){
    plotEnergies(results, target, type, refactorDF)
  }else{stop("An element in the target vector doesn't exist in the model or elements in the vector are not of the same type")}
}

#### Plot Compound ####
getCompoundResults <- function(results, compoundName){
  initCompoundVal <- results[[1]]$variables[paste0(compoundName, ".initialMolecules")]
  compoundVals <- c(initCompoundVal,
                    sapply(results,
                           function(singleIterationResults){
                             singleIterationResults$variables[paste0(compoundName, ".finalMolecules")]
                           }
                    )
  )
  compoundValsDF <- data.frame(compoundName = compoundName, 
                               timeStep = 0:length(results), 
                               concentration = compoundVals)
  return(compoundValsDF)
}

plotCompounds <- function(results, target, type){
  if("allCompounds" %in% target){
    # make a list of all of the compound names by
    # finding all names that have ".initialMolecules" in them.
    modelVarNames = names(results[[1]]$variables)
    initialValueNames = modelVarNames[grep(".initialMolecules", modelVarNames)]
    # Ooops!  Except remove the Ox. and Hx. variables
    initialValueNames = initialValueNames[-grep("x.", initialValueNames)]
    compoundNames <- sub(".initialMolecules", "", initialValueNames)
  }else{
    compoundNames <- target
  }
  compoundValsDF <- do.call("rbind", lapply(compoundNames, getCompoundResults, results = results))
  
  compoundPlot <- ggplot(compoundValsDF, aes(x = timeStep, y = concentration, col = compoundName)) +
    ylab("Amount of Compound")+
    xlab("Time Step")+
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    compoundPlot + geom_line(size = 2) + labs(colour = "Compound \nName")
  }else if(type == "point"){
    compoundPlot + geom_point(size = 2) + labs(colour = "Compound \nName")
  }else if(type == "bar"){
    compoundPlot+ geom_bar(stat = "identity", aes(fill = compoundName))+labs(fill = "Compound \nName", colour = "Compound \nName")
  }
}

#### Plot Transfers ####
getTransferResults <- function(results, poolName, inOutOrAll){
  modelVarNames = names(results[[1]]$variables)
  transfersInIndex = grep(paste0("_", poolName,".atoms"), modelVarNames)
  transfersOutIndex = grep(paste0("_", poolName,"_"), modelVarNames)
  transfersIn <- lapply(1:length(results),
                        function(i){
                          molsTransferred = results[[i]]$variables[transfersInIndex]
                          fromPools = results[[i]]$molTransferValsByProcess[rowIndices,"fromPool"]
                          if(length(molsTransferred)==0){
                            data.frame(timeStep = i, 
                                       processName = NA,
                                       molsTransferred = 0,
                                       stringsAsFactors = F)
                          }else{
                            data.frame(timeStep = i, 
                                       processName = row.names(results[[i]]$molTransferValsByProcess)[rowIndices],
                                       molsTransferred = molsTransferred,
                                       stringsAsFactors = F)
                          }
                        })
  transfersInDF <- do.call("rbind", transfersIn)
  transfersOut <- lapply(1:length(results),
                         function(i){
                           rowIndices = results[[i]]$molTransferValsByProcess$fromPool == poolName
                           molsTransferred = results[[i]]$molTransferValsByProcess[rowIndices,"molTransfered"]
                           if(length(molsTransferred)==0){
                             data.frame(timeStep = i, 
                                        processName = NA,
                                        molsTransferred = 0,
                                        stringsAsFactors = F)
                           }else{
                             data.frame(timeStep = i, 
                                        processName = row.names(results[[i]]$molTransferValsByProcess)[rowIndices],
                                        molsTransferred = -1 * molsTransferred,
                                        stringsAsFactors = F)
                           }
                         })
  transfersOutDF <- do.call("rbind", transfersOut)
  allTransfersDF <- rbind(transfersInDF, transfersOutDF)
  if(inOutOrAll == "in"){
    return(transfersInDF)
  }else if(inOutOrAll == "out"){
    return(transfersOutDF)
  }else if (inOutOrAll == "all"){
    return(allTransfersDF)
  }
}

plotTransfers <- function(results, target, type, refactorDF = NULL, inOutOrAll = "all"){
  transferDF <- getTransferResults(results, poolName = target, inOutOrAll = inOutOrAll)
  if(is.null(refactorDF)){
    refactorDF <- data.frame(oldNames = unique(transferDF$processName),
                             newNames = unique(transferDF$processName))
  }
  transferDF$newProcessName <- refactorDF[match(transferDF$processName, refactorDF$oldNames), ]$newNames
  refactoredTransfers <- tapply(X = transferDF$molsTransferred, INDEX = list(transferDF$newProcessName, transferDF$timeStep), FUN = sum)
  transferDF <- data.frame(newProcessName = rep(row.names(refactoredTransfers), times = ncol(refactoredTransfers)), 
                           timeStep = rep(as.numeric(colnames(refactoredTransfers)), each = nrow(refactoredTransfers)),
                           molsTransferred = as.vector(refactoredTransfers),
                           stringsAsFactors = F)
  
  transferPlot <- ggplot(transferDF, aes(x = timeStep, y = molsTransferred, col = newProcessName)) +
    ylab(paste0("Amount of ", substr(target, nchar(target), nchar(target)) ," transferred \n in to and out of ", substr(target, 1, nchar(target)-2)))+
    xlab("Time Step")+
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    transferPlot+ geom_line(size = 2) + labs(colour = "Process \nName")
  }else if(type == "point"){
    transferPlot + geom_point(size = 2) + labs(colour = "Process \nName")
  }else if(type == "bar"){
    transferPlot + geom_bar(stat = "identity", aes(fill = newProcessName))+labs(fill = "Process \nName", colour = "Process \nName")
  }
}
#### Plot Energies ####
getEnergyResults <- function(results, processName){
  energies <- sapply(results, 
                     function(singleIterationResults){
                       singleIterationResults$processEnergyVals[processName,"energy"]
                     }
  )
  energiesValsDF <- data.frame(processName = processName, 
                               timeStep = 1:length(results), 
                               energy = energies)
  return(energiesValsDF)
}


plotEnergies <- function(results, target, type, refactorDF = NULL){
  if("allProcesses" %in% target){
    processNames <- row.names(results$Iteration_1$processEnergyVals)
  }else if ("allCatabProcesses" %in% target){
    processNames <- row.names(results[[1]]$processEnergyVals[results[[1]]$processEnergyVals$procType == "catabolic",])
  }else if ("allAnabProcesses" %in% target){
    processNames <- row.names(results[[1]]$processEnergyVals[results[[1]]$processEnergyVals$procType == "anabolic",])
  }else{
    processNames <- target
  }
  energiesDF <- do.call("rbind", lapply(processNames, getEnergyResults, results = results))
  
  if(is.null(refactorDF)){
    refactorDF <- data.frame(oldNames = row.names(results[[1]]$processEnergyVals),
                             newNames = row.names(results[[1]]$processEnergyVals))
  }
  energiesDF$newProcessName <- refactorDF[match(energiesDF$processName, refactorDF$oldNames), ]$newNames
  refactoredEnergies <- tapply(X = energiesDF$energy, INDEX = list(energiesDF$newProcessName, energiesDF$timeStep), FUN = sum)
  energiesDF <- data.frame(newProcessName = rep(row.names(refactoredEnergies), times = ncol(refactoredEnergies)), 
                           timeStep = rep(as.numeric(colnames(refactoredEnergies)), each = nrow(refactoredEnergies)),
                           energy = as.vector(refactoredEnergies),
                           stringsAsFactors = F)
  
  energyPlot <- ggplot(energiesDF, aes(x = as.numeric(timeStep), y = energy, col = newProcessName)) +
    ylab("Energy")+
    xlab("Time Step")+
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    energyPlot + geom_line(size = 2) + labs(colour = "Process \nName")
  }else if(type == "point"){
    energyPlot + geom_point(size = 2) + labs(colour = "Process \nName")
  }else if(type == "bar"){
    energyPlot + geom_bar(stat = "identity", aes(fill = newProcessName))+labs(fill = "Process \nName", colour = "Process \nName")
  }
  
}
 #### Plot Isotopic Compositions ####

getdelValueResults<- function(results, poolName, AtomicWeight, RStd){
  AtomicFractions = c(list(results[[1]]$isotopeVals$initialIsotopicRatios[[poolName]]),
                      lapply(results, function(timeStepResults){
                        timeStepResults$isotopeVals$finalIsotopicRatios[[poolName]]
                      }))
  names(AtomicFractions)[1]<-"Initial_Conditions"
  delVals <- sapply(AtomicFractions, function(AtomicFractions){
    AF = AtomicFractions[AtomicWeight]
    R<- AF/(1-AF)
    del <- round((R/RStd-1)*1000, digits = 0)
  })
  return(data.frame(timeStep = 0:length(results), 
                    isotopeName = rep(paste0(poolName, AtomicWeight), times = length(results)+1),
                    delValue = delVals,
                    stringsAsFactors = F))
}

plotIsotopicComposition <- function(results, poolNames, AtomicWeights, RStds, type){
  delValsDF<- do.call("rbind", 
                      mapply(getdelValueResults,
                             poolName = poolNames,
                             AtomicWeight = as.character(AtomicWeights),
                             RStd = RStds, 
                             MoreArgs = list(results = results),
                             SIMPLIFY = F))
  isotopePlot <- ggplot(delValsDF, aes(x = timeStep, y = delValue, col = isotopeName))+
    ylab("del Value")+
    xlab("Time (days)")+
    theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          panel.background = element_rect(fill = "white"),  
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
          legend.key = element_rect(fill = "white"))
  if(type == "line"){
    isotopePlot + geom_line(size = 2) + labs(colour = "Isotope \nName")
  }else if(type == "point"){
    isotopePlot + geom_point(size = 2) + labs(colour = "Isotope \nName")
  }else if(type == "bar"){
    isotopePlot + geom_bar(stat = "identity", aes(fill = isotopeName))+labs(fill = "Isotope \nName", colour = "Isotope \nName")
  }
}

# Plot process energy per unit biomass?

#### Plot Data ####
plotData <- function(observationsDF, xVarName, yVarName, colorVarName = NULL, add = F){
  x <- enquo(xVarName)
  y <- enquo(yVarName)
  colour <- enquo(colorVarName)
  if(add == T){
    return(geom_point(data = observationsDF, mapping = aes(x = !!x, y =  !!y, colour = !!colour), size = 2))
  }else{
    return(ggplot(data = observationsDF, mapping = aes(x =!!x, 
                                                       y =!!y,
                                                       colour = !!colour)) +
             geom_point(size = 2) +
             theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),  
                   panel.background = element_rect(fill = "white"),  
                   axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                   legend.key = element_rect(fill = "white")))
  }
}

#### Make color vector ####
getColors <- function(ggplotObject, skip = 0){
  n <- length(unique(ggplot_build(ggplotObject)$data[[1]]$colour))
  colors <- c("#0288fc", "#33ac24", "#8f44cb", "#a5d64b", "#ff459d", "#009f4f", "#bd0057", 
              "#00c8b3","#844a00", "#d0c974", "#ff814a", "#4c6200", '#cfbdf7', "#ff5b62","#87c5a0", 
              "#ff91b3", "#8e7100", "#cd9875","#a70b13","#017ea9")
  colors[(1+skip):(n+skip)]
}