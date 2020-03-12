#'@title Make a gangstaRapper
#'
#'@description Creates a gangstaRapper object. The model is iterated when the 'executeRapper'
#'function is called with the gangstaRapper object as its argument.
#'
#'@param execute placeholder
#'@param drivingValues placeholder 
#'@param leakInDF A data frame where each column is named according to the convention
#' "add.to.<lpSolveVariableName>" and values in each row represent the amount added to the 
#'  variable associated with the column during a single timestep.
#'@param lpModelFilePath the file path for the .lp model as a character string
#'@param gangstas The list of gangsta objects used to generate the model.
#'@param updates placeholder
#'@param outputRequest A list of output requests. If this argument is not supplied, 
#' a default set of outputs will be generated. 
#'@param ... Named arguments describing model parameters or initial condition
#'   for state variables where the name of the argument represents the variable
#'   name that will store the state variable or parameter value within the rapper environment. 
#'@param initValues An alternative way to specify parameters and initial values
#'   of state variables using a named list. May be used in conjunction with
#'   \code{...} arguments.
#'
#'@return An object of class gangstaRapper.  
#'
#'@export
gangstaRapper <- function(execute = executeGangstaModel,
                          drivingValues = NULL, 
                          leakInDF = NULL,
                          lpModelFilePath, 
                          gangstas,
                          updates = list(),
                          outputRequest = NULL,
                          ...,
                          initValues = list()){
  # Read the lpModel from file path
  lpModel <- lpSolveAPI::read.lp(lpModelFilePath)
  
  # Get lp variable names representing the "finalmol" variables and "initialmol" variables
  namesToUpdateList <- getValueNamesToUpdate(lpModel)
  
  # Make a data frame of driving values
  drivingValues <- makeDrivingValuesDF(drivingValues = drivingValues, 
                                       leakInDF = leakInDF,
                                       namesToUpdateList = namesToUpdateList) 

  # Make output request if NULL
  if(is.null(outputRequest)){outputRequest <- defaultOutput(gangstas)}
  
  # Get names of lpModelVariables
  lpModelNames <- dimnames(lpModel)[[2]]
  lpModelInitVals <- structure(as.list(as.numeric(x = rep(NA, times = length(lpModelNames)))), names = lpModelNames)
  
  # Get names of lpModel Variables not in the list of updates
  lpModelNamesNotInUpdateList <- lpModelNames[!(lpModelNames %in% sapply(updates, function(update){update$ID}))]
  initialValuesNamesNotInUpdateList <- lpModelNames[grep(".initialMolecules", lpModelNames)]
  initialValuesNamesNotInUpdateList <- initialValuesNamesNotInUpdateList[-grep("x.", initialValuesNamesNotInUpdateList)]
  finalValueNamesNotInUpdateList <- sub("initial", "final", initialValuesNamesNotInUpdateList)
  
  # Create the gangstaRapper object
  gangstaRapper = rapper::rapper(
    execute = execute,
    drivingValues = drivingValues,
    lpModel = lpModel,
    gangstas = gangstas,
    updates = updates,
    outputRequest = outputRequest,
    lpModelNames = lpModelNames,
    initialValuesNamesNotInUpdateList = initialValuesNamesNotInUpdateList,
    finalValueNamesNotInUpdateList =  finalValueNamesNotInUpdateList,
    ...,
    initValues = c(initValues, lpModelInitVals)
  )
  class(gangstaRapper) <- c("gangstaRapper", class(gangstaRapper))
  return(gangstaRapper)
}