#'@title Execute one iteration of a GANGSTA Model 
#'
#'@description The executeRapper function will iterately update driving values
#'and call this function, which updates the lp model, runs the model for one time step,
#'and reports results from the model run.  
#'
#'@param rapper The gangstaRapper environment containing the lpModel and all 
#'other variables and functions needed to update and solve the lpModel. 
#'The default rapper argument for this function is parent.frame(),
#'which references the global environment. 
#'
#'@return A list of requested model output for a single time step. 
#'
#'@export
executeGangstaModel = function(rapper = parent.frame()){
  if(timestep > 0){
    # Update variables in the rapper environment based on a user supplied list of updates and update functions
    do.call(what = lapply, args = list(X = updates, FUN = update, rapper = rapper), envir = rapper)

    # For lpModel variables whose names are not in the update list, set the initial value 
    # equal to the final value from the previous timestep. 
    finalValueIndex <- match(finalValueNamesNotInUpdateList, lpModelNames)
    newInitVals <- lpSolveAPI::get.variables(lpModel)[finalValueIndex]
    names(newInitVals) <- initialValuesNamesNotInUpdateList
    lpSolveAPI::set.bounds(
      lpModel,
      lower = newInitVals,
      upper = newInitVals,
      columns = match(initialValuesNamesNotInUpdateList, lpModelNames)
    )
  }
  # Solve the lp model
  rapper$lpStatus <- suppressWarnings(lpSolveAPI::solve.lpExtPtr(lpModel))
  
  # Get variables from lpModel and set values in environment
  solvedValues <- lpSolveAPI::get.variables(lpModel)
  mapply(assign, 
         x = lpModelNames, 
         value = solvedValues, 
         MoreArgs = list(envir = rapper))

  # Get output 
  results <- lapply(outputRequest, getOutput, rapper = rapper)
  
  # Set names of results to names of outputRequest
  names(results) <- names(outputRequest)
  
  return(results)
}


