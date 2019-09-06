# remember that executeRapper() already iterates and updates drivingValues to
# match the iteration you are on. Write this function for 1 single iteration,
# which will then be iterated using lapply in executeRapper.
# Why is the default for this function parent.frame()???? - becuase it will run a model that is in the global environment
gangstaModel = function(rapper = parent.frame()){
  if(timestep > 0){
    # get solved values from prior time step
    # Where is this getSolvedValues function????
    # solvedValues = gangsta::getSolvedValues(finalValueNames, rapper)
    finalValueIndex = match(finalValueNames, dimnames(lpModel)[[2]])
    newInitVals = lpSolveAPI::get.variables(lpModel)[finalValueIndex]
    names(newInitVals) = initialValueNames
    if(leakIn){
      # get leak in values
      plusValues = unlist(mget(addToValueNames, envir = rapper))
      # add leak ins to correct solved values
      newInitVals[names(addToValueNames)] = newInitVals[names(addToValueNames)] + plusValues
    }
    if(kineticUptake){
      # calculate the new environmental concentration
      # calculate the new initial concentraton in the model
    }
    # set init values in lpmodel
    mapply(
      function(newInitVal, initValueColumnNumber){
        lpSolveAPI::set.bounds(
          rapper$lpModel,
          lower = newInitVal,
          upper = newInitVal,
          columns = initValueColumnNumber
        )
      },
      newInitVal = newInitVals,
      initValueColumnNumber = match(initialValueNames, dimnames(lpModel)[[2]])
    )
  }
  if(dynamicRespiration){
    # set respiration values according to r = 6.792e-5+(temperature*1e-7)
    r = 6.792e-5+(temperature*1e-7)
    lapply(
      respirationRowsAndColumns,
      function(rc)
        lpSolveAPI::set.mat(
          lpModel,
          i = rc["i"],
          j = rc["j"],
          value = r)
    )
  }
  lpSolveAPI::solve.lpExtPtr(lpModel)
  results = lpSolveAPI::get.variables(lpModel)
  names(results) = dimnames(lpModel)[[2]]
  return(list(variables = results, modelMatrix = getModelMatrix(rapper)))
}


