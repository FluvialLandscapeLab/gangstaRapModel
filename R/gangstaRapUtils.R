#'@export
getValueNamesToUpdate = function(lpModel){
  # get variable names from model
  modelVarNames = dimnames(lpModel)[[2]]

  # make a list of all of the names that need to be updated between model runs by
  # finding all names that have ".intialMolecules" in them.
  initialValueNames = modelVarNames[grep(".initialMolecules", modelVarNames)]
  # Ooops!  Except remove the Ox. and Hx. variables
  initialValueNames = initialValueNames[-grep("x.", initialValueNames)]

  # create the names of that variables that have the values to be used in updating
  # "updateName" variables.  (i.e. the ".finalMolecules" variables have the values
  # from the prior time step.)
  finalValueNames = sub("initial", "final", initialValueNames)
  return(list(initialValueNames = initialValueNames, finalValueNames = finalValueNames))
}

#'@export
getLeakInValueNames = function(initialValueNames, drivingValues) {
  addToValueNames = paste0("add.to.", initialValueNames)
  hasLeakIn = addToValueNames %in% names(drivingValues)
  addToValueNames =
    structure(
      addToValueNames[hasLeakIn],
      names = initialValueNames[hasLeakIn]
    )
  return(addToValueNames)
}

#'@export
get.matRow <- function(rowNum, lpModel){
  sapply(1:ncol(lpModel), function(colNum) {lpSolveAPI::get.mat(i = rowNum, j = colNum, lprec = lpModel)})
}


# helper function that returns the row and column numbers of a constraint.  The
# "constraintID" is a vector of variables names used by the constraint.
#'@export
findConstraintRowAndColumn = function(constraintID, lpModel) {
  modelVarNames = dimnames(lpModel)[[2]]
  # check to be sure all variable names in the constraintID are in the model.
  badName = !constraintID %in% modelVarNames
  if(any(badName)) {
    stop("Variable(s) named '", paste(constraintID[badName], collapse = "', '"), "' are not in the model.")
  }

  # the model has a matrix where each row represents a constraint and each
  # column represents a variable. Each value in the matrix is a slope
  # associated with one variable and one constraint.
  # So, identifying a constraint row means that we find the row
  # with no-zero slopes for each variable in the constraintID and zero slope
  # for every variable not in the constraintID.

  # the following columns need to have non-zero slopes
  # First variable name is the one that gets updated
  constraintColumns = match(constraintID, modelVarNames)
  constraintColumn = constraintColumns[1]

  # return row number matrix row where constraintColumns are non-zero and other columns are zero in a constrain row
  constraintRow =
    which(
      sapply(
        1:nrow(lpModel),
        function(rowNum) {
          nonZeroColumns <- which(get.matRow(rowNum, lpModel) != 0.0)
          return(identical(sort(as.integer(nonZeroColumns)), sort(as.integer(constraintColumns))))
        }
      )
    )

  # makes sure there is one and only one constraint row that matches constraintID
  if(length(constraintRow) == 0) stop("No constraint found that uses only the variable(s): '", paste(constraintID, sep="', '"), "'")
  if(length(constraintRow) > 1) stop("More than one constraint found that uses only the variable(s): '", paste(constraintID, sep="', '"), "'")
  return(structure(c(constraintRow, constraintColumn), names = c("i", "j")))
}

# getModelMatrix returns a matrix for the solved lpModel
#'@export
getModelMatrix = function(envir){
  mat = matrix(
    mapply(
      lpSolveAPI::get.mat,
      i = rep(1:nrow(envir$lpModel), ncol(envir$lpModel)),
      j = rep(1:ncol(envir$lpModel), each = nrow(envir$lpModel)),
      MoreArgs = list(lprec = envir$lpModel)
    ),
    nrow = nrow(envir$lpModel),
    dimnames = dimnames(envir$lpModel)
  )
}

# Make a driving variables DF 
#'@export
makeDrivingValuesDF <- function(leakInDF = NULL, drivingValues){
  if(!is.null(leakInDF)){
    if(is.null(drivingValues)){
      drivingValues <- leakInDF
      row.names(drivingValues) <- 0:(nrow(drivingValues)-1)
    }else{
      drivingValues <- cbind(drivingValues, leakInDF)
      row.names(drivingValues) <- 0:(nrow(drivingValues)-1)
    }
  }else if(is.null(drivingValues)){
    stop("Either a list of driving values or a leak in list, or both, must be provided")
  }
  return(drivingValues)
}

# Update a parameter, lpModel variable, or lpModel slope given
# an ID (name of parameter in rapper, name of variable in model, or row and column of constraint),
# and a calculation (numeric constant, expression, character vector, or function)
#'@export
update <- function(updateMethod, rapper){
  # Determine the type of the calculation (constant, expression, character vector, or function) 
  # and calculate the new variable accordingly
  if(class(updateMethod$calculation) == "numeric"){
    newValue <- updateMethod$calculation
  }else if(class(updateMethod$calculation) == "expression"){
    newValue <- eval(expr = updateMethod$calculation,
                     envir = rapper)
  }else if(class(updateMethod$calculation) == "character"){
    newValue <- simplify2array(mget(x = updateMethod$calculation,
                                    envir = rapper))
  }else if(class(updateMethod$calculation[[1]]) == "function"){
    if(length(updateMethod$calculation)>1){
      args <- as.list(updateMethod$calculation[2:length(updateMethod$calculation)])
    }else{args <- list()}
    newValue <- do.call(what = updateMethod$calculation[[1]],
                        args = args, 
                        envir = rapper)
  }else{
    error('updateMethod$calculation must be an expression, function/argument list, character vector, or numeric vector')
  }
  
  # Determine whether the value to be updated is an lpModel variable, lpModel slope, or rapper variable
  # and update the value in the appropriate location(s).
  if(updateMethod$ID %in% rapper$lpModelNames){
    # If ID represents an lpModel variable, set the value of the lpModel variable both
    # in the lpModel and in the rapper environment
    lpSolveAPI::set.bounds(
      rapper$lpModel,
      lower = newValue,
      upper = newValue,
      columns = match(updateMethod$ID, rapper$lpModelNames)
    )
    # Preprocessor that replaces list of variables associated with constraint with a row and column number
  }else if(length(updateMethod$ID) == 2){
    # If ID represents a slope in the lpModel, set the slope in the lpModel
    lpSolveAPI::set.mat(
      rapper$lpModel,
      i = updateMethod$ID["i"],
      j = updateMethod$ID["j"],
      value = newValue)
  }else if(all(updateMethod$ID %in% ls(rapper)[!(ls(rapper) %in% rapper$lpModelNames)])){
    # If ID is a rapper variable name (or a vector of rapper variable names), 
    # assign the calculated value(s) to the name of the rapper object (s)
    mapply(assign,
           x = updateMethod$ID,
           value = newValue,
           MoreArgs = list(envir = rapper))
  }else{
    stop(
      "The ID of an update must be the name of an lpModel variable, a numeric vector of length 
      two containing the row and column number of the constraint slope to be updated,
      the name of a parameter in the rapper environment, or a vector of rapper variable names"
    )
  }
}

# This function could be used to get a list of lpSolve variable names in the rapper environment. 
shortls = function(env){
  ls(env, pattern = "^[^.]+$")
}





