# This sandbox creates the gangstaRap model as a rapper object.

# Create example driving values. In the model any driving value starting with
# "add.to.<lpSolveVariableName>" will add the corresponding values to the
# variable in between timesteps. This is used in place of the old "leak in list"
# to add electron donors and acceptors into the model over time.
drivingValues = data.frame(
  add.to.DOM.initialMolecules = c(0,0,.002,0),
  add.to.Het.initialMolecules = c(0,0,.002,0),
  temperature = (1:4)+6,
  row.names = 0:3
)

# Respiration rate is fixed during a gangsta lpSolve model run. The
# gangstaRap model calculates dynamic respiration rates and inserts them into
# the lpModel between timesteps. To do this, the gangstaRap model has to know
# which constraints are associated with respiration rate inside the lpSolve matrix
# of constraint factors (slopes). ConstraintIDs to allow identification of
# specific constraints in an lpSolve model. Each constraint in an lpSolve model
# has a non-zero factor (slope) for the variables involved in the constraint.
# Thus, the constraint is identified by a vector containing the variable names
# with non-zero values in the lpSolve matrix. In this instance we are
# identifying the constraints that require respiration energy for each organism
# type to be equal to mols of biomass times a respiration rate.  Names in the
# following list are simply for reference -- they are not required.
constraintIDs <- list(
  hetRespirationRate = c("Het.finalMolecules", "Het.respirationEnergy"),
  autRespirationRate = c("Aut.finalMolecules", "Aut.respirationEnergy"),
  hetRespirationRate = c("Met.finalMolecules", "Met.respirationEnergy")
)

lpModel = lpSolveAPI::read.lp("/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Gangsta/lpFiles/gravelIncubations.lp")

# get the row and column numbers of the model matrix for each constraint in
# "constraintIDs."
respirationRowsAndColumns = lapply(constraintIDs, findConstraintRowAndColumn, lpModel)
# get lp variable names representing the "finalmol" variables and "initialmol"
# variables
namesToUpdateList = getValueNamesToUpdate(lpModel)
# add to the rapper object a list of variable names that receive "added"
# amounts, based on column names of the driving variable
addToValueNames = getLeakInValueNames(namesToUpdateList[["initialValueNames"]], drivingValues)

# Create the gantstaRap model as a "rapper" object.  lpModel and
# dynamicRespiration are parameters required by the gangstaModel that will be
# stored in the rapper environment.
m = rapper::rapper(
  gangstaModel,
  drivingValues,
  lpModel = lpModel,
  respirationRowsAndColumns = respirationRowsAndColumns,
  addToValueNames = addToValueNames,
  dynamicRespiration = T,
  getModelMatrix = getModelMatrix,
  initValues = namesToUpdateList
  )

# run the model and return the results.
result = executeRapper(m)
