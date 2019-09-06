<<<<<<< HEAD
# This sandbox creates the gangstaRap model as a rapper object.
rm(list = ls())
library(rapper)
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaRapper/R/gangstaRapUtils.R")
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaRapper/R/gangstaRapModel.R")

# Create example driving values. In the model any driving value starting with
# "add.to.<lpSolveVariableName>" will add the corresponding values to the
# variable in between timesteps. This is used in place of the old "leak in list"
# to add electron donors and acceptors into the model over time.
drivingValues = data.frame(
  add.to.H2.initialMolecules = c(0,rep(20, times = 19)),
  add.to.Acetate.initialMolecules = c(0,rep(10, times = 19)),
  add.to.DIC.initialMolecules = c(0,rep(5, times = 19)),
  add.to.Met.initialMolecules = c(0,rep(0.05, times = 19)),
  add.to.Aut.initialMolecules = c(0,rep(0.05, times = 19)),
  row.names = 0:19
)

lpModel = read.lp("/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Gangsta/lpFiles/gravelIncubations.lp")

# get lp variable names representing the "finalmol" variables and "initialmol"
# variables
namesToUpdateList = getValueNamesToUpdate(lpModel)
# add to the rapper object a list of variable names that receive "added"
# amounts, based on column names of the driving variable
addToValueNames = getLeakInValueNames(namesToUpdateList[["initialValueNames"]], drivingValues)

# Create the gantstaRap model as a "rapper" object.  lpModel and
# dynamicRespiration are parameters required by the gangstaModel that will be
# stored in the rapper environment.
# Put gangsta list in environment and modify execute function
m = rapper::rapper(
  execute = gangstaModel,
  drivingValues = drivingValues,
  lpModel = lpModel,
  addToValueNames = addToValueNames,
  dynamicRespiration = F,
  leakIn = T,
  kineticUptake = F,
  getModelMatrix = getModelMatrix,
  initValues = namesToUpdateList
  )

# run the model and return the results.
results = executeRapper(m)
||||||| merged common ancestors
=======
# This sandbox creates the gangstaRap model as a rapper object.

# Create example driving values. In the model any driving value starting with
# "add.to.<lpSolveVariableName>" will add the corresponding values to the
# variable in between timesteps. This is used in place of the old "leak in list"
# to add electron donors and acceptors into the model over time.
drivingValues = data.frame(
  add.to.H2.initialMolecules = c(0,rep(20, times = 19)),
  add.to.Acetate.initialMolecules = c(0,rep(10, times = 19)),
  add.to.DIC.initialMolecules = c(0,rep(5, times = 19)),
  add.to.Met.initialMolecules = c(0,rep(0.05, times = 19)),
  add.to.Aut.initialMolecules = c(0,rep(0.05, times = 19)),
  row.names = 0:19
)

lpModel = read.lp("/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Gangsta/lpFiles/gravelIncubations.lp")

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
  addToValueNames = addToValueNames,
  dynamicRespiration = F,
  getModelMatrix = getModelMatrix,
  initValues = namesToUpdateList
  )

# run the model and return the results.
results = executeRapper(m)
>>>>>>> cc69a29d4e27c0941fff00b89ef51947c1f0b60c
