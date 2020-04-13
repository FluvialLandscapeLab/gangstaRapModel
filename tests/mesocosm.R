# This sandbox creates the gangstaRap model as a rapper object.
rm(list = ls())
library(gangsta)
library(rapper)
library(gangstaRapper)

# Make gangstas and rewrite lpModel
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaSandbox/makelpFiles/makeMicrocosmModel.R")

# Create data frame of compounds to be leaked in at each time step
drivingValues <- data.frame(
  add.to.H2.initialMolecules = c(0,rep(20, times = 19)),
  add.to.Acetate.initialMolecules = c(0,rep(10, times = 19)),
  row.names = 0:19
)

# Make rapper object
mesocosmRapper <- gangstaRapper(execute = list(preModelUpdates, solveModel, updatelpSolveVarsInRapper, storeOutput),
                                drivingValues = drivingValues,
                                lpModelFilePath = "/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaSandbox/lpFiles/gravelIncubations.lp", 
                                gangstas = myGangstas,
                                updates = list(list(ID = "H2.initialMolecules",
                                                    calculation = expression(H2.finalMolecules + add.to.H2.initialMolecules)),
                                               list(ID = "Acetate.initialMolecules",
                                                    calculation = list(leakIn, ID = "Acetate.initialMolecules"))),
                                outputRequest = defaultOutput(myGangstas))

# run the model and return the results
results <- executeRapper(mesocosmRapper)
