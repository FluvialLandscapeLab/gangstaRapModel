# This sandbox creates the gangstaRap model as a rapper object.
rm(list = ls())
library(gangsta)
library(rapper)
library(gangstaRapper)

# Make gangstas and rewrite lpModel
source("/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Gangsta/makelpFiles/makeMicrocosmModel.R")

# Create data frame of compounds to be leaked in at each time step
leakInDF <- data.frame(
  add.to.H2.initialMolecules = c(0,rep(20, times = 19)),
  row.names = 0:19
)

# Make rapper object
mesocosmRapper <- gangstaRapper(leakInDF = leakInDF,
                                lpModelFilePath = "/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Gangsta/lpFiles/gravelIncubations.lp", 
                                gangstas = myGangstas,
                                updates = list(list(ID = "H2.initialMolecules",
                                                    calculation = expression(H2.finalMolecules + add.to.H2.initialMolecules))),
                                outputRequest = defaultOutput(myGangstas))

# run the model and return the results
results <- executeRapper(mesocosmRapper)
