# This sandbox creates the gangstaRap model as a rapper object.
#rm(list = ls())
library(gangsta)
library(rapper)
library(gangstaRapper)

###### Test without Isotopes ######
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


###### Test with Isotopes ######

# Get initial values from data
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaSandbox/getModelInitVals.R")

# Set initial autotrophic and heterotrophic biomass with units of micromoles per Liter
AutInitBiomass <- 1
HetInitBiomass <- 1

initialAFs_DIC <- c("12" = 0.9, "13" = 0.1)
initialAFs_CH4 <- c("12" = 0.1, "13" = 0.9)
initialAFs_DOC <- c("12" = 0.1, "13" = 0.9)
RCPDB <- 0.011237
# https://www.esrl.noaa.gov/gmd/education/isotopes/deltavalues.html

# Make the lp File
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaSandbox/makelpFiles/batchReactor.R")

# Make data frame of driving values
drivingValues <- data.frame(
  add.to.DIC.initialMolecules = c(0,rep(1, times = 19)),
  add.to.DIC.initialMolecules.AF.12 = rep(0.9, times = 20),
  add.to.DIC.initialMolecules.AF.13 = rep(0.1, times = 20),
  row.names = 0:19
)

# To track isotopes: 
#   1. add "updateisotopes" to list of execute functions
#   2. Specify "isotopesToTrack", a vector of pool names
#   3. Specify "isotopeStandards" from which delta values are calculated
#   4. Specify "initialAtomicFractions", a named list where list elements are named by pool and consist of
#      a named vector.The vector contains initial atomic fractions and is named according to atomic weight of the isotope. 
#   5. Specify isotopic composition of leakIns in the driving variables list, if applicable. The naming convention is
#      "add.to.<poolName>.initialMolecules.AF.AtomicMass"
#   6. Make the output request list with "defaultOutputWithIsotopes"
# Make rapper object with isotope calculations included
brRapperWithIsotopes <- gangstaRapper(execute = list(preModelUpdates, solveModel, updatelpSolveVarsInRapper, updateIsotopes, storeOutput),
                                      drivingValues = drivingValues,
                                      lpModelFilePath = "/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaSandbox/lpFiles/batchReactor.lp", 
                                      gangstas = myGangstas,
                                      isotopesToTrack = list("C" = c(12,13)),
                                      isotopeStandards = c("C" = RCPDB), 
                                      initialAtomicFractions = list("DIC_C" = initialAFs_DIC,
                                                                    "DOC_C" = initialAFs_DOC,
                                                                    "Aut_C" = initialAFs_DOC,
                                                                    "Het_C" = initialAFs_DOC),
                                      outputRequest = defaultOutputWithIsotopes(gangsta = myGangstas, 
                                                                                isotopesToTrack = list("C" = c(12,13)))
                                      )

# run the model and return the results
results <- executeRapper(brRapperWithIsotopes)

source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaSandbox/Plot/plottingFuncs_new.R")
