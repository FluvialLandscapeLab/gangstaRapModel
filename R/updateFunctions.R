# Leak In ####
#'@title Leak in a compound. 
#'
#'@description An "update" function that leaks in compounds by adding
#' an amount to the final amount from the previous time step. The amount is 
#' specified in the rapper environment as "add.to.<compoundName> and gets 
#' updated with each iteration according the user-specified leak-in
#' data frame. 
#'
#'@param ID The name of the variable  initial molecules of compound. 
#'
#'@export
#'
leakIn <- function(ID){
  addToName <- paste0("add.to.", ID)
  finalMoleculesName <- sub("initial", "final", ID)
  text <- paste(addToName, finalMoleculesName, sep = "+")
  eval(parse(text = text), envir = parent.frame())
}

# Isotope Updates ####
#'@title Calculate new isotopic compositions resulting from transfers.
#'
#'@description  An "update" function that calculates the new isotopic
#'composition of compound pools based on the amount of compound transferred to 
#'and from the pool, the isotopic compositions of "from" pools, and the  
#'isotopic composition of the compound pool at the beginning of the time step.
#'
#'@param element The name of the element to calculate isotopic pools
#'
#'@export
#'
# Remember to incorporate leak ins
# It would be better to update this after the model runs but before calling "getOutput"