rm(list = ls())
library(ggplot2)
library(ggsci)
library(RColorBrewer)
library(dplyr)
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaRapper/mesocosm.R")
source("/Users/libbymohr-msu/Documents/MSU/RProjects/gangstaRapper/plottingFuncs")

#### Plot Energies ####
energiesPlot <- plotModelResults(results = results, 
                 target = "allProcesses",
                 type = "bar") 
energyColors <- getColors(energiesPlot)
energiesPlot + scale_fill_manual(values = energyColors) + scale_color_manual(values = energyColors)


#### Plot Compounds ####
compounds1Plot<- plotModelResults(results = results,
                 target = c("CH4", "DOM", "Acetate", "NH4", "NO3", "O2", "H2"),
                 type = "line")
compound1Colors <- getColors(compounds1Plot)
compounds2Plot <- plotModelResults(results = results,
                                   target = c("N2", "DIC"),
                                   type = "line")
compound2Colors<- getColors(compounds2Plot, skip = 7)
organismsPlot <-  plotModelResults(results = results,
                                   target = c("Aut", "Het", "Met", "Acetoclast"),
                                   type = "line")
organismsColors<- getColors(organismsPlot, skip = 9)

compounds1Plot + scale_color_npg() + scale_fill_npg()
compounds2Plot + scale_color_manual(values = compound2Colors) + scale_fill_manual(values = compound2Colors)
organismsPlot + scale_color_manual(values = organismsColors)+scale_fill_manual(values = organismsColors)

#### Plot Transfers ####
DOMTransfers <- getTransferResults(results, "DOM_C", "all")
DOMRefactorDF <- data.frame(oldNames = unique(DOMTransfers$processName),
                            newNames = c("Autotroph Decay", "Heterotroph Decay", "Methanotroph Decay","Acetoclast Decay",
                                         "Aerobic DOM Oxidation","Denitrification", "Heterotrophic \nDOM Assimilation"),
                            stringsAsFactors = F)
DOMTransferPlot <- plotTransfers(results = results,
                                 target = "DOM_C",
                                 type = "bar",
                                 refactorDF = DOMRefactorDF)
DOMTransferPlotColors <- getColors(DOMTransferPlot)
DOMTransferPlot + scale_color_manual(values = DOMTransferPlotColors)+scale_fill_manual(values = DOMTransferPlotColors)

DICTransferPlot <- plotTransfers(results = results,
                                 target = "DIC_C",
                                 type = "bar")
DICTransferPlotColors <- getColors(DICTransferPlot)
DICTransferPlot + scale_color_futurama() +scale_fill_futurama()

NH4TransferPlot <- plotTransfers(results = results,
                                 target = "NH4_N",
                                 type = "bar")
NH4TransferPlotColors <- getColors(NH4TransferPlot)
NH4TransferPlot + scale_color_futurama() +scale_fill_futurama()

AcetateTransferPlot <- plotTransfers(results = results,
                                 target = "Acetate_C",
                                 type = "bar")
AcetateTransferPlot+ scale_color_futurama() +scale_fill_futurama()

