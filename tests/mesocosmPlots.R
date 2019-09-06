<<<<<<< HEAD
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

||||||| merged common ancestors
=======
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
                            newNames = c("Acetoclast Decay", "Autotroph Decay", "Heterotroph Decay",
                                         "Methanotroph Decay","Heterotrophic \nDOM Assimilation", "Aerobic DOM Oxidation","Denitrification", "NA"),
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

### Plot Isotope Model Output with Data ###
# Load and wrangle mesocosm data
load("/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Data Wrangle/postProcessedData/gas")
load("/Users/libbymohr-msu/Documents/MSU/RProjects/Microcosm Data Wrangle/postProcessedData/water")
gas <- gas[!(substr(gas$sampleID,3,3) %in% c("b","c")), ]
gas$days <- gas$minutesSinceAmendment/(24*60)
water$days <- water$minutesSinceAmendment/(24*60)
mesocosmData <- full_join(gas, water, by = "sampleID")
names(mesocosmData)[names(mesocosmData) == "days.x"]<- "DaysSinceAmendement"
names(mesocosmData)[names(mesocosmData) == "carboy.x"]<- "MesocosmName"
mesocosmData$MesocosmName <- factor(mesocosmData$MesocosmName, 
                                     levels = sort(unique(mesocosmData$MesocosmName)),
                                     labels = paste("Mesocosm", sort(unique(mesocosmData$MesocosmName))) )
rm("gas", "water")

CH4IsotopesModelPlot <- plotIsotopicComposition(results = results,
                                                poolNames = "CH4_C",
                                                AtomicWeights = "13",
                                                RStd = RstC,
                                                type = "line")
MesocosmColors <- pal_aaas()(7)
CH4IsotopesModelPlot + 
  ylab(expression(paste(delta ^13*C-CH[4], " (\u2030)"))) +
  plotData(observationsDF = mesocosmData, 
           xVarName = DaysSinceAmendement, 
           yVarName = delCH4, 
           colorVarName = MesocosmName,
           add = T) +
  scale_color_manual(values = c("black", MesocosmColors),
                     guide = guide_legend(title = "Legend", 
                                          override.aes = list(linetype = c("solid",rep("blank", 7)),
                                                              shape = c(NA, rep(16, 7)) )))

g <- ggplot_build(CH4IsotopesModelPlotWithData)
breaks <- g$plot$scales$scales[[3]]$get_breaks()
reorderedBreaks <- c(breaks[1:3], breaks[5:8], breaks[4])

CH4IsotopesModelPlotWithData +
  scale_color_manual(values = c(MesocosmColors, "black"),
                     breaks = reorderedBreaks,
                     guide = guide_legend(title = "Legend", 
                                          override.aes = list(linetype = c(rep("blank", 7), 
                                                                           "solid"),
                                                              shape = c(rep(16, 7),
                                                                        NA) )))
>>>>>>> cc69a29d4e27c0941fff00b89ef51947c1f0b60c
