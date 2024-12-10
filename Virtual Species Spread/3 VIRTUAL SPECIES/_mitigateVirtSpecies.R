# File name: _mitigateVirtSpecies.R
# Author: S. Bogen
#
# Uses reference data to simulate virtual species datasets mitigated for bias
# from ecoregion biome and taxonomic order
#-------------------------------------------------------------------------------

library(Matrix)
library(coda)
library(ape)
library(MCMCglmm)
library(dplyr)
library(sf)

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/2 SIMULATE VIRTUAL SPECIES")

# Read in GLMM:
load('glmmBogen2022')
model <- Sarah_Strategy
rm(Sarah_Strategy)

# Read in and prepare data:
load("../0 DATA/Data by Species/Data_VirtualSpecies_Gaussian_Bullock_2022_ver2.Rdata")
source('../0 DATA/DataPrep.R')
data.all <- getdata(DispersalKernel = "Gaussian",
                    IncResponseTraits = c("leafN.Mean.Logtrans",
                                          "LeafArea.Mean.Logtrans", 
                                          "SLA.Mean.Logtrans",
                                          "rootingdepth.Mean.Logtrans", 
                                          "WD.Mean.Logtrans"))

datMinst <- data.all$datMinst
shift <- data.all$datMin.centering # centering is the mean
scale <- data.all$datMin.scaling # scaling is the sd
datMinst2 <- data.all$dataVS.analysis
datIVs <- datMinst[, c("DS_Bullock", "GF", "PH.Mean.Logtrans", 
                       "SeedMass.Mean.Logtrans", "Order")]  # independent variables

# Read in dataSetObservations and biomeProp
load("../1 CHARACTERIZE BIAS/biasWeights.RData")

# Clean up dataSet Observations
dsObs <- dataSetObservations[, c(1, 4)]
dsObs$ECO_CODE[dsObs$ECO_CODE=="Lake"] <- "empty"

# Clean up biomeProp - drop any category with fewer than ten observations
biomeProp$propSample * 2387
# Based on the above result, drop:
#  3 (Tropical and subtropicalconiferous forests)
#  9 (Flooded grasslands and savannas)
#  14 (Mangroves)

biomeProp_smaller <- biomeProp[c(-3, -9, -14), ]

#-------------------------------------------------------------------------------
# Mitigate Bias by Ecoregion Biome
#-------------------------------------------------------------------------------

# Get more information
plantRichSHP <- st_read("../0 DATA/Bias/Number of Plant Species by Terrestrial Ecoregion/data/commondata/data0/wwf_ecos_plant_spcs.shp")
ecoIDrefDF <- st_drop_geometry(plantRichSHP)[, c("REALM", "BIOME", "ECO_ID", 
                                                  "ECO_CODE")]
rm(plantRichSHP)

# collapse down to Unique values and remove Lake, Rock and Ice
foo <- unique.data.frame((ecoIDrefDF[ecoIDrefDF$ECO_ID > 0, ]))
# DROP instances of missing data, for now
bar <- merge.data.frame(dsObs, foo)
dsObsReference <- bar

# clean up space
rm(foo, bar, dsObs, dataSetObservations,
   dispersalmodes_unique_datasets)

# Plan: simulate 23,870 virtual species
set.seed(1234)
numVS <- 23870

# get the number of desired observations per ecoregion biome (ROUND UP)
numPerBiome <- ceiling(numVS * biomeProp$propPopulation)
names(numPerBiome) <- biomeProp$BIOME

speciesList <- c()
# tmp <- sample(dsObsReference$acceptedbinomial[dsObsReference$BIOME==i], size = numPerBiome[i], replace = TRUE)
# speciesList <- c(speciesList, tmp)

for(i in c(1, 2, 4, 5, 6, 7, 8, 10, 11, 12, 13)){
  
  tmp <- sample(dsObsReference$acceptedbinomial[dsObsReference$BIOME==i], 
                size = numPerBiome[i], replace = TRUE)
  speciesList <- c(speciesList, tmp)
  
}

# Now build the data set: use datMinst
speciesListMINI <- head(speciesList)
length(datMinst$Species)
length(unique(datMinst$Species))

ind <- c() #  will eventually hold the list of row numbers to pull from datMinst
bar <- as.data.frame(table(speciesList))
for(i in 1:nrow(bar)){
  ind <- c(ind, rep(which(datMinst$Species==bar[i, 1]), bar[i, 2]))
}

dataIVs_biome <- datMinst[ind, c("DS_Bullock", "GF", "PH.Mean.Logtrans", 
                          "SeedMass.Mean.Logtrans", "Order")]  # independent variables

numSims <- nrow(dataIVs_biome)
dataResp_biome <- data.frame(rep(0, numSims), rep(0, numSims), rep(0, numSims), 
                       rep(0, numSims), rep(0, numSims), rep(0, numSims), 
                       rep(0, numSims))
names(dataResp_biome) <- c("LeafArea.Mean.Logtrans", "rootingdepth.Mean.Logtrans",
                           "leafN.Mean.Logtrans", "SLA.Mean.Logtrans",
                           "WD.Mean.Logtrans", "logMaxLambda", "logGaussian2D")
simInput_biome <- data.frame(dataIVs_biome, dataResp_biome)
# NOW use these as input for the model

moo <- simulate.MCMCglmm(model, newdata = simInput_biome)
virtOutputs <- as.data.frame(matrix(moo, nrow = numSims))
colnames(virtOutputs) <- c('LA', 'RD', 'LeafN', 'SLA', 'WD', 'Lambda', 'logGauss2D')
virtSpecies_biome <- cbind(simInput_biome[, 1:5], virtOutputs)

# de-standardize - all will then be log transformed but not standardized
DS_Bullock <- virtSpecies_biome$DS_Bullock
GF <- virtSpecies_biome$GF
logPH <- virtSpecies_biome$PH.Mean.Logtrans * scale[9] + shift[9]         # 9
logSM <- virtSpecies_biome$SeedMass.Mean.Logtrans * scale[10] + shift[10] # 10
logLA <- virtSpecies_biome$LA * scale[2] + shift[2]                       # 2
logRD <- virtSpecies_biome$RD * scale[4] + shift[4]                       # 4
logLeafN <- virtSpecies_biome$LeafN * scale[1] + shift[1]                 # 1
logSLA <- virtSpecies_biome$SLA * scale[3] + shift[5]                     # 3
logWD <- virtSpecies_biome$WD * scale[5] + shift[5]                       # 5
logLambda <- virtSpecies_biome$Lambda * scale[7] + shift[7]               # 7
logGauss2D <- virtSpecies_biome$logGauss2D * scale[6] + shift[6]          # 6

predDeStandardized <- data.frame(DS_Bullock, GF, logPH, logSM, logLA, logRD, 
                                 logLeafN, logSLA, logWD, logLambda, logGauss2D)

# clean up space
rm(DS_Bullock, GF, logPH, logSM, logLA, logRD, logLeafN, logSLA, logWD, 
   logLambda, logGauss2D)

# write all, regardless of lambda value
write.csv(predDeStandardized, "simulations/biomeSim_all2.csv")

#-------------------------------------------------------------------------------
# Mitigate Bias by Taxonomic Order
#-------------------------------------------------------------------------------

# Clean up orderProp
orderProp$observedProp * 2387

numPerOrder <- ceiling(numVS * orderProp$expectedProp)
names(numPerOrder) <- orderProp$ORDER

speciesList <- c()

for(i in 1:nrow(orderProp)){
  
  tmp <- sample(datMinst$Species[datMinst$Order==names(numPerOrder)[i]], 
                size = numPerOrder[i], replace = TRUE)
  speciesList <- c(speciesList, tmp)
  
}

# Now get indeces of species
ind <- c() #  will eventually hold the list of row numbers to pull from datMinst
bar <- as.data.frame(table(speciesList))
for(i in 1:nrow(bar)){
  ind <- c(ind, rep(which(datMinst$Species==bar[i, 1]), bar[i, 2]))
}

dataIVs_order <- datMinst[ind, c("DS_Bullock", "GF", "PH.Mean.Logtrans", 
                                 "SeedMass.Mean.Logtrans", "Order")]  # independent variables

numSims <- nrow(dataIVs_order)
dataResp_order <- data.frame(rep(0, numSims), rep(0, numSims), rep(0, numSims), 
                             rep(0, numSims), rep(0, numSims), rep(0, numSims), 
                             rep(0, numSims))
names(dataResp_order) <- c("LeafArea.Mean.Logtrans", "rootingdepth.Mean.Logtrans",
                           "leafN.Mean.Logtrans", "SLA.Mean.Logtrans",
                           "WD.Mean.Logtrans", "logMaxLambda", "logGaussian2D")
simInput_order <- data.frame(dataIVs_order, dataResp_order)
# NOW use these as input for the model

moo <- simulate.MCMCglmm(model, newdata = simInput_order)
virtOutputs <- as.data.frame(matrix(moo, nrow = numSims))
colnames(virtOutputs) <- c('LA', 'RD', 'LeafN', 'SLA', 'WD', 'Lambda', 'logGauss2D')
virtSpecies_order <- cbind(simInput_order[, 1:5], virtOutputs)

# de-standardize - all will then be log transformed but not standardized
DS_Bullock <- virtSpecies_order$DS_Bullock
GF <- virtSpecies_order$GF
logPH <- virtSpecies_order$PH.Mean.Logtrans * scale[9] + shift[9]         # 9
logSM <- virtSpecies_order$SeedMass.Mean.Logtrans * scale[10] + shift[10] # 10
logLA <- virtSpecies_order$LA * scale[2] + shift[2]                       # 2
logRD <- virtSpecies_order$RD * scale[4] + shift[4]                       # 4
logLeafN <- virtSpecies_order$LeafN * scale[1] + shift[1]                 # 1
logSLA <- virtSpecies_order$SLA * scale[3] + shift[5]                     # 3
logWD <- virtSpecies_order$WD * scale[5] + shift[5]                       # 5
logLambda <- virtSpecies_order$Lambda * scale[7] + shift[7]               # 7
logGauss2D <- virtSpecies_order$logGauss2D * scale[6] + shift[6]          # 6

predDeStandardized_order <- data.frame(DS_Bullock, GF, logPH, logSM, logLA, logRD, 
                                 logLeafN, logSLA, logWD, logLambda, logGauss2D)

# clean up space
rm(DS_Bullock, GF, logPH, logSM, logLA, logRD, logLeafN, logSLA, logWD, 
   logLambda, logGauss2D)

# write all, regardless of lambda value
write.csv(predDeStandardized_order, "simulations/orderSim_all.csv")


