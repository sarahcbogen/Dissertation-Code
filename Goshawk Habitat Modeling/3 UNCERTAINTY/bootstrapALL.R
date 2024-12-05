# Script name: bootstrapALL.R
# Author: S. Bogen
#
# Runs bootstrapping for each version of the AHP model.
#-------------------------------------------------------------------------------

library(ahpsurvey)
library(dplyr)
library(tidyr)
library(magrittr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(Hmisc)
library(RColorBrewer)
library(BAMMtools)

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 UNCERTAINTY")

source("../1 SUPPORT CODE/bootHelperFuncs.R")

# Read in literature and expert data
litData_raw <- read.csv("../2 LITERATURE REVIEW/litData_CLEAN.csv")
expertData <- read.csv("../0 DATA/AHP Survey/AHP_8Resp-Positive.csv", 
                       header = TRUE)

# Pre-processing for litData - Change all zero values to 1 and remove means
litData_raw$SLOPE_MIN <- replace(litData_raw$SLOPE_MIN, 
                                 litData_raw$SLOPE_MIN==0, 1)
litData <- litData_raw[, c(-4, -7, -10, -13, -16, -19)]
rm(litData_raw)

#-------------------------------------------------------------------------------
# Define bootstrap sample
#-------------------------------------------------------------------------------

# For each rep, randomly select 19 lit sources and 8 experts.
# Save indexes in a data frame called bootInfo

outfile <- "bootCI_ALL.RData"
reps <- 500 # TODO: Do three times and compare. Increase reps if needed.
set.seed(1234)

bootInfo <- data.frame()

for (i in 1:reps){
  
  bootInfo <- rbind(bootInfo, c(sample(1:19, replace = TRUE), 
                                sample(1:8, replace = TRUE)))
}

colnames(bootInfo) <- c("lit1", "lit2", "lit3", "lit4", "lit5", "lit6", "lit7",
                        "lit8", "lit9", "lit10", "lit11", "lit12", "lit13",
                        "lit14", "lit15", "lit16", "lit17", "lit18", "lit19",
                        "exp1", "exp2", "exp3", "exp4", "exp5", "exp6",
                        "exp7", "exp8")

#-------------------------------------------------------------------------------
# Get covariate suitable ranges for each bootstrap replicate using geoMeans
#-------------------------------------------------------------------------------

# Suitable ranges for each variable
bootSuitVals <- data.frame(matrix(nrow = 0, ncol = 12))

# Number of data points for each variable
bootLitNs <- data.frame(matrix(nrow = 0, ncol = 6))
justMinCols <- c(2, 4, 6, 8, 10, 12) # Count based on number of !NA mins

for(i in 1:reps){
  
  # Define bootstrap subset
  indeces <- as.numeric(bootInfo[i, 1:19])
  
  # Get suitable values
  geoMean_of_litSources <- apply(litData[indeces, -1], 2, geoMean, na.rm = TRUE)
  bootSuitVals <- rbind(bootSuitVals, geoMean_of_litSources)
  rm(geoMean_of_litSources)
  
  # Get number of non-NA values for each variable
  bootLitNs <- rbind(bootLitNs, colSums(!is.na(litData[indeces, justMinCols])))
  
}

# Define column names
colnames(bootSuitVals) <- colnames(litData)[-1]
colnames(bootLitNs) <- c("CC", "SA", "CBH", "BA", "ELEV", "SLOPE")

# Identify literature reps that have ANY covariates with no lit sources

repsDiscarded <- c()

for (i in 1:reps){
  if (sum(bootLitNs[i, ] == 0)){
    repsDiscarded <- c(repsDiscarded, i)
  }
}

#-------------------------------------------------------------------------------
# Use AHP to get covariate weights for each bootstrap replicate
#-------------------------------------------------------------------------------

bootWeights <- data.frame(matrix(nrow = 0, ncol = 8))

for(i in 1:reps){
  
  # Define bootstrap subset
  indeces <- as.numeric(bootInfo[i, 20:27])
  
  # get weights
  bootWeights <- rbind(bootWeights, calcWeights(expertData[indeces, ]))
  
  # Clean up space
  rm(indeces)
  
}

colnames(bootWeights) <- c("FT", "CC", "SA", "CBH", "BA", 
                           "ELEV", "SLOPE", "ASPECT")

#-------------------------------------------------------------------------------
# Generate raster stack of HSIs for Basic AHP model.
#-------------------------------------------------------------------------------

bootAshleyHSI <- stack()
bootFishlakeHSI <- stack()

# Run the hsm for the bootstraped samples with at least one lit source defining
# suitabile values for each covariate.

for (i in as.vector(1:reps)[-repsDiscarded]){

  newSuitVals <- as.numeric(bootSuitVals[i, ])
  newWeights <- as.numeric(bootWeights[i, ])

  newPredAshley <- hsm("ashley", newSuitVals, newWeights)
  newPredFishlake <- hsm("fishlake", newSuitVals, newWeights)

  bootAshleyHSI <- stack(bootAshleyHSI, newPredAshley)
  bootFishlakeHSI <- stack(bootFishlakeHSI, newPredFishlake)

  rm(newPredAshley, newPredFishlake, newSuitVals, newWeights)

}

# Get endpoints of a 95% bootstrapped CI of HSI for each cell
getGeoCI95 <- function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
bootAshleyBasic <- calc(bootAshleyHSI, getGeoCI95)
bootFishlakeBasic <- calc(bootFishlakeHSI, getGeoCI95)

# Remove HSI stacks
rm(bootAshleyHSI, bootFishlakeHSI)

# Write rasters
writeRaster(bootAshleyBasic$layer.1, "ashleyBasicLOW.tif")
writeRaster(bootAshleyBasic$layer.2, "ashleyBasicHIGH.tif")
writeRaster(bootFishlakeBasic$layer.1, "fishlakeBasicLOW.tif")
writeRaster(bootFishlakeBasic$layer.2, "fishlakeBasicHIGH.tif")

#-------------------------------------------------------------------------------
# Generate raster stack of HSIs for Smoothed AHP model.
#-------------------------------------------------------------------------------

bootAshleyHSI <- stack()
bootFishlakeHSI <- stack()

for (i in as.vector(1:reps)[-repsDiscarded]){

  indeces <- as.numeric(bootInfo[i, 1:19])
  newLitData <- litData[indeces, ]
  newWeights <- as.numeric(bootWeights[i, ])

  newPredAshley <- hsmSmoothed("ashley", newLitData, newWeights)
  newPredFishlake <- hsmSmoothed("fishlake", newLitData, newWeights)

  bootAshleyHSI <- stack(bootAshleyHSI, newPredAshley)
  bootFishlakeHSI <- stack(bootFishlakeHSI, newPredFishlake)

  rm(newPredAshley, newPredFishlake, newLitData, newWeights)
  
  print(i) # shows progress as the loop runs

}

# Get endpoints of a 95% bootstrapped CI of HSI for each cell
getGeoCI95 <- function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
bootAshleySmoothed <- calc(bootAshleyHSI, getGeoCI95)
bootFishlakeSmoothed <- calc(bootFishlakeHSI, getGeoCI95)

# Remove HSI stacks
rm(bootAshleyHSI, bootFishlakeHSI)

# Write rasters
writeRaster(bootAshleySmoothed$layer.1, "ashleySmoothedLOW.tif")
writeRaster(bootAshleySmoothed$layer.2, "ashleySmoothedHIGH.tif")
writeRaster(bootFishlakeSmoothed$layer.1, "fishlakeSmoothedLOW.tif")
writeRaster(bootFishlakeSmoothed$layer.2, "fishlakeSmoohedHIGH.tif")

#-------------------------------------------------------------------------------
# Generate raster stack of HSIs for Fuzzy AHP model.
#-------------------------------------------------------------------------------

# Note here: input will be the litData and the weights.

bootAshleyHSI <- stack()
bootFishlakeHSI <- stack()

for (i in as.vector(1:reps)[-repsDiscarded]){

  indeces <- as.numeric(bootInfo[i, 1:19])
  newLitData <- litData[indeces, -1]
  newWeights <- as.numeric(bootWeights[i, ])

  newPredAshley <- hsmJagged("ashley", newLitData, newWeights)
  newPredFishlake <- hsmJagged("fishlake", newLitData, newWeights)

  bootAshleyHSI <- stack(bootAshleyHSI, newPredAshley)
  bootFishlakeHSI <- stack(bootFishlakeHSI, newPredFishlake)

  rm(newPredAshley, newPredFishlake, newLitData, newWeights)

}

# Get endpoints of a 95% bootstrapped CI of HSI for each cell
# getGeoCI95 <- function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
bootAshleyFuzzy <- calc(bootAshleyHSI, getGeoCI95)
bootFishlakeFuzzy <- calc(bootFishlakeHSI, getGeoCI95)

# Remove HSI stacks
rm(bootAshleyHSI, bootFishlakeHSI)

# Write rasters
writeRaster(bootAshleyFuzzy$layer.1, "ashleyFuzzyLOW.tif")
writeRaster(bootAshleyFuzzy$layer.2, "ashleyFuzzyHIGH.tif")
writeRaster(bootFishlakeFuzzy$layer.1, "fishlakeFuzzyLOW.tif")
writeRaster(bootFishlakeFuzzy$layer.2, "fishlakeFuzzyHIGH.tif")

#-------------------------------------------------------------------------------
# Save info used to generate results and remove all heavy files
#-------------------------------------------------------------------------------

save(bootInfo, bootLitNs, bootSuitVals, bootWeights, expertData, 
     litData, repsDiscarded, file = "info.RData")

