# Script name: buildAHP_ALL.R
# Author: S. Bogen
#
# Builds AHP rasters for all three versions of the model for Fishlake and
# Ashley and saves them to the AHP sub-folder.
#-------------------------------------------------------------------------------

library(raster)
library(Hmisc)
library(BAMMtools)
library(tidyverse)
library(ahpsurvey)

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/2 HSMS")

# Read in lit data and expert data
litData_raw <- read.csv("../0 DATA/literature review/litData_CLEAN.csv")
litData <- litData_raw[, c(-4, -7, -10, -13, -16, -19)]
rm(litData_raw)

expertData <- read.csv("../0 DATA/expert survey/AHP_8Resp-Positive.csv", 
                       header = TRUE)
# Read in functions
source("../1 SUPPORT CODE/ahpHelperFuncs.R")

#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------

# get suitable value endpoints and weights
theSuitVals <- apply(litData[, -1], 2, geoMean, na.rm = TRUE)
theWeights <- calcWeights(expertData)

#-------------------------------------------------------------------------------
# Generate predictions
#-------------------------------------------------------------------------------

predBasic.ashley <- hsm("ashley", theSuitVals, theWeights)
predBasic.fishlake <- hsm("fishlake", theSuitVals, theWeights)

predSmoothed.ashley <- hsmSmoothed("ashley", litData, theWeights)
predSmoothed.fishlake <- hsmSmoothed("fishlake", litData, theWeights)

predFuzzy.ashley <- hsmJagged("ashley", litData, theWeights)
predFuzzy.fishlake <- hsmJagged("fishlake", litData, theWeights)

#-------------------------------------------------------------------------------
# Plot predictions
#-------------------------------------------------------------------------------

par(mfrow=c(1, 2))
plot(predBasic.ashley, zlim = c(0, 1))
plot(predBasic.fishlake, zlim = c(0, 1))

par(mfrow=c(1, 2))
plot(predSmoothed.ashley, zlim = c(0, 1))
plot(predSmoothed.fishlake, zlim = c(0, 1))

par(mfrow=c(1, 2))
plot(predFuzzy.ashley, zlim = c(0, 1))
plot(predFuzzy.fishlake, zlim = c(0, 1))

#-------------------------------------------------------------------------------
# Write rasters
#-------------------------------------------------------------------------------

writeRaster(predBasic.ashley, file = "AHP/predBasic.ashley.tif")
writeRaster(predBasic.fishlake, file = "AHP/predBasic.fishlake.tif")

writeRaster(predSmoothed.ashley, file = "AHP/predSmoothed.ashley.tif")
writeRaster(predSmoothed.fishlake, file = "AHP/predSmoothed.fishlake.tif")

writeRaster(predFuzzy.ashley, file = "AHP/predFuzzy.ashley.tif")
writeRaster(predFuzzy.fishlake, file = "AHP/predFuzzy.fishlake.tif")

#-------------------------------------------------------------------------------
# clean up space
#-------------------------------------------------------------------------------

rm(predBasic.ashley, predBasic.fishlake, predSmoothed.ashley, 
   predSmoothed.fishlake, predFuzzy.ashley, predFuzzy.fishlake, breaks_jenks)

