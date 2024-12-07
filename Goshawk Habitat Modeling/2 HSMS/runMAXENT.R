# Script name: maxentHSM.R
# Author: S. Bogen
#
# Generates predictions for the habitat suitability model based on MaxEnt and
# data from Fishlake NF and Ashley NF
# Saves continuous HSM rasters to MAXENT sub-folder
#-------------------------------------------------------------------------------

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(Hmisc)
library(RColorBrewer)
library(BAMMtools)
library(dismo)
library(rJava)
library(sf)
library(pROC)

# CHANGE TO THE WORKING DIRECTORY ON YOUR SYSTEM
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/2 HSMS")

# Define the standard projection (should match that used in "cleaning.R")
standardProj <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96",
                      "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0", 
                      "+units=m +no_defs")

#-------------------------------------------------------------------------------
# Train with ASHLEY
#-------------------------------------------------------------------------------

# Read in cleaned covariates
name <- 'ASHLEY'

FT <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/ForestType", capitalize(name), ".tif", sep = ""))
CC <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/CanopyCover", capitalize(name), ".tif", sep = ""))
SA <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/StandAge", capitalize(name), ".tif", sep = ""))
CBH <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                    "/CanopyBaseHeight", capitalize(name), ".tif", sep = ""))
BA <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/BasalArea", capitalize(name), ".tif", sep = ""))
ELEV <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                     "/Elevation", capitalize(name), ".tif", sep = ""))
SLOPE <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                      "/Slope", capitalize(name), ".tif", sep = ""))
ASPECT <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                       "/Aspect", capitalize(name), ".tif", sep = ""))

# Handling NA values for Stand Age - is this appropriate? Yes.
SA[is.na(SA)] <- 0

# Assemble cleaned covariates into a stack; standardize names
ashley.covariates <- stack(FT, CC, SA, CBH, BA, ELEV, SLOPE, ASPECT)
names(ashley.covariates) <- c("ForestType", "CanopyCover", "StandAge", 
                              "CanopyBaseHeight", "BasalArea", "Elevation", 
                              "Slope", "Aspect")

ANF_nests <-read.csv("../0 DATA/verification data/RAW/ANF_nests.csv")
WGS84 <- paste("+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
WGS84_ <- crs(WGS84)

# Create 'sf' from the CSV
plot_ANF_nests <- st_as_sf(ANF_nests, coords = c("X", "Y"), crs = WGS84_)

# Create spatial points dataframe for use with sp package
ANF_sites <- as_Spatial(plot_ANF_nests, cast = TRUE, 
                        IDs = paste0("ID", seq_along(plot_ANF_nests)))

# Reproject the ANF nest sites using the spatial points dataframe
ashley.nests.proj <- spTransform(ANF_sites, CRS(standardProj))

set.seed(1234)

# Partition data for training and testing
allpoints <- ashley.nests.proj@coords
fold <- kfold(allpoints, k=5)
# THIS ONE - TRAIN WITH ALL
train <- allpoints

# Forest Type (FT) is a factor
# All other covariates are continuous. Takes a hot second.
me.ashley_trained <- maxent(ashley.covariates, train, factors = 'ForestType')

#-------------------------------------------------------------------------------
# Train with Fishlake
#-------------------------------------------------------------------------------

# Read in cleaned covariates
name <- 'FISHLAKE'

FT <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/ForestType", capitalize(name), ".tif", sep = ""))
CC <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/CanopyCover", capitalize(name), ".tif", sep = ""))
SA <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/StandAge", capitalize(name), ".tif", sep = ""))
CBH <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                    "/CanopyBaseHeight", capitalize(name), ".tif", sep = ""))
BA <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                   "/BasalArea", capitalize(name), ".tif", sep = ""))
ELEV <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                     "/Elevation", capitalize(name), ".tif", sep = ""))
SLOPE <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                      "/Slope", capitalize(name), ".tif", sep = ""))
ASPECT <- raster(paste("../0 DATA/spatial covariates/CLEAN/", toupper(name), 
                       "/Aspect", capitalize(name), ".tif", sep = ""))

# Handling NA values for Stand Age - is this appropriate?
SA[is.na(SA)] <- 0

# Assemble cleaned covariates into a stack
fishlake.covariates <- stack(FT, CC, SA, CBH, BA, ELEV, SLOPE, ASPECT)
names(fishlake.covariates) <- c("ForestType", "CanopyCover", "StandAge", 
                                "CanopyBaseHeight", "BasalArea", "Elevation", 
                                "Slope", "Aspect")

# Read in fishlake training data
fishlake.nests <- readOGR(file.path("../0 DATA/verification data/RAW", 
                                    "/WildlifeGoshawkNest_20190416",
                                    "WildlifeGoshawkNest_20190416.shp"))
fishlake.nests.proj <- spTransform(fishlake.nests, CRS(standardProj))
fishlake_crs <- paste("+proj=utm +zone=12 +ellps=GRS80",
                      "+towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Partition data for training and testing
allpoints <- fishlake.nests.proj@coords
fold <- kfold(allpoints, k=3)

# THIS ONE - TRAIN WITH ALL
train <- allpoints

# Forest Type (FT) is a factor
# All other covariates are continuous. Takes a hot second
me.fishlake_trained <- maxent(fishlake.covariates, train, factors = 'ForestType')

rm(ASPECT, BA, CBH, CC, ELEV, FT, SA, SLOPE)

#-------------------------------------------------------------------------------
# Generate predictions
#-------------------------------------------------------------------------------

par(mfrow = c(2, 2))

# Ashley prediction, trained with Fishlake data. Takes a hot second.
ashleyPredict.continuous <- predict(me.fishlake_trained, ashley.covariates)
plot(ashleyPredict.continuous)

# Fishlake prediction, trained with Ashley data. Takes a hot second.
fishlakePredict.continuous <- predict(me.ashley_trained, fishlake.covariates)
plot(fishlakePredict.continuous)

# Ashley prediction, trained with Ashley data. Takes a hot second.
ashleyPredictAshley <- predict(me.ashley_trained, ashley.covariates)
plot(ashleyPredictAshley)

# Fishlake prediction, trained with Fishlake data. Takes a hot second.
fishlakePredictFishlake <- predict(me.fishlake_trained, fishlake.covariates)
plot(fishlakePredictFishlake)

#-------------------------------------------------------------------------------
# Write Rasters
#-------------------------------------------------------------------------------

writeRaster(ashleyPredict.continuous, "MAXENT/ashley_trainedFishlake.tif", 
            overwrite=TRUE)
writeRaster(fishlakePredict.continuous, "MAXENT/fishlake_trainedAshley.tif", 
            overwrite=TRUE)

writeRaster(ashleyPredictAshley, "MAXENT/ashley_trainedAshley.tif", 
            overwrite=TRUE)
writeRaster(fishlakePredictFishlake, "MAXENT/fishlake_trainedFishlake.tif", 
            overwrite=TRUE)




