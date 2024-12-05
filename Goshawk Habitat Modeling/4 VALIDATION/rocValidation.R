# Script name: rocValidation.R
# Author: S. Bogen
#
# Uses verification data to generate ROC curve objects for each model and
# each forest. Saves objects to rocs.RData.
#-------------------------------------------------------------------------------

library(pROC)
library(rgdal)
library(sp)
library(sf)
library(RColorBrewer)
library(dismo)
library(raster)
library(rgeos)

# CHANGE TO THE WORKING DIRECTORY ON YOUR SYSTEM
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/4 VALIDATAION")

# Define the standard projection (should match that used in "cleaning.R")
standardProj <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96",
                      "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0", 
                      "+units=m +no_defs")

# Read in verification data for Ashley
ANF_nests <-read.csv("../0 DATA/verification data/RAW/ANF_nests.csv")
WGS84 <- paste("+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
WGS84_ <- crs(WGS84)
plot_ANF_nests <- st_as_sf(ANF_nests, coords = c("X", "Y"), crs = WGS84_)
ANF_sites <- as_Spatial(plot_ANF_nests, cast = TRUE, 
                        IDs = paste0("ID", seq_along(plot_ANF_nests)))
ashley.nests.proj <- spTransform(ANF_sites, CRS(standardProj))
rm(WGS84, WGS84_, fishlake.nests, ashley.nests, ANF_nests, 
   ANF_sites, plot_ANF_nests)

# Read in verificaiton data for Fishlake
fishlake.nests <- readOGR(file.path("../0 DATA/verification data/RAW", 
                                    "/WildlifeGoshawkNest_20190416",
                                    "WildlifeGoshawkNest_20190416.shp"))
fishlake.nests.proj <- spTransform(fishlake.nests, CRS(standardProj))
rm(fishlake.nests)

# Read in continuous HSI rasters for AHP
fishlakeAHP.base <- raster("../2 HSMS/AHP/predBasic.fishlake.tif")
ashleyAHP.base <- raster("../2 HSMS/AHP/predBasic.ashley.tif")
fishlakeAHP.smoothed <- raster("../2 HSMS/AHP/predSmoothed.fishlake.tif")
ashleyAHP.smoothed <- raster("../2 HSMS/AHP/predSmoothed.ashley.tif")
fishlakeAHP.fuzzy <- raster("../2 HSMS/AHP/predFuzzy.fishlake.tif")
ashleyAHP.fuzzy <- raster("../2 HSMS/AHP/predFuzzy.ashley.tif")

# Read in continuous HSI rasters for MaxENT
fishlakeME <- raster("../2 HSMS/MAXENT/fishlake_trainedAshley.tif")
ashleyME <- raster("../2 HSMS/MAXENT/ashley_trainedFishlake.tif")
fishlakeInSample <- raster("../2 HSMS/MAXENT/fishlake_trainedFishlake.tif")
ashleyInSample <- raster("../2 HSMS/MAXENT/ashley_trainedAshley.tif")

#-------------------------------------------------------------------------------
# Perform ROC Analysis
#-------------------------------------------------------------------------------

# For the ROC function: Input should be the HSI raster and verification points.
# Output the ROC object.

forestROC <- function(hsiRaster, backPoints, verPoints){
  
  # Generate pseudoabsence data to compare to presence data
  # background <- randomPoints(hsiRaster, 300)
  
  # Get HSI values for for presence data AND pseudoabsence data
  backValues <- raster::extract(hsiRaster, SpatialPoints(backPoints), 
                                method = 'simple')
  obsValues <- raster::extract(hsiRaster, SpatialPoints(verPoints), 
                               method = 'simple')
  
  # Prep and run ROC analysis
  response <- rep(c(0, 1), c(length(backValues), length(obsValues)))
  predictor <- c(backValues, obsValues)
  rocResult <- roc(response, predictor, ci=TRUE)
  
  # return the ROC result object
  return(rocResult)
}

# Get pseudoabsence background points - base on ME rasters
# Ensure they are not in the same grid cells as presence points
set.seed(1234)
ashleyBackground <- randomPoints(ashleyME, 300, ashley.nests.proj)
fishlakeBackground <- randomPoints(fishlakeME, 300, fishlake.nests.proj)

fishlakeInSample.roc <- forestROC(fishlakeInSample, fishlakeBackground, 
                                  fishlake.nests.proj)
ashleyInSample.roc <- forestROC(ashleyInSample, ashleyBackground, 
                                ashley.nests.proj)
fishlakeME.roc <- forestROC(fishlakeME, fishlakeBackground, fishlake.nests.proj)
ashleyME.roc <- forestROC(ashleyME, ashleyBackground, ashley.nests.proj)
base.fishlake.roc <- forestROC(fishlakeAHP.base, fishlakeBackground, 
                               fishlake.nests.proj)
base.ashley.roc <- forestROC(ashleyAHP.base, ashleyBackground, 
                             ashley.nests.proj)
ci95.fishlake.roc <- forestROC(fishlakeAHP.smoothed, fishlakeBackground, 
                               fishlake.nests.proj)
ci95.ashley.roc <- forestROC(ashleyAHP.smoothed, ashleyBackground, 
                             ashley.nests.proj)
fuzzy.fishlake.roc <- forestROC(fishlakeAHP.fuzzy, fishlakeBackground, 
                                fishlake.nests.proj)
fuzzy.ashley.roc <- forestROC(ashleyAHP.fuzzy, ashleyBackground, 
                              ashley.nests.proj)

#-------------------------------------------------------------------------------
# Play with distance
#-------------------------------------------------------------------------------

# Choose more background points! 10,000
# Ensure background points are not within 1.5 km of absence points

foo <- st_as_sf(fishlake.nests.proj)
bar <- st_as_sf(SpatialPoints(fishlakeBackground, 
                              proj4string = crs(fishlake.nests.proj)))

# "distance" between first and fiftieth background points
st_distance(bar[1, 1], bar[50, 1])

# pairwise distance between nests
st_distance(foo, foo)

# Distance between the first background point and each nest
# Number of nests within 1.5 km of first background point

toOmit <- c()

for (i in 1:300){
  
  numCloseNests <- sum(as.numeric(st_distance(bar[i, 1], foo)) < 1500)
  
  if(numCloseNests!=0){
    toOmit <- c(toOmit, i)
  }
  
}

# Need to figure out how to remove toOmit stuff from background points
# (removes background points within 1.5 km of a verification point)
bar[-toOmit, ]

# Try it:
set.seed(1234)
ashleyBackground <- randomPoints(ashleyME, 10000, ashley.nests.proj)
fishlakeBackground <- randomPoints(fishlakeME, 10500, fishlake.nests.proj)

filterPoints <- function(presenceRP, pseudoAbsenceRP){
  
  presence <- st_as_sf(presenceRP)
  pseudoAbsence <- st_as_sf(SpatialPoints(pseudoAbsenceRP, 
                                          proj4string = crs(presenceRP)))
  
  toOmit <- c()
  
  for (i in 1:dim(pseudoAbsence)[1]){
    
    numCloseNests <- sum(as.numeric(st_distance(pseudoAbsence[i, 1], presence)) < 1500)
    
    if(numCloseNests!=0){
      toOmit <- c(toOmit, i)
    }
    
  }
  
  st_coordinates(pseudoAbsence[-toOmit, ])
  
}

set.seed(1234)

ashleyBackground <- randomPoints(ashleyME, 10500, ashley.nests.proj)
fishlakeBackground <- randomPoints(fishlakeME, 10500, fishlake.nests.proj)

ashleyPseudoAbsence <- filterPoints(ashley.nests.proj, ashleyBackground)
fishlakePseudoAbsence <- filterPoints(fishlake.nests.proj, fishlakeBackground)


fishlakeME.roc <- forestROC(fishlakeME, fishlakePseudoAbsence, 
                            fishlake.nests.proj)
ashleyME.roc <- forestROC(ashleyME, ashleyPseudoAbsence, ashley.nests.proj)
base.fishlake.roc <- forestROC(fishlakeAHP.base, fishlakePseudoAbsence, 
                               fishlake.nests.proj)
base.ashley.roc <- forestROC(ashleyAHP.base, ashleyPseudoAbsence, 
                             ashley.nests.proj)
ci95.fishlake.roc <- forestROC(fishlakeAHP.smoothed, fishlakePseudoAbsence, 
                               fishlake.nests.proj)
ci95.ashley.roc <- forestROC(ashleyAHP.smoothed, ashleyPseudoAbsence, 
                             ashley.nests.proj)
fuzzy.fishlake.roc <- forestROC(fishlakeAHP.fuzzy, fishlakePseudoAbsence, 
                                fishlake.nests.proj)
fuzzy.ashley.roc <- forestROC(ashleyAHP.fuzzy, ashleyPseudoAbsence, 
                              ashley.nests.proj)

# Save results

save(ashleyInSample.roc, ashleyME.roc, base.ashley.roc, ci95.ashley.roc, 
     fuzzy.ashley.roc, fishlakeInSample.roc, fishlakeME.roc, base.fishlake.roc, 
     ci95.fishlake.roc, fuzzy.fishlake.roc, file = "rocs.RData")
