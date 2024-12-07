# (REFERENCE ONLY)
# File name: spatialHalfbake.R
# Author: S. Bogen
#
# Assembles full-extent rasters for elevation and basal area and saves new
# raster files to HALFBAKED/elevation.tif and HALFBAKED/basalArea.tif
#
# Note: This takes a while. Stronger-than-a-laptop computing power recommended.
#-------------------------------------------------------------------------------

library(rgdal)
library(raster)
library(landsat)

# Set working directory
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/0 DATA/spatial covariates")

#-------------------------------------------------------------------------------
# Elevation
#-------------------------------------------------------------------------------

# Get list of raw elevation raster files
dem.dirs <- list.dirs("RAW/COVARIATES/DEM/dem_tifs", recursive="FALSE")
dem.files <- c()
for (dir in dem.dirs) {
  dem.files <- c(dem.files, file.path(dir, list.files(dir)[1]))
}

# Read in all raw elevation raster files
dem.rasts <- c()
for (file in dem.files) {
  dem.rasts <- c(dem.rasts, raster(file))
}

# Merge together raw elevation raster files
elevation.full <- merge(dem.rasts[[1]], dem.rasts[[2]], dem.rasts[[3]],
                        dem.rasts[[4]], dem.rasts[[5]], dem.rasts[[6]],
                        dem.rasts[[7]], dem.rasts[[8]], dem.rasts[[9]], 
                        dem.rasts[[10]], dem.rasts[[11]], dem.rasts[[12]],
                        dem.rasts[[13]], dem.rasts[[14]], dem.rasts[[15]],
                        dem.rasts[[16]], dem.rasts[[17]], dem.rasts[[18]], 
                        dem.rasts[[19]], dem.rasts[[20]], dem.rasts[[21]],
                        dem.rasts[[22]], dem.rasts[[23]], dem.rasts[[24]],
                        dem.rasts[[25]], dem.rasts[[26]], dem.rasts[[27]],
                        dem.rasts[[28]], dem.rasts[[29]], dem.rasts[[30]], 
                        dem.rasts[[31]], dem.rasts[[32]])

slope.rasts <- c()
aspect.rasts <- c()
for (i in 1:32){
  
  elevation_grid <- as(dem.rasts[[i]], "SpatialGridDataFrame")
  slopeasp_grid <- slopeasp(elevation_grid)
  aspect_grid <- slopeasp_grid$aspect
  slope_grid <- slopeasp_grid$slope
  aspect_raster <- as(aspect_grid, 'RasterLayer')
  slope_raster <- as(slope_grid, 'RasterLayer')
  
  slope.rasts <- c(slope.rasts, slope_raster)
  aspect.rasts <- c(aspect.rasts, aspect_raster)
  
  print(i)
}

# Save full elevation raster to the HALFBAKED subdirectory
writeRaster(elevation.full, "HALFBAKED/elevation.tif", overwrite = TRUE)

#-------------------------------------------------------------------------------
# Basal Area
#-------------------------------------------------------------------------------

dir <- "RAW/COVARIATES/CONUS_Basal_Area/Data/RasterMaps"
dir2 <- "RAW/COVARIATES/CONUS_Basal_Area/Data 2/RasterMaps"
dir3 <- "RAW/COVARIATES/CONUS_Basal_Area/Data 3/RasterMaps"
dir4 <- "RAW/COVARIATES/CONUS_Basal_Area/Data 4/RasterMaps"

ba.files1 <- list.files(dir)
ba.files2 <- list.files(dir2)
ba.files3 <- list.files(dir3)
ba.files4 <- list.files(dir4)

# Read in basal area raster files from data directory 1
ba.stack1 <- stack()
for (file in ba.files1) {
  ba.stack1 <- addLayer(ba.stack1, raster(file.path(dir, file)))
}

ba.stack2 <- stack()
for (file in ba.files2) {
  ba.stack2 <- addLayer(ba.stack2, raster(file.path(dir2, file)))
}

ba.stack3 <- stack()
for (file in ba.files3) {
  ba.stack3 <- addLayer(ba.stack3, raster(file.path(dir3, file)))
}

ba.stack4 <- stack()
for (file in ba.files4) {
  ba.stack4 <- addLayer(ba.stack4, raster(file.path(dir4, file)))
}

basalArea.sum.1 <- sum(ba.stack1)
basalArea.sum.2 <- sum(ba.stack2)
basalArea.sum.3 <- sum(ba.stack3)
basalArea.sum.4 <- sum(ba.stack4)
basalArea <- sum(basalArea.sum.1, basalArea.sum.2, 
                 basalArea.sum.3, basalArea.sum.4)

writeRaster(basalArea, "HALFBAKED/basalArea.tif", overwrite = TRUE)


