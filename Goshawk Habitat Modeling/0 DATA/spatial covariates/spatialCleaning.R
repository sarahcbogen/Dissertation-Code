# (REFERENCE ONLY)
# Script name: spatialCleaning.R
# Author: S. Bogen
#
# Pre-condition: boundaryCleaning.R and spatialHalfbake.R have already been run
#
# Reads in covariates from the RAW and HALFBAKED sub-directories. Reprojects,
# resamples, and crops covariate rasters to the extent of the Ashley National
# Forest and Fishlake National Forest. Saves cleaned rasters as geotiff files
# to the CLEAN sub-directory.
#-------------------------------------------------------------------------------

library(sp)
library(rgdal)
library(raster)
library(landsat)

# Set working directory
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/0 DATA/spatial covariates")

#-------------------------------------------------------------------------------

standardRes <- c(250, 250)
standardProj <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96",
                      "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0", 
                      "+units=m +no_defs")

# READ IN PROJECTED BOUNDARIES
utah_forests.proj <- readOGR("../spatial boundaries/CLEAN/UtahForests.shp")

# Get individual projected forest boundaries
ashley.proj <- utah_forests.proj[utah_forests.proj@data$FORESTNAME
                                 =="Ashley National Forest", ]
fishlake.proj <- utah_forests.proj[utah_forests.proj@data$FORESTNAME
                                   =="Fishlake National Forest", ]

# -Read in enviornmental rasters-------------------------------------------------

forestType <- raster("RAW/COVARIATES/conus_forest-type/conus_foresttype.img")
canopyCover <- raster("RAW/COVARIATES/usfs_carto_CONUS_2016/usfs_2016_treecanopy_cartographic_12-14-2018.img")
standAge <- raster("RAW/COVARIATES/usgs_landcarbon/fsage_a1b_2010.tif")
canopyBulkDensity <- raster("RAW/COVARIATES/CanopyBulkDensity_CBD/us_140cbd/w001001.adf")
canopyBaseHeight <- raster("RAW/COVARIATES/CanopyBaseHeight_CBH/us_140cbh/w001001.adf")
basalArea <- raster("HALFBAKED/basalArea.tif")
elevation <- raster("HALFBAKED/elevation.tif")

# Standardize projection and resolution

forestType.proj <- projectRaster(forestType, res=standardRes, 
                                 crs=standardProj, method="ngb")
canopyCover.proj <- projectRaster(canopyCover, res=standardRes, 
                                 crs=standardProj, method="ngb")
standAge.proj <- projectRaster(standAge, res=standardRes, 
                                 crs=standardProj, method="ngb")
canopyBulkDensity.proj <- projectRaster(canopyBulkDensity, res=standardRes, 
                                 crs=standardProj, method="ngb")
canopyBaseHeight.proj <- projectRaster(canopyBaseHeight, res=standardRes, 
                                 crs=standardProj, method="ngb")
elevation.proj <- projectRaster(elevation, res=standardRes, 
                                crs=standardProj, method="ngb")
basalArea.proj <- projectRaster(basalArea, res=standardRes, 
                                crs=standardProj, method="ngb")

# Save reprojected covariates
writeRaster(forestType.proj, "HALFBAKED/forestType_proj.tif")
writeRaster(canopyCover.proj, "HALFBAKED/canopyCover_proj.tif")
writeRaster(standAge.proj, "HALFBAKED/standAge_proj.tif")
writeRaster(canopyBulkDensity.proj, "HALFBAKED/canopyBulkDensity_proj.tif")
writeRaster(canopyBaseHeight.proj, "HALFBAKED/canopyBaseHeight_proj.tif")
writeRaster(elevation.proj, "HALFBAKED/elevation_proj.tif")
writeRaster(basalArea.proj, "HALFBAKED/basalArea_proj.tif")

# snap to same grid as forestType via resample
canopyCover.resample <- resample(canopyCover.proj, forestType.proj, 
                                 method = "bilinear")
standAge.resample <- resample(standAge.proj, forestType.proj, 
                              method = "bilinear")
canopyBaseHeight.resample <- resample(canopyBaseHeight.proj, forestType.proj, 
                                      method = "bilinear")
canopyBulkDensity.resample <- resample(canopyBulkDensity.proj, forestType.proj, 
                                       method = "bilinear")
elevation.resample <- resample(elevation.proj, forestType.proj, 
                               method = "bilinear")
basalArea.resample <- resample(basalArea.proj, forestType.proj, 
                               method = "bilinear")

# Save resampled covariates
writeRaster(forestType.proj, "HALFBAKED/forestType_resample.tif")
writeRaster(canopyCover.resample, "HALFBAKED/canopyCover_resample.tif")
writeRaster(standAge.resample, "HALFBAKED/standAge_resample.tif")
writeRaster(canopyBaseHeight.resample, "HALFBAKED/canopyBaseHeight_resample.tif")
writeRaster(canopyBulkDensity.resample, "HALFBAKED/canopyBulkDensity_resample.tif")
writeRaster(elevation.resample, "HALFBAKED/elevation_resample.tif")
writeRaster(basalArea.resample, "HALFBAKED/basalArea_resample.tif")

#-------------------------------------------------------------------------------

# Read in full, projected rasters
forestType.proj <- raster("HALFBAKED/forestType_proj.tif")
canopyCover.proj <- raster("HALFBAKED/canopyCover_proj.tif")
standAge.proj <- raster("HALFBAKED/standAge_proj.tif")
canopyBulkDensity.proj <- raster("HALFBAKED/canopyBulkDensity_proj.tif")
canopyBaseHeight.proj <- raster("HALFBAKED/canopyBaseHeight_proj.tif")
elevation.proj <- raster("HALFBAKED/elevation_proj.tif")
basalArea.proj <- raster("HALFBAKED/basalArea_proj.tif")

# Read in full, resampled rasters
forestType.resample <- raster("HALFBAKED/forestType_resample.tif")
canopyCover.resample <- raster("HALFBAKED/canopyCover_resample.tif")
standAge.resample <- raster("HALFBAKED/standAge_resample.tif")
canopyBaseHeight.proj <- raster("HALFBAKED/canopyBaseHeight_resample.tif")
canopyBulkDensity.proj <- raster("HALFBAKED/canopyBulkDensity_resample.tif")
elevation.resample <- raster("HALFBAKED/elevation_resample.tif")
basalArea.resample <- raster("HALFBAKED/basalArea_resample.tif")

#-------------------------------------------------------------------------------
# Utah National Forests
#-------------------------------------------------------------------------------

# Crop the first to the extent of the Utah National forests boundary
forestType.uf <- crop(forestType.resample, as.vector(extent(utah_forests.proj)))

# Get the raster extent
ufExtent <- as.vector(extent(forestType.uf))

# Crop the rest
canopyCover.uf <- crop(canopyCover.resample, ufExtent)
standAge.uf <- crop(standAge.resample, ufExtent)
canopyBaseHeight.uf <- crop(canopyBaseHeight.resample, ufExtent)
canopyBulkDensity.uf <- crop(canopyBulkDensity.resample, ufExtent)
elevation.uf <- crop(elevation.resample, ufExtent)
basalArea.uf <- crop(basalArea.resample, ufExtent)

# Derive Slope and Aspect
aspect.uf.grid <- slopeasp(as(elevation.uf, "SpatialGridDataFrame"))$aspect
aspect.uf <- as(aspect.uf.grid, "RasterLayer")
slope.uf.grid <- slopeasp(as(elevation.uf, "SpatialGridDataFrame"))$slope
slope.uf <- as(aspect.uf.grid, "RasterLayer")

# write UtahNF Raster files
writeRaster(forestType.uf, "CLEAN/UF/ForestTypeUF.tif", overwrite=TRUE)
writeRaster(canopyCover.uf, "CLEAN/UF/CanopyCoverUF.tif", overwrite=TRUE)
writeRaster(standAge.uf, "CLEAN/UF/StandAgeUF.tif", overwrite=TRUE)
writeRaster(canopyBaseHeight.uf, "CLEAN/UF/CanopyBaseHeightUF.tif", 
            overwrite=TRUE)
writeRaster(canopyBulkDensity.uf, "CLEAN/UF/CanopyBulkDensityUF.tif", 
            overwrite=TRUE)
writeRaster(basalArea.uf, "CLEAN/UF/BasalAreaUF.tif", overwrite=TRUE)
writeRaster(elevation.uf, "CLEAN/UF/ElevationUF.tif", overwrite=TRUE)
writeRaster(slope.uf, "CLEAN/UF/SlopeUF.tif", overwrite=TRUE)
writeRaster(aspect.uf, "CLEAN/UF/AspectUF.tif", overwrite=TRUE)

#-------------------------------------------------------------------------------
# Ashley National Forest
#-------------------------------------------------------------------------------

# Crop the first to the extent of the Ashley boundary
forestType.ashley <- crop(forestType.resample, as.vector(extent(ashley.proj)))

# Get the raster extent
ashleyExtent <- as.vector(extent(forestType.ashley))

# Crop the rest
canopyCover.ashley <- crop(canopyCover.resample, ashleyExtent)
standAge.ashley <- crop(standAge.resample, ashleyExtent)
canopyBaseHeight.ashley <- crop(canopyBaseHeight.resample, ashleyExtent)
canopyBulkDensity.ashley <- crop(canopyBulkDensity.resample, ashleyExtent)
elevation.ashley <- crop(elevation.resample, ashleyExtent)
basalArea.ashley <- crop(basalArea.resample, ashleyExtent)

# Derive Slope and Aspect
aspect.ashley.grid <- slopeasp(as(elevation.ashley, "SpatialGridDataFrame"))$aspect
aspect.ashley <- as(aspect.ashley.grid, "RasterLayer")
slope.ashley.grid <- slopeasp(as(elevation.ashley, "SpatialGridDataFrame"))$slope
slope.ashley <- as(aspect.ashley.grid, "RasterLayer")

# write Ashley Raster files
writeRaster(forestType.ashley, "CLEAN/ASHLEY/ForestTypeAshley.tif", overwrite=TRUE)
writeRaster(canopyCover.ashley, "CLEAN/ASHLEY/CanopyCoverAshley.tif", overwrite=TRUE)
writeRaster(standAge.ashley, "CLEAN/ASHLEY/StandAgeAshley.tif", overwrite=TRUE)
writeRaster(canopyBaseHeight.ashley, "CLEAN/ASHLEY/CanopyBaseHeightAshley.tif", overwrite=TRUE)
writeRaster(canopyBulkDensity.ashley, "CLEAN/ASHLEY/CanopyBulkDensityAshley.tif", overwrite=TRUE)
writeRaster(basalArea.ashley, "CLEAN/ASHLEY/BasalAreaAshley.tif", overwrite=TRUE)
writeRaster(elevation.ashley, "CLEAN/ASHLEY/ElevationAshley.tif", overwrite=TRUE)
writeRaster(slope.ashley, "CLEAN/ASHLEY/SlopeAshley.tif", overwrite=TRUE)
writeRaster(aspect.ashley, "CLEAN/ASHLEY/AspectAshley.tif", overwrite=TRUE)

#-------------------------------------------------------------------------------
# Fishlake National Forest
#-------------------------------------------------------------------------------

# Crop the first to the extent of the Fishlake boundary
forestType.fishlake <- crop(forestType.resample, as.vector(extent(fishlake.proj)))

# Get the raster extent
fishlakeExtent <- as.vector(extent(forestType.fishlake))

# Crop the rest
canopyCover.fishlake <- crop(canopyCover.resample, fishlakeExtent)
canopyCover.fishlake <- crop(canopyCover.resample, fishlakeExtent)
standAge.fishlake <- crop(standAge.resample, fishlakeExtent)
canopyBaseHeight.fishlake <- crop(canopyBaseHeight.resample, fishlakeExtent)
canopyBulkDensity.fishlake <- crop(canopyBulkDensity.resample, fishlakeExtent)
elevation.fishlake <- crop(elevation.resample, fishlakeExtent)
basalArea.fishlake <- crop(basalArea.resample, fishlakeExtent)

# Derive Slope and Aspect
aspect.fishlake.grid <- slopeasp(as(elevation.fishlake, "SpatialGridDataFrame"))$aspect
aspect.fishlake <- as(aspect.fishlake.grid, "RasterLayer")
slope.fishlake.grid <- slopeasp(as(elevation.fishlake, "SpatialGridDataFrame"))$slope
slope.fishlake <- as(aspect.fishlake.grid, "RasterLayer")

# write Fishlake Raster files
writeRaster(forestType.fishlake, "CLEAN/FISHLAKE/ForestTypeFishlake.tif", overwrite=TRUE)
writeRaster(canopyCover.fishlake, "CLEAN/FISHLAKE/CanopyCoverFishlake.tif", overwrite=TRUE)
writeRaster(standAge.fishlake, "CLEAN/FISHLAKE/StandAgeFishlake.tif", overwrite=TRUE)
writeRaster(canopyBaseHeight.fishlake, "CLEAN/FISHLAKE/CanopyBaseHeightFishlake.tif", overwrite=TRUE)
writeRaster(canopyBulkDensity.fishlake, "CLEAN/FISHLAKE/CanopyBulkDensityFishlake.tif", overwrite=TRUE)
writeRaster(basalArea.fishlake, "CLEAN/FISHLAKE/BasalAreaFishlake.tif", overwrite=TRUE)
writeRaster(elevation.fishlake, "CLEAN/FISHLAKE/ElevationFishlake.tif", overwrite=TRUE)
writeRaster(slope.fishlake, "CLEAN/FISHLAKE/SlopeFishlake.tif", overwrite=TRUE)
writeRaster(aspect.fishlake, "CLEAN/FISHLAKE/AspectFishlake.tif", overwrite=TRUE)




