# (REFERENCE ONLY)
# Script name: verificationCleaning.R
# Author: S. Bogen
#
# Requires verification data points for Ashley National Forest and Fishlake 
# National Forest. Converts RAW verification data points into CLEAN verification
# data points that have the same common standard projection as the covaraite 
# rasters.
#-------------------------------------------------------------------------------

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/0 DATA/verification data")

standardProj <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96",
                      "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0", 
                      "+units=m +no_defs")

# RE-PROJECT VERIFICATION DATA SETS---------------------------------------------

fishlake.nests <- readOGR("RAW//WildlifeGoshawkNest_20190416/WildlifeGoshawkNest_20190416.shp")
fishlake.nests.proj <- spTransform(fishlake.nests, CRS(standardProj))
writeOGR(obj = fishlake.nests.proj, dsn = "CLEAN/verification/fishlake.shp", 
         layer = "nests", driver = "ESRI Shapefile", overwrite = T)