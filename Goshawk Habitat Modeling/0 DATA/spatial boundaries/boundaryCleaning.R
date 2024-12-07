# Script name: boundaryCleaning.R
# Author: S. Bogen
#
# Converts RAW boundaries into CLEAN boundaires that have a common standard
# projection.
#-------------------------------------------------------------------------------

# Set working directory
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/0 DATA/spatial boundaries")

#-------------------------------------------------------------------------------

standardProj <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96",
                      "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0", 
                      "+units=m +no_defs")

# Read in and subset boundary files
utah.full <- readOGR("RAW/Utah/Utah.shp")
utah <- utah.full[utah.full@data$STATE == "Utah", ]
forests <- readOGR("RAW/S_USA.AdministrativeForest.shp")
utah_forests <- forests[forests@data$FORESTNAME == "Ashley National Forest" |
                        forests@data$FORESTNAME == "Fishlake National Forest", ]

# Re-project boundaries
utah_forests.proj <- spTransform(utah_forests, CRS(standardProj))
utah.proj <- spTransform(utah, CRS(standardProj))

# Save re-projected boundaries
writeOGR(obj = utah_forests.proj, dsn = "CLEAN/UtahForests.shp", 
         layer = "boundaries", driver = "ESRI Shapefile", overwrite = T)
writeOGR(obj = utah.proj, dsn = "CLEAN/Utah.shp", layer = "utah", 
         driver = "ESRI Shapefile", overwrite = T)

# Clean up space
rm(forests, utah, utah_forests, utah_forests.proj,
   utah.full, utah.proj, standardProj)
