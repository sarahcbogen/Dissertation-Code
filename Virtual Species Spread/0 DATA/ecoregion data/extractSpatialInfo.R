# Script name: extractSpatialInfo.R
# Author: S. Bogen
#
# Gets spatial data for all trait observations contributing to the final data
# set. This is a SLOW but nonetheless accurate and reproducible script.
#-------------------------------------------------------------------------------

library(sf)
library(dplyr)

setwd("~/Desktop/Dissertation Code/Virtual Species Spread/0 DATA/ecoregion data")

# Load in ecoregion SHAPEFILE with plant species richnesses taken from:
# https://databasin.org/datasets/43478f840ac84173979b22631c2ed672/

path <- file.path("../reference data", 
                  "Number of Plant Species by Terrestrial Ecoregion",
                  "data/commondata/data0/wwf_ecos_plant_spcs.shp")
plantRichSHP <- st_read(path)
plantRichDF <- st_drop_geometry(plantRichSHP)[, c("ECO_NAME", "ECO_ID", 
                                                  "ECO_CODE", "plant_spcs")]
plantRichREF <- distinct(plantRichDF)
rm(path)

# View reference data
barplot(plantRichREF$plant_spcs[plantRichREF$plant_spcs>0], ylab = "richness")

#-------------------------------------------------------------------------------
# Lambda observation data
#-------------------------------------------------------------------------------

load("../observation data/COMPADRE Data/cleanedCOMPADRE.Rdata")

# Add empty columns to data frame
allMatrixFrame$ECO_NAME <- rep(NA, 3013)
allMatrixFrame$ECO_ID <- rep(NA, 3013)
allMatrixFrame$ECO_CODE <- rep(NA, 3013)

numNA <-0

for (i in 1:3013){
  
  if (!is.na(allMatrixFrame[i, "Lon"]) & !is.na(allMatrixFrame[i, "Lat"])){
    
    pt <- st_point(c(allMatrixFrame[i, "Lon"], allMatrixFrame[i, "Lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      allMatrixFrame$ECO_NAME[i] <- "empty"
      allMatrixFrame$ECO_ID[i] <- -1
      allMatrixFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      allMatrixFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      allMatrixFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      allMatrixFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  print(i)
  
}

save(allMatrixFrame, file = "observations with ecoregion/lambdaOBS.RData")
rm(allMatrixFrame)

#-------------------------------------------------------------------------------
# Dispersal mode observation data
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")

# Add empty columns to data frame
dispersalmodeObsFrame$ECO_NAME <- rep(NA, 2280)
dispersalmodeObsFrame$ECO_ID <- rep(NA, 2280)
dispersalmodeObsFrame$ECO_CODE <- rep(NA, 2280)

numNA <-0

for (i in 1:2280){
  
  if (!is.na(dispersalmodeObsFrame[i, "lon"]) & !is.na(dispersalmodeObsFrame[i, "lat"])){
    
    pt <- st_point(c(dispersalmodeObsFrame[i, "lon"], dispersalmodeObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      dispersalmodeObsFrame$ECO_NAME[i] <- "empty"
      dispersalmodeObsFrame$ECO_ID[i] <- -1
      dispersalmodeObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      dispersalmodeObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      dispersalmodeObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      dispersalmodeObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  print(i)
  
}

save(dispersalmodeObsFrame, file = "observations with ecoregion/dmOBS.RData")
rm(dispersalmodeObsFrame)

#-------------------------------------------------------------------------------
# Dispersal mode observation data
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)

# Add empty columns to data frame
leafAreaObsFrame$ECO_NAME <- rep(NA, 16614)
leafAreaObsFrame$ECO_ID <- rep(NA, 16614)
leafAreaObsFrame$ECO_CODE <- rep(NA, 16614)

numNA <-0

for (i in 2281:16614){
  
  if (!is.na(leafAreaObsFrame[i, "lon"]) & !is.na(leafAreaObsFrame[i, "lat"])){
    
    pt <- st_point(c(leafAreaObsFrame[i, "lon"], leafAreaObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      leafAreaObsFrame$ECO_NAME[i] <- "empty"
      leafAreaObsFrame$ECO_ID[i] <- -1
      leafAreaObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      leafAreaObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      leafAreaObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      leafAreaObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  print(i)
  
}

save(leafAreaObsFrame, file = "observations with ecoregion/laOBS.RData")
rm(leafAreaObsFrame)
rm(foo, bar)

#-------------------------------------------------------------------------------
# Leaf nitrogen observation data
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)

# Add empty columns to data frame
leafNObsFrame$ECO_NAME <- rep(NA, 42181)
leafNObsFrame$ECO_ID <- rep(NA, 42181)
leafNObsFrame$ECO_CODE <- rep(NA, 42181)

numNA <-0

for (i in 1:42181){
  
  if (!is.na(leafNObsFrame[i, "lon"]) & !is.na(leafNObsFrame[i, "lat"])){
    
    pt <- st_point(c(leafNObsFrame[i, "lon"], leafNObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      leafNObsFrame$ECO_NAME[i] <- "empty"
      leafNObsFrame$ECO_ID[i] <- -1
      leafNObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      leafNObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      leafNObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      leafNObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(leafNObsFrame, file = "observations with ecoregion/leafNOBS.RData")
rm(foo, bar)
rm(leafNObsFrame)

#-------------------------------------------------------------------------------
# PH Gen observation data
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)
rm(leafNObsFrame)

# Add empty columns to data frame
PHgenObsFrame$ECO_NAME <- rep(NA, 7335)
PHgenObsFrame$ECO_ID <- rep(NA, 7335)
PHgenObsFrame$ECO_CODE <- rep(NA, 7335)

numNA <-0

for (i in 1:7335){
  
  if (!is.na(PHgenObsFrame[i, "lon"]) & !is.na(PHgenObsFrame[i, "lat"])){
    
    pt <- st_point(c(PHgenObsFrame[i, "lon"], PHgenObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      PHgenObsFrame$ECO_NAME[i] <- "empty"
      PHgenObsFrame$ECO_ID[i] <- -1
      PHgenObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      PHgenObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      PHgenObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      PHgenObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(PHgenObsFrame, file = "observations with ecoregion/PHgenOBS.RData")
rm(foo, bar)
rm(PHgenObsFrame)

#-------------------------------------------------------------------------------
# PH Veg observation data
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)
rm(leafNObsFrame)
rm(PHgenObsFrame)

# Add empty columns to data frame
PHvegObsFrame$ECO_NAME <- rep(NA, 39438)
PHvegObsFrame$ECO_ID <- rep(NA, 39438)
PHvegObsFrame$ECO_CODE <- rep(NA, 39438)

numNA <- 0

for (i in 1:39438){
  
  if (!is.na(PHvegObsFrame[i, "lon"]) & !is.na(PHvegObsFrame[i, "lat"])){
    
    pt <- st_point(c(PHvegObsFrame[i, "lon"], PHvegObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      PHvegObsFrame$ECO_NAME[i] <- "empty"
      PHvegObsFrame$ECO_ID[i] <- -1
      PHvegObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      PHvegObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      PHvegObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      PHvegObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(PHvegObsFrame, file = "observations with ecoregion/PHvegOBS.RData")
rm(foo, bar)
rm(PHvegObsFrame)

#-------------------------------------------------------------------------------
# Rooting Depth
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)
rm(leafNObsFrame)
rm(PHgenObsFrame)
rm(PHvegObsFrame)

# Add empty columns to data frame
rootingdepthObsFrame$ECO_NAME <- rep(NA, 1498)
rootingdepthObsFrame$ECO_ID <- rep(NA, 1498)
rootingdepthObsFrame$ECO_CODE <- rep(NA, 1498)

numNA <- 0

for (i in 1:1498){
  
  if (!is.na(rootingdepthObsFrame[i, "lon"]) & !is.na(rootingdepthObsFrame[i, "lat"])){
    
    pt <- st_point(c(rootingdepthObsFrame[i, "lon"], rootingdepthObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      rootingdepthObsFrame$ECO_NAME[i] <- "empty"
      rootingdepthObsFrame$ECO_ID[i] <- -1
      rootingdepthObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      rootingdepthObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      rootingdepthObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      rootingdepthObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(rootingdepthObsFrame, file = "observations with ecoregion/rdOBS.RData")
rm(foo, bar)
rm(rootingdepthObsFrame)

#-------------------------------------------------------------------------------
# Specific Leaf Area
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)
rm(leafNObsFrame)
rm(PHgenObsFrame)
rm(PHvegObsFrame)
rm(rootingdepthObsFrame)

# Add empty columns to data frame
slaObsFrame$ECO_NAME <- rep(NA, 32900)
slaObsFrame$ECO_ID <- rep(NA, 32900)
slaObsFrame$ECO_CODE <- rep(NA, 32900)

numNA <- 0

for (i in 1:32900){
  
  if (!is.na(slaObsFrame[i, "lon"]) & !is.na(slaObsFrame[i, "lat"])){
    
    pt <- st_point(c(slaObsFrame[i, "lon"], slaObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      slaObsFrame$ECO_NAME[i] <- "empty"
      slaObsFrame$ECO_ID[i] <- -1
      slaObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      slaObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      slaObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      slaObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(slaObsFrame, file = "observations with ecoregion/slaOBS.RData")
rm(foo, bar)
rm(slaObsFrame)

#-------------------------------------------------------------------------------
# Wood Density
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)
rm(leafNObsFrame)
rm(PHgenObsFrame)
rm(PHvegObsFrame)
rm(rootingdepthObsFrame)
rm(slaObsFrame)

# Add empty columns to data frame
wdObsFrame$ECO_NAME <- rep(NA, 9902)
wdObsFrame$ECO_ID <- rep(NA, 9902)
wdObsFrame$ECO_CODE <- rep(NA, 9902)

numNA <- 0

for (i in 1:9902){
  
  if (!is.na(wdObsFrame[i, "lon"]) & !is.na(wdObsFrame[i, "lat"])){
    
    pt <- st_point(c(wdObsFrame[i, "lon"], wdObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      wdObsFrame$ECO_NAME[i] <- "empty"
      wdObsFrame$ECO_ID[i] <- -1
      wdObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      wdObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      wdObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      wdObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(wdObsFrame, file = "observations with ecoregion/wdOBS.RData")
rm(foo, bar)
rm(wdObsFrame)

#-------------------------------------------------------------------------------
# Seed Mass
#-------------------------------------------------------------------------------

load("../observation data/TRY Data/CLEAN/observations.Rdata")
rm(dispersalmodeObsFrame)
rm(leafAreaObsFrame)
rm(leafNObsFrame)
rm(PHgenObsFrame)
rm(PHvegObsFrame)
rm(rootingdepthObsFrame)
rm(slaObsFrame)
rm(wdObsFrame)

# Add empty columns to data frame
seedmassObsFrame$ECO_NAME <- rep(NA, 12911)
seedmassObsFrame$ECO_ID <- rep(NA, 12911)
seedmassObsFrame$ECO_CODE <- rep(NA, 12911)

numNA <- 0

for (i in 9903:12911){
  
  if (!is.na(seedmassObsFrame[i, "lon"]) & !is.na(seedmassObsFrame[i, "lat"])){
    
    pt <- st_point(c(seedmassObsFrame[i, "lon"], seedmassObsFrame[i, "lat"]))
    foo <- st_within(pt, plantRichSHP)
    bar <- plantRichDF[foo[[1]], ]
    
    if(length(unlist(foo))==0){
      
      # No ecoregion returned. Fill in as such.
      seedmassObsFrame$ECO_NAME[i] <- "empty"
      seedmassObsFrame$ECO_ID[i] <- -1
      seedmassObsFrame$ECO_CODE[i] <- "empty"
      
    } else{
      
      # Valid ecoregion detected. Collect info.
      seedmassObsFrame$ECO_NAME[i] <- plantRichDF[foo[[1]], "ECO_NAME"]
      seedmassObsFrame$ECO_ID[i] <- plantRichDF[foo[[1]], "ECO_ID"]
      seedmassObsFrame$ECO_CODE[i] <- plantRichDF[foo[[1]], "ECO_CODE"]
      
    }
    
  } else {
    numNA <- numNA + 1
  }
  
  if (i %% 100 == 0) {print(i)}
  
}

save(seedmassObsFrame, file = "observations with ecoregion/smOBS.RData")
rm(foo, bar)
rm(wdObsFrame)
