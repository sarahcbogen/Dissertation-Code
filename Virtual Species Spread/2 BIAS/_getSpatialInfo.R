# Script name: getSpatialInfo.R
# Author: S. Bogen
#
# Examines the data set used to create the statistical model and simulate
# everything. Creates a separate data frame of all observations that contribute
# to that data set and their ecoregions.
#
# Output is spatialInfo.Rdata
#-------------------------------------------------------------------------------

library(sf)
library(dplyr)
library(tidyr)

setwd("~/Desktop/Dissertation Code/Virtual Species Spread/2 BIAS")

# Read in dataset used for the GLMM:
load("../0 DATA/observation data/Data_VirtualSpecies_Gaussian_Bullock_2022_ver2.Rdata")
source('../1 SUPPORT CODE/DataPrep.R')
data.all <- getdata(DispersalKernel = "Gaussian",
                    IncResponseTraits = c("leafN.Mean.Logtrans",
                                          "LeafArea.Mean.Logtrans", 
                                          "SLA.Mean.Logtrans",
                                          "rootingdepth.Mean.Logtrans", 
                                          "WD.Mean.Logtrans"))
datMinst <- data.all$datMinst

# load in info to be used to look up ecoregion info
load("../0 DATA/spatial data/observations with ecoregion/dmOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/leafNOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/laOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/slaOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/rdOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/wdOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/lambdaOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/PHgenOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/PHvegOBS.RData")
load("../0 DATA/spatial data/observations with ecoregion/smOBS.RData")

# datMinst is what was used for the statistical model. However, we need some of
# the info for dataVS in order to do this.

# Loop through datMinst to get the species name, use dataVS to look up key info
# Need from datMinst: which columns are not NA. Go through each column.

# Each line of new data set should have Species name, ECO_NAME, ECO_ID, ECO_CODE
# There should be NO NAs under ObservationID

dataSetObservations <- data.frame()

for(i in 1:2387){

currentSpec <- datMinst$Species[i]

# DS_Bullock - dispersalmodeObsFrame
# get max, with order from highest to lowest being vertebrate, wind, ant, 
#          autochory, unassisted

# ALL species in data set should have a dispersal mode
if(is.na(datMinst[i, "DS_Bullock"])){
  print("Yikes, there is a problem with dispersal mode!")
} else{
  dsCont <- dispersalmodeObsFrame[dispersalmodeObsFrame$acceptedbinomial == currentSpec, 
                                  c("acceptedbinomial", "ECO_NAME", "ECO_ID", "ECO_CODE")]
}

# GF - none. Also, ALL species in data set should have a GF

# leafN.Mean.Logtrans - leafNObsFrame - get all
if(!is.na(datMinst[i, "leafN.Mean.Logtrans"])){
  leafNCont <- leafNObsFrame[leafNObsFrame$acceptedbinomial == currentSpec, 
                          c("acceptedbinomial", "ECO_NAME", "ECO_ID", "ECO_CODE")]
} else{
  leafNCont <- data.frame()
}

# LeafArea.Mean.Logtrans - leafAreaObsFrame - get all
if(!is.na(datMinst[i, "LeafArea.Mean.Logtrans"])){
  leafAreaCont <- leafAreaObsFrame[leafAreaObsFrame$acceptedbinomial == currentSpec, 
                             c("acceptedbinomial", "ECO_NAME", "ECO_ID", "ECO_CODE")]
} else{
  leafAreaCont <- data.frame()
}

# SLA.Mean.Logtrans - slaObsFrame - get all
if(!is.na(datMinst[i, "SLA.Mean.Logtrans"])){
  slaCont <- slaObsFrame[slaObsFrame$acceptedbinomial == currentSpec, 
                                   c("acceptedbinomial", "ECO_NAME", "ECO_ID", "ECO_CODE")]
} else{
  slaCont <- data.frame()
}

# rootingdepth.Mean.Logtrans - rootingdepthObsFrame - get all
if(!is.na(datMinst[i, "rootingdepth.Mean.Logtrans"])){
  rdCont <- rootingdepthObsFrame[rootingdepthObsFrame$Species == currentSpec, 
                         c("Species", "ECO_NAME", "ECO_ID", "ECO_CODE")]
  names(rdCont)[1] <- "acceptedbinomial"
} else{
  rdCont <- data.frame()
}

# WD.Mean.Logtrans - wdObsFrame - get all
if(!is.na(datMinst[i, "WD.Mean.Logtrans"])){
  wdCont <- wdObsFrame[wdObsFrame$acceptedbinomial == currentSpec, 
                                 c("acceptedbinomial", "ECO_NAME", "ECO_ID", "ECO_CODE")]
} else{
  wdCont <- data.frame()
}

# logGaussian2D - none

# logMaxLambda - allMatrixFrame - get max
if(!is.na(datMinst[i, "logMaxLambda"])){
  lambdaCont <- allMatrixFrame[allMatrixFrame$acceptedbinomial == currentSpec, 
                       c("Species", "ECO_NAME", "ECO_ID", "ECO_CODE")]
  names(lambdaCont)[1] <- "acceptedbinomial"
} else{
  lambdaCont <- data.frame()
}

currentAllObs <- rbind(dsCont, leafNCont, leafAreaCont, slaCont, rdCont,
                       wdCont, lambdaCont)

dataSetObservations <- rbind(dataSetObservations, currentAllObs)

print(i)

}

# clean up space
rm(lambdaCont, leafAreaCont, leafNCont, rdCont, dsCont, wdCont, slaCont)
rm(allMatrixFrame, dispersalmodeObsFrame, leafAreaObsFrame, leafNObsFrame,
   PHgenObsFrame, PHvegObsFrame, rootingdepthObsFrame, seedmassObsFrame,
   slaObsFrame, wdObsFrame,
   dispersalmodes_unique_datasets)
rm(dataVS, datMinst, data.all)

#-------------------------------------------------------------------------------
# Now calculate species richness by ecoregion and compare to ref data
#-------------------------------------------------------------------------------

# dataSetObservations is the set of observations in data set

# get reference data
shpFile <- file.path("../0 DATA/reference data/",
                     "Number of Plant Species by Terrestrial Ecoregion",
                     "data/commondata/data0/wwf_ecos_plant_spcs.shp")
plantRichSHP <- st_read(shpFile)
plantRichDF <- st_drop_geometry(plantRichSHP)[, c("REALM", "BIOME", "ECO_NAME", "ECO_ID", 
                                                  "ECO_CODE", "plant_spcs")]
plantRichREF <- distinct(plantRichDF)

# clean up space
rm(plantRichSHP, plantRichDF)

barplot(plantRichREF$plant_spcs[plantRichREF$plant_spcs>0], ylab = "richness")

# Use ECO_CODE

result <- dataSetObservations %>%
  group_by(ECO_CODE) %>%
  summarise(distinct_acceptedbinomial = n_distinct(acceptedbinomial))

dataRichness <- merge.data.frame(plantRichREF, result, by = "ECO_CODE", all.x = TRUE)
dataRichness <- mutate(dataRichness, distinct_acceptedbinomial = replace_na(distinct_acceptedbinomial, 0))

#-------------------------------------------------------------------------------
# All info is in one frame now - dataRichness.
# The column "distinct_acceptedbinomial" is the richness within the dataset
# The column "plant_spcs" is the reference value.
#-------------------------------------------------------------------------------

barplot(dataRichness$plant_spcs[dataRichness$plant_spcs>=0], ylab = "richness")
barplot(dataRichness$distinct_acceptedbinomial*20, col = "red", border = "red", 
        add = TRUE, ylim = c(0, 200))


# sort dataRichness by biome
dataRichness <- dataRichness[order(dataRichness$BIOME), ]
par(mfrow=c(2, 1))
barplot(dataRichness$plant_spcs[dataRichness$plant_spcs>=0], ylab = "richness")
barplot(dataRichness$distinct_acceptedbinomial, col = "red", border = "red", 
        ylab = "richness")

color_palette <- c("1" = "forestgreen", # Tropical and subtropical moist broadleaf forests
                   "2" = "green", # Tropical and subtropical dry broadleaf forests
                   "3" = "blue", # Tropical and subtropical coniferous forests
                   "4" = "lightblue", # Temperate broadleaf and mixed forests
                   "5" = "gray", # Temperate coniferous forests
                   "6" = "darkgray", # Boreal forests/taiga
                   "7" = "darkorange", # Tropical and subtropical grasslands, savannas and shrublands
                   "8" = "red", # Temperate grasslands, savannas and shrublands
                   "9" = "darkred", # Flooded grasslands and savannas
                   "10" = "purple", # Montane grasslands and shrublands
                   "11" = "violet", # Tundra
                   "12" = "cyan", # Mediterranean forests, woodlands, and scrub
                   "13" = "brown", # Deserts and xeric shrublands
                   "14" = "pink") # Mangroves


dataRichness$BIOME <- factor(dataRichness$BIOME, levels = names(color_palette))

# Create the barplot with different colors for each BIOME
barplot(dataRichness$plant_spcs[dataRichness$plant_spcs >= 0], 
        col = color_palette[dataRichness$BIOME],
        border = color_palette[dataRichness$BIOME],
        ylab = "richness",
        main = "Reference Data by Biome")
barplot(dataRichness$distinct_acceptedbinomial, ylab = "richness")

#---------------------------------------------------------

# Group by Realm
result_df <- dataRichness %>%
  group_by(REALM) %>%
  summarise(total_plant_spcs = sum(plant_spcs))
another_result_df <- dataRichness %>%
  group_by(REALM) %>%
  summarise(total_accepted = sum(distinct_acceptedbinomial))
richByREALM <- merge.data.frame(result_df, another_result_df)[-9, ]
rm(result_df, another_result_df)

# Group by Biome
result_df <- dataRichness %>%
  group_by(BIOME) %>%
  summarise(total_plant_spcs = sum(plant_spcs))
another_result_df <- dataRichness%>%
  group_by(BIOME) %>%
  summarise(total_accepted = sum(distinct_acceptedbinomial))
richByBIOME <- merge.data.frame(result_df, another_result_df)
rm(result_df, another_result_df)

# group expected richness by both realm and biome
REALM_BIOME <- paste(dataRichness$REALM, dataRichness$BIOME, sep = "_")
newDF <- cbind(REALM_BIOME, dataRichness)
result_df <- newDF %>%
  group_by(REALM_BIOME) %>%
  summarise(total_plant_spcs = sum(plant_spcs))
another_result_df <- newDF %>%
  group_by(REALM_BIOME) %>%
  summarise(total_accepted = sum(distinct_acceptedbinomial))
richByREALM_BIOME <- merge.data.frame(result_df, another_result_df)
rm(newDF, result_df, another_result_df)

#-------------------------------------------------------------------------------

# spatialREADY.RDatawill now have information on expected 
save(dataSetObservations, richByREALM, richByBIOME, richByREALM_BIOME, 
     file = "spatialREADY.RData")





