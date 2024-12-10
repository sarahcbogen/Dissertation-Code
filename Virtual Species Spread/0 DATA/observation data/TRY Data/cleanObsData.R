# Script name: cleanObsData.R
# Author: S. Bogen
#
# Each file called traitname2.Rdata contains the following R objects:
#   
#   "should give you the lat, long, and altitude covariates and an intermediate 
#    file called seedmass.unmanip.mature_reduced that should give you the 
#    accepted name used in the means file (“acceptedbinomial”) and the 
#    ObservationID to link to the spatial locations.
# 
# - traitname_mean, a data frame that should be identical to the corresponding 
#   data frame within the traitname.Rdata file.
# - traitname.cov.loc, a data frame containing the latitude, longitude, and 
#   altitude values for each observation
# - traitname.unmanip.mature_reduced, a data frame serving as an intermediate 
#   file that allows us to link the traitname_mean information to other 
#   information using the ObservationID.
# Species is the TNRS-resolved species name
#-------------------------------------------------------------------------------

library(dplyr)

setwd("~/Desktop/Dissertation Code/Virtual Species Spread/0 DATA/observation data/TRY Data")

#----------------------------------------------------------
# Process Seed mass data
#----------------------------------------------------------

load("RAW/seedmass2.Rdata")

# Want to create an observation matrix and a species matrix with matching
# species names.

#----Species matrix-------
seedmassSpeciesFrame <- seedmass_mean
rm(seedmass_mean)

#----Observation matrix------

# first get ObservationID and acceptedbinomial for merging - remove dups
# Duplicate issue reported to NB (Feb 14 2024 via Slack)
mergeKey <- distinct(seedmass.unmanip.mature_reduced[, c("ObservationID", 
                                                         "acceptedbinomial")])
rm(seedmass.unmanip.mature_reduced)

# get lon, lat, and alt info
latFrame <- seedmass.cov.loc[seedmass.cov.loc$DataName=="Latitude", 
                             c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- seedmass.cov.loc[seedmass.cov.loc$DataName=="Longitude", 
                             c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon"

# ONLY grab information with complete lat and lon info.
bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
length(unique(lonFrame$ObservationID))

moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
seedmassObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

rm(bar, moo, latFrame, lonFrame, mergeKey, seedmass.cov.loc)

#----------------------------------------------------------
# Process Leaf area data
#----------------------------------------------------------

load("RAW/LeafArea2.Rdata")

# SPECIES FRAME
leafAreaSpeciesFrame <- LeafArea_mean
rm(LeafArea_mean)

# OBSERVATION FRAME

mergeKey <- LeafArea.unmanip.mature_reduced[, c("ObservationID", 
                                                "acceptedbinomial")]
length(unique(mergeKey$ObservationID)) # no duplicate issues
rm(LeafArea.unmanip.mature_reduced)

latFrame <- LeafArea.cov.loc[LeafArea.cov.loc$DataName=="Latitude", 
                             c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- LeafArea.cov.loc[LeafArea.cov.loc$DataName=="Longitude", 
                             c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
leafAreaObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, LeafArea.cov.loc)

#----------------------------------------------------------
# Process Leaf N data TODO
#----------------------------------------------------------

load("RAW/LeafN2.Rdata")

# SPECIES FRAME
leafNSpeciesFrame <- leafN_mean
rm(leafN_mean)

# OBSERVATION FRAME

mergeKey <- leafN.unmanip.mature_reduced[, c("ObservationID", 
                                             "acceptedbinomial")]
length(unique(mergeKey$ObservationID)) # DUPLICATES! need to report to NB.
mergeKey <- distinct(leafN.unmanip.mature_reduced[, c("ObservationID", 
                                                      "acceptedbinomial")])
rm(leafN.unmanip.mature_reduced)

latFrame <- leafN.cov.loc[leafN.cov.loc$DataName=="Latitude", 
                             c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- leafN.cov.loc[leafN.cov.loc$DataName=="Longitude", 
                             c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
leafNObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, leafN.cov.loc, references)

#----------------------------------------------------------
# Process Rooting Depth data
#----------------------------------------------------------

load("RAW/rootingdepth2.Rdata")

# SPECIES FRAME
rootingdepthSpeciesFrame <- rootingdepth_mean
rm(rootingdepth_mean)

# OBSERVATION FRAME

mergeKey <- rootingdepth.unmanip.mature_reduced[, c("ObservationID", 
                                                    "acceptedbinomial")]
length(unique(mergeKey$ObservationID)) # No duplicates detected
rm(rootingdepth.unmanip.mature_reduced)

latFrame <- rootingdepth.cov.loc[rootingdepth.cov.loc$DataName=="Latitude", 
                             c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- rootingdepth.cov.loc[rootingdepth.cov.loc$DataName=="Longitude", 
                             c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
rootingdepthObsFrame <- moo[, c("ObservationID", "acceptedbinomial", 
                                "lat", "lon")]
names(rootingdepthObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, rootingdepth.cov.loc)

#----------------------------------------------------------
# Process Specific Leaf Area data
#----------------------------------------------------------

load("RAW/SLA2.Rdata")

# SPECIES FRAME
slaSpeciesFrame <- SLA_mean
rm(SLA_mean)

# OBSERVATION FRAME

mergeKey <- SLA.unmanip.mature_reduced[, c("ObservationID", "acceptedbinomial")]
length(unique(mergeKey$ObservationID))
rm(SLA.unmanip.mature_reduced)

latFrame <- SLA.cov.loc[SLA.cov.loc$DataName=="Latitude", 
                             c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- SLA.cov.loc[SLA.cov.loc$DataName=="Longitude", 
                             c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
slaObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, SLA.cov.loc, references)

#----------------------------------------------------------
# Process Wood Density data
#----------------------------------------------------------

load("RAW/WD2.Rdata")

# SPECIES FRAME
wdSpeciesFrame <- WD_mean
rm(WD_mean)

# OBSERVATION FRAME

mergeKey <- WD.unmanip.mature_reduced[, c("ObservationID", "acceptedbinomial")]
length(unique(mergeKey$ObservationID)) # UH OH also doesn't match - address issue

mergeKey <- distinct(WD.unmanip.mature_reduced[, c("ObservationID", 
                                                   "acceptedbinomial")])

rm(WD.unmanip.mature_reduced)

latFrame <- WD.cov.loc[WD.cov.loc$DataName=="Latitude", 
                        c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- WD.cov.loc[WD.cov.loc$DataName=="Longitude", 
                        c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
wdObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, WD.cov.loc, references)

#----------------------------------------------------------
# Process PHgen data
#----------------------------------------------------------

load("RAW/PHgen2.Rdata")

# SPECIES FRAME
PHgenSpeciesFrame <- PH_gen_mean
rm(PH_gen_mean)

# OBSERVATION FRAME

mergeKey <- PH.unmanip.mature_reduced[, c("ObservationID", "acceptedbinomial")]
length(unique(mergeKey$ObservationID)) # Good! No duplicate issues
rm(PH.unmanip.mature_reduced)

latFrame <- PH.cov.loc[PH.cov.loc$DataName=="Latitude", 
                        c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- PH.cov.loc[PH.cov.loc$DataName=="Longitude", 
                        c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
PHgenObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, PH.cov.loc, references)

#----------------------------------------------------------
# Process PHveg data
#----------------------------------------------------------

load("RAW/PHveg2.Rdata")

# SPECIES FRAME
PHvegSpeciesFrame <- PH_veg_mean
rm(PH_veg_mean)

# OBSERVATION FRAME

mergeKey <- PH.unmanip.mature_reduced[, c("ObservationID", "acceptedbinomial")]
length(unique(mergeKey$ObservationID)) # Yikes! some duplicate issues
mergeKey <- distinct(PH.unmanip.mature_reduced[, c("ObservationID", 
                                                   "acceptedbinomial")])
rm(PH.unmanip.mature_reduced)

latFrame <- PH.cov.loc[PH.cov.loc$DataName=="Latitude", 
                       c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- PH.cov.loc[PH.cov.loc$DataName=="Longitude", 
                       c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
PHvegObsFrame <- moo[, c("ObservationID", "acceptedbinomial", "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

# CLEAN UP SPACE
rm(bar, moo, mergeKey, latFrame, lonFrame, PH.cov.loc, references)

#----------------------------------------------------------
# Process Dispersal Mode 
#----------------------------------------------------------

load("RAW/dispersalmodeTRY2.Rdata")

# SPECIES FRAME
# (none for this one)
rm(unique_dispersal_modes)

# OBSERVATION FRAME

mergeKey <- dispersalmode_reduced[, c("ObservationID", "acceptedbinomial")]
length(unique(mergeKey$ObservationID))
# Yikes! some major duplicate issues - probably reasonable since categorical?
mergeKey <- distinct(dispersalmode_reduced[, c("ObservationID", 
                                               "acceptedbinomial")])
rm(dispersalmode_reduced)

latFrame <- dispersalmode.cov.loc[dispersalmode.cov.loc$DataName=="Latitude", 
                       c("ObservationID", "StdValue")]
names(latFrame)[2] <- "lat"
lonFrame <- dispersalmode.cov.loc[dispersalmode.cov.loc$DataName=="Longitude", 
                       c("ObservationID", "StdValue")]
names(lonFrame)[2] <- "lon" 

bar <- merge.data.frame(latFrame, lonFrame, by = c("ObservationID"))
moo <- merge.data.frame(bar, mergeKey, by = "ObservationID")
dispersalmodeObsFrame <- moo[, c("ObservationID", "acceptedbinomial", 
                                 "lat", "lon")]
names(seedmassObsFrame)[2] <- "Species"

rm(bar, moo, mergeKey, latFrame, lonFrame, dispersalmode.cov.loc, references)

#----------------------------------------------------------
# Save Results 
#----------------------------------------------------------

# put all observation frames in one object # Important!
save(dispersalmodeObsFrame, leafAreaObsFrame, leafNObsFrame, PHgenObsFrame,
     PHvegObsFrame, rootingdepthObsFrame, slaObsFrame, wdObsFrame,
     seedmassObsFrame,
     file = "TRY Data/READY/observations.Rdata")

# put all species frames in one object # Less important
save(leafAreaSpeciesFrame, leafNSpeciesFrame, PHgenSpeciesFrame,
     PHvegSpeciesFrame, rootingdepthSpeciesFrame, slaSpeciesFrame, 
     wdSpeciesFrame, seedmassSpeciesFrame,
     file = "TRY Data/READY/species.Rdata")

