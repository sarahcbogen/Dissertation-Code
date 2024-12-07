# File Name: litReviewCleaning.R
# Author: S. Bogen
#
# Requires the file RAW/lit_review_data_noOrg.xlsx, which has a single sheet of
# data taken from the first 184 rows of the "DATA" tab of the file
# RAW/HSM_variable_parameters.
#
# Produces the file litData_CLEAN.csv, which has 19 literature source rows
# labeled with author names and columns for *_MIN, *_MAX, and *_MEAN values
# for the covariates Canopy Cover (CC), Stand Age (SA), Canopy Base Height (CBH),
# Basal Area, (BA), Elevation (ELEV), and Slope (SLOPE). NAs indicate 
# information not reported by a literature source.
#-------------------------------------------------------------------------------

library(readxl)

# Set working directory
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/0 DATA/literature review")

# Read in raw data
raw <- read_xlsx("RAW/lit_review_data_noOrg.xlsx")[, c(1, 6:9)]

# Standardize "Important Variables" names, omitting Squires and Reggario 
# "nest stand" data entries
raw$`Important Variables`[68] <- "Elevation"
raw$`Important Variables`[21] <- "Canopy cover"

# Pull apart by variable
cc <- raw[raw$`Important Variables`=="Canopy cover", -2] #  Expect 16
sa <- raw[raw$`Important Variables`=="Tree age", -2] #  Expect 4
cbh <- raw[raw$`Important Variables`=="Canopy base height", -2] #  Expect 4
ba <- raw[raw$`Important Variables`=="Basal area", -2] #  Expect 13
elev <- raw[raw$`Important Variables`=="Elevation", -2] #  Expect 7
slope <- raw[raw$`Important Variables`=="Slope", -2] #  Expect 16

# Rename min and max columns
names(cc) <- c("Paper", "CC_MIN", "CC_MAX", "CC_MEAN")
names(sa) <- c("Paper", "SA_MIN", "SA_MAX", "SA_MEAN")
names(cbh) <- c("Paper", "CBH_MIN", "CBH_MAX", "CBH_MEAN")
names(ba) <- c("Paper", "BA_MIN", "BA_MAX", "BA_MEAN")
names(elev) <- c("Paper", "ELEV_MIN", "ELEV_MAX", "ELEV_MEAN")
names(slope) <- c("Paper", "SLOPE_MIN", "SLOPE_MAX", "SLOPE_MEAN")

# Merge by paper name
varsList <- list(cc, sa, cbh, ba, elev, slope)
litData_full <- Reduce(function(x, y) merge(x, y, by = "Paper", all=TRUE), 
                       varsList)

# save cleaned data as a csv file
write.csv(litData_full, "litData_CLEAN.csv", row.names = FALSE)

# clean up workspace
rm(ba, cbh, cc, elev, sa, slope, varsList, litData_full, raw)

