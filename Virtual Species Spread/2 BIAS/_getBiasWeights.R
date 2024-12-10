# Script name: _getBiasWeights.R
# Author: S. Bogen
#
# Outputs biasWeights.Rdata, which contains the weights to use to characterize 
# and mitigate both taxonomic and ecoregion-based bias.
#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

# Set working directory
setwd("~/Desktop/Dissertation Code/Virtual Species Spread/2 BIAS")

#-------------------------------------------------------------------------------
# Spatial Bias
#-------------------------------------------------------------------------------

load("spatialInfo.RData")

# Some quick cleaning
tmp <- richByBIOME[-15, ]
richByBIOME <- tmp[c(1, 7, 8, 9, 10, 11, 12, 13, 14, 2, 3, 4, 5, 6), ]

# Barplot by Biome - Expected richness
par(mfrow = c(2, 1))
barplot(richByBIOME$total_plant_spcs, names.arg = richByBIOME$BIOME,
        ylab = "Species Richness", xlab = "Biome Code", main = "Reference")
barplot(richByBIOME$total_accepted, add = FALSE, col = "red", 
        names.arg = richByBIOME$BIOME, main = "Data")
plot(scale(richByBIOME$total_plant_spcs), scale(richByBIOME$total_accepted),
     xlab = "expected", ylab = "observed")

# Barplot by Realm - Expected richness
barplot(richByREALM$total_plant_spcs, names.arg = richByREALM$REALM,
        ylab = "Species Richness", xlab = "Realm")
barplot(richByREALM$total_accepted, add = FALSE, col = "red")
plot(richByREALM$total_plant_spcs, richByREALM$total_accepted,
     xlab = "expected", ylab = "observed")

# Use a simple weighting method:
# https://sampleweighting.com/how-to-weight-data/

# boostWeight is what we can do to boost
# reduceWeight is the factor we can use if the goal is to reduce

biomeProp <- data.frame("BIOME" = richByBIOME$BIOME,
                        "propPopulation" = richByBIOME$total_plant_spcs/sum(richByBIOME$total_plant_spcs),
                        "propSample" = richByBIOME$total_accepted/sum(richByBIOME$total_accepted))
biomeProp <- cbind(biomeProp,
                   "boostWeight" = biomeProp$propPopulation / biomeProp$propSample)

realmProp <- data.frame("REALM" = richByREALM$REALM,
                        "propPopulation" = richByREALM$total_plant_spcs/sum(richByREALM$total_plant_spcs),
                        "propSample" = richByREALM$total_accepted/sum(richByREALM$total_accepted))
realmProp <- cbind(realmProp,
                   "boostWeight" = realmProp$propPopulation / realmProp$propSample,
                   "reduceWeight" = realmProp$propSample / realmProp$propPopulation)

bothProp <- data.frame("REALM_BIOME" = richByREALM_BIOME$REALM_BIOME,
                       "propPopulation" = richByREALM_BIOME$total_plant_spcs/sum(richByREALM_BIOME$total_plant_spcs),
                       "propSample" = richByREALM_BIOME$total_accepted/sum(richByREALM_BIOME$total_accepted))
bothProp <- cbind(bothProp,
                  "boostWeight" = bothProp$propPopulation / bothProp$propSample)

# clean up space
rm(bothProp, realmProp, richByBIOME, richByREALM, richByREALM_BIOME, tmp)

#-------------------------------------------------------------------------------
# Taxonomic Bias - easier
#-------------------------------------------------------------------------------

richByOrder <- as.data.frame(read_excel("../0 DATA/reference data/Taxonomic Order/data_on_order.xlsx"))


# Make a quick bar plot
par(mar=c(2, 6, 2, 2))
barplot(richByOrder$Total, names = richByOrder$ORDER, horiz = TRUE, las = 1,
        xlab = "Richness", cex.names = 0.7, main = "Reference Data (Expected Richness)")

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
datOrders <- datMinst[, c("Species", "Order")]

# Clean up space
rm(data.all, dataVS, datMinst)

observedOrderRichness <- as.data.frame(table(datOrders$Order))
names(observedOrderRichness) <- c("ORDER", "observedCount")

orderCounts <- merge.data.frame(richByOrder, observedOrderRichness)#[, c(-2, -3)]
expectedPropWoody <- orderCounts$Woody / orderCounts$Total
orderCounts <- data.frame(orderCounts, expectedPropWoody)[, c(-2, -3)]

names(orderCounts)[2] <- "expectedCount"
orderProp <- data.frame("ORDER" = orderCounts$ORDER,
                        "expectedProp" = orderCounts$expectedCount / sum(orderCounts$expectedCount),
                        "observedProp" = orderCounts$observedCount / sum(orderCounts$observedCount),
                        "expectedFracWoody" = orderCounts$expectedPropWoody)

# Assign all gymnosperms a prop of "-9999" to denote they are gymnosperms
# Cupressales
# Cycadales
# Gnetales
# Pinales
orderProp$expectedFracWoody[orderProp$ORDER=="Cupressales"] <- -99
orderProp$expectedFracWoody[orderProp$ORDER=="Cycadales"] <- -99
orderProp$expectedFracWoody[orderProp$ORDER=="Gnetales"] <- -99
orderProp$expectedFracWoody[orderProp$ORDER=="Pinales"] <- -99

# Look up info about single-family orders: - Get citations!
#  Boraginales appears to be primarily woody (assign fraction 1)
#  Metteniusales appears to be primarily woody (assign fraction 1)
orderProp$expectedFracWoody[orderProp$ORDER=="Boraginales"] <- 1
orderProp$expectedFracWoody[orderProp$ORDER=="Metteniusales"] <- 1

# Order the order frame by decreasing size of expectedFracWoody
orderProp <- orderProp[order(orderProp$expectedFracWoody, decreasing = FALSE), ]

# Try again, but in ggplot
# Reshape data for ease of use in ggplot2
orderProp_melted <- melt(orderProp[, -4], id.vars = "ORDER")

# Create the side-by-side bar chart
ggplot(orderProp_melted, aes(x = ORDER, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5),
           width = 0.5) +
  labs(title = "Proportion Species Richness by Order",
       y = "Proportion",
       fill = "") +
  scale_fill_manual(values = c("expectedProp" = "blue", "observedProp" = "red"),
                    labels = c("Reference", "Sample")) +
  theme_minimal() +
  coord_flip()


# Try this


# Assuming 'ORDER' is a factor variable in your data frame
orderProp_melted$ORDER <- factor(orderProp_melted$ORDER, levels = unique(orderProp_melted$ORDER))

ggplot(orderProp_melted, aes(x = ORDER, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5),
           width = 0.5) +
  labs(title = "Proportion Species Richness by Order",
       y = "Proportion",
       fill = "") +
  scale_fill_manual(values = c("expectedProp" = "blue", "observedProp" = "red"),
                    labels = c("Reference", "Sample")) +
  theme_minimal() +
  coord_flip()



# Sample data (replace with your actual data)
orderProp_melted <- data.frame(
  ORDER = c("Order1", "Order2", "Order3", "Order4"),
  expectedProp = c(0.25, 0.35, 0.45, 0.55),
  observedProp = c(0.3, 0.4, 0.5, 0.6)
)

# Define colors
colors <- c("blue", "red")

# Plot
barplot(
  t(as.matrix(orderProp_melted[, -1])),
  beside = TRUE,
  col = colors,
  legend.text = c("Reference", "Sample"),
  args.legend = list(x = "topright"),
  main = "Proportion Species Richness by Order",
  ylab = "Proportion",
  names.arg = orderProp_melted$ORDER
)

#-------------------------------------------------------------------------------
# Save data
#-------------------------------------------------------------------------------

save(dataSetObservations, biomeProp, orderProp, file = "biasWeights.RData")

