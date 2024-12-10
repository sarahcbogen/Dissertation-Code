# Script name: _visualizeBias.R
# Author: S. Bogen
#
# Produces Figures 4.2 and 4.9
#-------------------------------------------------------------------------------

setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/5 FIGURES")

library(ggplot2)
library(reshape2)

load("../2 BIAS/biasWeights.Rdata")

#-------------------------------------------------------------------------------
# Biome Bar chart
#-------------------------------------------------------------------------------

biomeProp <- biomeProp[, -4]

# Reshape data for ease of use in ggplot2
biomeProp_melted <- melt(biomeProp, id.vars = "BIOME")

# Create the side-by-side bar chart
ggplot(biomeProp_melted, aes(x = BIOME, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5),
           width = 0.5) +
  labs(title = "Proportion of Species Richness by Biome",
       y = "Proportion",
       x = "Biome",
       fill = "") +
  scale_fill_manual(values = c("propPopulation" = "blue", "propSample" = "red"),
                    labels = c("Reference", "Sample")) +
  theme_minimal()

ggsave("biomeBias.pdf", width = 8, height = 5.5)
  
#-------------------------------------------------------------------------------
# Order Bar chart
#-------------------------------------------------------------------------------

orderProp <- orderProp[, -4]

# Reshape data for ease of use in ggplot2
orderProp_melted <- melt(orderProp, id.vars = "ORDER")

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

ggsave("orderBias.pdf", width = 5.5, height = 8)

