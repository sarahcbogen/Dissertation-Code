# Script name: _plotMaps.R
# Author: S. Bogen
#
# Visualizes the continuous HSI outputs of the three AHP habitat suitability 
# models for each forest
#-------------------------------------------------------------------------------

library(raster)
library(stars)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)

# Define color schemes - need one for discrete and one for continuous
pretty <- brewer.pal(n = 3, name = 'RdYlBu')

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 FIGURES")

# Read in relevant forest boundaries
forests <- st_read("../0 DATA/spatial boundaries/CLEAN/UtahForests.shp")
ashley.bound <- forests[forests$FORESTNAME=="Ashley National Forest", ]
fishlake.bound <- forests[forests$FORESTNAME=="Fishlake National Forest", ]

# Read in continuous HSI rasters for AHP
fishlakeAHP.base <- raster("../2 HSMS/AHP/predBasic.fishlake.tif")
ashleyAHP.base <- raster("../2 HSMS/AHP/predBasic.ashley.tif")
fishlakeAHP.smoothed <- raster("../2 HSMS/AHP/predSmoothed.fishlake.tif")
ashleyAHP.smoothed <- raster("../2 HSMS/AHP/predSmoothed.ashley.tif")
fishlakeAHP.fuzzy <- raster("../2 HSMS/AHP/predFuzzy.fishlake.tif")
ashleyAHP.fuzzy <- raster("../2 HSMS/AHP/predFuzzy.ashley.tif")

# Read in continuous HSI rasters for MaxENT
fishlakeME <- raster("../2 HSMS/MAXENT/fishlake_trainedAshley.tif")
ashleyME <- raster("../2 HSMS/MAXENT/ashley_trainedFishlake.tif")

#-------------------------------------------------------------------------------
# Final plots - Continuous - THE FIGURE
#-------------------------------------------------------------------------------

fishlakeStack <- stack(fishlakeAHP.base, fishlakeAHP.smoothed, fishlakeAHP.fuzzy)
names(fishlakeStack) <- c("base", "smoothed", "fuzzy")
fishlakeStars <- st_as_stars(fishlakeStack)
rm(fishlakeStack)

ashleyStack <- stack(ashleyAHP.base, ashleyAHP.smoothed, ashleyAHP.fuzzy)
names(ashleyStack) <- c("base", "smoothed", "fuzzy")
ashleyStars <- st_as_stars(ashleyStack)
rm(ashleyStack)

panelLabels <- as_labeller(c(`base` = "AHP Base",
                             `smoothed` = "AHP Smoothed",
                             `fuzzy` = "AHP Jagged"))


ashleyContPlot <- ggplot() +
  geom_stars(data = ashleyStars) +
  # geom_sf(data = st_as_sf(ashley_star), fill = NA, col = "white", lwd = 0.5) + 
  geom_sf(data = st_as_sf(ashley.bound), fill = NA, col = "white", lwd = 0.5) + 
  facet_wrap(vars(band), labeller = panelLabels) +
  scale_fill_gradient(low = "darkslategray", high = "gray95", limits = c(0, 1)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0)) +
  ggtitle("Habitat Suitability Levels") +
  labs(x = "", y = "Ashley National Forest\n", fill = "")

ashleyContPlot

fishlakeContPlot <- ggplot() +
  geom_stars(data = fishlakeStars) +
  geom_sf(data = st_as_sf(fishlake.bound), fill = NA, col = "white", lwd = 0.5) + 
  facet_wrap(vars(band), labeller = panelLabels) +
  scale_fill_gradient(low = "darkslategray", high = "gray95", limits = c(0, 1)) +
  theme(legend.position = "right", 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  ggtitle("") +
  labs(x = "", y = "Fishlake National Forest\n", fill = "")

fishlakeContPlot

combinedContinuous <- grid.arrange(ashleyContPlot, fishlakeContPlot, 
                                   nrow = 2, heights = c(0.83, 1))

# Saving 9.36 x 8.61 in image
# ggsave("continuousHSI.pdf", combinedContinuous, width = 9.36, height = 8.61)



