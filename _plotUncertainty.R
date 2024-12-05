# Script name: _plotUncertainty.R
# Author: S. Bogen
#
# Visualizes the bootstrapped 95% CI widths for the three AHP habitat
# suitability models for each forest
#-------------------------------------------------------------------------------

library(raster)
library(sf)
library(gridExtra)
library(stars)
library(ggplot2)

# Set to the working directory on your system
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 FIGURES")

# Read in predictions
ashleyBasic <- raster("../2 AHP MODELS/3 PREDICTIONS/predBasic.ashley.tif")
ashleySmoothed <- raster("../2 AHP MODELS/3 PREDICTIONS/predSmoothed.ashley.tif")
ashleyFuzzy <- raster("../2 AHP MODELS/3 PREDICTIONS/predFuzzy.ashley.tif")

fishlakeBasic <- raster("../2 AHP MODELS/3 PREDICTIONS/predBasic.fishlake.tif")
fishlakeSmoothed <- raster("../2 AHP MODELS/3 PREDICTIONS/predSmoothed.fishlake.tif")
fishlakeFuzzy <- raster("../2 AHP MODELS/3 PREDICTIONS/predFuzzy.fishlake.tif")

# Read in CI bounds

ashleyBasicHIGH <- raster("../3 UNCERTAINTY/bootAll500_1/ashleyBasicHIGH.tif")
ashleyBasicLOW <- raster("../3 UNCERTAINTY/bootAll500_1/ashleyBasicLOW.tif")
ashleySmoothedHIGH <- raster("../3 UNCERTAINTY/bootAll500_1/ashleySmoothedHIGH.tif")
ashleySmoothedLOW <- raster("../3 UNCERTAINTY/bootAll500_1/ashleySmoothedLOW.tif")
ashleyFuzzyHIGH <- raster("../3 UNCERTAINTY/bootAll500_1/ashleyFuzzyHIGH.tif")
ashleyFuzzyLOW <- raster("../3 UNCERTAINTY/bootAll500_1/ashleyFuzzyLOW.tif")

fishlakeBasicHIGH <- raster("../3 UNCERTAINTY/bootAll500_1/fishlakeBasicHIGH.tif")
fishlakeBasicLOW <- raster("../3 UNCERTAINTY/bootAll500_1/fishlakeBasicLOW.tif")
fishlakeSmoothedHIGH <- raster("../3 UNCERTAINTY/bootAll500_1/fishlakeSmoothedHIGH.tif")
fishlakeSmoothedLOW <- raster("../3 UNCERTAINTY/bootAll500_1/fishlakeSmoothedLOW.tif")
fishlakeFuzzyHIGH <- raster("../3 UNCERTAINTY/bootAll500_1/fishlakeFuzzyHIGH.tif")
fishlakeFuzzyLOW <- raster("../3 UNCERTAINTY/bootAll500_1/fishlakeFuzzyLOW.tif")

# read in forest outlines
forests <- st_read("../0 DATA/spatial boundaries/CLEAN/UtahForests.shp")
ashley.bound <- forests[forests$FORESTNAME=="Ashley National Forest", ]
fishlake.bound <- forests[forests$FORESTNAME=="Fishlake National Forest", ]

#-------------------------------------------------------------------------------
# Plot CI absolute widths
#-------------------------------------------------------------------------------

# GET CI widts - Fishlake
fishlakeCIstack <- stack(fishlakeBasicHIGH - fishlakeBasicLOW, 
                         fishlakeSmoothedHIGH - fishlakeSmoothedLOW, 
                         fishlakeFuzzyHIGH - fishlakeFuzzyLOW)
names(fishlakeCIstack) <- c("base", "smoothed", "fuzzy")
fishlakeCIstars <- st_as_stars(fishlakeCIstack)
rm(fishlakeCIstack)

# GET CI widts - Ashley
ashleyCIstack <- stack(ashleyBasicHIGH - ashleyBasicLOW, 
                       ashleySmoothedHIGH - ashleySmoothedLOW, 
                       ashleyFuzzyHIGH - ashleyFuzzyLOW)
names(ashleyCIstack) <- c("base", "smoothed", "fuzzy")
ashleyCIstars <- st_as_stars(ashleyCIstack)
rm(ashleyCIstack)

panelLabels <- as_labeller(c(`base` = "AHP Base", 
                             `smoothed` = "AHP Smoothed", 
                             `fuzzy` = "AHP Jagged"))

panelLabels <- as_labeller(c(`base` = "AHP Base", 
                             `smoothed` = "AHP Smoothed", 
                             `fuzzy` = "AHP Jagged"))

#---------------------------------------------------------------

ashleyCIplot <- ggplot() +
  geom_stars(data = ashleyCIstars) +
  geom_sf(data = st_as_sf(ashley.bound), fill = NA, col = "white", lwd = 0.5) + 
  facet_wrap(vars(band), labeller = panelLabels) +
  scale_fill_gradient(low = "indianred4", high = "gray95", limits = c(0, 0.71)) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0)) +
  ggtitle("95% Bootstrapped CI Width") +
  labs(x = "", y = "Ashley National Forest\n", fill = "")

ashleyCIplot

fishlakeCIplot <- ggplot() +
  geom_stars(data = fishlakeCIstars) +
  geom_sf(data = st_as_sf(fishlake.bound), fill = NA, col = "white", lwd = 0.5) +
  facet_wrap(vars(band), labeller = panelLabels) +
  scale_fill_gradient(low = "indianred4", high = "gray95", limits = c(0, 0.71)) +
  theme(legend.position = "right", legend.key.width = unit(0.75, 'cm'),
        legend.margin = margin(t = -.5, unit = "cm"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0)) +
  labs(x = "", y = "Fishlake National Forest\n", fill = "")

combinedCI <- grid.arrange(ashleyCIplot, 
             fishlakeCIplot, 
             nrow = 2,
             heights = c(0.83, 1))

#-------------------------------------------------------------------------------
# Save the Result
#-------------------------------------------------------------------------------

# Saving 9.36 x 8.61 in image
# ggsave("ciWidths.pdf", combinedCI)