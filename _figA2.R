# Script name: _figA2.R
# Author: S. Bogen
#
# Generates Dissertation Figure A2
# (CI width vs HSI for each forest and method)
#-------------------------------------------------------------------------------

library(raster)
library(ggplot2)
library(dplyr)

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

#-------------------------------------------------------------------------------
# HSI vs uncertainty
#-------------------------------------------------------------------------------

ash_df <- data.frame(HSI = as.vector(ashleyBasic),
                     uncertainty = as.vector(ashleyBasicHIGH - ashleyBasicLOW),
                     group = "Ashley Basic")
ash_df_s <- data.frame(HSI = as.vector(ashleySmoothed),
                     uncertainty = as.vector(ashleySmoothedHIGH - ashleySmoothedLOW),
                     group = "Ashley Smoothed")
ash_df_j <- data.frame(HSI = as.vector(ashleyFuzzy),
                       uncertainty = as.vector(ashleyFuzzyHIGH - ashleyFuzzyLOW),
                       group = "Ashley Jagged")

fish_df <- data.frame(HSI = as.vector(fishlakeBasic),
                      uncertainty = as.vector(fishlakeBasicHIGH - fishlakeBasicLOW),
                      group = "Fishlake Basic")
fish_df_s <- data.frame(HSI = as.vector(fishlakeSmoothed),
                       uncertainty = as.vector(fishlakeSmoothedHIGH - fishlakeSmoothedLOW),
                       group = "Fishlake Smoothed")
fish_df_j <- data.frame(HSI = as.vector(fishlakeFuzzy),
                       uncertainty = as.vector(fishlakeFuzzyHIGH - fishlakeFuzzyLOW),
                       group = "Fishlake Jagged")

badf <- bind_rows(ash_df, ash_df_s, ash_df_j, fish_df, fish_df_s, fish_df_j) |>
  group_by(group) |>
  slice_sample(n = 10000) |>
  ungroup() |>
  mutate(group = factor(group, levels = c("Ashley Basic",
                                          "Ashley Smoothed",
                                          "Ashley Jagged",
                                          "Fishlake Basic",
                                          "Fishlake Smoothed",
                                          "Fishlake Jagged")))

rm(ash_df, ash_df_s, ash_df_j, fish_df, fish_df_s, fish_df_j)

ggplot(badf, aes(x = HSI, y = uncertainty)) +
  geom_point(alpha = 0.25, size = 0.5) +
  facet_wrap(~ group) +
  theme_bw()

#-------------------------------------------------------------------------------
# Save Result
#-------------------------------------------------------------------------------

ggsave("suitabilityVuncertainty.png")

