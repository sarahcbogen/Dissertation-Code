# Script name: _cstarHistograms.R
# Author: S. Bogen
#
# Generates Figure 3.2, B.1, B.2, and B.3
#-------------------------------------------------------------------------------

library(Matrix)
library(coda)
library(ape)
library(MCMCglmm)
library(pracma)
library(RColorBrewer)

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/5 FIGURES")

# Read in data
simData <- read.csv("../3 VIRTUAL SPECIES/VS growing with speeds/unmitigated.csv")
cstarGauss <- exp(simData$logSpeedGauss)
cstarLap <- exp(simData$logSpeedLap)

par(mfrow = c(1, 2))
ccSpeed <- log(420)

#-------------------------------------------------------------------------------
# Histograms - Gaussian Kernel, simple simulation 10
#-------------------------------------------------------------------------------

dev.off()

# Save figure
pdf("unmitigatedGauss.pdf", width = 8, height = 5)

par(mfrow=c(2, 3))

vec <- exp(simData[simData$DS_Bullock=="vertebrate", ]$logSpeedGauss)
hist(log(vec), main = "Vertebrate Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.appendage", ]$logSpeedGauss)
hist(log(vec), main = "Wind Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ant", ]$logSpeedGauss)
hist(log(vec), main = "Ant Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ballistic", ]$logSpeedGauss)
hist(log(vec), main = "Ballistic Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.none", ]$logSpeedGauss)
hist(log(vec), main = "Unassisted", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15), breaks = 30)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- cstarGauss
hist(log(vec), main = "All Dispersal Modes", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", breaks = 30,
     xlim = c(-15, 15), col = "gray50")
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

# Save figure
dev.off()


#-------------------------------------------------------------------------------
# Histograms - Laplace Kernel, simple simulation 10
#-------------------------------------------------------------------------------

dev.off()

pdf("histsLaplace.pdf", width = 8, height = 5)
par(mfrow=c(2, 3))

vec <- exp(simData[simData$DS_Bullock=="vertebrate", ]$logSpeedLap)
hist(log(vec), main = "Vertebrate Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.appendage", ]$logSpeedLap)
hist(log(vec), main = "Wind Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ant", ]$logSpeedLap)
hist(log(vec), main = "Ant Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ballistic", ]$logSpeedLap)
hist(log(vec), main = "Ballistic Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.none", ]$logSpeedLap)
hist(log(vec), main = "Unassisted", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15), breaks = 30)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData$logSpeedLap)
hist(log(vec), main = "All Dispersal Modes", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", breaks = 30,
     xlim = c(-15, 15), col = "gray50")
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

dev.off()

#-------------------------------------------------------------------------------
# Histograms - Gaussian Dispersal Kernel, Biome Mitigated
#-------------------------------------------------------------------------------

# Read in data
simData <- read.csv("../3 VIRTUAL SPECIES/VS growing with speeds/biomeMitigated.csv")
cstarGauss <- exp(simData$logSpeedGauss)

dev.off()

pdf("histsGauss_Biome.pdf", width = 8, height = 5)

par(mfrow=c(2, 3))
ccSpeed <- log(420)
xLimits <- c(-25, 25)

vec <- exp(simData[simData$DS_Bullock=="vertebrate", ]$logSpeedGauss)
hist(log(vec), main = "Vertebrate Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=xLimits)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.appendage", ]$logSpeedGauss)
hist(log(vec), main = "Wind Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim = xLimits)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ant", ]$logSpeedGauss)
hist(log(vec), main = "Ant Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim = xLimits)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ballistic", ]$logSpeedGauss)
hist(log(vec), main = "Ballistic Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=xLimits)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
textLocX <- range(log(cstarGauss))[1] + 5
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)
# text(textLocX, 200, paste(round(perBelowGauss, 3), "%", sep = ""), cex = 1.5)
rm(textLocX)

vec <- exp(simData[simData$DS_Bullock=="wind.none", ]$logSpeedGauss)
hist(log(vec), main = "Unassisted", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim = xLimits, breaks = 30)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- cstarGauss
hist(log(vec), main = "All Dispersal Modes", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", breaks = 30,
     xlim = xLimits, col = "gray50")
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
textLocX <- range(log(cstarGauss))[1] + 5
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)
# text(textLocX, 200, paste(round(perBelowGauss, 3), "%", sep = ""), cex = 1.5)
rm(textLocX)

dev.off()

#-------------------------------------------------------------------------------
# Histograms - Gaussian Dispersal Kernel, Order Mitigated
#-------------------------------------------------------------------------------

# Read in data
simData <- read.csv("../3 VIRTUAL SPECIES/VS growing with speeds/orderMitigated.csv")
cstarGauss <- exp(simData$logSpeedGauss)

dev.off()

pdf("histsGauss_order.pdf", width = 8, height = 5)

par(mfrow=c(2, 3))
ccSpeed <- log(420)

vec <- exp(simData[simData$DS_Bullock=="vertebrate", ]$logSpeedGauss)
hist(log(vec), main = "Vertebrate Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.appendage", ]$logSpeedGauss)
hist(log(vec), main = "Wind Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
textLocX <- range(log(cstarGauss))[1] + 5
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)
# text(textLocX, 200, paste(round(perBelowGauss, 3), "%", sep = ""), cex = 1.5)
rm(textLocX)

vec <- exp(simData[simData$DS_Bullock=="ant", ]$logSpeedGauss)
hist(log(vec), main = "Ant Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="ballistic", ]$logSpeedGauss)
hist(log(vec), main = "Ballistic Dispersal", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15))
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- exp(simData[simData$DS_Bullock=="wind.none", ]$logSpeedGauss)
hist(log(vec), main = "Unassisted", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", xlim=c(-15, 15), breaks = 30)
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)

vec <- cstarGauss
hist(log(vec), main = "All Dispersal Modes", 
     xlab = "Rate of spread log(m/yr)", ylab = "Number of species", breaks = 30,
     xlim = c(-15, 15), col = "gray50")
abline(v=ccSpeed, col="red", lwd=4, lty="dashed")
perBelowGauss <- sum(log(vec) < ccSpeed , na.rm = TRUE) / 
  (length(vec) - sum(is.na(log(vec)))) * 100
textLocX <- range(log(cstarGauss))[1] + 5
mtext(paste(round(perBelowGauss, 3), "%", sep = ""), side = 3)
# text(textLocX, 200, paste(round(perBelowGauss, 3), "%", sep = ""), cex = 1.5)
rm(textLocX)

dev.off()

