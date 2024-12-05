# Script name: _figA1.R
# Author: S. Bogen
#
# Generates Dissertation Figure A1 
# (suitability plots for each covariate and method)
#-------------------------------------------------------------------------------

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 FIGURES")

# Read in literature data and bootstrap results
litData <- read.csv("../0 DATA/literature review/litData_CLEAN.csv")
load("../3 UNCERTAINTY/bootAll500_1/simInfo.RData")

# Get 95 percent CIs for smoothed methods
low95 <- sapply(bootSuitVals, quantile, probs = 0.025, na.rm = TRUE)
high95 <- sapply(bootSuitVals, quantile, probs = 0.975, na.rm = TRUE)

# set up plot space
pdf("suitabilityPlots.pdf", width = 10.5, height = 6)
par(mfrow=c(2, 3), mar=c(4.1, 4.1, 2.1, 2.1))

#-------------------------------------------------------------------------------
# Canopy Cover
#-------------------------------------------------------------------------------

x <- seq(1, 100, by = 0.1)
m <- rep(NA, length(x))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$CC_MIN & x[i] < litData$CC_MAX, na.rm = TRUE)
}

numInput <- sum(!is.na(litData$CC_MAX) & !is.na(litData$CC_MIN))
plot(x, m/numInput, xlab = "canopy cover (%)", ylab = "suitability", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "gray30", cex.lab = 1.2)
title(main = "a", adj = "0")

geoMin <- exp(mean(log(litData$CC_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$CC_MAX), na.rm = TRUE))

lines(c(0, geoMin, geoMin, geoMax, geoMax, 100), 
      c(0, 0, 1, 1, 0, 0), col = "blue", lwd = 2)

lines(c(0, low95[1], high95[1], low95[2], high95[2], 100),
      c(0, 0, 1, 1, 0, 0), col = "red", lwd = 3, lty = "dashed")

#-------------------------------------------------------------------------------
# Stand Age
#-------------------------------------------------------------------------------

x <- seq(1, 300, by = 0.1)
m <- rep(NA, length(x))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$SA_MIN & x[i] < litData$SA_MAX, na.rm = TRUE)
}

numInput <- sum(!is.na(litData$SA_MAX) & !is.na(litData$SA_MIN))
plot(x, m/numInput, xlab = "stand age (yrs)", ylab = "suitability", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "gray30", cex.lab = 1.2)
title(main = "b", adj = "0")

geoMin <- exp(mean(log(litData$SA_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$SA_MAX), na.rm = TRUE))

lines(c(1, geoMin, geoMin, geoMax, geoMax, 300), 
      c(0, 0, 1, 1, 0, 0), col = "blue", lwd = 2)

lines(c(0, low95[3], high95[3], low95[4], high95[4], 300),
      c(0, 0, 1, 1, 0, 0), col = "red", lwd = 3, lty = "dashed")

#-------------------------------------------------------------------------------
# Canopy Base Height
#-------------------------------------------------------------------------------

x <- seq(1, 35, by = 0.1)
m <- rep(NA, length(x))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$CBH_MIN & x[i] < litData$CBH_MAX, na.rm = TRUE)
}

numInput <- sum(!is.na(litData$CBH_MAX) & !is.na(litData$CBH_MIN))
plot(x, m/numInput, main = "", xlab = "canopy base height (m)", 
     ylab = "suitability", ylim = c(0, 1), type = "l", lwd = 2, col = "gray30", 
     cex.lab = 1.2)
title(main = "c", adj = "0")

geoMin <- exp(mean(log(litData$CBH_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$CBH_MAX), na.rm = TRUE))

lines(c(1, geoMin, geoMin, geoMax, geoMax, 35), 
      c(0, 0, 1, 1, 0, 0), col = "blue", lwd = 2)

lines(c(0, low95[5], (high95[5] - low95[6])/2 + low95[6], high95[6], 35),
      c(0, 0, 1, 0, 0), col = "red", lwd = 3, lty = "dashed")

#-------------------------------------------------------------------------------
# Basal Area
#-------------------------------------------------------------------------------

x <- seq(1, 120, by = 0.1)
m <- rep(NA, length(x))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$BA_MIN & x[i] < litData$BA_MAX, na.rm = TRUE)
}

numInput <- sum(!is.na(litData$BA_MAX) & !is.na(litData$BA_MIN))
plot(x, m/numInput, xlab = "basal area (square m)", ylab = "suitability", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "gray30", cex.lab = 1.2)
title(main = "d", adj = "0")

geoMin <- exp(mean(log(litData$BA_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$BA_MAX), na.rm = TRUE))

lines(c(1, geoMin, geoMin, geoMax, geoMax, 120), 
      c(0, 0, 1, 1, 0, 0), col = "blue", lwd = 2)

lines(c(0, low95[7], high95[7], low95[8], high95[8], 120),
      c(0, 0, 1, 1, 0, 0), col = "red", lwd = 3, lty = "dashed")

#-------------------------------------------------------------------------------
# Elevation
#-------------------------------------------------------------------------------

x <- seq(500, 5300, by = 0.1)
m <- rep(NA, length(x))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$ELEV_MIN & x[i] < litData$ELEV_MAX, na.rm = TRUE)
}

numInput <- sum(!is.na(litData$ELEV_MAX) & !is.na(litData$ELEV_MIN))
plot(x, m/numInput, xlab = "elevation (m)", ylab = "suitability", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "gray30", cex.lab = 1.2)
title(main = "e", adj = "0")

geoMin <- exp(mean(log(litData$ELEV_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$ELEV_MAX), na.rm = TRUE))

lines(c(500, geoMin, geoMin, geoMax, geoMax, 5300), 
      c(0, 0, 1, 1, 0, 0), col = "blue", lwd = 2)

lines(c(0, low95[9], (high95[9] - low95[10])/2 + low95[10], high95[10], 5300),
      c(0, 0, 1, 0, 0), col = "red", lwd = 3, lty = "dashed")

#-------------------------------------------------------------------------------
# Slope
#-------------------------------------------------------------------------------

x <- seq(0, 90, by = 0.1)
m <- rep(NA, length(x))

# AHHH MAKE SURE YOU DEAL WITH ZEROES! Changed to one.
litData$SLOPE_MIN[litData$SLOPE_MIN==0] <- 1

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$SLOPE_MIN & x[i] < litData$SLOPE_MAX, na.rm = TRUE)
}

numInput <- sum(!is.na(litData$SLOPE_MAX) & !is.na(litData$SLOPE_MIN))
plot(x, m/numInput, xlab = "slope (%)", ylab = "suitability", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "gray30", cex.lab = 1.2)
title(main = "f", adj = "0")

geoMin <- exp(mean(log(litData$SLOPE_MIN), na.rm = TRUE)) 
geoMax <- exp(mean(log(litData$SLOPE_MAX), na.rm = TRUE))

lines(c(0, geoMin, geoMin, geoMax, geoMax, 90), 
      c(0, 0, 1, 1, 0, 0), col = "blue", lwd = 2)

lines(c(0, low95[11], high95[11], low95[12], high95[12], 90),
      c(0, 0, 1, 1, 0, 0), col = "red", lwd = 3, lty = "dashed")

#-------------------------------------------------------------------------------
# Clean-up and close
#-------------------------------------------------------------------------------

rm(geoMax, geoMin, high95, low95, i, m, numInput, repsDiscarded, x, 
   bootInfo, bootLitNs, bootSuitVals, bootWeights, expertData, litData)
dev.off()

