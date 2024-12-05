# Script name: _buildSuitPlots.R
# Author: S. Bogen
#
# Creates the a figure illustrating the three methods to map raw covariate
# values to suitability values
#-------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 FIGURES")

# Read in data
litData <- read.csv("../0 DATA/literature review/litData_CLEAN.csv")
load("../3 UNCERTAINTY/bootAll500_1/simInfo.RData")

geoMin <- exp(mean(log(litData$CC_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$CC_MAX), na.rm = TRUE))

low95 <- sapply(bootSuitVals, quantile, probs = 0.025, na.rm = TRUE)
high95 <- sapply(bootSuitVals, quantile, probs = 0.975, na.rm = TRUE)

rm(bootInfo, bootLitNs, bootSuitVals, bootWeights, expertData, repsDiscarded)

#-------------------------------------------------------------------------------
# Sample figure a - No overlap in smoothed method
# (Use Canopy Cover data)
#-------------------------------------------------------------------------------

# Open output pdf
pdf(file = "methodExPlots.pdf", width = 7, height = 4.7)

par(mfcol=c(3, 2), mar = c(2, 3, 2, 3) + 0.1)

#----------Basic Method------------

plot(c(10, geoMin, geoMin, geoMax, geoMax, 100), 
     c(0, 0, 1, 1, 0, 0), col = "black", lwd = 2, type = "l",
     ylab = "suitability", xlab = "covariate value",
     xaxt = "n", yaxt = "n", xlim = c(10, 100))
axis(2, at = c(0, 1), lwd = 0.75)
axis(1, at = c(geoMin, geoMax), labels = c("l", "h"), 
     font = c(3, 3), lwd = 0.75)
title("a", adj = 0, cex.main = 1.5)
title("Basic Method", line = 0.5, cex.main = 1.2, font.main = 1)
title(ylab = "suitability", line = 0.5)

#----------Smoothed Method------------

# Your existing plot code without the line plot
plot(0, type = "n", ylab = "suitability", xlab = "covariate value",
     xaxt = "n", yaxt = "n", xlim = c(10, 100), ylim = c(0, 1))
axis(2, at = c(0, 1), lwd = 0.75)
axis(1, at = c(low95[1], high95[1], low95[2], high95[2]), 
     labels = c("a", "b", "c", "d"),
     font = c(3, 3, 3, 3), lwd = 0.75)

# Shade the areas between 'a' and 'b', 'c' and 'd'
polygon(c(low95[1], high95[1], high95[1], low95[1]),
        c(-0.025, -0.025, 1.025, 1.025), col = "gray93", border = NA)
polygon(c(low95[2], high95[2], high95[2], low95[2]),
        c(-0.025, -0.025, 1.025, 1.025), col = "gray93", border = NA)

# Add vertical lines showing original geomeans
abline(v = geoMin, lty = "dashed", col = "gray50")
abline(v = geoMax, lty = "dashed", col = "gray50")

# Add the line plot on top
lines(c(10, low95[1], high95[1], low95[2], high95[2], 100),
      c(0, 0, 1, 1, 0, 0), col = "black", lwd = 2)
title("Smoothed Method", line = 0.5, cex.main = 1.2, font.main = 1)
title(ylab = "suitability", line = 0.5)

#----------Jagged Method------------

x <- seq(10, 100, by = 0.1)
m <- rep(NA, length(x))
numInput <- sum(!is.na(litData$CC_MAX) & !is.na(litData$CC_MIN))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$CC_MIN & x[i] < litData$CC_MAX, na.rm = TRUE)
}

plot(x, m/numInput, xlab = "", ylab = "", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "black",
     xaxt = "n", yaxt = "n", xlim = c(10, 100))
axis(2, at = c(0, 1), lwd = 0.75)

title("Jagged Method", line = 0.5, cex.main = 1.2, font.main = 1)
title(ylab = "suitability", line = 0.5)
title(xlab = "covariate value", line = 0.8)

rm(x, m)

#-------------------------------------------------------------------------------
# Second figure b - Overlap in Smoothed method
# (Use Canopy Base Height)
#-------------------------------------------------------------------------------

geoMin <- exp(mean(log(litData$CBH_MIN), na.rm = TRUE))
geoMax <- exp(mean(log(litData$CBH_MAX), na.rm = TRUE))

#----------Basic Method------------

plot(c(0, geoMin, geoMin, geoMax, geoMax, 35), 
     c(0, 0, 1, 1, 0, 0), col = "black", lwd = 2, type = "l",
     ylab = "suitability", xlab = "covariate value",
     xaxt = "n", yaxt = "n", xlim = c(0, 35))
axis(2, at = c(0, 1), lwd = 0.75)
axis(1, at = c(geoMin, geoMax), labels = c("l", "h"), 
     font = c(3, 3), lwd = 0.75)
title("b", adj = 0, cex.main = 1.5)
title("Basic Method", line = 0.5, cex.main = 1.2, font.main = 1)
title(ylab = "suitability", line = 0.5)

#----------Smoothed Method------------

# Your existing plot code without the line plot
plot(0, type = "n", ylab = "suitability", xlab = "covariate value",
     xaxt = "n", yaxt = "n", xlim = c(0, 35), ylim = c(0, 1))
axis(2, at = c(0, 1), lwd = 0.75)
axis(1, at = c(low95[5], high95[5], low95[6], high95[6]), 
     labels = c("a", "b", "c", "d"),
     font = c(3, 3, 3, 3), lwd = 0.75)

# Shade the areas between 'a' and 'b', 'c' and 'd'
polygon(c(low95[5], high95[5], high95[5], low95[5]),
        c(-0.025, -0.025, 1.025, 1.025), col = "gray93", border = NA)

polygon(c(low95[6], high95[6], high95[6], low95[6]),
        c(-0.025, -0.025, 1.025, 1.025), col = "gray93", border = NA)

# double-shade overlapping
polygon(c(high95[5], low95[6], low95[6], high95[5]),
        c(-0.025, -0.025, 1.025, 1.025), col = "gray75", border = NA)

midpt <- (high95[5] + low95[6])/2

abline(v = midpt, lty = "dashed", col = "gray50")

# Add the line plot on top
lines(c(0, low95[5], midpt, high95[6], 35),
      c(0, 0, 1, 0, 0), col = "black", lwd = 2)
title("Smoothed Method", line = 0.5, cex.main = 1.2, font.main = 1)
title(ylab = "suitability", line = 0.5)

#----------Jagged Method------------

x <- seq(0, 35, by = 0.1)
m <- rep(NA, length(x))
numInput <- sum(!is.na(litData$CBH_MAX) & !is.na(litData$CBH_MIN))

for (i in 1:length(m)){
  m[i] <- sum(x[i] > litData$CBH_MIN & x[i] < litData$CBH_MAX, na.rm = TRUE)
}

plot(x, m/numInput, xlab = "", ylab = "", 
     ylim = c(0, 1), type = "l", lwd = 2, col = "black",
     xaxt = "n", yaxt = "n", xlim = c(0, 35))
axis(2, at = c(0, 1), lwd = 0.75)

title("Jagged Method", line = 0.5, cex.main = 1.2, font.main = 1)
title(ylab = "suitability", line = 0.5)
title(xlab = "covariate value", line = 0.8)

rm(x, m)

# Close output pdf
dev.off()


