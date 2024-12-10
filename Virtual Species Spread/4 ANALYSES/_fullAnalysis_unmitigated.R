# Script name: _fullAnalysis_unmitigated.R
# Author: S. Bogen
#
# Runs PCA and linear modeling on full, unmitigated data set. Outputs PCA result
# object and linear model objects and parallel analysis result object
#-------------------------------------------------------------------------------

library(RColorBrewer)
library(gridExtra)
library(grid)
library(factoextra)
library(ggplot2)
library(sjPlot)
library(psych)
library(lmtest)
library(factoextra)

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/4 ANALYSES")

# Read in data for growing populations with speed
growingWithSpeeds <- read.csv("../3 VIRTUAL SPECIES/VS growing with speeds/unmitigated.csv")

# Scale each numeric column in the growing
growingScaled <- as.data.frame(scale(growingWithSpeeds[, 3:9]))

#-------------------------------------------------------------------------------
# Plotting function for loadings bar plots
#-------------------------------------------------------------------------------

graphLoads <- function(pcaObject, pcaNum, title){
  
  trait <- names(pcaObject$rotation[, pcaNum])
  rotation <- pcaObject$rotation[, pcaNum]
  foo <- data.frame(trait, rotation)
  ggplot(data = foo,  aes(x = trait, y = rotation)) +
    geom_bar(stat = "identity") +
    labs(title = title, x = "", y = "loading") +
    theme_minimal() + coord_flip()
  
}

#-------------------------------------------------------------------------------
# Run and visualize the PCA
#-------------------------------------------------------------------------------

# super-simple PCA - just traits
pca_growing <- prcomp(growingScaled)

# Scree plot
scree <- fviz_eig(pca_growing); scree

# quick peek at loadings on each PCA
grid.arrange(graphLoads(pca_growing, 1, "PCA 1"),
             graphLoads(pca_growing, 2, "PCA 2"),
             graphLoads(pca_growing, 3, "PCA 3"), nrow = 1)

# Do parallel analysis to choose number of PCs for linear model
parallel_result <- fa.parallel(growingScaled, quant = 0.95)
parallel_result
# Keep 2

#-------------------------------------------------------------------------------
# Full model with all PCs identified by parallel analysis
# use 0.01 as the p-value threshold due to our large sample size
#-------------------------------------------------------------------------------

# Assemble a data frame for linear modeling
lmFrame <- data.frame(pc1 = pca_growing$x[, 1], pc2 = pca_growing$x[, 2],
                      pc3 = pca_growing$x[, 3], gf = growingWithSpeeds$GF,
                      dm = growingWithSpeeds$DS_Bullock, 
                      logCstar = growingWithSpeeds$logSpeedGauss)

# Fit the full model
fullModel <- lm(logCstar ~ pc1*dm*gf + pc2*dm*gf, data = lmFrame)
summary(fullModel)

# FIRST do the assumption checking on the full model

drop1(fullModel, test = "F") #  drop dm:gf:pc2
red1 <- lm(logCstar ~ pc1*dm*gf + pc2*dm*gf - dm:gf:pc2, data = lmFrame)
lrtest(fullModel, red1) # p value slightly grater that 0.01 - keep going

drop1(red1, test = "F") #  drop pc1:dm:gf for largest p-value and smallest aic
red2 <- lm(logCstar ~ pc1*dm*gf + pc2*dm*gf - dm:gf:pc2 - pc1:dm:gf, 
           data = lmFrame)
lrtest(red1, red2) # super small p-value - STOP and use red1

# final model
summary(red1)

#-------------------------------------------------------------------------------
# Linear model
#-------------------------------------------------------------------------------

# lmReduced <- red1
lmReduced <- fullModel # Go back to the full model

# Assumption checking on reduced model
par(mfrow=c(1, 3))
plot(lmReduced, 1) # linearity
plot(lmReduced, 2) # normality of residuals
plot(lmReduced, 3) # constant variance 
dev.off()

summary(lmReduced)

# Plot effects

plot_model(lmReduced, type = "pred", terms = c("pc1", "gf", "dm"), title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 1", y = "log(cstar)"))

plot_model(lmReduced, type = "pred", terms = c("pc2", "gf", "dm"), title = "",
           legend.title - "Growth\nForm", 
           axis.title = labs(x = "PC 2", y = "log(cstar)"))


table(growingWithSpeeds$GF)
table(growingWithSpeeds$DS_Bullock)

par(mfrow=c(1, 2))
barplot(table(growingWithSpeeds$GF))
barplot(table(growingWithSpeeds$DS_Bullock))

# Plot marginal effects of pc1 and pc2 overall
min <- min(lmFrame$logCstar)
plot_model(lmReduced, type = "pred", terms = c("pc1"), col = "black") + xlim(-15, 10) + ylim(-2, 8)
plot_model(lmReduced, type = "pred", terms = c("pc2"), col = "black") + xlim(-15, 10) + ylim(-2, 8)

# Calculate margins (stata)
library(margins)
margins(lmReduced)

#-------------------------------------------------------------------------------
# Save PCA and selected lm
#-------------------------------------------------------------------------------

save(pca_growing, lmReduced, file = "resultsUnmitigated.Rdata")

