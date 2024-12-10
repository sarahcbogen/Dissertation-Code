# Script name: _fullAnalysis_biome.R
# Author: S. Bogen
#
# Runs PCA and linear modeling on the virtual species dataset mitigated
# for biome-based bias.
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
growingWithSpeeds <- read.csv("../3 VIRTUAL SPECIES/VS growing with speeds/biomeMitigated.csv")

#-------------------------------------------------------------------------------
# Read in, Inspect data and drop entries as needed, scale functional traits
#-------------------------------------------------------------------------------

table(growingWithSpeeds$GF)
table(growingWithSpeeds$DS_Bullock)

par(mfrow=c(1, 2))
barplot(table(growingWithSpeeds$GF))
barplot(table(growingWithSpeeds$DS_Bullock))

table(growingWithSpeeds$GF, growingWithSpeeds$DS_Bullock)

# drop all ant-dispersed stuff (32 total, no herbs)
growingWithSpeeds <- growingWithSpeeds[!growingWithSpeeds$DS_Bullock=="ant", ]

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


# Biplot with density
p <- fviz_pca_biplot(pca_growing, c(1, 2), label = "var", pointsize = 0.5, 
                     col.var = "steelblue", col.ind = "white", repel = TRUE, 
                     axes.linetype = "blank")
p + geom_point(data = as.data.frame(pca_growing$x[, c(1, 2)]), size = 0.6,
               aes(x = PC1, y = PC2), color = "black", alpha = 0.05) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

#-------------------------------------------------------------------------------
# Full model with all PCs identified by parallel analysis
# use 0.01 as the p-value threshold due to our large sample size
#-------------------------------------------------------------------------------

# Assemble a data frame for linear modeling
lmFrame <- data.frame(pc1 = pca_growing$x[, 1], pc2 = pca_growing$x[, 2],
                      pc3 = pca_growing$x[, 3], gf = growingWithSpeeds$GF,
                      dm = growingWithSpeeds$DS_Bullock, 
                      logCstar = growingWithSpeeds$logSpeedGauss)


# Fit the full model - but do not include ANY interactions combining dm and gf
fullModel <- lm(logCstar ~ pc1*gf*dm + pc2*gf*dm, data = lmFrame)
summary(fullModel)

# FIRST do the assumption checking on the full model TODO
par(mfrow=c(1, 3))
plot(fullModel, 1) # linearity - BAD
plot(fullModel, 2) # normality of residuals - not terrible
plot(fullModel, 3) # constant variance - BAD
dev.off()

drop1(fullModel, test = "F") #  drop dm:gf:pc2
red1 <- lm(logCstar ~ pc1*dm*gf + pc2*dm*gf - dm:gf:pc2, data = lmFrame)
lrtest(fullModel, red1) # p value very small - stop and use full model.

#-------------------------------------------------------------------------------
# Linear model
#-------------------------------------------------------------------------------

lmFinal <- fullModel

summary(lmFinal)

# Define plot panel labels

panelLabels <- as_labeller(c(`vertebrate` = "AHP Base", 
                             `wind.appendage` = "Foo", 
                             `ant` = "changethis",
                             `wind.none` = "changethistoo",
                             `ballistic` = "andthis"))

# Plot marginal effects

plot_model(lmFinal, type = "pred", terms = c("pc1", "gf", "dm"),
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 1", y = "log(cstar)"))

plot_model(lmFinal, type = "pred", terms = c("pc2", "gf", "dm"), title = "",
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 2", y = "log(cstar)"))

#-------------------------------------------------------------------------------
# Save PCA and selected lm
#-------------------------------------------------------------------------------

pca_biome <- pca_growing
lm_biome <- lmFinal
save(pca_biome, lm_biome, file = "resultsBiome.Rdata")












