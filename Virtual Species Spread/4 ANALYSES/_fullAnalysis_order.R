# Script name: _fullAnalysis_biome.R
# Author: S. Bogen
#
# Runs PCA and linear modeling on the virtual species dataset mitigated
# for order-based bias.
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
growingWithSpeeds <- read.csv("../3 VIRTUAL SPECIES/VS growing with speeds/orderMitigated.csv")

#-------------------------------------------------------------------------------
# Read in, Inspect data and drop entries as needed, scale functional traits
#-------------------------------------------------------------------------------

table(growingWithSpeeds$GF)
table(growingWithSpeeds$DS_Bullock)

par(mfrow=c(1, 2))
barplot(table(growingWithSpeeds$GF))
barplot(table(growingWithSpeeds$DS_Bullock))

table(growingWithSpeeds$GF, growingWithSpeeds$DS_Bullock)

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
# Keep 2

#-------------------------------------------------------------------------------
# Full model with all PCs identified by parallel analysis
# use 0.01 as the p-value threshold due to our large sample size
#-------------------------------------------------------------------------------

# Assemble a data frame for linear modeling
lmFrame <- data.frame(pc1 = pca_growing$x[, 1], negPC2 = -1 * pca_growing$x[, 2],
                      pc3 = pca_growing$x[, 3], gf = growingWithSpeeds$GF,
                      dm = growingWithSpeeds$DS_Bullock, 
                      logCstar = growingWithSpeeds$logSpeedGauss)

# Fit the full model - but do not include ANY interactions combining dm and gf
fullModel <- lm(logCstar ~ pc1*gf*dm + negPC2*gf*dm, data = lmFrame)
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

plot_model(lmFinal, type = "pred", terms = c("negPC2", "gf", "dm"), title = "",
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "-1 * (PC 2)", y = "log(cstar)"))


#-------------------------------------------------------------------------------
# Figures for Defense results
#-------------------------------------------------------------------------------

lmFinal$model$dm <- factor(lmFinal$model$dm,
                             levels = c("vertebrate",
                                        "wind.appendage",
                                        "ant",
                                        "ballistic",
                                        "wind.none"))

panelLabels <- as_labeller(c(`vertebrate` = "Vertebrate",
                             `wind.appendage` = "Wind",
                             `ant` = "Ant",
                             `ballistic` = "Ballistic",
                             `wind.none` = "Unassisted"))

# PC 1
plot_model(lmFinal, 
           type = "pred", 
           terms = c("pc1", "gf", "dm"), 
           title = "", 
           # legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 1 (Order Mitigated)", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 5, labeller = panelLabels) +
  theme_bw() +
  theme(legend.position = "none")

# ggsave("../5 DEFENSE FIGURES/pc1_order.pdf", width = 12, height = 3)

# PC 2
plot_model(lmFinal, 
           type = "pred", 
           terms = c("negPC2", "gf", "dm"), 
           title = "", 
           # legend.title = "Growth\nForm", 
           axis.title = labs(x = "-1 * [PC 2 (Order Mitigated)]", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 5, labeller = panelLabels) +
  theme_bw() +
  theme(legend.position = "none")

# ggsave("../5 DEFENSE FIGURES/pc2_order.pdf", width = 12, height = 3)

#-------------------------------------------------------------------------------
# Save PCA and selected lm
#-------------------------------------------------------------------------------

pca_order <- pca_growing
lm_order <- lmFinal
save(pca_order, lm_order, file = "resultsOrder.Rdata")

