# Script name: _plotLMresults.R
# Author: S. Bogen
#
# Produces Figures 3.5, 3.6, 4.5, 4.6, 4.7, and 4.8
#-------------------------------------------------------------------------------

library(ggplot2)
library(sjPlot)

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/5 FIGURES")

# Load results objects
load("../4 ANALYSES/resultsUnmitigated.Rdata")
load("../4 ANALYSES/resultsBiome.Rdata")
load("../4 ANALYSES/resultsOrder.Rdata")

# Remove unneeded PCA objects
rm(pca_biome, pca_order, pca_growing)

#-------------------------------------------------------------------------------
# Unmitigated
#-------------------------------------------------------------------------------

lmReduced$model$dm <- factor(lmReduced$model$dm,
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

# PC 1- unmitigated
plot_model(lmReduced, 
           type = "pred", 
           terms = c("pc1", "gf", "dm"), 
           title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 1", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 3, labeller = panelLabels) +
  theme_bw()
ggsave("pc1_unmitigated.pdf", width = 8, height = 5.5)

# PC 2 - umitigated 
plot_model(lmReduced, 
           type = "pred", 
           terms = c("pc2", "gf", "dm"), 
           title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 2", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 3, labeller = panelLabels) +
  theme_bw()
ggsave("pc2_unmitigated.pdf", width = 8, height = 5.5)

#-------------------------------------------------------------------------------
# Order mitigated
#-------------------------------------------------------------------------------

lm_order$model$dm <- factor(lm_order$model$dm,
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
plot_model(lm_order, 
           type = "pred", 
           terms = c("pc1", "gf", "dm"), 
           title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 1 (Order Mitigated)", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 3, labeller = panelLabels) +
  theme_bw()

ggsave("pc1_order.pdf", width = 8, height = 5.5)

# PC 2
plot_model(lm_order, 
           type = "pred", 
           terms = c("negPC2", "gf", "dm"), 
           title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "-1 * [PC 2 (Order Mitigated)]", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 3, labeller = panelLabels) +
  theme_bw()

ggsave("pc2_order.pdf", width = 8, height = 5.5)

#-------------------------------------------------------------------------------
# Biome mitigated
#-------------------------------------------------------------------------------

lm_biome$model$dm <- factor(lm_biome$model$dm,
                            levels = c("vertebrate",
                                       "wind.appendage",
                                       "ballistic",
                                       "wind.none"))

panelLabels <- as_labeller(c(`vertebrate` = "Vertebrate",
                             `wind.appendage` = "Wind",
                             `ballistic` = "Ballistic",
                             `wind.none` = "Unassisted"))

# Plot marginal effects

# PC 1
plot_model(lm_biome, 
           type = "pred", 
           terms = c("pc1", "gf", "dm"), 
           title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 1 (Order Mitigated)", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 2, labeller = panelLabels) +
  theme_bw()

ggsave("pc1_biome.pdf", width = 8, height = 5.5)

# PC 2
plot_model(lm_biome, 
           type = "pred", 
           terms = c("pc2", "gf", "dm"), 
           title = "", 
           legend.title = "Growth\nForm", 
           axis.title = labs(x = "PC 2", y = "log(cstar)"),
           colors = c("#a6611a", "#018571")) +
  facet_wrap(~facet, ncol = 2, labeller = panelLabels) +
  theme_bw()

ggsave("pc2_biome.pdf", width = 8, height = 5.5)


#-------------------------------------------------------------------------------
# Also -  view summaries and plot diagnostics for all (for appendix)
#-------------------------------------------------------------------------------

summary(lmReduced)
summary(lm_biome)
summary(lmReduced)

# Assumption checking on unmitigated model
par(mfrow=c(1, 3))
plot(lmReduced, 1) # linearity
plot(lmReduced, 2) # normality of residuals
plot(lmReduced, 3) # constant variance 
dev.off()

# Assumption checking on biome-mitigated model
par(mfrow=c(1, 3))
plot(lm_biome, 1) # linearity
plot(lm_biome, 2) # normality of residuals
plot(lm_biome, 3) # constant variance 
dev.off()

# Assumption checking on order-mitigated model
par(mfrow=c(1, 3))
plot(lm_order, 1) # linearity
plot(lm_order, 2) # normality of residuals
plot(lm_order, 3) # constant variance 
dev.off()
