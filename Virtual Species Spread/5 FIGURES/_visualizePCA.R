# Script name: _visualizePCA.R
# Author: S. Bogen
#
# Produces Figures 3.3, 3.4, 4.3, and 4.4
#-------------------------------------------------------------------------------

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/5 FIGURES")

# Load results objects
load("../4 ANALYSES/resultsUnmitigated.Rdata")
load("../4 ANALYSES/resultsBiome.Rdata")
load("../4 ANALYSES/resultsOrder.Rdata")

# Remove unneeded LM objects
rm(lm_biome, lm_order, lmReduced)

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
# Save the PCA loading bar plots
#-------------------------------------------------------------------------------

# Unmitigated
pdf("pcaLoadings_unmitigated.pdf", width = 8, height = 3)
grid.arrange(graphLoads(pca_growing, 1, "PCA 1"),
             graphLoads(pca_growing, 2, "PCA 2"),
             graphLoads(pca_growing, 3, "PCA 3"), nrow = 1)
dev.off()

# Biome mitigated
pdf("pcaLoadings_biome.pdf", width = 8, height = 3)
grid.arrange(graphLoads(pca_biome, 1, "PCA 1"),
             graphLoads(pca_biome, 2, "PCA 2"),
             graphLoads(pca_biome, 3, "PCA 3"), nrow = 1)
dev.off()

# Order mitigated
pdf("pcaLoadings_order.pdf", width = 8, height = 3)
grid.arrange(graphLoads(pca_order, 1, "PCA 1"),
             graphLoads(pca_order, 2, "PCA 2"),
             graphLoads(pca_order, 3, "PCA 3"), nrow = 1)
dev.off()

#-------------------------------------------------------------------------------
# Save biplot - Unmitigated
#-------------------------------------------------------------------------------

# Unmitigated
p <- fviz_pca_biplot(pca_growing, c(1, 2), label = "var", pointsize = 0.5, 
                     col.var = "steelblue", col.ind = "white", repel = TRUE, 
                     axes.linetype = "blank")
p + geom_point(data = as.data.frame(pca_growing$x[, c(1, 2)]), size = 0.6,
               aes(x = PC1, y = PC2), color = "black", alpha = 0.05) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

ggsave("pcaBiplot_unmitigated.pdf", width = 8, height = 5.5)


