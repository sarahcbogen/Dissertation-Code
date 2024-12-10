# Script name: buildScatterPlots.R
# Author: S. Bogen
#
# Built scatter plots for my defense presentations.
# Input: model used to simulate virtual species AND input data
# Outputs: pairwise scatter plots representing sparse input data, modeled data,
# and virtual species capturing variation. Lower panels show the number of
# data points captured in each scatter plot.
#-------------------------------------------------------------------------------

library(Matrix)
library(coda)
library(ape)
library(MCMCglmm)

# Set to working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/5 FIGURES")

# Read in input and simulation data
datReal <- read.csv("../3 VIRTUAL SPECIES/VS for scatter plot/inputDataForScatter.csv")
simpleSim <- read.csv("../3 VIRTUAL SPECIES/VS for scatter plot/simpleSimForScatter.csv")
sim10 <- read.csv("../3 VIRTUAL SPECIES/VS for scatter plot/sim10forScatter.csv")#[, -1]

#-------------------------------------------------------------------------------
# Define functions that draw the panels for each scatter plot
#-------------------------------------------------------------------------------

# plot pairwise scatter plots for all variables - real data in black, model in blue

upper.default <- function(x, y){
  points(x, y, pch=19, cex=rep(c(0.2, 0.2), 
                               c(nrow(simpleSim), 
                                 nrow(datReal))), 
         col = rep(c("light blue", "black"),
                   c(nrow(simpleSim), nrow(datReal))))
}

upper.sim10 <- function(x, y){
  points(x, y, pch=19, cex=rep(c(0.2, 0.2), 
                               c(nrow(sim10), 
                                 nrow(datReal))), 
         col = rep(c("light blue", "black"),
                   c(nrow(sim10), nrow(datReal))))
}

upper.real <- function(x, y){
  points(x, y, pch=19, cex = 0.2, col = "black")
}

# Add labels
lower.labs <- function(x, y){
  
  points(x, y, col = "white")
  
  z <- x + y
  count <- sum(!is.na(z))
  
  mid_x <- mean(range(x, na.rm = TRUE))
  mid_y <- mean(range(y, na.rm = TRUE))
  
  # text(mid_x, mid_y, "foo", col = "navyblue")
  
  text(mid_x, mid_y, paste("n =", as.character(count)), col = "gray30",
       cex = 1.2)
}

lower.all <- function(x, y){
  
  points(x, y, col = "white")
  
  mid_x <- mean(range(x, na.rm = TRUE))
  mid_y <- mean(range(y, na.rm = TRUE))
  
  text(mid_x, mid_y, paste("n = 2387"), col = "steelblue",
       cex = 1.2)
}

lower.sim <- function(x, y){
  
  points(x, y, col = "white")
  
  mid_x <- mean(range(x, na.rm = TRUE))
  mid_y <- mean(range(y, na.rm = TRUE))
  
  text(mid_x, mid_y, paste("n = 23,870"), col = "steelblue",
       cex = 1.2)
}

#-------------------------------------------------------------------------------
# Build scatter plots
#-------------------------------------------------------------------------------

# Only real data
# "Sparse input data" figure in dissertation presentation
pairs(datReal, lower.panel = lower.labs, upper.panel=upper.real,
      labels = c("Leaf\nArea", "Rooting\nDepth", "Leaf\nNitrogen", 
                 "Specific\nLeaf\nArea", "Wood\nDensity", "", "a"),
      cex.labels = 1.5)

# Real and simple simulation
# "Modeled data" figure in dissertation presentation
pairs(rbind(simpleSim, datReal),lower.panel = lower.all, upper.panel=upper.default,
      labels = c("Leaf\nArea", "Rooting\nDepth", "Leaf\nNitrogen", 
                 "Specific\nLeaf\nArea", "Wood\nDensity", "", "a"),
      cex.labels = 1.5)
mtext(expression(lambda), side = 1)

# Real and simple simulation x 10
# "Virtual Species capturing variation" figure in dissertation presentation
pairs(rbind(sim10, datReal),lower.panel = lower.sim, upper.panel=upper.sim10,
      labels = c("Leaf\nArea", "Rooting\nDepth", "Leaf\nNitrogen", 
                 "Specific\nLeaf\nArea", "Wood\nDensity", "", "a"),
      cex.labels = 1.5)

# TODO: save as pngs for use in dissertation

#-------------------------------------------------------------------------------
# Build scatter plots for dissertation
#-------------------------------------------------------------------------------

# Open a PNG device
png("scatter_plot_real.png", width = 600, height = 600)

# Create the pairs plot
# Note to self: using expression(lambda) causes alignment issues with the
# other labels. Font must also be uniform, so I cannot italicize only lambda.
# Adding text labels after the fact does not work.
pairs(datReal, lower.panel = lower.labs, upper.panel=upper.real,
      labels = c("Leaf\nArea", "Rooting\nDepth", "Leaf\nNitrogen", 
                 "Specific\nLeaf\nArea", "Wood\nDensity", "", "a"),
      cex.labels = 1.5)
mtext(expression(lambda), side = 1, line = -5.5, adj = 0.79, cex = 1.5)

# Close the PNG device
dev.off()


png("scatter_plot_sim.png", width = 600, height = 600)
pairs(rbind(sim10, datReal),lower.panel = lower.sim, upper.panel=upper.sim10,
      labels = c("Leaf\nArea", "Rooting\nDepth", "Leaf\nNitrogen", 
                 "Specific\nLeaf\nArea", "Wood\nDensity", "", "a"),
      cex.labels = 1.5)
mtext(expression(lambda), side = 1, line = -5.5, adj = 0.79, cex = 1.5)
dev.off()


