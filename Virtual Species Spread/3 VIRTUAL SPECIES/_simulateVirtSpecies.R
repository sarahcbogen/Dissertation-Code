# File name: _simulateVirtSpecies.R
# Author: S. Bogen
#
# Uses input data that was on the scale used to fit the model. Transforms the 
# model outputs back onto their original scales afterward.
# Does NOT filter out species by Lambda - that is handled in the speed
# calculation step.
#
# ALSO OUTPUTS data frames for use in scatter plot figures - real data,
# simple simulation data, and tenfold simulation data. 
#-------------------------------------------------------------------------------

library(Matrix)
library(coda)
library(ape)
library(MCMCglmm)

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/3 VIRTUAL SPECIES")

# Read in GLMM:
load('../1 SUPPORT CODE/glmmBogen2022')
model <- Sarah_Strategy
rm(Sarah_Strategy)

# Read in and prepare data:
load("../0 DATA/observation data/Data_VirtualSpecies_Gaussian_Bullock_2022_ver2.Rdata")
source('../1 SUPPORT CODE/DataPrep.R')
data.all <- getdata(DispersalKernel = "Gaussian",
                    IncResponseTraits = c("leafN.Mean.Logtrans",
                                          "LeafArea.Mean.Logtrans", 
                                          "SLA.Mean.Logtrans",
                                          "rootingdepth.Mean.Logtrans", 
                                          "WD.Mean.Logtrans"))

datMinst <- data.all$datMinst
shift <- data.all$datMin.centering # centering is the mean
scale <- data.all$datMin.scaling # scaling is the sd
datMinst2 <- data.all$dataVS.analysis
datIVs <- datMinst[, c("DS_Bullock", "GF", "PH.Mean.Logtrans", 
                       "SeedMass.Mean.Logtrans", 
                       "Order")]  # independent variables

#-------------------------------------------------------------------------------
# Grab real data - will be useful for building scatter plots
#-------------------------------------------------------------------------------

# Extract real data
datReal <- datMinst[ , c("LeafArea.Mean.Logtrans",
                         "rootingdepth.Mean.Logtrans",
                         "leafN.Mean.Logtrans",
                         "SLA.Mean.Logtrans",
                         "WD.Mean.Logtrans",
                         "logMaxLambda",
                         "logGaussian2D")]
colnames(datReal) <- c('LA', 'RD', 'LeafN', 'SLA', 'WD', 'Lambda', 'logGauss2D')

#-------------------------------------------------------------------------------
# Simple Simulation - will be useful for building scatter plots
#-------------------------------------------------------------------------------

# Run simple simulation and restructure
set.seed(1234)
simpleSim <- simulate.MCMCglmm(model, newdata = datMinst)
foo <- matrix(as.vector(simpleSim), nrow = 2387)
colnames(foo) <- c('LA', 'RD', 'LeafN', 'SLA', 'WD', 'Lambda', 'logGauss2D')
simpleSim <- as.data.frame(foo)
rm(foo)

# NOTE that the scatter plots depict log-transformed AND standardized values
# All other analyses use log-transformed ONLY values (not standardized)

#-------------------------------------------------------------------------------
# Simple Simulation 10-fold
#-------------------------------------------------------------------------------

set.seed(1234)
# sim <- simulate.MCMCglmm(model, nsim = 1, newdata = datMinst, verbose = TRUE)
# simDF <- as.data.frame(matrix(sim, nrow = 2387)) # Not at all certain on this
# colnames(simDF) <- c('LA', 'RD', 'LeafN', 'SLA', 'WD', 'Lambda', 'logGauss2D')
# pred <- simulate.MCMCglmm(model, verbose = TRUE)
# predDF <- as.data.frame(matrix(pred, nrow = dim(datMinst)[1]))

# Run the simulation
pred <- simulate.MCMCglmm(model, nsim = 10, verbose = TRUE)

# Re-structure and merge the simulation output
predDF1 <- as.data.frame(matrix(pred[, 1], nrow = dim(datMinst)[1]))
predDF2 <- as.data.frame(matrix(pred[, 2], nrow = dim(datMinst)[1]))
predDF3 <- as.data.frame(matrix(pred[, 3], nrow = dim(datMinst)[1]))
predDF4 <- as.data.frame(matrix(pred[, 4], nrow = dim(datMinst)[1]))
predDF5 <- as.data.frame(matrix(pred[, 5], nrow = dim(datMinst)[1]))
predDF6 <- as.data.frame(matrix(pred[, 6], nrow = dim(datMinst)[1]))
predDF7 <- as.data.frame(matrix(pred[, 7], nrow = dim(datMinst)[1]))
predDF8 <- as.data.frame(matrix(pred[, 8], nrow = dim(datMinst)[1]))
predDF9 <- as.data.frame(matrix(pred[, 9], nrow = dim(datMinst)[1]))
predDF10 <- as.data.frame(matrix(pred[, 10], nrow = dim(datMinst)[1]))
predDF <- rbind(predDF1, predDF2, predDF3, predDF4, predDF5, predDF6, predDF7,
                predDF8, predDF9, predDF10)

# clean up space
rm(predDF1, predDF2, predDF3, predDF4, predDF5, predDF6, predDF7,
   predDF8, predDF9, predDF10)

# Merge model inputs and model outputs into a single frame
# This is what will be used for the scatter plots figure
colnames(predDF) <- c('LA', 'RD', 'LeafN', 'SLA', 'WD', 'Lambda', 'logGauss2D')

# Concatenate independent variables with the predicted values
predFull <- data.frame(rbind(datIVs, datIVs, datIVs, datIVs, datIVs,
                             datIVs, datIVs, datIVs, datIVs, datIVs), predDF)

# de-standardize - all will then be log transformed but not standardized
DS_Bullock <- predFull$DS_Bullock
GF <- predFull$GF
logPH <- predFull$PH.Mean.Logtrans * scale[9] + shift[9]         # 9
logSM <- predFull$SeedMass.Mean.Logtrans * scale[10] + shift[10] # 10
logLA <- predFull$LA * scale[2] + shift[2]                       # 2
logRD <- predFull$RD * scale[4] + shift[4]                       # 4
logLeafN <- predFull$LeafN * scale[1] + shift[1]                 # 1
logSLA <- predFull$SLA * scale[3] + shift[5]                     # 3
logWD <- predFull$WD * scale[5] + shift[5]                       # 5
logLambda <- predFull$Lambda * scale[7] + shift[7]               # 7
logGauss2D <- predFull$logGauss2D * scale[6] + shift[6]          # 6

predDeStandardized <- data.frame(DS_Bullock, GF, logPH, logSM, logLA, logRD, 
                                 logLeafN, logSLA, logWD, logLambda, logGauss2D)

# clean up space
rm(DS_Bullock, GF, logPH, logSM, logLA, logRD, logLeafN, logSLA, logWD, 
   logLambda, logGauss2D)

# Save destandardized virtual species, regardless of lambda value
# Numerical values are still log-transformed
write.csv(predDeStandardized, "VS all/simpleSim10_all.csv")

### Save scatter plot info ###
# All of these values are still log-transformed and standardized
write.csv(datReal, "VS for scatter plot/inputDataForScatter.csv", row.names = FALSE)
write.csv(simpleSim, "VS for scatter plot/simpleSimForScatter.csv", row.names = FALSE)
write.csv(predDF, "VS for scatter plot/sim10forScatter.csv", row.names = FALSE)
