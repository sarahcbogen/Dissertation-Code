# Script Name: _estimateSpread.R
# Author: S. Bogen
#
# Calculates spread rates for all virtual species. Outputs data frames for all
# virtual species along with calculated spread reates.
#-------------------------------------------------------------------------------

library(pracma)

# Set working directory
setwd("~/Desktop/Virtual Species Chapters Code/PIPELINE/3 VIRTUAL SPECIES")

# Read in virtual species from all three simulations
dataUM <- read.csv("../3 VIRTUAL SPECIES/VS all/simpleSim10_all.csv")
dataOrder <- read.csv("../3 VIRTUAL SPECIES/VS all/orderSim_all.csv")
dataBiome <- read.csv("../3 VIRTUAL SPECIES/VS all/biomeSim_all2.csv")

# Filter out non-growing populations (lambda < 1, logLambda < 0 )
dataUMgrowing <- dataUM[dataUM$logLambda >= 0, -1]
dataOrderGrowing <- dataOrder[dataOrder$logLambda >= 0, -1]
dataBiomeGrowing <- dataBiome[dataBiome$logLambda >= 0, -1]

# Remove full data frames
rm(dataUM, dataOrder, dataBiome)

#-------------------------------------------------------------------------------
# Define cstar calculation functions
#-------------------------------------------------------------------------------

# Inputs are the 2d gaussian dispersal parameter and the population growth
# rate parameter
calculateCSTARgauss <- function(gauss2d, lambda){
  
  mu = 0  # The kernel function is centered at zero
  
  # calculate mean dispersal (Mean Absolute Deviation) from gauss2d
  # MAD2D <- a*sqrt(pi)/2 FROM NATHAN
  mad <- gauss2d*sqrt(pi)/2
  
  # calculate the gauss1d dispersal parameter from mad
  sigma <- mad * sqrt(pi/2) # CORRECTED

  # calcuate mgf and cstar for Gaussian kernel
  s <- sqrt(2*log(lambda)/sigma^2)
  log_mgfGauss1d <- mu*s + sigma^2 * s^2 / 2 
  cstarGauss <- 1/s*(log(lambda) + log_mgfGauss1d)
  
  return(cstarGauss)
  
}

# IF I make a function:
# Input will be gauss2d AND lambda for the desired data set
# Stuff to do within function:
#   Calculate mad from gauss2D
#   Calculate the Laplace paramter from mad
#   Create an empty vector for numerically estimated cstarLap values
#   Use a loop and the "optimize" function to fill in cstarLap values
# 
# Output should be a vector of cstarLap

calculateCSTARlap <- function(gauss2d, lambda){
  
  mu = 0  # The kernel function is centered at zero
  
  # calculate mean dispersal (Mean Absolute Deviation) from gauss2d
  # MAD2D <- a*sqrt(pi)/2 FROM NATHAN
  mad <- gauss2d*sqrt(pi)/2
  
  # calculate Laplace parameter from mean dispersal
  # MAD1D = b*ln(2)
  b <- mad/log(2)
  
  # Critical spread rate (cstar)
  # cstar = min(s>0) (1/s)ln[lambda*M(s)]
  # cstarLaplace = min(0<s<1/b) ln(lambda)/s - ln(1-b^2s^2)/s
  # For the Laplace distribtuion, the minimum does not have a solution in closed
  #     form so we need to use a numerical approach
  
  cstarLap <- c()
  
  for (i in 1:length(lambda)){
    f <- function(s) (log(lambda[i])/s - log(1-b[i]^2*s^2)/s)
    cstarLap <- c(cstarLap, 
                  optimize(f, interval = c(0, 1/b[i]), tol = 0.0001)$objective)
  }
  
  return(cstarLap)
  
}

#-------------------------------------------------------------------------------
# Calcuate cstar value or values for each simulated dataset
#-------------------------------------------------------------------------------

# Unmitigated simulation - gaussian and laplace
cstarGaussUM <- calculateCSTARgauss(exp(dataUMgrowing$logGauss2D),
                                    exp(dataUMgrowing$logLambda))
cstarLapUM <- calculateCSTARlap(exp(dataUMgrowing$logGauss2D),
                                exp(dataUMgrowing$logLambda))

# Order-mitigated simulation - gaussian only
cstarGaussOrder <- calculateCSTARgauss(exp(dataOrderGrowing$logGauss2D),
                                       exp(dataOrderGrowing$logLambda))

# Biome-mitigated simulation - gaussian only
cstarGaussBiome <- calculateCSTARgauss(exp(dataBiomeGrowing$logGauss2D),
                                       exp(dataBiomeGrowing$logLambda))

#-------------------------------------------------------------------------------
# Log-transform speeds, build full virtual species data frames and write to csv
#-------------------------------------------------------------------------------

logSpeedGauss <- log(cstarGaussUM)
logSpeedLap <- log(cstarLapUM)
unmitigatedFullFrame <- data.frame(dataUMgrowing, logSpeedGauss, logSpeedLap)

logSpeedGauss <- log(cstarGaussOrder)
orderFullFrame <- data.frame(dataOrderGrowing, logSpeedGauss)

logSpeedGauss <- log(cstarGaussBiome)
biomeFullFrame <- data.frame(dataBiomeGrowing, logSpeedGauss)

# clean up space
rm(cstarGaussUM, cstarLapUM, cstarGaussOrder, cstarGaussBiome, 
   logSpeedGauss, logSpeedLap,
   dataUMgrowing, dataOrderGrowing, dataBiomeGrowing)

# write data frames to files
write.csv(unmitigatedFullFrame, row.names = FALSE, 
          "VS growing with speeds/unmitigated.csv")
write.csv(orderFullFrame, row.names = FALSE, 
          "VS growing with speeds/orderMitigated.csv")
write.csv(biomeFullFrame, row.names = FALSE, 
          "VS growing with speeds/biomeMitigated.csv")
