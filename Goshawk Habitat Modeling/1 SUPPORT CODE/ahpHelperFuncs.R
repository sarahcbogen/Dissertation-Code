# File name: ahpHelperFuncs.R
# Author: S. Bogen
#
# Contains functions to be used in multiple steps of the pipeline, including
# prediction generation and uncertainty quantification. Designed for specific 
# use in this analysis pipeline and not intended to be fully portable or 
# generalizable to other contexts.
#-------------------------------------------------------------------------------

geoMean <- function(x, na.rm = FALSE, trim = 0, ...){
  
  # Computes the geometric mean of a vector. Code adapted from url:
  # r-bloggers.com/2011/01/really-useful-bits-of-code-that-are-missing-from-r/ 
  #
  # Args:
  #   x: A numeric vector that may contain NA values
  #   na.rm: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #   trim: arg trim to be passed to mean(). Default is 0.
  #   ...: additional arguments to be passed to mean()
  #
  # Returns:
  #   A numeric value representing the geometric mean
  
  exp(mean(log(x, ...), na.rm = na.rm, trim = trim, ...))
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

calcWeights <- function(surveydata){
  
  # Generates covariate weights (the w vector) from expert survey responses
  #
  # Args:
  #   surveydata: A data matrix containing expert survey responses in format
  #               output by the spreadsheet response collection tool
  #
  # Returns:
  #   A numeric vector of standardized priority weights for each covariate
  
  atts <- c("FT", "CC", "SA", "CBH", "BA", "ELEV", "SLOPE", "ASPECT")
  survey.ahp <- surveydata %>% ahp.mat(atts = atts, reciprocal = TRUE)
  
  # Resolving inconsistencies using Harker's method 
  set.seed(1234)
  edited <- ahp.harker(survey.ahp, atts, iterations = 20, stopcr = 0.1,
                       printiter = FALSE)
  
  #Final aggregated weights - normalize and return result
  aeigen_edit <- ahp.aggpref(edited, atts, method = "eigen", 
                             aggmethod = "geometric")
  return(sum(aeigen_edit)^(-1)*aeigen_edit)
  
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

hsmBasic <- function(name, suitVals, weights) {
  
  # Uses the "Basic" AHP method to generate a habitat suitability raster for a
  # specified forest.
  #
  # Args:
  #   name: The name of the forest (Ashley or Fishlake)
  #   suitVals: A vector containing the min and max values for the lit-defined
  #             suitable range of each covariate
  #   weights: The appropriate w vector returned by the calcWeights() function
  #
  # Returns:
  #   A habitat suitability raster for the extent of the specified forest
  
  #--------Read in covariates--------------
  
  FT <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/ForestType", capitalize(name), ".tif", sep = ""))
  CC <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/CanopyCover", capitalize(name), ".tif", sep = ""))
  SA <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/StandAge", capitalize(name), ".tif", sep = ""))
  CBH <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                      "/CanopyBaseHeight", capitalize(name), ".tif", sep = ""))
  BA <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/BasalArea", capitalize(name), ".tif", sep = ""))
  ELEV <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                       "/Elevation", capitalize(name), ".tif", sep = ""))
  SLOPE <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                        "/Slope", capitalize(name), ".tif", sep = ""))
  ASPECT <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                         "/Aspect", capitalize(name), ".tif", sep = ""))
  
  #------Create boolean rasters------------

  # Forest type is hard coded - numbers from 0 up to and not 
  # including 500 AND 901 (Conifers and mixed stand) AND (Aspen)
  FT_bool <- (FT > 0 & FT < 500) | FT==901
  
  # Cutoffs simply defined by suitable range endpoints from lit review
  CC_bool <- CC >= suitVals[1] & CC <= suitVals[2]
  SA_bool <- SA >= suitVals[3] & SA <= suitVals[4]
  CBH_bool <- FT >= suitVals[5] & FT <= suitVals[6]
  BA_bool <- BA >= suitVals[7] & BA <= suitVals[8]
  ELEV_bool <- ELEV >= suitVals[9] & ELEV <= suitVals[10]
  
  # Slope is defined by lit review but must be converted from percent to degrees
  slopeMin <- atan(suitVals[11]/100) * (180/pi)
  slopeMax <- atan(suitVals[12]/100) * (180/pi)
  SLOPE_bool <- SLOPE >= slopeMin & SLOPE <= slopeMax
  
  # Aspect is also hard-coded - 0-45 degrees (North-NorthEast Facing slopes)
  ASPECT_bool <- ASPECT >= 0 & ASPECT <= 45
  
  # Change all NA values in the stand age boolean to 0 (outside of suitable range)
  SA_bool[is.na(SA_bool)] <- 0
  
  #--------Generate prediction-------------

  pred <- weights[1]*FT_bool + weights[2]*CC_bool + weights[3]*SA_bool + 
    weights[4]*CBH_bool + weights[5]*BA_bool + weights[6]*ELEV_bool + 
    weights[7]*SLOPE_bool + weights[8]*ASPECT_bool
  
  return (pred)
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

helperSmoothed <- function(rawRaster, inA, inB, inC, inD){
  
  # Supports the hsmSmoothed() function by creating the individual suitability
  # raster for a given covariate
  #
  # Args:
  #   rawRaster: Covariate raster for the forest of interest
  #   inA: Bootstrapped low minimum for the covariate of interest
  #   inB: Bootstrapped high minimum for the covaraite of interest
  #   inC: Bootstrapped low maximum for the covariate of interest
  #   inD: Bootstrapped high maximum for the covariate of interest
  #
  # Returns:
  #   A suitability raster for the covariate of interst
  
  # Define a and d
  a <- inA
  d <- inD
  
  # Define b and c, controlling for case where b < c
  if(inC < inB){
    b <- (inB - inC)/2 + inC
    c <- (inB - inC)/2 + inC
  } else {
    b <- inB
    c <- inC
  }
  
  # Set suitability for values less than a (0)
  sec1 <- (rawRaster <= a) * 0
  
  # Set suitability for values between a and b (linear function)
  sec2prep <- (rawRaster > a & rawRaster <= b) * rawRaster
  sec2 <- calc(sec2prep, fun = function(x){if(x!=0){(x - a) / (b - a)}else{0}})
  
  # Set suitability for values between b and c (1)
  sec3 <- (rawRaster > b & rawRaster <= c) * 1
  
  # Set suitability for values between c and d (linear function)
  sec4prep <- (rawRaster > c & rawRaster <= d) * rawRaster
  sec4 <- calc(sec4prep, fun = function(x){if(x!=0){(d - x) / (d - c)}else{0}})
  
  # Set suitability for values greater than d (0)
  sec5 <- (rawRaster > d) * 0
  
  # Sum up and return result
  suitRaster <- sum(sec1, sec2, sec3, sec4, sec5)
  return(suitRaster)
  
}

#-------------------------------------------------------------------------------

hsmSmoothed <- function (name, litData, weights) {
  
  # Uses the "Smoothed" AHP method to generate a habitat suitability raster for
  # a specified forest.
  #
  # Args:
  #   name: The name of the forest (Ashley or Fishlake)
  #   litData: A data frame containing min and max values of each covariate
  #            described in each data source. NA values are acceptable.
  #   weights: The appropriate w vector returned by the calcWeights() function
  #
  # Returns:
  #   A habitat suitability raster for the extent of the specified forest
  
  #--------Read in covariates--------------
  
  FT <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/ForestType", capitalize(name), ".tif", sep = ""))
  CC <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/CanopyCover", capitalize(name), ".tif", sep = ""))
  SA <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/StandAge", capitalize(name), ".tif", sep = ""))
  CBH <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                      "/CanopyBaseHeight", capitalize(name), ".tif", sep = ""))
  BA <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/BasalArea", capitalize(name), ".tif", sep = ""))
  ELEV <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                       "/Elevation", capitalize(name), ".tif", sep = ""))
  SLOPE <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                        "/Slope", capitalize(name), ".tif", sep = ""))
  ASPECT <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                         "/Aspect", capitalize(name), ".tif", sep = ""))
  
  # Deal with NA values - stand age to 0 , slope to 0, aspect to -1
  SA[is.na(SA)] <- 0
  SLOPE[is.na(SLOPE)] <- 0
  ASPECT[is.na(ASPECT)] <- -1
  
  #------Get the bootstrap values--------------
  
  set.seed(1234)
  
  helperBootSuitVals <- data.frame(matrix(nrow = 0, ncol = 12))
  
  for (i in 1:500){
    
    indeces <- sample(1:19, replace = TRUE)
    
    geoMean_of_litSources <- apply(litData[indeces, -1], 2, 
                                   geoMean, na.rm = TRUE)
    helperBootSuitVals <- rbind(helperBootSuitVals, geoMean_of_litSources)
    rm(geoMean_of_litSources)
    
  }
  
  low95 <- sapply(helperBootSuitVals, quantile, probs = 0.025, na.rm = TRUE)
  high95 <- sapply(helperBootSuitVals, quantile, probs = 0.975, na.rm = TRUE)
  
  #------Create suitability rasters------------
  
  # Forest type is hard coded - numbers from 0 up to and not 
  # including 500 AND 901 (Conifers and mixed stand) AND (Aspen)
  FT_suit <- (FT > 0 & FT < 500) | FT==901
  
  # CC defined by lit review
  CC_suit <- helperBootSmoothed(CC, low95[1], high95[1], low95[2], high95[2])
  
  # SA defined by lit review
  SA_suit <- helperBootSmoothed(SA, low95[3], high95[3], low95[4], high95[4])
  
  # CBH defined by lit review
  CBH_suit <- helperBootSmoothed(CBH, low95[5], high95[5], low95[6], high95[6])
  
  # BA defined by lit review
  BA_suit <- helperBootSmoothed(BA, low95[7], high95[7], low95[8], high95[8])
  
  # ELEV defined by lit review
  ELEV_suit <- helperBootSmoothed(ELEV, low95[9], high95[9], 
                                  low95[10], high95[10])
  
  # Slope is defined by lit review but must be converted from percent to degrees
  SLOPE_suit <- helperBootSmoothed(SLOPE, atan(low95[11]/100) * (180/pi), 
                                   atan(high95[11]/100) * (180/pi),
                                   atan(low95[12]/100) * (180/pi),
                                   atan(high95[12]/100) * (180/pi))
  
  # Aspect is also hard-coded - 0-45 degrees (North-NorthEast Facing slopes)
  ASPECT_suit <- ASPECT >= 0 & ASPECT <= 45
  
  #--------Generate prediction-------------
  
  pred <- weights[1]*FT_suit + weights[2]*CC_suit + weights[3]*SA_suit + 
    weights[4]*CBH_suit + weights[5]*BA_suit + weights[6]*ELEV_suit + 
    weights[7]*SLOPE_suit + weights[8]*ASPECT_suit
  
  return(pred)
  
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

hsmJagged <- function (name, litData, weights) {
  
  # Uses the "Jagged" AHP method to generate a habitat suitability raster for a
  # specified forest.
  #
  # Args:
  #   name: The name of the forest (Ashley or Fishlake)
  #   litData: A data frame containing min and max values of each covariate
  #            described in each data source. NA values are acceptable.
  #   weights: The appropriate w vector returned by the calcWeights() function
  #
  # Returns:
  #   A habitat suitability raster for the extent of the specified forest
  
  #--------Read in covariates--------------
  
  FT <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/ForestType", capitalize(name), ".tif", sep = ""))
  CC <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/CanopyCover", capitalize(name), ".tif", sep = ""))
  SA <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/StandAge", capitalize(name), ".tif", sep = ""))
  CBH <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                      "/CanopyBaseHeight", capitalize(name), ".tif", sep = ""))
  BA <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                     "/BasalArea", capitalize(name), ".tif", sep = ""))
  ELEV <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                       "/Elevation", capitalize(name), ".tif", sep = ""))
  SLOPE <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                        "/Slope", capitalize(name), ".tif", sep = ""))
  ASPECT <- raster(paste("../0 DATA/spatial_data/CLEAN/", toupper(name), 
                         "/Aspect", capitalize(name), ".tif", sep = ""))
  
  #------Create suitability rasters------------
  
  # Forest type is hard coded - numbers from 0 up to and not 
  # including 500 AND 901 (Conifers and mixed stand) AND (Aspen)
  FT_suit <- (FT > 0 & FT < 500) | FT==901
  
  # CC defined by lit review
  temp <- stack()
  ccMin <- litData$CC_MIN[!is.na(litData$CC_MIN)]
  ccMax <- litData$CC_MAX[!is.na(litData$CC_MAX)]
  for (i in 1:length(ccMin)){
    newLay <- CC > ccMin[i] & CC < ccMax[i]
    temp <- stack(temp, newLay)
  }
  CC_suit <- calc(temp, mean)
  rm(temp)
  
  # SA defined by lit review
  temp <- stack()
  saMin <- litData$SA_MIN[!is.na(litData$SA_MIN)]
  saMax <- litData$SA_MAX[!is.na(litData$SA_MAX)]
  for (i in 1:length(saMin)){
    newLay <- SA > saMin[i] & SA < saMax[i]
    temp <- stack(temp, newLay)
  }
  SA_suit <- calc(temp, mean)
  rm(temp)
  
  # CBH defined by lit review
  temp <- stack()
  cbhMin <- litData$CBH_MIN[!is.na(litData$CBH_MIN)]
  cbhMax <- litData$CBH_MAX[!is.na(litData$CBH_MAX)]
  for (i in 1:length(cbhMin)){
    newLay <- CBH > cbhMin[i] & CBH < cbhMax[i]
    temp <- stack(temp, newLay)
  }
  CBH_suit <- calc(temp, mean)
  rm(temp)
  
  # BA defined by lit review
  temp <- stack()
  baMin <- litData$BA_MIN[!is.na(litData$BA_MIN)]
  baMax <- litData$BA_MAX[!is.na(litData$BA_MAX)]
  for (i in 1:length(baMin)){
    newLay <- BA > baMin[i] & BA < baMax[i]
    temp <- stack(temp, newLay)
  }
  BA_suit <- calc(temp, mean)
  rm(temp)
  
  # ELEV defined by lit review
  temp <- stack()
  elevMin <- litData$ELEV_MIN[!is.na(litData$ELEV_MIN)]
  elevMax <- litData$ELEV_MAX[!is.na(litData$ELEV_MAX)]
  for (i in 1:length(elevMin)){
    newLay <- ELEV > elevMin[i] & ELEV < elevMax[i]
    temp <- stack(temp, newLay)
  }
  ELEV_suit <- calc(temp, mean)
  rm(temp)
  
  # Slope is defined by lit review but must be converted from percent to degrees
  temp <- stack()
  slopeMinPrep <- litData$SLOPE_MIN[!is.na(litData$SLOPE_MIN)]
  slopeMaxPrep <- litData$SLOPE_MAX[!is.na(litData$SLOPE_MAX)]
  slopeMin <- atan(slopeMinPrep/100) * (180/pi)
  slopeMax <- atan(slopeMaxPrep/100) * (180/pi)
  for (i in 1:length(slopeMin)){
    newLay <- SLOPE > slopeMin[i] & SLOPE < slopeMax[i]
    temp <- stack(temp, newLay)
  }
  SLOPE_suit <- calc(temp, mean)
  rm(temp)
  
  # Aspect is also hard-coded - 0-45 degrees (North-NorthEast Facing slopes)
  ASPECT_suit <- ASPECT >= 0 & ASPECT <= 45
  
  # Change all NA values in the stand age boolean to 0 (outside of suitable range)
  SA_suit[is.na(SA_suit)] <- 0
  
  #--------Generate prediction-------------
  
  pred <- weights[1]*FT_suit + weights[2]*CC_suit + weights[3]*SA_suit + 
    weights[4]*CBH_suit + weights[5]*BA_suit + weights[6]*ELEV_suit + 
    weights[7]*SLOPE_suit + weights[8]*ASPECT_suit
  
  return (pred)
  
}