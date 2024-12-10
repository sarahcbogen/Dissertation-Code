# DataPrep.R
# Original code by NB. Edited for readability and annotated by SB.
# 
# Contains a single function: getdata()
#
# Purpose: get subset of species with corresponding trait values for analysis
#   Include all species with at least one trait value for the traits used in the 
#   multivariate response.
#
# Assumes there exists a data frame called dataVS that contains species-level
# information on functional traits, growth forms, and dispersal modes
#
# Inputs
#   excl: row information that denotes what species to exclude based on 
#         attributes such as growth form, family, dispersal mode, or magnitude 
#         of lambda. Example: excl = dataVS$Lambda>8
#   DispersalKernel: "Gaussian", "Exponential", or "ExpPower"
#   IncGF: subset of growth forms to include if NULL, include all growth forms 
#          in dataset unless specified by excl
#   IncTraits are the functional traits to include in the response (aside from 
#   dispersal parameters and population growth rates).
#
# Outputs (as a list)
#   dataMinst
#   datMin.centering
#   datMin.scaling
#   data_orig
#   dataVS.analysis
#-------------------------------------------------------------------------------

getdata <- function(excl = dataVS$GF=="climber", DispersalKernel = "Gaussian", 
                    IncGF=NULL, 
                    IncResponseTraits=c("leafN.Mean.Logtrans", 
                                        "LeafArea.Mean.Logtrans", 
                                        "SLA.Mean.Logtrans", 
                                        "rootingdepth.Mean.Logtrans", 
                                        "WD.Mean.Logtrans")) {

  # Define response variable list for each Kernel
  switch(DispersalKernel, 
         Gaussian = {AllResponseVal <- c(IncResponseTraits, "logGaussian2D", 
                                         "logMaxLambda")}, 
         Exponential = {AllResponseVal <- c(IncResponseTraits, "logExp", 
                                            "logMaxLambda")},
         ExpPower = {AllResponseVal <- c(IncResponseTraits, "loga", "logb", 
                                         "logMaxLambda")})
  
  # Get data
	data_orig <-dataVS[, c("Species", "DS_Bullock", "GF", AllResponseVal, 
	                      "SeedMass.Mean.Logtrans", "PH.Mean.Logtrans" , 
	                      "logMeanDisp", "Accepted_family", "Order")]

	#--------------------------------
	# Data cleaning
	#--------------------------------
	
  # Get rid of rows/species as specified with excl
  if (!is.null(excl)) data_orig <- data_orig[-which(excl),] 
  
  # Include a subset of growth forms as specified by IncGF  
  if (!is.null(IncGF)) data_orig <- data_orig[data_orig$GF== IncGF,]
	
  # Combine graminoids and herbs
	data_orig[which(data_orig$GF=="graminoid"),"GF"] <- "herb"

  # Combine shrubs and trees into woody category	
	data_orig[which(data_orig$GF=="shrub"),"GF"] <- "woody"
	data_orig[which(data_orig$GF=="tree"),"GF"] <- "woody"

  # Convert dispersal mode, growth form, and order to factors
  data_orig$DS_Bullock <- factor(data_orig$DS_Bullock)
  data_orig$GF <- factor(data_orig$GF)
  data_orig$Order <- factor(data_orig$Order)

  # all rows should have information on Order, dispersal mode, and growth from. 
  # If not, stop.
	if(sum(is.na(data_orig$Order)) > 0) stop("Not all species have an order assigned")
	if(sum(is.na(data_orig$DS_Bullock)) > 0) stop("Not all species have a dispersal mode assigned")
	if(sum(is.na(data_orig$GF)) > 0) stop("Not all species have a growth form assigned")	

  # Remove rows that don't have information on plant height or seed mass
  ess <- is.na(data_orig$PH.Mean.Logtrans) | is.na(data_orig$SeedMass.Mean.Logtrans)
  temp.analysis <- data_orig[-which(ess), ]
 
  # Remove rows with no data on included functional traits used in the
  # response AND no data on dispersal or demography.
  temp.analysis$MissResponseTraits<-apply(temp.analysis[, IncResponseTraits], 
                                          1, function(x) sum(is.na(x)))
  temp.analysis$MissAllResponseVal <-apply(temp.analysis[, AllResponseVal], 
                                           1, function(x) sum(is.na(x)))

  out <- temp.analysis$MissAllResponseVal == (length(AllResponseVal)) 
  dataVS.analysis <- temp.analysis[!out, ]
  rm(temp.analysis) 

  #--------------------------------
  # Standardize data for analysis
  #--------------------------------
  
  inc <- c(AllResponseVal, "logMeanDisp", "PH.Mean.Logtrans", 
           "SeedMass.Mean.Logtrans") 
  
  datMin.centering <- sapply(dataVS.analysis[,inc], mean, na.rm= T)
  datMin.scaling <- sapply(dataVS.analysis[,inc], sd, na.rm= T)
  datMinst <- dataVS.analysis
  datMinst[, inc] <- apply(datMinst[, inc], 2, 
                           function(x) (x-mean(x, na.rm= T))/sd(x, na.rm= T))

  # columns to paste together
	cols <- c("DS_Bullock", "GF")
	
  # create a new column `x` with the three columns collapsed together
	datMinst$GF_DS <- apply( datMinst[ , cols ] , 1 , paste , collapse = "_" )	
	datMinst$GF_DS <- factor(datMinst$GF_DS)
	
	# build and return output list
	dat <- list(datMinst = datMinst, datMin.centering = datMin.centering, 
	            datMin.scaling = datMin.scaling, data_orig=data_orig, 
	            dataVS.analysis = dataVS.analysis)
	return(dat)	
}