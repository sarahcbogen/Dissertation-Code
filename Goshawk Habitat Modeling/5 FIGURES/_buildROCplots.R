# Script name: _buildROCplots.R
# Author: S. Bogen
#
# Builds a four-panel plot comparing the ROC curves AUC values for each model 
# and forest
#-------------------------------------------------------------------------------

library(RColorBrewer)
library(Hmisc)

# CHANGE TO THE WORKING DIRECTORY ON YOUR SYSTEM
setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 FIGURES")

# Load up ROC analysis results
load("../4 VALIDATAION/rocs.RData")

# Set up colors for plotting
prettier <- brewer.pal(n = 5, name = 'RdYlBu')

#-------------------------------------------------------------------------------
# Get AUC values
#-------------------------------------------------------------------------------

ashleyAUCs <- c(as.numeric(ashleyInSample.roc$auc),
                as.numeric(ashleyME.roc$auc),
                as.numeric(base.ashley.roc$auc),
                as.numeric(ci95.ashley.roc$auc),
                as.numeric(fuzzy.ashley.roc$auc))

fishlakeAUCs <- c(as.numeric(fishlakeInSample.roc$auc),
                  as.numeric(fishlakeME.roc$auc),
                  as.numeric(base.fishlake.roc$auc),
                  as.numeric(ci95.fishlake.roc$auc),
                  as.numeric(fuzzy.fishlake.roc$auc))

modelNames <- c("MaxEnt local", "MaxENT sep", "AHP Basic", 
                "AHP Smoothed", "AHP Jagged") 

grps <- as.factor(c(1, 1, 2, 2, 2))

#-------------------------------------------------------------------------------
# Combined figure, based on 10k buffered simulations
#-------------------------------------------------------------------------------

# a, b, c, d not lining up? Try running this first!
# dev.off() 

# Uncomment to save new figure version
#pdf("foo.pdf", width = 8, height = 8)
par(mfcol=c(2, 2))

#---------------------------------------------

plot(ashleyInSample.roc, col="black", mar = c(4, 5, 4, 3))
plot(ashleyME.roc, col="black", mar = c(4, 5, 4, 3), lty = 3, add = TRUE)
plot(base.ashley.roc, col=prettier[2], add = TRUE)
plot(ci95.ashley.roc, col=prettier[2], add = TRUE, lty = 6)
plot(fuzzy.ashley.roc, col=prettier[2], add = TRUE, lty = 3)
title("a", adj = 0)
legend(0.7, 0.4, legend=c("local", "sep"),
       col=c("black", "black"),
       lty=c(1, 3),
       box.lwd = NA,
       title = "MaxEnt", title.font = 2, title.adj = 0.15,
       cex=0.8)
legend(0.4, 0.4, legend=c("basic", "smoothed", "jagged"),
       col=c(prettier[2], prettier[2], prettier[2]),
       lty=c(1, 6, 3),
       box.lwd = NA,
       title = "AHP", title.font = 2, title.adj = 0.15,
       cex=0.8)

#---------------------------------------------

plot(fishlakeInSample.roc, col="black", mar = c(4, 5, 4, 3))
plot(fishlakeME.roc, col="black", mar = c(4, 5, 4, 3), lty = 3, add = TRUE)
plot(base.fishlake.roc, col=prettier[1], add = TRUE)
plot(ci95.fishlake.roc, col=prettier[1], add = TRUE, lty = 6)
plot(fuzzy.fishlake.roc, col=prettier[1], add = TRUE, lty = 3)
title("c", adj = 0)
legend(0.7, 0.4, legend=c("local", "sep"),
       col=c("black", "black"),
       lty=c(1, 3),
       box.lwd = NA,
       title = "MaxEnt", title.font = 2, title.adj = 0.15,
       cex=0.8)
legend(0.4, 0.4, legend=c("basic", "smoothed", "jagged"),
       col=c(prettier[1], prettier[1], prettier[1]),
       lty=c(1, 6, 3),
       box.lwd = NA,
       title = "AHP", title.font = 2, title.adj = 0.15,
       cex=0.8)

#---------------------------------------------

dotchart2(ashleyAUCs, labels = modelNames, mar = c(10, 10, 10, 10),
          groups = grps, cex.group.labels = 0.01, lty = 3,
          xlab = "AUC", xlim = c(0.75, 1), pch = 19, cex = 0.8,
          sort = FALSE)
title("b", adj = 0)

segments(as.numeric(fuzzy.ashley.roc$ci)[1], 2.05, 
         as.numeric(fuzzy.ashley.roc$ci)[3], 2.05)
segments(as.numeric(ci95.ashley.roc$ci)[1], 2.7, 
         as.numeric(ci95.ashley.roc$ci)[3], 2.7)
segments(as.numeric(base.ashley.roc$ci)[1], 3.35, 
         as.numeric(base.ashley.roc$ci)[3], 3.35)
segments(as.numeric(ashleyME.roc$ci)[1], 5.3, 
         as.numeric(ashleyME.roc$ci)[3], 5.3)
segments(as.numeric(ashleyInSample.roc$ci)[1], 5.95, 
         as.numeric(ashleyInSample.roc$ci)[3], 5.95)

#---------------------------------------------

dotchart2(fishlakeAUCs, labels = modelNames,
          groups = grps, cex.group.labels = 0.01, lty = 3,
          xlab = "AUC", xlim = c(0.75, 1), pch = 19, cex = 0.8,
          sort = FALSE)
title("d", adj = 0)

segments(as.numeric(fuzzy.fishlake.roc$ci)[1], 2.05, 
         as.numeric(fuzzy.fishlake.roc$ci)[3], 2.05)
segments(as.numeric(ci95.fishlake.roc$ci)[1], 2.7, 
         as.numeric(ci95.fishlake.roc$ci)[3], 2.7)
segments(as.numeric(base.fishlake.roc$ci)[1], 3.35, 
         as.numeric(base.fishlake.roc$ci)[3], 3.35)
segments(as.numeric(fishlakeME.roc$ci)[1], 5.3, 
         as.numeric(fishlakeME.roc$ci)[3], 5.3)
segments(as.numeric(fishlakeInSample.roc$ci)[1], 5.95, 
         as.numeric(fishlakeInSample.roc$ci)[3], 5.95)

#---------------------------------------------

# Uncomment to save new figure version
# dev.off()

