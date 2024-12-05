# Script name: _buildStudyArea.R
# Author: S. Bogen
#
# Creates the figure illustrating the study area of Utah with Ashley and
# Fishlake National forests indicated
#-------------------------------------------------------------------------------

library(sf)
library(RColorBrewer)
library(ggplot2)

setwd("~/Desktop/Goshawk Chapter Code/PIPELINE/5 FIGURES")

# Read in Utah
utah.full <- st_read("../0 DATA/spatial boundaries/RAW/Utah/Utah.shp")
utah <- utah.full[utah.full$STATE == "Utah", ]

# Define standard projection
standProj <- st_crs('+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs')

# Read in Fishlake
forests <- st_read("../0 DATA/spatial boundaries/RAW/S_USA.AdministrativeForest.shp")
fishlake <- forests[forests$FORESTNAME=="Fishlake National Forest", ]
ashley <- forests[forests$FORESTNAME=="Ashley National Forest", ]
fishlake.proj <- st_transform(fishlake, standProj)
ashley.proj <- st_transform(ashley, standProj)


# Select color palette
prettier <- brewer.pal(n = 5, name = 'RdYlBu')

#-------------------------------------------------------------------------------
# Assemble study area plot
#-------------------------------------------------------------------------------

studyArea <- ggplot() +
  geom_sf(data = utah, fill = "white", lwd = 1) +
  geom_sf(data = fishlake.proj, aes(fill = 'Fishlake National Forest')) +
  geom_sf(data = ashley.proj, aes(fill = 'Ashley National Forest')) +
  scale_fill_manual(name = "", 
                    values = c('Fishlake National Forest' = prettier[2], 
                               'Ashley National Forest' = prettier[5])) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "white", color = "gray"))

studyArea

#-------------------------------------------------------------------------------
# Save as figure
#-------------------------------------------------------------------------------

ggsave("studyArea.pdf", studyArea)






