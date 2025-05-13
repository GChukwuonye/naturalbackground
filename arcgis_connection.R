#Author: Gift Chukwuonye
#Date:May 05 2025
#Project Description: Attempting to connect arcgis pro to R to automate the mapping process
#for more information, contact Gift on chukwuonye.gift@azdeq.gov 
#install.packages("arcgis", repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org"))
library(arcgis)
library(arcgisbinding)
library(sf)
arc.check_product()
fc<- arc.open("C:/Users/237708/Documents/ArcGIS/Projects/WFM_XRF/WFM_XRF.gdb")
print(fc)
data<- arc.select(fc)
