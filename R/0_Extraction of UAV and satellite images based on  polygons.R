######################################################################################
############ Extraction of UAV and satellite images based on  polygons ###############
######################################################################################
library(raster)
library(sp)
library(rgdal)

##########################
######## Plot F101a  #####
##########################
# road images
# UAV image
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F101/UAV/F101a.tif")
# satellite image
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
# Polygon
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/F001alleB?umeREa.shp")
# see how many trees
tree <- shpfile$objectid
# transform the shapefile to dataframe
dfshp <- as(shpfile,"data.frame")

# extract values
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Enhanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Normalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/2cm/","UAV2cm_a",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/UAV/","UAV50cm_a",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/Satellite/","PA_a",i,".csv"))
}

# F101b
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F101/UAV/F101b.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/F001alleB?umeREb.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/2cm/","UAV2cm_b",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/UAV/","UAV50cm_b",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/Satellite/","PA_b",i,".csv"))
}

# F101d
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F101/UAV/F101d.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/F001alleB?umeREd.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/2cm/","UAV2cm_d",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/UAV/","UAV50cm_d",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/Satellite/","PA_d",i,".csv"))
}


# F102a
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F102/UAV/F102a.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F102/Polygon/F102a_reproject.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/2cm/","UAV2cm_a",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/50cm/UAV/","UAV50cm_a",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/50cm/Satellite/","PA_a",i,".csv"))
}


# F102b
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F102/UAV/F102b.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F102/Polygon/F102b_reproject.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/2cm/","UAV2cm_b",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/50cm/UAV/","UAV50cm_b",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/50cm/Satellite/","PA_b",i,".csv"))
}


# F103
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F103/UAV/F103.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F103/Polygon/F103_reproject.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F103/CSV/2cm/","UAV2cm",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F103/CSV/50cm/UAV/","UAV50cm",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F103/CSV/50cm/Satellite/","PA",i,".csv"))
}



# F301
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F301/UAV/F301.tif")
UAV2cm <- stack(UAV2cm$F301.1/10000,UAV2cm$F301.2/10000, UAV2cm$F301.4/10000, UAV2cm$F301.5/10000, UAV2cm$F301.6/10000)
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/2022satellite data/D_NNDiffusePanSharpening.dat")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F301/Polygon/F301_reproject.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F301/CSV/2cm/","UAV2cm",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/F301/CSV/50cm/UAV/","UAV50cm",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F301/CSV/50cm/Satellite/","PA",i,".csv"))
}



# G005
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/G005/UAV/G005.tif")
UAV2cm <- stack(UAV2cm$G005.1/10000,UAV2cm$G005.2/10000, UAV2cm$G005.3/10000, UAV2cm$G005.4/10000, UAV2cm$G005.5/10000)
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/2022satellite data/B_NNDiffusePanSharpening.dat")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/G005/Polygon/G005alleB?ume.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  dftree <- dfshp[which(dfshp$objectid == i),][1,]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/G005/CSV/2cm/","UAV2cm",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/G005/CSV/50cm/UAV/","UAV50cm",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/G005/CSV/50cm/Satellite/","PA",i,".csv"))
}

# G006
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/G006/UAV/G006.tif")
UAV2cm <- stack(UAV2cm$G006.1/10000,UAV2cm$G006.2/10000, UAV2cm$G006.3/10000, UAV2cm$G006.4/10000, UAV2cm$G006.5/10000)
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/2022satellite data/A_NNDiffusePanSharpening.dat")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/G006/Polygon/G006Polygone_Blue.shp")
tree <- shpfile$Objekt
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, Objekt == i)
  dftree <- dfshp[which(dfshp$Objekt == i),][1,]
  UAV <- crop(UAV2cm, treeid)
  UAV2 <- mask(UAV, treeid)
  names(UAV2) <- c("Band_1", "Band_2", "Band_3", "Band_4", "Band_5")
  # Atmospherically resistance vegetation index
  ARVI <- UAV2$Band_5-(2*UAV2$Band_3-UAV2$Band_1)/UAV2$Band_5 + (2*UAV2$Band_3-UAV2$Band_1)
  # Chlorophy index green
  CIG <- UAV2$Band_5/UAV2$Band_2 - 1
  # Ehanced vegetation index
  EVI <- 2.5*(UAV2$Band_5-2*UAV2$Band_3)/(UAV2$Band_5 + 6*UAV2$Band_3-7.5*UAV2$Band_1 + 1)
  # Green normalized difference vegetation index
  GNDVI <- (UAV2$Band_5 - UAV2$Band_2) / (UAV2$Band_5 + UAV2$Band_2)
  # Nornalized difference vegetation index
  NDVI <- (UAV2$Band_5 - UAV2$Band_3) /(UAV2$Band_5 + UAV2$Band_3)
  # Plant senescence reflectance index
  PSRI <- (UAV2$Band_3 - UAV2$Band_1) / UAV2$Band_4
  # Rededge normalized difference vegetation index
  RNDVI <- (UAV2$Band_5 - UAV2$Band_4) /(UAV2$Band_5 + UAV2$Band_4)
  UAV2_all <- stack(UAV2, ARVI, CIG, EVI, GNDVI, NDVI, PSRI, RNDVI)
  names(UAV2_all) <- c("blue","green","red","rededge","nir","arvi","cig","evi","gndvi","ndvi","psri","rndvi")
  
  # Satellite
  PA1 <- crop(PA,treeid)
  PA2 <- mask(PA1, treeid)
  names(PA2) <- c("b1","b2","b3","b4")
  UAV50 <- crop(UAV2_all,PA2)
  UAV50 <- resample(UAV50,PA2)
  UAV50 <- mask(UAV50,PA2$b1)
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- stack(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV for different datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/G006/CSV/2cm/","UAV2cm",i,".csv"))
  
  df50cm <- as.data.frame(getValues(UAV50))
  df50cm <- na.omit(df50cm)
  dfall50cm <- cbind(df50cm,  dftree)
  write.csv(dfall50cm,paste0("T:/groups2/ForestCare/Analysis/G006/CSV/50cm/UAV/","UAV50cm",i,".csv"))
  
  dfPA <- as.data.frame(getValues(PA2_all))
  dfPA <- na.omit(dfPA)
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/G006/CSV/50cm/Satellite/","PA",i,".csv"))
}







