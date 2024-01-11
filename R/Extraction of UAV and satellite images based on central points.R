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
# satellte image
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
# Polygon
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/Central Point_buffer_F001a.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/Central Point_F001a.shp")
# see how many trees
tree <- shpfile$objectid
# transform the shapfile to dataframe
dfshp <- as(shpfile,"data.frame")

# extract values
# F101
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/central/","central_UAV2cm_a",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/central/","central_PA_a",i,".csv"))
}

# F101b
# UAV image
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F101/UAV/F101b.tif")
# satellte image
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
# Polygon
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/Central Point_buffer_F001b.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/Central Point_F001b.shp")
# see how many trees
tree <- shpfile$objectid
# transform the shapfile to dataframe
dfshp <- as(shpfile,"data.frame")

# extract values
# F101b
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/central/","central_UAV2cm_b",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/central/","central_PA_b",i,".csv"))
}

# F101d
# UAV image
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F101/UAV/F101d.tif")
# satellte image
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
# Polygon
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/Central Point_buffer_F001d.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F101/Polygon/Central Point_F001d.shp")
# see how many trees
tree <- shpfile$objectid
# transform the shapfile to dataframe
dfshp <- as(shpfile,"data.frame")

# extract values
# F101d
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/central/","central_UAV2cm_d",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F101/CSV/central/","central_PA_d",i,".csv"))
}


# F102a
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F102/UAV/F102a.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F102/Polygon/Central point buffer_F102a.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F102/Polygon/Central point_F102a.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/central/","central_UAV2cm_a",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/central/","central_PA_a",i,".csv"))
}


# F102b
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F102/UAV/F102b.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F102/Polygon/Central point buffer_F102b.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F102/Polygon/Central point_F102b.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/central/","central_UAV2cm_b",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F102/CSV/central/","central_PA_b",i,".csv"))
}


# F103
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F103/UAV/F103.tif")
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/A10158 - U Freiburg/9-24-2021_Ortho/9-24-2021_Ortho_4Band.tif")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F103/Polygon/Central point buffer_F103.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F103/Polygon/Central point_F103.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F103/CSV/central/","central_UAV2cm",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F103/CSV/central/","central_PA",i,".csv"))
}



# F301
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/F301/UAV/F301.tif")
UAV2cm <- stack(UAV2cm$F301.1/10000,UAV2cm$F301.2/10000, UAV2cm$F301.4/10000, UAV2cm$F301.5/10000, UAV2cm$F301.6/10000)
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/2022satellite data/D_NNDiffusePanSharpening.dat")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/F301/Polygon/Central point buffer_F301.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/F301/Polygon/Central point_F301.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/F301/CSV/central/","central_UAV2cm",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/F301/CSV/central/","central_PA",i,".csv"))
}



# G005
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/G005/UAV/G005.tif")
UAV2cm <- stack(UAV2cm$G005.1/10000,UAV2cm$G005.2/10000, UAV2cm$G005.3/10000, UAV2cm$G005.4/10000, UAV2cm$G005.5/10000)
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/2022satellite data/B_NNDiffusePanSharpening.dat")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/G005/Polygon/Central point buffer_G005.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/G005/Polygon/Central point_G005.shp")
tree <- shpfile$objectid
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, objectid == i)
  treeid1 <- subset(shpfile1, objectid == i)
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree[1,])
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/G005/CSV/central/","central_UAV2cm",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree[1,])
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/G005/CSV/central/","central_PA",i,".csv"))
}

# G006
UAV2cm <- stack("T:/groups2/ForestCare/Analysis/G006/UAV/G006.tif")
UAV2cm <- stack(UAV2cm$G006.1/10000,UAV2cm$G006.2/10000, UAV2cm$G006.3/10000, UAV2cm$G006.4/10000, UAV2cm$G006.5/10000)
PA <- stack("T:/groups2/ForestCare/Analysis/Satellite data/2022satellite data/A_NNDiffusePanSharpening.dat")
shpfile <- readOGR("T:/groups2/ForestCare/Analysis/G006/Polygon/Central point buffer_G006.shp")
shpfile1 <- readOGR("T:/groups2/ForestCare/Analysis/G006/Polygon/Central point_G006.shp")
tree <- shpfile$Objekt
dfshp <- as(shpfile,"data.frame")
for (i in tree) {
  treeid <- subset(shpfile, Objekt == i)
  treeid1 <- subset(shpfile1, Objekt == i)
  dftree <- dfshp[which(dfshp$Objekt == i),]
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
  PA2 <- data.frame(raster::extract(PA,treeid1))
  names(PA2) <- c("b1","b2","b3","b4")
  # Satellite VIs
  ARVI1 <- PA2$b4-(2*PA2$b3-PA2$b1)/PA2$b4 + (2*PA2$b3-PA2$b1)
  CIG1 <- PA2$b4/PA2$b2 - 1
  EVI1 <- 2.5*(PA2$b4-2*PA2$b3)/(PA2$b4 + 6*PA2$b3-7.5*PA2$b1 + 1)
  GNDVI1 <- (PA2$b4 - PA2$b2) / (PA2$b4 + PA2$b2)
  NDVI1 <- (PA2$b4 - PA2$b3) /(PA2$b4 + PA2$b3)
  PA2_all <- data.frame(PA2, ARVI1, CIG1, EVI1, GNDVI1, NDVI1)
  names(PA2_all) <- c("blue","green","red","nir","arvi","cig","evi","gndvi","ndvi")
  
  # CSV forUAV datasets
  df2cm <- as.data.frame(getValues(UAV2_all))
  df2cm <- na.omit(df2cm)
  dfall2cm <- cbind(df2cm,  dftree)
  write.csv(dfall2cm,paste0("T:/groups2/ForestCare/Analysis/G006/CSV/central/","central_UAV2cm",i,".csv"))
  
  dfPA <- PA2_all
  dfallPA <- cbind(dfPA,  dftree)
  write.csv(dfallPA,paste0("T:/groups2/ForestCare/Analysis/G006/CSV/central/","central_PA",i,".csv"))
}



