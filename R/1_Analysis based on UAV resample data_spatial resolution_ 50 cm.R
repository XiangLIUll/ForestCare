##############################################################################
#####  Analysis based on UAV resample data (spatial resolution: 50 cm)   #####          
##############################################################################
library(tidyverse)
library(readr)
library(randomForest)
library(ggplot2)
library(ggpubr)
##########################################################
#####  Step 1: calculate stats from original data      ###          
##########################################################
# function for calculating stats
StateSummary <- function (dd) {
  func <- function(x) {
    c(Max = max(x, na.rm = TRUE), 
      Min = min(x, na.rm = TRUE),
      Range = max(x,na.rm = TRUE) - min(x,na.rm = TRUE),
      Mean = mean(x, na.rm = TRUE), 
      SD = sd(x, na.rm = TRUE), 
      CV = sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))
  }
  sm <- as.data.frame((apply(dd, 2, func)))
  return(sm)
}

### Calculate stats value for each polygon 
# F101
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F101/CSV/50cm/UAV/")
newF101 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F101/CSV/50cm/UAV/",i))[,-1]
  NDVI_max_min <- (cbind(max(plotdf[,10]),min(plotdf[,6])))
  colnames(NDVI_max_min) <- c("Max","Min")
  print(NDVI_max_min)
  # df1 <- df[which(df1$)]
  df_mean <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[4,]
  df_max <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[1,]
  df_min <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[2,]
  df_ran <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[3,]
  df_sd <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[5,]
  df_cv <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[6,]
  df1 <- cbind(df_mean,df_max, df_min, df_ran, df_sd, df_cv, plotdf[1,-(1:13)])
  newF101 <- rbind(newF101,df1)
}
names(newF101) = gsub('"', "", names(newF101)) 
colnames(newF101)[1:72] <- c(paste0(names(newF101)[1:12],"_mean"), paste0(names(newF101)[1:12],"_max"),
                             paste0(names(newF101)[1:12],"_min"), paste0(names(newF101)[1:12],"_ran"),
                             paste0(names(newF101)[1:12],"_sd"), paste0(names(newF101)[1:12],"_cv"))
write.csv(newF101, "T:/groups2/ForestCare/Analysis/stat outputs/F101_50cm.csv") 



# F102
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F102/CSV/50cm/UAV/")
newF102 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F102/CSV/50cm/UAV/",i))[,-1]
  NDVI_max_min <- (cbind(max(plotdf[,10]),min(plotdf[,6])))
  colnames(NDVI_max_min) <- c("Max","Min")
  print(NDVI_max_min)
  # df1 <- df[which(df1$)]
  df_mean <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[4,]
  df_max <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[1,]
  df_min <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[2,]
  df_ran <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[3,]
  df_sd <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[5,]
  df_cv <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[6,]
  df1 <- cbind(df_mean,df_max, df_min, df_ran, df_sd, df_cv, plotdf[1,-(1:13)])
  newF102 <- rbind(newF102,df1)
}
names(newF102) = gsub('"', "", names(newF102)) 
colnames(newF102)[1:72] <- c(paste0(names(newF102)[1:12],"_mean"), paste0(names(newF102)[1:12],"_max"),
                             paste0(names(newF102)[1:12],"_min"), paste0(names(newF102)[1:12],"_ran"),
                             paste0(names(newF102)[1:12],"_sd"), paste0(names(newF102)[1:12],"_cv"))
write.csv(newF102, "T:/groups2/ForestCare/Analysis/stat outputs/F102_50cm.csv") 


# F103
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F103/CSV/50cm/UAV/")
newF103 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F103/CSV/50cm/UAV/",i))[,-1]
  NDVI_max_min <- (cbind(max(plotdf[,10]),min(plotdf[,6])))
  colnames(NDVI_max_min) <- c("Max","Min")
  print(NDVI_max_min)
  # df1 <- df[which(df1$)]
  df_mean <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[4,]
  df_max <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[1,]
  df_min <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[2,]
  df_ran <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[3,]
  df_sd <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[5,]
  df_cv <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[6,]
  df1 <- cbind(df_mean,df_max, df_min, df_ran, df_sd, df_cv, plotdf[1,-(1:13)])
  newF103 <- rbind(newF103,df1)
}
names(newF103) = gsub('"', "", names(newF103)) 
colnames(newF103)[1:72] <- c(paste0(names(newF103)[1:12],"_mean"), paste0(names(newF103)[1:12],"_max"),
                             paste0(names(newF103)[1:12],"_min"), paste0(names(newF103)[1:12],"_ran"),
                             paste0(names(newF103)[1:12],"_sd"), paste0(names(newF103)[1:12],"_cv"))
write.csv(newF103, "T:/groups2/ForestCare/Analysis/stat outputs/F103_50cm.csv") 



# F301
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F301/CSV/50cm/UAV/")
newF301 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F301/CSV/50cm/UAV/",i))[,-1]
  NDVI_max_min <- (cbind(max(plotdf[,10]),min(plotdf[,6])))
  colnames(NDVI_max_min) <- c("Max","Min")
  print(NDVI_max_min)
  # df1 <- df[which(df1$)]
  df_mean <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[4,]
  df_max <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[1,]
  df_min <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[2,]
  df_ran <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[3,]
  df_sd <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[5,]
  df_cv <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[6,]
  df1 <- cbind(df_mean,df_max, df_min, df_ran, df_sd, df_cv, plotdf[1,-(1:13)])
  newF301 <- rbind(newF301,df1)
}
names(newF301) = gsub('"', "", names(newF301)) 
colnames(newF301)[1:72] <- c(paste0(names(newF301)[1:12],"_mean"), paste0(names(newF301)[1:12],"_max"),
                             paste0(names(newF301)[1:12],"_min"), paste0(names(newF301)[1:12],"_ran"),
                             paste0(names(newF301)[1:12],"_sd"), paste0(names(newF301)[1:12],"_cv"))
write.csv(newF301, "T:/groups2/ForestCare/Analysis/stat outputs/F301_50cm.csv") 

# G005
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/G005/CSV/50cm/UAV/")
newG005 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/G005/CSV/50cm/UAV/",i))[,-1]
  NDVI_max_min <- (cbind(max(plotdf[,10]),min(plotdf[,6])))
  colnames(NDVI_max_min) <- c("Max","Min")
  print(NDVI_max_min)
  # df1 <- df[which(df1$)]
  df_mean <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[4,]
  df_max <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[1,]
  df_min <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[2,]
  df_ran <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[3,]
  df_sd <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[5,]
  df_cv <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[6,]
  df1 <- cbind(df_mean,df_max, df_min, df_ran, df_sd, df_cv, plotdf[1,-(1:13)])
  newG005 <- rbind(newG005,df1)
}
names(newG005) = gsub('"', "", names(newG005)) 
colnames(newG005)[1:72] <- c(paste0(names(newG005)[1:12],"_mean"), paste0(names(newG005)[1:12],"_max"),
                             paste0(names(newG005)[1:12],"_min"), paste0(names(newG005)[1:12],"_ran"),
                             paste0(names(newG005)[1:12],"_sd"), paste0(names(newG005)[1:12],"_cv"))
write.csv(newG005, "T:/groups2/ForestCare/Analysis/stat outputs/G005_50cm.csv") 

# G006
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/G006/CSV/50cm/UAV/")
newG006 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/G006/CSV/50cm/UAV/",i))[,-1]
  NDVI_max_min <- (cbind(max(plotdf[,10]),min(plotdf[,6])))
  colnames(NDVI_max_min) <- c("Max","Min")
  print(NDVI_max_min)
  # df1 <- df[which(df1$)]
  df_mean <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[4,]
  df_max <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[1,]
  df_min <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[2,]
  df_ran <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[3,]
  df_sd <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[5,]
  df_cv <- StateSummary(as.data.frame(apply(plotdf[,1:12], 2, as.numeric)))[6,]
  df1 <- cbind(df_mean,df_max, df_min, df_ran, df_sd, df_cv, plotdf[1,-(1:13)])
  newG006 <- rbind(newG006,df1)
}
names(newG006) = gsub('"', "", names(newG006)) 
colnames(newG006)[1:72] <- c(paste0(names(newG006)[1:12],"_mean"), paste0(names(newG006)[1:12],"_max"),
                             paste0(names(newG006)[1:12],"_min"), paste0(names(newG006)[1:12],"_ran"),
                             paste0(names(newG006)[1:12],"_sd"), paste0(names(newG006)[1:12],"_cv"))
write.csv(newG006, "T:/groups2/ForestCare/Analysis/stat outputs/G006_50cm.csv") 


#############################################################
### Step 2: analysis using the new stat data
#############################################################
df1 <- read.csv("T:/groups2/ForestCare/Analysis/stat outputs/F101_50cm.csv")[,-1]
df2 <- read.csv("T:/groups2/ForestCare/Analysis/stat outputs/F102_50cm.csv")[,-1]
df3 <- read.csv("T:/groups2/ForestCare/Analysis/stat outputs/F103_50cm.csv")[,-1]
df4 <- read.csv("T:/groups2/ForestCare/Analysis/stat outputs/F301_50cm.csv")[,-1]
df5 <- read.csv("T:/groups2/ForestCare/Analysis/stat outputs/G005_50cm.csv")[,-1]
df6 <- read.csv("T:/groups2/ForestCare/Analysis/stat outputs/G006_50cm.csv")[,-1]
# names between df6 and other plots do not match, replace them
names(df6) <- names(df5)
df <- rbind(df1,df2,df3,df4,df5,df6)
newdf <- data.frame(df)
newdf[,1:72][is.na(newdf[,1:72])] <- 0
newdf[,1:72][sapply(newdf[,1:72], is.infinite)] <- 0
newdf <- newdf[-which(is.na(newdf$stamm_vita)),]
table(newdf$stamm_vita)
newdf$stam <- factor(newdf$stamm_vita, 
                     levels = c("abgestorben", "gesund", "leichter Schaden", "mittlerer Schaden", "schwerer Schaden"),
                     labels = c("dead", "healthy", "slight damage", "medium damage", "serious damage"))
table(newdf$stam)

A <- ggboxplot(newdf,"stam","blue_mean",color = "stam")

B <- ggboxplot(newdf,"stam","green_mean",color = "stam")

C <- ggboxplot(newdf,"stam","red_mean",color = "stam")

D <- ggboxplot(newdf,"stam","nir_mean",color = "stam")

E <- ggboxplot(newdf,"stam","rededge_mean",color = "stam")

F <- ggboxplot(newdf,"stam","ndvi_mean",color = "stam")

ggarrange(A,B,C,D,E,F,ncol=3,nrow=2,common.legend=TRUE,legend = "top",hjust = 0,labels= c("(a)","(b)","(c)","(d)","(e)","(f)"))
ggsave("H:/ForestCare/stamm.jpg",width = 28,height = 18, units = "cm",dpi = 1000)


## randomforest
library(randomForest)
newdata <- cbind(newdf$stam,newdf[,1:72])
newdata1 <- na.omit(newdata)
rf <- randomForest(newdata1[,-1],as.factor(newdata1[,1]))
varImpPlot(rf,main = "variable imaportance for stamm_vital_kat")



# krone
table(newdf$krone_vi_3)
newdf$krone <- factor(newdf$krone_vi_3, 
                      levels = c("abgestorben", "gesund", "leichter Schaden", "mittlerer Schaden", "schwerer Schaden"),
                      labels = c("dead", "healthy", "slight damage", "medium damage", "serious damage"))
table(newdf$krone)

A <- ggboxplot(newdf,"krone","blue_mean",color = "krone")

B <- ggboxplot(newdf,"krone","green_mean",color = "krone")

C <- ggboxplot(newdf,"krone","red_mean",color = "krone")

D <- ggboxplot(newdf,"krone","nir_mean",color = "krone")

E <- ggboxplot(newdf,"krone","rededge_mean",color = "krone")

F <- ggboxplot(newdf,"krone","ndvi_mean",color = "krone")

ggarrange(A,B,C,D,E,F,ncol=3,nrow=2,common.legend=TRUE,legend = "top",hjust = 0,labels= c("(a)","(b)","(c)","(d)","(e)","(f)"))
ggsave("H:/ForestCare/kronem.jpg",width = 28,height = 18, units = "cm",dpi = 1000)


## randomforest
newdata2 <- cbind(newdf$krone,newdf[,1:72])
newdata2 <- na.omit(newdata2)
rf <- randomForest(newdata2[,-1],as.factor(newdata2[,1]))
varImpPlot(rf,main = "variable imaportance for kronem_vital_kat")






