##############################################################################
#####  Analysis based on UAV original data (spatial resolution: 2 cm)   #####          
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
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F101/CSV/2cm/")
newF101 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F101/CSV/2cm/",i))[,-1]
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
write.csv(newF101, "T:/groups2/ForestCare/Analysis/stat outputs/F101_2cm.csv") 

# F102
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F102/CSV/2cm/")
newF102 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F102/CSV/2cm/",i))[,-1]
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
write.csv(newF102, "T:/groups2/ForestCare/Analysis/stat outputs/F102_2cm.csv") 


filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F103/CSV/2cm/")
newF103 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F103/CSV/2cm/",i))[,-1]
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
write.csv(newF103, "T:/groups2/ForestCare/Analysis/stat outputs/F103_2cm.csv") 


# F301
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/F301/CSV/2cm/")
newF301 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/F301/CSV/2cm/",i))[,-1]
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
write.csv(newF301, "T:/groups2/ForestCare/Analysis/stat outputs/F301_2cm.csv") 

# G005
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/G005/CSV/2cm/")
newG005 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/G005/CSV/2cm/",i))[,-1]
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
write.csv(newG005, "T:/groups2/ForestCare/Analysis/stat outputs/G005_2cm.csv") 

# G006
filelist <- list.files(pattern = "csv",path = "T:/groups2/ForestCare/Analysis/G006/CSV/2cm/")
newG006 <- data.frame()
for (i in filelist) {
  plotdf <- read.csv(paste0("T:/groups2/ForestCare/Analysis/G006/CSV/2cm/",i))[,-1]
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
write.csv(newG006, "T:/groups2/ForestCare/Analysis/stat outputs/G006_2cm.csv") 



#############################################################
### Step 2: analysis using new stat data
#############################################################
df1 <- read.csv("H:/ForestCare/Analysis/stat outputs/F101_2cm.csv")[,-1]
df2 <- read.csv("H:/ForestCare/Analysis/stat outputs/F102_2cm.csv")[,-1]
df3 <- read.csv("H:/ForestCare/Analysis/stat outputs/F103_2cm.csv")[,-1]
df4 <- read.csv("H:/ForestCare/Analysis/stat outputs/F301_2cm.csv")[,-1]
df5 <- read.csv("H:/ForestCare/Analysis/stat outputs/G005_2cm.csv")[,-1]
df6 <- read.csv("H:/ForestCare/Analysis/stat outputs/G006_2cm.csv")[,-1]

# names between df6 and other plots do not match, replace them
names(df6) <- names(df5)
df <- rbind(df1,df2,df3,df4,df5,df6)

table(df1$baumklasse)

# export the data
write.csv(df,"Datasets/UAV_2cm_polygon.csv")

newdf <- data.frame(df)
newdf[,1:118][is.na(newdf[,1:118])] <- 0
newdf[,1:118][sapply(newdf[,1:118], is.infinite)] <- 0


table(newdf$stamm_vita)
newdf$stamm_vita <- factor(newdf$stamm_vita, 
                           levels = c("abgestorben", "gesund", "leichter Schaden", "mittlerer Schaden", "schwerer Schaden"),
                           labels = c("dead", "healthy", "s_damage", "m_damage", "h_damage"))
table(newdf$stamm_vita)

newdf1 <- newdf[-which(is.na(newdf$stamm_vita)),]
A <- ggboxplot(newdf1,"stamm_vita","blue_mean",color = "stamm_vita")

B <- ggboxplot(newdf1,"stamm_vita","green_mean",color = "stamm_vita")

C <- ggboxplot(newdf1,"stamm_vita","red_mean",color = "stamm_vita")

D <- ggboxplot(newdf1,"stamm_vita","nir_mean",color = "stamm_vita")

E <- ggboxplot(newdf1,"stamm_vita","rededge_mean",color = "stamm_vita")

(F <- ggboxplot(newdf1,"stamm_vita1","ndvi_mean",color = "stamm_vita1") + 
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(width = 0.75)) +
    theme_bw() + #scale_x_discrete(labels = c("Healthy", "Slight damage", "Medium damage", "Serious damage", "Dead")) +
    labs(x = "Stem", y = "NDVI") + theme(legend.position = "none"))
ggsave("Output figures/Polygon_Stem_UAV2cm_NDVI.jpg",width = 28,height = 18, units = "cm",dpi = 1000)

ggarrange(A,B,C,D,E,F,ncol=3,nrow=2,common.legend=TRUE,legend = "top",hjust = 0,labels= c("(a)","(b)","(c)","(d)","(e)","(f)"))
ggsave("H:/ForestCare1/stamm.jpg",width = 40,height = 18, units = "cm",dpi = 1000)


## randomforest
newdata <- cbind(newdf1$stamm_vita,newdf1[,1:72])
newdata1 <- na.omit(newdata)
rf <- randomForest(newdata1[,-1],as.factor(newdata1[,1]))
varImpPlot(rf,main = "variable imaportance for stamm_vital_kat")



# krone
newdf1 <- newdf[-which(newdf$krone_vi_3 == "0"),]
table(newdf1$krone_vi_3)
newdf1$crown_vita <- factor(newdf1$krone_vi_3, 
                           levels = c( "gesund", "leichter Schaden", "mittlerer Schaden", "schwerer Schaden","abgestorben"),
                           labels = c("Healthy", "Slightly damaged", "Medium damaged", "Heavily damaged", "Dead"))
table(newdf1$crown_vita)

classes <- names(table(newdf1$crown_vita))
all_combinations <- combn(classes, 2, simplify = TRUE)
unique_combinations <- lapply(1:ncol(all_combinations), function(x) {
  sort(all_combinations[, x])})

newdf1$cig_mean <- ifelse(newdf1$cig_mean > 10, 10, newdf1$cig_mean)
summary(aov(cig_mean ~ crown_vita, data = newdf1))
A <- ggboxplot(newdf1,"crown_vita","cig_mean",color = "crown_vita", add = "mean",xlab = FALSE,bxp.errorbar = TRUE) +
  scale_color_discrete(name = "Health stage") + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + ylab("CIG") +
  stat_compare_means(comparisons = unique_combinations, 
                     symnum.args = list(cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01,  1), symbols = c("****", "***", "**", "*", "ns")),
                     hide.ns = FALSE, label = "p.signif",geom = "pointrange", bracket.size = 0)
    
newdf1$psri_mean <- ifelse(newdf1$psri_mean > 1, 1, ifelse(newdf1$psri_mean < -1, -1, newdf1$psri_mean))
summary(aov(psri_mean ~ crown_vita, data = newdf1))
B <- ggboxplot(newdf1,"crown_vita","psri_mean",color = "crown_vita", add = "mean",xlab = FALSE,bxp.errorbar = TRUE) +
  scale_color_discrete(name = "Health stage") + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(legend.position = "none",axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + ylab("PSRI") +
  stat_compare_means(comparisons = unique_combinations, 
                     symnum.args = list(cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01,  1), symbols = c("****", "***", "**", "*", "ns")),
                     hide.ns = FALSE, label = "p.signif",geom = "pointrange", bracket.size = 0) 

summary(aov(gndvi_mean ~ crown_vita, data = newdf1))
C <- ggboxplot(newdf1,"crown_vita","gndvi_mean",color = "crown_vita", add = "mean",xlab = FALSE,bxp.errorbar = TRUE) +
  scale_color_discrete(name = "Health stage") + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + ylab("GNDVI") +
  stat_compare_means(comparisons = unique_combinations, 
                     symnum.args = list(cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01,  1), symbols = c("****", "***", "**", "*", "ns")),
                     hide.ns = FALSE, label = "p.signif",geom = "pointrange", bracket.size = 0) 

summary(aov(arvi_mean ~ crown_vita, data = newdf1))
D <- ggboxplot(newdf1,"crown_vita","arvi_mean",color = "crown_vita", add = "mean",xlab = FALSE,bxp.errorbar = TRUE) +
  scale_color_discrete(name = "Health stage") + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + ylab("ARVI") +
  stat_compare_means(comparisons = unique_combinations, 
                     symnum.args = list(cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01,  1), symbols = c("****", "***", "**", "*", "ns")),
                     hide.ns = FALSE, label = "p.signif",geom = "pointrange", bracket.size = 0) 

summary(aov(rndvi_mean ~ crown_vita, data = newdf1))
E <- ggboxplot(newdf1,"crown_vita","rndvi_mean",color = "crown_vita", add = "mean",xlab = FALSE,bxp.errorbar = TRUE) +
  scale_color_discrete(name = "Health stage") + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + ylab("RNDVI") +
  stat_compare_means(comparisons = unique_combinations,
                     symnum.args = list(cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01,  1), symbols = c("****", "***", "**", "*", "ns")),
                     hide.ns = FALSE, label = "p.signif",geom = "pointrange", bracket.size = 0) 

summary(aov(ndvi_mean ~ crown_vita, data = newdf1))
F <- ggboxplot(newdf1,"crown_vita","ndvi_mean",color = "crown_vita", add = "mean",xlab = FALSE,bxp.errorbar = TRUE) +
  scale_color_discrete(name = "Health stage") + 
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + ylab("NDVI") +
  stat_compare_means(comparisons = unique_combinations, 
                     symnum.args = list(cutpoints = c(0, 0.00001, 0.0001, 0.001, 0.01,  1), symbols = c("****", "***", "**", "*", "ns")),
                     hide.ns = FALSE, label = "p.signif",geom = "pointrange", bracket.size = 0) 


ggarrange(A,B,C,D,E,F,ncol=3,nrow=2,common.legend=TRUE,legend = "top",hjust = 0,labels= c("(a)","(b)","(c)","(d)","(e)","(f)"))
ggsave("H:/ForestCare/forestCare/ForestCare/Output figures/UAV_crown_vita.jpg",width = 35,height = 20, units = "cm",dpi = 1000)

(F <- ggboxplot(newdf2,"crown_vita","ndvi_mean",color = "crown_vita") + 
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(width = 0.75)) +
    theme_bw() + #scale_x_discrete(labels = c("Healthy", "Slight damage", "Medium damage", "Serious damage", "Dead")) +
    labs(x = "krone", y = "NDVI") + theme(legend.position = "none"))
ggsave("Output figures/Polygon_krone_UAV2cm_NDVI.jpg",width = 28,height = 18, units = "cm",dpi = 1000)


## randomforest
newdata2 <- cbind(newdf1$crown_vita,newdf1[,1:72])
newdata3 <- na.omit(newdata2)
colnames(newdata3)[1] <- "crown_vita"
rf1 <- randomForest(newdata3[,-1],as.factor(newdata3[,1]))
varImpPlot(rf1,main = "variable imaportance for crown_vita")

## if only consider three class: dead,health,damage
newdata3$class <- ifelse(newdata3$crown_vita == "dead", "dead", ifelse(newdata3$crown_vita == "healthy", "health", "damage"))
ggboxplot(newdata3,"class","ndvi_mean",color = "class", add = "mean")

rf2 <- randomForest(newdata3[,-c(1,74)],as.factor(newdata3[,74]))
varImpPlot(rf2,main = "variable imaportance for crown_vita")




