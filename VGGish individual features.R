library(plyr)
library(stringr)
library(ggpubr)
library(apcluster)
library(tuneR)

Tempfiles <-
  list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/downsampled_files/vggish_embeddings_original",full.names = T,
             recursive = T)

# VGGish Mean SD ----------------------------------------------------------
VGGishDFMeanSD <- data.frame()

for(i in 1:length(Tempfiles)){
  print(i)
  Tempcsv <- read.delim(Tempfiles[i])
  Duration <- nrow(Tempcsv)
  n.slash  <- str_count(Tempfiles[i], pattern = "/")[1] + 1
  
  Temp.name <- str_split_fixed(Tempfiles[i],pattern = "/",n=n.slash)[,n.slash]
  Class <- str_split_fixed(Temp.name,pattern = '.csv',n=2)[,1]
  
  Individual <- paste(str_split_fixed(Class,
                                      pattern = '_',n=3)[,1],
                      str_split_fixed(Class,
                                      pattern = '_',n=3)[,2],sep='_')
  
    Tempcsv <- colMeans(Tempcsv)
  TempRow <- data.frame()
  
  TempRow <- rbind.data.frame(TempRow,Tempcsv)
  colnames(TempRow) <- paste('V',seq(1,128,1),sep='')
  
  TempRow$Individual <- Individual
  TempRow$Duration <- Duration
  VGGishDFMeanSD <- rbind.data.frame(VGGishDFMeanSD,TempRow )
}


VGGishDFMeanSD <- 
  subset(VGGishDFMeanSD, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

VGGishDFMeanSD <- droplevels(VGGishDFMeanSD)

VGGishDFMeanSD$Individual <- as.factor(VGGishDFMeanSD$Individual)

ml.model.rf.vggish <-
  randomForest::randomForest(x = VGGishDFMeanSD[,-c(129)], y = VGGishDFMeanSD$Individual)

1-min(ml.model.rf.vggish$err.rate[,1])

VGGishPercentCorrect <- 
  round(1-min(ml.model.rf.vggish$err.rate[,1]),2)



AcousticSignals.umap <-
  umap::umap(VGGishDFMeanSD[,-c(129)],
             #labels=as.factor(VGGishDFMeanSD$Validation),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishDFMeanSD$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")

VGGishPercentCorrect <- 
  round(1-min(ml.model.rf.vggish$err.rate[,1]),3)


VGGishScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                   y = "Dim.2",
                                   color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish',VGGishPercentCorrect*100, '% Correct'))

VGGishScatter

write.csv(VGGishDFMeanSD,'data/VGGishDFMeanSDoriginal.csv',row.names = F)


# Noise added -------------------------------------------------------------
Tempfiles <-
  list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/vggish_embeddings_6dB",full.names = T,
             recursive = T)

# VGGish Mean SD ----------------------------------------------------------
VGGishDFNoiseMeanSD <- data.frame()

for(i in 1:length(Tempfiles)){
  print(i)
  Tempcsv <- read.delim(Tempfiles[i])
  Duration <- nrow(Tempcsv)
  n.slash  <- str_count(Tempfiles[i], pattern = "/")[1] + 1
  
  Temp.name <- str_split_fixed(Tempfiles[i],pattern = "/",n=n.slash)[,n.slash]
  Class <- str_split_fixed(Temp.name,pattern = '.csv',n=2)[,1]
  
  Individual <- paste(str_split_fixed(Class,
                                      pattern = '_',n=3)[,1],
                      str_split_fixed(Class,
                                      pattern = '_',n=3)[,2],sep='_')
  
  Tempcsv <- colMeans(Tempcsv)
  TempRow <- data.frame()
  
  TempRow <- rbind.data.frame(TempRow,Tempcsv)
  colnames(TempRow) <- paste('V',seq(1,128,1),sep='')
  
  TempRow$Individual <- Individual
  TempRow$Duration <- Duration
  VGGishDFNoiseMeanSD <- rbind.data.frame(VGGishDFNoiseMeanSD,TempRow )
}


VGGishDFNoiseMeanSD <- 
  subset(VGGishDFNoiseMeanSD, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

VGGishDFNoiseMeanSD <- droplevels(VGGishDFNoiseMeanSD)

VGGishDFNoiseMeanSD$Individual <- as.factor(VGGishDFNoiseMeanSD$Individual)

ml.model.rf.vggish <-
  randomForest::randomForest(x = VGGishDFNoiseMeanSD[,-c(129)], y = VGGishDFNoiseMeanSD$Individual)

1-min(ml.model.rf.vggish$err.rate[,1])

VGGishPercentCorrect <- 
  round(1-min(ml.model.rf.vggish$err.rate[,1]),2)



AcousticSignals.umap <-
  umap::umap(VGGishDFNoiseMeanSD[,-c(129)],
             #labels=as.factor(VGGishDFNoiseMeanSD$Validation),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishDFNoiseMeanSD$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")

VGGishPercentCorrect <- 
  round(1-min(ml.model.rf.vggish$err.rate[,1]),3)


VGGishScatterNoise <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                   y = "Dim.2",
                                   color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish Noise',VGGishPercentCorrect*100, '% Correct'))

VGGishScatterNoise

write.csv(VGGishDFNoiseMeanSD,'data/VGGishDFMeanSD6dB.csv',row.names = F)

