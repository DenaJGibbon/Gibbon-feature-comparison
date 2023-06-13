library(soundecology)

# Set input directory
input.dir <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles"

call.timing.list <-
  list.files(input.dir, full.names = T, pattern = '.wav')

call.timing.list.short <-
  list.files(input.dir, full.names = F, pattern = '.wav')

AcousticIndicesDF <- data.frame()

for (a in 1:length(call.timing.list)) {
  print(a)
  short.wav <- readWave(call.timing.list[a])
  
  ACI <- acoustic_complexity(short.wav, min_freq=500, max_freq = 2000)$AciTotAll_left
  
  ADI <- acoustic_diversity(short.wav, max_freq = 2000)$adi_left
  
  AEI <- acoustic_evenness(short.wav, max_freq = 2000)$aei_left
  
  bioindex <- bioacoustic_index(short.wav, min_freq=500, max_freq = 2000)$left_area
  
  duration <- seewave::duration(short.wav)
  
  Individual <- paste(str_split_fixed(call.timing.list.short[a],
                                      pattern = '_',n=3)[,1],
                      str_split_fixed(call.timing.list.short[a],
                                      pattern = '_',n=3)[,2],sep='_')
  
  
  TempAcoDiv <- cbind.data.frame(ACI,ADI,AEI,bioindex,duration,Individual)
  AcousticIndicesDF <- rbind.data.frame(AcousticIndicesDF,TempAcoDiv)
}


AcousticIndicesDF <- 
  subset(AcousticIndicesDF, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

AcousticIndicesDF <- droplevels(AcousticIndicesDF)

AcousticIndicesDF$Individual <- as.factor(AcousticIndicesDF$Individual)

ml.model.rf.acoustic <-
  randomForest::randomForest(x = AcousticIndicesDF[,-c(6)], y = AcousticIndicesDF$Individual)

1-min(ml.model.rf.acoustic$err.rate[,1])


AcousticSignals.umap <-
  umap::umap(AcousticIndicesDF[,-c(6)],
             #labels=as.factor(AcousticIndices$Validation),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],AcousticIndicesDF$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesPercentCorrect <- 
  round(1-min(ml.model.rf.acoustic$err.rate[,1]),3)


AcousticIndicesScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                   y = "Dim.2",
                                   color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('Acoustic Indices',AcousticIndicesPercentCorrect*100, '% Correct'))

AcousticIndicesScatter


plot.for.AcousticSignals$Site <- str_split_fixed(plot.for.AcousticSignals$Class,pattern = '_',n=2)[,1]
plot.for.AcousticSignals$Site  <- substr(plot.for.AcousticSignals$Site, start=1,stop=2)
plot.for.AcousticSignals$Site  <- recode_factor(plot.for.AcousticSignals$Site, VJ = "SA")

plot.for.AcousticSignals$Site  <- factor(plot.for.AcousticSignals$Site, levels=c( "CR", "DK", "DV", "IC", "KB", "MB","SA"))

ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Site',alpha=0.25)+
  scale_color_manual(values = viridis::viridis (length(unique(plot.for.AcousticSignals$Site)))) +ggtitle( ('Acoustic Indices by Site'))

AcousticIndicesScatter

write.csv(AcousticIndicesDF,'data/AcousticIndicesDForiginal.csv',row.names = F)

library(soundecology)

# Set input directory
input.dir <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/output6db"

call.timing.list <-
  list.files(input.dir, full.names = T, pattern = '.wav')

call.timing.list.short <-
  list.files(input.dir, full.names = F, pattern = '.wav')

AcousticIndicesNoiseDF <- data.frame()

for (a in 1:length(call.timing.list)) {
  print(a)
  short.wav <- readWave(call.timing.list[a])
  
  ACI <- acoustic_complexity(short.wav, min_freq=500, max_freq = 2000)$AciTotAll_left
  
  ADI <- acoustic_diversity(short.wav, max_freq = 2000)$adi_left
  
  AEI <- acoustic_evenness(short.wav, max_freq = 2000)$aei_left
  
  bioindex <- bioacoustic_index(short.wav, min_freq=500, max_freq = 2000)$left_area
  
  duration <- seewave::duration(short.wav)
  
  Individual <- paste(str_split_fixed(call.timing.list.short[a],
                                      pattern = '_',n=3)[,1],
                      str_split_fixed(call.timing.list.short[a],
                                      pattern = '_',n=3)[,2],sep='_')
  
  
  TempAcoDiv <- cbind.data.frame(ACI,ADI,AEI,bioindex,duration,Individual)
  AcousticIndicesNoiseDF <- rbind.data.frame(AcousticIndicesNoiseDF,TempAcoDiv)
}


AcousticIndicesNoiseDF <- 
  subset(AcousticIndicesNoiseDF, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

AcousticIndicesNoiseDF <- droplevels(AcousticIndicesNoiseDF)

AcousticIndicesNoiseDF$Individual <- as.factor(AcousticIndicesNoiseDF$Individual)



ml.model.rf.acoustic <-
  randomForest::randomForest(x = AcousticIndicesNoiseDF[,-c(6)], y = AcousticIndicesNoiseDF$Individual)

1-min(ml.model.rf.acoustic$err.rate[,1])


AcousticSignals.umap <-
  umap::umap(AcousticIndicesNoiseDF[,-c(6)],
             #labels=as.factor(AcousticIndicesNoise$Validation),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],AcousticIndicesNoiseDF$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesNoisePercentCorrect <- 
  round(1-min(ml.model.rf.acoustic$err.rate[,1]),3)


AcousticIndicesNoiseScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('Acoustic IndicesNoise',AcousticIndicesNoisePercentCorrect*100, '% Correct'))

AcousticIndicesNoiseScatter


plot.for.AcousticSignals$Site <- str_split_fixed(plot.for.AcousticSignals$Class,pattern = '_',n=2)[,1]
plot.for.AcousticSignals$Site  <- substr(plot.for.AcousticSignals$Site, start=1,stop=2)
plot.for.AcousticSignals$Site  <- recode_factor(plot.for.AcousticSignals$Site, VJ = "SA")

plot.for.AcousticSignals$Site  <- factor(plot.for.AcousticSignals$Site, levels=c( "CR", "DK", "DV", "IC", "KB", "MB","SA"))

ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                  y = "Dim.2",
                  color='Site',alpha=0.25)+
  scale_color_manual(values = viridis::viridis (length(unique(plot.for.AcousticSignals$Site)))) +ggtitle( ('Acoustic IndicesNoise by Site'))

AcousticIndicesNoiseScatter

write.csv(AcousticIndicesNoiseDF,'data/AcousticIndicesNoiseDForiginal.csv',row.names = F)
