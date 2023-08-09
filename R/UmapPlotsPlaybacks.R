library(ggpubr)
library(umap)
library(randomForest)
library(stringr)
library(caret)
library(dbscan)
library(aricode)

# MFCC Randomization ------------------------------------------------------

# Read MFCCPlaybacks.csv
MFCCPlaybacks <- read.csv('data/MFCCPlaybacks.csv')

# Extract the recorder information from the 'Recording' column
MFCCPlaybacks$Recorder <- str_split_fixed(MFCCPlaybacks$Recording, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderMFCC <- unique(MFCCPlaybacks$Recorder)

# Convert 'Individual' column to a factor
MFCCPlaybacks$Individual <- as.factor(MFCCPlaybacks$Individual)


# Subset the data for the current recorder
MFCCPlaybacksSingleRecorderM2 <- droplevels(subset(MFCCPlaybacks, Recorder == 'M2'))
MFCCPlaybacksSingleRecorderM6 <- droplevels(subset(MFCCPlaybacks, Recorder == 'M6'))

MFCCM2.umap <-
  umap::umap(MFCCPlaybacksSingleRecorderM2 [, -c(26:28)],
             #labels=as.factor(MFCC$Validation),
             controlscale=TRUE,scale=3,n_neighbors=5)

plot.for.MFCCM2 <-
  cbind.data.frame(MFCCM2.umap$layout[,1:2],MFCCPlaybacksSingleRecorderM2 $Individual)

colnames(plot.for.MFCCM2) <-
  c("Dim.1", "Dim.2","Class")


MFCCM2Scatter <- ggpubr::ggscatter(data = plot.for.MFCCM2,x = "Dim.1",
                                    y = "Dim.2",
                                    color='Class')+guides(color='none')+
  scale_color_manual(values =viridis::viridis (length(
    unique(plot.for.MFCCM2$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCCs'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  +
  theme(plot.title = element_text(hjust = 1))  

MFCCM2Scatter

# MFCCM6.umap <-
#   umap::umap(MFCCPlaybacksSingleRecorderM6 [, -c(26:28)],
#              #labels=as.factor(MFCC$Validation),
#              controlscale=TRUE,scale=3,n_neighbors=5)
# 
# plot.for.MFCCM6 <-
#   cbind.data.frame(MFCCM6.umap$layout[,1:2],MFCCPlaybacksSingleRecorderM6 $Individual)
# 
# colnames(plot.for.MFCCM6) <-
#   c("Dim.1", "Dim.2","Class")
# 
# 
# MFCCM6Scatter <- ggpubr::ggscatter(data = plot.for.MFCCM6,x = "Dim.1",
#                                    y = "Dim.2",
#                                    color='Class')+guides(color='none')+
#   scale_color_manual(values =viridis::viridis (length(
#     unique(plot.for.MFCCM6$Class)
#   ))) + guides(color="none")+ggtitle( paste('MFCCs'))+theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  
# 
# MFCCM6Scatter


# BirdNET Randomization ---------------------------------------------------
file_names <- dir('data/BirdNET/', full.names = T)#where you have your files

BirdNETPlaybacks <- do.call(rbind,lapply(file_names,read.csv))

# Extract the recorder information from the 'Recording' column
BirdNETPlaybacks$Recorder <- as.factor(BirdNETPlaybacks$Recorder)

# Get unique recorders
UniqueRecorderBirdNET <- unique(BirdNETPlaybacks$Recorder)

# Convert 'Individual' column to a factor
BirdNETPlaybacks$Individual <- as.factor(BirdNETPlaybacks$Individual)

# Subset the data for the current recorder
BirdNETPlaybacksSingleRecorderM2 <- droplevels(subset(BirdNETPlaybacks, Recorder == 'M2'))
BirdNETPlaybacksSingleRecorderM6 <- droplevels(subset(BirdNETPlaybacks, Recorder == 'M6'))

BirdNETM2.umap <-
  umap::umap(BirdNETPlaybacksSingleRecorderM2 [, -c(2048:2050,2052)],
             #labels=as.factor(BirdNET$Validation),
             controlscale=TRUE,scale=3,n_neighbors=5)

plot.for.BirdNETM2 <-
  cbind.data.frame(BirdNETM2.umap$layout[,1:2],BirdNETPlaybacksSingleRecorderM2 $Individual)

colnames(plot.for.BirdNETM2) <-
  c("Dim.1", "Dim.2","Class")


BirdNETM2Scatter <- ggpubr::ggscatter(data = plot.for.BirdNETM2,x = "Dim.1",
                                   y = "Dim.2",
                                   color='Class')+guides(color='none')+
  scale_color_manual(values =viridis::viridis (length(
    unique(plot.for.BirdNETM2$Class)
  ))) + guides(color="none")+ggtitle( paste('BirdNET'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  

BirdNETM2Scatter

# BirdNETM6.umap <-
#   umap::umap(BirdNETPlaybacksSingleRecorderM6 [, -c(2048:2050,2052)],
#              #labels=as.factor(BirdNET$Validation),
#              controlscale=TRUE,scale=3,n_neighbors=5)
# 
# plot.for.BirdNETM6 <-
#   cbind.data.frame(BirdNETM6.umap$layout[,1:2],BirdNETPlaybacksSingleRecorderM6 $Individual)
# 
# colnames(plot.for.BirdNETM6) <-
#   c("Dim.1", "Dim.2","Class")
# 
# 
# BirdNETM6Scatter <- ggpubr::ggscatter(data = plot.for.BirdNETM6,x = "Dim.1",
#                                    y = "Dim.2",
#                                    color='Class')+guides(color='none')+
#   scale_color_manual(values =viridis::viridis (length(
#     unique(plot.for.BirdNETM6$Class)
#   ))) + guides(color="none")+ggtitle( paste('BirdNET'))+theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  
# 
# BirdNETM6Scatter

# VGGish Randomization ---------------------------------------------------
file_names <- dir('data/VGGish/', full.names = T)#where you have your files

VGGishPlaybacks <- do.call(rbind,lapply(file_names,read.csv))

# Extract the recorder information from the 'RecorderID' column
VGGishPlaybacks$Recorder <- str_split_fixed(VGGishPlaybacks$RecorderID, pattern = '_', n = 2)[, 1]

VGGishPlaybacks$Recorder <- as.factor(VGGishPlaybacks$Recorder)

# Get unique recorders
UniqueRecorderVGGish <- unique(VGGishPlaybacks$Recorder)

# Convert 'Individual' column to a factor
VGGishPlaybacks$Individual <- as.factor(VGGishPlaybacks$Individual)

# Subset the data for the current recorder
VGGishPlaybacksSingleRecorderM2 <- droplevels(subset(VGGishPlaybacks, Recorder == 'M2'))
VGGishPlaybacksSingleRecorderM6 <- droplevels(subset(VGGishPlaybacks, Recorder == 'M6'))

VGGishM2.umap <-
  umap::umap(VGGishPlaybacksSingleRecorderM2 [, -c(257,259,260)],
             #labels=as.factor(VGGish$Validation),
             controlscale=TRUE,scale=3,n_neighbors=5)

plot.for.VGGishM2 <-
  cbind.data.frame(VGGishM2.umap$layout[,1:2],VGGishPlaybacksSingleRecorderM2 $Individual)

colnames(plot.for.VGGishM2) <-
  c("Dim.1", "Dim.2","Class")


VGGishM2Scatter <- ggpubr::ggscatter(data = plot.for.VGGishM2,x = "Dim.1",
                                      y = "Dim.2",
                                      color='Class')+guides(color='none')+
  scale_color_manual(values =viridis::viridis (length(
    unique(plot.for.VGGishM2$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  

VGGishM2Scatter

# VGGishM6.umap <-
#   umap::umap(VGGishPlaybacksSingleRecorderM6 [, -c(257,259,260)],
#              #labels=as.factor(VGGish$Validation),
#              controlscale=TRUE,scale=3,n_neighbors=5)
# 
# plot.for.VGGishM6 <-
#   cbind.data.frame(VGGishM6.umap$layout[,1:2],VGGishPlaybacksSingleRecorderM6 $Individual)
# 
# colnames(plot.for.VGGishM6) <-
#   c("Dim.1", "Dim.2","Class")
# 
# 
# VGGishM6Scatter <- ggpubr::ggscatter(data = plot.for.VGGishM6,x = "Dim.1",
#                                       y = "Dim.2",
#                                       color='Class')+guides(color='none')+
#   scale_color_manual(values =viridis::viridis (length(
#     unique(plot.for.VGGishM6$Class)
#   ))) + guides(color="none")+ggtitle( paste('VGGish'))+theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  
# 
# VGGishM6Scatter

# wav2vec2 Randomization ---------------------------------------------------
# Read wav2vec2Playbacks.csv
wav2vec2Playbacks <- read.csv('data/wav2vecDFPlayback.csv')

# Extract the recorder information from the 'Recording' column
wav2vec2Playbacks$Recorder <- str_split_fixed(wav2vec2Playbacks$RecorderID, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderwav2vec2 <- unique(wav2vec2Playbacks$Recorder)

# Convert 'Individual' column to a factor
wav2vec2Playbacks$Individual <- as.factor(wav2vec2Playbacks$Individual)

# Get unique recorders
UniqueRecorderwav2vec2 <- unique(wav2vec2Playbacks$Recorder)

# Convert 'Individual' column to a factor
wav2vec2Playbacks$Individual <- as.factor(wav2vec2Playbacks$Individual)

# Subset the data for the current recorder
wav2vec2PlaybacksSingleRecorderM2 <- droplevels(subset(wav2vec2Playbacks, Recorder == 'M2'))
wav2vec2PlaybacksSingleRecorderM6 <- droplevels(subset(wav2vec2Playbacks, Recorder == 'M6'))

wav2vec2M2.umap <-
  umap::umap(wav2vec2PlaybacksSingleRecorderM2 [, -c(769,771:773)],
             #labels=as.factor(wav2vec2$Validation),
             controlscale=TRUE,scale=3,n_neighbors=5)

plot.for.wav2vec2M2 <-
  cbind.data.frame(wav2vec2M2.umap$layout[,1:2],wav2vec2PlaybacksSingleRecorderM2 $Individual)

colnames(plot.for.wav2vec2M2) <-
  c("Dim.1", "Dim.2","Class")


wav2vec2M2Scatter <- ggpubr::ggscatter(data = plot.for.wav2vec2M2,x = "Dim.1",
                                     y = "Dim.2",
                                     color='Class')+guides(color='none')+
  scale_color_manual(values =viridis::viridis (length(
    unique(plot.for.wav2vec2M2$Class)
  ))) + guides(color="none")+ggtitle( paste('wav2vec2'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  

wav2vec2M2Scatter

# wav2vec2M6.umap <-
#   umap::umap(wav2vec2PlaybacksSingleRecorderM6 [, -c(769,771:773)],
#              #labels=as.factor(wav2vec2$Validation),
#              controlscale=TRUE,scale=3,n_neighbors=5)
# 
# plot.for.wav2vec2M6 <-
#   cbind.data.frame(wav2vec2M6.umap$layout[,1:2],wav2vec2PlaybacksSingleRecorderM6 $Individual)
# 
# colnames(plot.for.wav2vec2M6) <-
#   c("Dim.1", "Dim.2","Class")
# 
# 
# wav2vec2M6Scatter <- ggpubr::ggscatter(data = plot.for.wav2vec2M6,x = "Dim.1",
#                                      y = "Dim.2",
#                                      color='Class')+guides(color='none')+
#   scale_color_manual(values =viridis::viridis (length(
#     unique(plot.for.wav2vec2M6$Class)
#   ))) + guides(color="none")+ggtitle( paste('wav2vec2'))+theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  
# 
# wav2vec2M6Scatter

# AcousticIndices Randomization ---------------------------------------------------
AcousticIndicesPlaybacks <- read.csv('data/AcousticIndicesDFPlayback.csv')

# Extract the recorder information from the 'Recording' column
AcousticIndicesPlaybacks$Recorder <- str_split_fixed(AcousticIndicesPlaybacks$RecorderID, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderAcousticIndices <- unique(AcousticIndicesPlaybacks$Recorder)

# Convert 'Individual' column to a factor
AcousticIndicesPlaybacks$Individual <- as.factor(AcousticIndicesPlaybacks$Individual)

# Convert 'Individual' column to a factor
AcousticIndicesPlaybacks$Individual <- as.factor(AcousticIndicesPlaybacks$Individual)

# Subset the data for the current recorder
AcousticIndicesPlaybacksSingleRecorderM2 <- droplevels(subset(AcousticIndicesPlaybacks, Recorder == 'M2'))
AcousticIndicesPlaybacksSingleRecorderM6 <- droplevels(subset(AcousticIndicesPlaybacks, Recorder == 'M6'))

AcousticIndicesM2.umap <-
  umap::umap(AcousticIndicesPlaybacksSingleRecorderM2 [, -c(6:8)],
             #labels=as.factor(AcousticIndices$Validation),
             controlscale=TRUE,scale=3,n_neighbors=5)

plot.for.AcousticIndicesM2 <-
  cbind.data.frame(AcousticIndicesM2.umap$layout[,1:2],AcousticIndicesPlaybacksSingleRecorderM2 $Individual)

colnames(plot.for.AcousticIndicesM2) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesM2Scatter <- ggpubr::ggscatter(data = plot.for.AcousticIndicesM2,x = "Dim.1",
                                       y = "Dim.2",
                                       color='Class')+guides(color='none')+
  scale_color_manual(values =viridis::viridis (length(
    unique(plot.for.AcousticIndicesM2$Class)
  ))) + guides(color="none")+ggtitle( paste('AcousticIndices'))+theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  

AcousticIndicesM2Scatter

# AcousticIndicesM6.umap <-
#   umap::umap(AcousticIndicesPlaybacksSingleRecorderM6 [, -c(6:8)],
#              #labels=as.factor(AcousticIndices$Validation),
#              controlscale=TRUE,scale=3,n_neighbors=5)
# 
# plot.for.AcousticIndicesM6 <-
#   cbind.data.frame(AcousticIndicesM6.umap$layout[,1:2],AcousticIndicesPlaybacksSingleRecorderM6 $Individual)
# 
# colnames(plot.for.AcousticIndicesM6) <-
#   c("Dim.1", "Dim.2","Class")
# 
# 
# AcousticIndicesM6Scatter <- ggpubr::ggscatter(data = plot.for.AcousticIndicesM6,x = "Dim.1",
#                                        y = "Dim.2",
#                                        color='Class')+guides(color='none')+
#   scale_color_manual(values =viridis::viridis (length(
#     unique(plot.for.AcousticIndicesM6$Class)
#   ))) + guides(color="none")+ggtitle( paste('AcousticIndices'))+theme(
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())+   theme(plot.title = element_text(hjust = 1))  
# 
# AcousticIndicesM6Scatter


cowplot::plot_grid(AcousticIndicesM2Scatter,
                   BirdNETM2Scatter,
                   MFCCM2Scatter,
                   VGGishM2Scatter,wav2vec2M2Scatter
                   )

# cowplot::plot_grid(AcousticIndicesM6Scatter,
#                    BirdNETM6Scatter,
#                    MFCCM6Scatter,
#                    VGGishM6Scatter,wav2vec2M6Scatter
# )

