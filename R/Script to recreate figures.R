library(ggpubr)
library(umap)
library(stringr)
library(cowplot)
library(viridis)

# Plotting SNR  -----------------------------------------------------------
SNR_file_names <- dir('data/snr_df/', full.names = T) # Directory containing the saved SNR files

# Read all SNR files and combine them into a single data frame
AllPlaybacksSNR <- do.call(rbind, lapply(SNR_file_names, read.csv))

# Extract time information from great.call column
AllPlaybacksSNR$time <- str_split_fixed(AllPlaybacksSNR$great.call, pattern='_', n=4)[, 3]
AllPlaybacksSNR$time <- substr(AllPlaybacksSNR$time, 1, 4)

# Convert Recorder column to factor and relabel levels
AllPlaybacksSNR$Recorder <- as.factor(AllPlaybacksSNR$recorder)
levels(AllPlaybacksSNR$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                      'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

# Create boxplots for SNR analysis
ggboxplot(data = AllPlaybacksSNR, x = 'Recorder', y = "SNR.dB", outlier.shape = NA) + ylab('SNR (dB re 20 μPa)')


# Plotting randomization results -------------------------------------------------

# Load and process data from the 'data/randomization_hdbscan/' directory
file_names <- dir('data/randomization_hdbscan/', full.names = TRUE) # List all file names in the directory

# Combine data from all files into a single data frame using read.csv and do.call(rbind, ...)
AllPlaybacksRand <- do.call(rbind, lapply(file_names, read.csv))

# Convert 'AccuracyVal' column to percentage scale
AllPlaybacksRand$AccuracyVal <- AllPlaybacksRand$AccuracyVal * 100

# Convert factors for 'Recorder' and 'Features' columns to meaningful labels
AllPlaybacksRand$Recorder <- as.factor(AllPlaybacksRand$Recorder)
levels(AllPlaybacksRand$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                       'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

AllPlaybacksRand$Features <- as.factor(AllPlaybacksRand$Features)
levels(AllPlaybacksRand$Features) <- c('Acoustic Indices', 'BirdNET', 'MFCCs',
                                       'VGGish', 'Wav2Vec2')

# Create ggplot2 plot 'PlotA' for Classification Accuracy by Features and Recorder
PlotA <- ggboxplot(data = AllPlaybacksRand,
                   color = 'Recorder', fill = 'Recorder', y = 'AccuracyVal', x = 'Features') +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Classification Accuracy')

# Create ggplot2 plot 'PlotBHDBSCAN' for Normalized Mutual Information (HDBSCAN) by Features and Recorder
PlotBHDBSCAN <- ggboxplot(data = AllPlaybacksRand,
                          color = 'Recorder', fill = 'Recorder', y = 'NMI.val', x = 'Features') +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Normalized Mutual Information (HDBSCAN)')

# Calculate the deviation between 'N.Individual' and 'N.cluster' columns and add it as 'Deviation' column
AllPlaybacksRand$Deviation <- AllPlaybacksRand$N.Individual - AllPlaybacksRand$N.cluster

# Create ggplot2 plot 'PlotCHDBSCAN' for Number of Clusters (HDBSCAN) by Features and Recorder
PlotCHDBSCAN <- ggboxplot(data = AllPlaybacksRand,
                          color = 'Recorder', fill = 'Recorder', y = 'N.cluster', x = 'Features', outlier.shape = NA) +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Number of clusters (HDBSCAN)') +
  geom_hline(yintercept = 12, linetype = 'dashed') + ylim(0, 25) + xlab('')

# Create a cowplot grid with PlotA and PlotB, labeled as 'A' and 'B', respectively, in 2 rows
cowplot::plot_grid(PlotA, PlotBHDBSCAN, labels = c('A', 'B'), nrow = 2, label_x = 0.95)

# Repeat the process for the 'data/randomization_affinity/' directory
# Get the list of all file names in the 'data/randomization_affinity/' directory, with their full paths
file_names <- dir('data/randomization_affinity/', full.names = TRUE)

# Read each CSV file in the list and combine all of them into a single data frame
AllPlaybacksRand <- do.call(rbind, lapply(file_names, read.csv))

# Convert the 'AccuracyVal' column values to percentage by multiplying with 100
AllPlaybacksRand$AccuracyVal <- AllPlaybacksRand$AccuracyVal * 100

# Convert the 'Recorder' column to a factor data type
AllPlaybacksRand$Recorder <- as.factor(AllPlaybacksRand$Recorder)

# Rename the factor levels of the 'Recorder' column to more descriptive labels
levels(AllPlaybacksRand$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                       'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

# Convert the 'Features' column to a factor data type
AllPlaybacksRand$Features <- as.factor(AllPlaybacksRand$Features)

# Rename the factor levels of the 'Features' column to more descriptive labels
levels(AllPlaybacksRand$Features) <- c('Acoustic Indices', 'BirdNET', 'MFCCs',
                                       'VGGish', 'Wav2Vec2')

# Create ggplot2 plot 'PlotBaffinity' for Normalized Mutual Information (affinity) by Features and Recorder
PlotBaffinity <- ggboxplot(data = AllPlaybacksRand,
                           color = 'Recorder', fill = 'Recorder', y = 'NMI.val', x = 'Features') +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Normalized Mutual Information (affinity)') + xlab('')

# Calculate the deviation between 'N.Individual' and 'N.cluster' columns and add it as 'Deviation' column
AllPlaybacksRand$Deviation <- AllPlaybacksRand$N.Individual - AllPlaybacksRand$N.cluster

# Create ggplot2 plot 'PlotCaffinity' for Number of Clusters (affinity) by Features and Recorder
PlotCaffinity <- ggboxplot(data = AllPlaybacksRand,
                           color = 'Recorder', fill = 'Recorder', y = 'N.cluster', x = 'Features', outlier.shape = NA) +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Number of clusters (affinity)') +
  geom_hline(yintercept = 12, linetype = 'dashed') + ylim(0, 25) + xlab('')

# Create a cowplot grid with PlotBaffinity and PlotBHDBSCAN, labeled as 'A' and 'B', respectively, in 2 rows
cowplot::plot_grid(PlotBaffinity, PlotBHDBSCAN, labels = c('A', 'B'), nrow = 2, label_x = 0.95)

# Create a cowplot grid with PlotCaffinity and PlotCHDBSCAN, labeled as 'A' and 'B', respectively, in 2 rows
cowplot::plot_grid(PlotCaffinity, PlotCHDBSCAN, labels = c('A', 'B'), nrow = 2, label_x = 0.95)


# Summary table of performance --------------------------------------------------
library(ggplot2)
library(cowplot)
library(viridis)
library(dplyr)
library(kableExtra)

# Load and process data from the 'data/randomization_hdbscan/' directory
file_names <- dir('data/randomization_hdbscan/', full.names = TRUE) # List all file names in the directory

# Combine data from all files into a single data frame using read.csv and do.call(rbind, ...)
AllPlaybacksRand <- do.call(rbind, lapply(file_names, read.csv))

# Number of unique playbacks (No plot or table generated for this section)
MFCCPlaybacks <- read.csv('data/features/MFCCPlaybacks.csv')
uniquepd <- str_split_fixed(MFCCPlaybacks$Recording, pattern = '_', n = 2)[, 2]
uniquepd <- substr((uniquepd), 1, 13)
unique(uniquepd)

# Calculate the mean and standard deviation by feature type and recorder distance
summary_data <- AllPlaybacksRand %>%
  group_by(Features, Recorder) %>%
  summarise(mean_value = mean(AccuracyVal, na.rm = TRUE),
            sd_value = sd(AccuracyVal, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_value)) # Order the table by recorder_distance

# Create the kable table with custom font
my_table <- kable(summary_data) %>%
  kable_styling()

my_table

# Save the kable table as a PDF file named 'summary_data.pdf'
kableExtra::save_kable(my_table, file = 'Table 1 Online Supporting Material.pdf')


# MFCC UMAP Plots ------------------------------------------------------

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

# BirdNET UMAP Plots ---------------------------------------------------
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


# VGGish UMAP Plots ---------------------------------------------------
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


# wav2vec2 UMAP Plots ---------------------------------------------------
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


# AcousticIndices UMAP Plots ---------------------------------------------------
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

cowplot::plot_grid(AcousticIndicesM2Scatter,
                   BirdNETM2Scatter,
                   MFCCM2Scatter,
                   VGGishM2Scatter,wav2vec2M2Scatter
                   )




