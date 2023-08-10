# Load packages
library(ggpubr)
library(umap)
library(randomForest)
library(stringr)
library(caret)
library(dbscan)
library(aricode)

# V1 removes duration as a feature
# Note for apcluster used default q
# MFCC Randomization ------------------------------------------------------

# Read MFCCPlaybacks.csv
MFCCPlaybacks <- read.csv('data/MFCCPlaybacks.csv')

# Extract the recorder information from the 'Recording' column
MFCCPlaybacks$Recorder <- str_split_fixed(MFCCPlaybacks$Recording, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderMFCC <- unique(MFCCPlaybacks$Recorder)

# Convert 'Individual' column to a factor
MFCCPlaybacks$Individual <- as.factor(MFCCPlaybacks$Individual)

# Initialize an empty data frame for randomization accuracy
RandomizationAccuracyMFCC <- data.frame()

# Loop over each unique recorder
for (a in 1:length(UniqueRecorderMFCC)) {
  
  # Subset the data for the current recorder
  MFCCPlaybacksSingleRecorder <- subset(MFCCPlaybacks, Recorder == UniqueRecorderMFCC[a])
  
  # Perform randomization 20 times
  for (b in 1:20) {
    set.seed(b)
    
    # Split the data into training and test sets
    TrainingN <- 0.8 * nrow(MFCCPlaybacksSingleRecorder)
    TrainingSamples <- sample(seq(1:nrow(MFCCPlaybacksSingleRecorder)), TrainingN, replace = FALSE)
    TrainingData <- droplevels(MFCCPlaybacksSingleRecorder[TrainingSamples, ])
    TestData <- droplevels(MFCCPlaybacksSingleRecorder[-TrainingSamples, ])
    
    # Build a random forest model using the training data
    ml.model.rf.mfcc.original <- randomForest::randomForest(
      x = TrainingData[, -c(25:28)],
      y = TrainingData$Individual,
      ntree = 500,
      random_state = 0,
      keep.forest = TRUE
    )
    
    # Make predictions on the test data
    TestPredictions <- predict(ml.model.rf.mfcc.original, newdata = TestData[, -c(25:28)])
    
    # Compute accuracy of the predictions
    ConfMatrix <- confusionMatrix(TestData$Individual, TestPredictions, mode='everything')
    AccuracyVal <- ConfMatrix$overall[1]
    
    # Store accuracy and recorder information
    Recorder <- UniqueRecorderMFCC[a]
    Features <- 'MFCC'
   
    AcousticSignals.umap <- umap::umap( TrainingData[, -c(25:28)],
               controlscale=TRUE,scale=3,n_neighbors=10)
    
    #TempCluster <- hdbscan(AcousticSignals.umap$layout[,1:2], minPts = 20)
    TempCluster <- apcluster::apcluster(
      apcluster::negDistMat(r = 2),# q=0,
      AcousticSignals.umap$layout[,1:2],
      maxits = 5000,
      convits = 500,
      nonoise = T
    )
    N.cluster <- length(unique(TempCluster@idx))
    N.Individual <- length(unique(TrainingData$Individual))
    NMI.val <- NMI(TempCluster@idx,TrainingData$Individual)
    TempMFCCRow <- cbind.data.frame(AccuracyVal, Recorder,Features,N.cluster,N.Individual,NMI.val)
    
    RandomizationAccuracyMFCC <- rbind.data.frame(RandomizationAccuracyMFCC, TempMFCCRow)
    
    # Print and write RandomizationAccuracyMFCC to a CSV file
    print(TempMFCCRow)
    write.csv(RandomizationAccuracyMFCC, 'data/randomization_affinity/RandomizationAccuracyMFCC.csv')
  }
}

# BirdNET Randomization ------------------------------------------------------
# Read BirdNETPlaybacks.csv
file_names <- dir('data/BirdNET/', full.names = T) #where you have your files

BirdNETPlaybacks <- do.call(rbind,lapply(file_names,read.csv))

# Extract the recorder information from the 'Recording' column
BirdNETPlaybacks$Recorder <- as.factor(BirdNETPlaybacks$Recorder)

# Get unique recorders
UniqueRecorderBirdNET <- unique(BirdNETPlaybacks$Recorder)

# Convert 'Individual' column to a factor
BirdNETPlaybacks$Individual <- as.factor(BirdNETPlaybacks$Individual)


# Initialize an empty data frame for randomization accuracy
RandomizationAccuracyBirdNET <- data.frame()

# Loop over each unique recorder
for (a in 1:length(UniqueRecorderBirdNET)) {
  
  # Subset the data for the current recorder
  BirdNETPlaybacksSingleRecorder <- droplevels(subset(BirdNETPlaybacks, Recorder == UniqueRecorderBirdNET[a]))
  
  # Perform randomization 20 times
  for (b in 1:20) {
    set.seed(b)
    
    # Split the data into training and test sets
    TrainingN <- 0.8 * nrow(BirdNETPlaybacksSingleRecorder)
    TrainingSamples <- sample(seq(1:nrow(BirdNETPlaybacksSingleRecorder)), TrainingN, replace = FALSE)
    TrainingData <- droplevels(BirdNETPlaybacksSingleRecorder[TrainingSamples, ])
    TestData <- droplevels(BirdNETPlaybacksSingleRecorder[-TrainingSamples, ])
    
    # Build a random forest model using the training data
    ml.model.rf.BirdNET.original <- randomForest::randomForest(
      x = TrainingData[, -c(2048:2050:2052)],
      y = TrainingData$Individual,
      ntree = 500,
      random_state = 0,
      keep.forest = TRUE
    )
    
    # Make predictions on the test data
    TestPredictions <- predict(ml.model.rf.BirdNET.original, newdata = TestData[, -c(2048:2050:2052)])
    
    # Compute accuracy of the predictions
    ConfMatrix <- confusionMatrix(TestData$Individual, TestPredictions)
    AccuracyVal <- ConfMatrix$overall[1]
    
    # Store accuracy and recorder information
    Recorder <- UniqueRecorderBirdNET[a]
    Features <- 'BirdNet'
    
    AcousticSignals.umap <- umap::umap( TrainingData[, -c(2048:2050:2052)],
                                        controlscale=TRUE,scale=3,n_neighbors=10)
    
    #TempCluster <- hdbscan(AcousticSignals.umap$layout[,1:2], minPts = 20)
    TempCluster <- apcluster::apcluster(
      apcluster::negDistMat(r = 2),# q=0,
      AcousticSignals.umap$layout[,1:2],
      maxits = 5000,
      convits = 500,
      nonoise = T
    )
    N.cluster <- length(unique(TempCluster@idx))
    N.Individual <- length(unique(TrainingData$Individual))
    NMI.val <- NMI(TempCluster@idx,TrainingData$Individual)
    TempBirdNETRow <- cbind.data.frame(AccuracyVal, Recorder,Features,N.cluster,N.Individual,NMI.val)
    
    RandomizationAccuracyBirdNET <- rbind.data.frame(RandomizationAccuracyBirdNET, TempBirdNETRow)
    
    # Print and write RandomizationAccuracyBirdNET to a CSV file
    print(TempBirdNETRow)
    write.csv(RandomizationAccuracyBirdNET, 'data/randomization_affinity/RandomizationAccuracyBirdNET.csv')
  }
}

# VGGish Randomization ------------------------------------------------------
# Read VGGishPlaybacks.csv
file_names <- dir('data/VGGish/', full.names = T)#where you have your files

VGGishPlaybacks <- do.call(rbind,lapply(file_names,read.csv))

# Extract the recorder information from the 'Recording' column
VGGishPlaybacks$Recorder <- str_split_fixed(VGGishPlaybacks$RecorderID, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderVGGish <- unique(VGGishPlaybacks$Recorder)

# Convert 'Individual' column to a factor
VGGishPlaybacks$Individual <- as.factor(VGGishPlaybacks$Individual)

# Initialize an empty data frame for randomization accuracy
RandomizationAccuracyVGGish <- data.frame()

# Loop over each unique recorder
for (a in 1:length(UniqueRecorderVGGish)) {
  
  # Subset the data for the current recorder
  VGGishPlaybacksSingleRecorder <- subset(VGGishPlaybacks, Recorder == UniqueRecorderVGGish[a])
  
  # Perform randomization 20 times
  for (b in 1:20) {
    set.seed(b)
    
    # Split the data into training and test sets
    TrainingN <- 0.8 * nrow(VGGishPlaybacksSingleRecorder)
    TrainingSamples <- sample(seq(1:nrow(VGGishPlaybacksSingleRecorder)), TrainingN, replace = FALSE)
    TrainingData <- droplevels(VGGishPlaybacksSingleRecorder[TrainingSamples, ])
    TestData <- droplevels(VGGishPlaybacksSingleRecorder[-TrainingSamples, ])
    
    # Build a random forest model using the training data
    ml.model.rf.VGGish.original <- randomForest::randomForest(
      x = TrainingData[, -c(257:260)],
      y = TrainingData$Individual,
      ntree = 500,
      random_state = 0,
      keep.forest = TRUE
    )
    
    # Make predictions on the test data
    TestPredictions <- predict(ml.model.rf.VGGish.original, newdata = TestData[, -c(257:260)])
    
    # Compute accuracy of the predictions
    ConfMatrix <- confusionMatrix(TestData$Individual, TestPredictions)
    AccuracyVal <- ConfMatrix$overall[1]
    
    # Store accuracy and recorder information
    Recorder <- UniqueRecorderVGGish[a]
    Features <- 'VGGish'
    
    AcousticSignals.umap <- umap::umap( TrainingData[, -c(257:260)],
                                        controlscale=TRUE,scale=3,n_neighbors=10)
    
    #TempCluster <- hdbscan(AcousticSignals.umap$layout[,1:2], minPts = 20)
    TempCluster <- apcluster::apcluster(
      apcluster::negDistMat(r = 2),# q=0,
      AcousticSignals.umap$layout[,1:2],
      maxits = 5000,
      convits = 500,
      nonoise = T
    )
    N.cluster <- length(unique(TempCluster@idx))
    N.Individual <- length(unique(TrainingData$Individual))
    NMI.val <- NMI(TempCluster@idx,TrainingData$Individual)
    TempVGGishRow <- cbind.data.frame(AccuracyVal, Recorder,Features,N.cluster,N.Individual,NMI.val)
    
    RandomizationAccuracyVGGish <- rbind.data.frame(RandomizationAccuracyVGGish, TempVGGishRow)
    
    # Print and write RandomizationAccuracyVGGish to a CSV file
    print(TempVGGishRow)
    write.csv(RandomizationAccuracyVGGish, 'data/randomization_affinity/RandomizationAccuracyVGGish.csv')
  }
}

# wav2vec2 Randomization ------------------------------------------------------

# Read wav2vec2Playbacks.csv
wav2vec2Playbacks <- read.csv('data/wav2vecmeansdDFPlayback.csv')

# Extract the recorder information from the 'Recording' column
wav2vec2Playbacks$Recorder <- str_split_fixed(wav2vec2Playbacks$RecorderID, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderwav2vec2 <- unique(wav2vec2Playbacks$Recorder)

# Convert 'Individual' column to a factor
wav2vec2Playbacks$Individual <- as.factor(wav2vec2Playbacks$Individual)

# Initialize an empty data frame for randomization accuracy
RandomizationAccuracywav2vec2 <- data.frame()

# Loop over each unique recorder
for (a in 1:length(UniqueRecorderwav2vec2)) {
  
  # Subset the data for the current recorder
  wav2vec2PlaybacksSingleRecorder <- subset(wav2vec2Playbacks, Recorder == UniqueRecorderwav2vec2[a])
  
  # Perform randomization 20 times
  for (b in 1:20) {
    set.seed(b)
    
    # Split the data into training and test sets
    TrainingN <- 0.8 * nrow(wav2vec2PlaybacksSingleRecorder)
    TrainingSamples <- sample(seq(1:nrow(wav2vec2PlaybacksSingleRecorder)), TrainingN, replace = FALSE)
    TrainingData <- droplevels(wav2vec2PlaybacksSingleRecorder[TrainingSamples, ])
    TestData <- droplevels(wav2vec2PlaybacksSingleRecorder[-TrainingSamples, ])
    
    # Build a random forest model using the training data
    ml.model.rf.wav2vec2.original <- randomForest::randomForest(
      x = TrainingData[, -c(41:44)],
      y = TrainingData$Individual,
      ntree = 500,
      random_state = 0,
      keep.forest = TRUE
    )
    
    # Make predictions on the test data
    TestPredictions <- predict(ml.model.rf.wav2vec2.original, newdata = TestData[, -c(41:44)])
    
    # Compute accuracy of the predictions
    ConfMatrix <- confusionMatrix(TestData$Individual, TestPredictions)
    AccuracyVal <- ConfMatrix$overall[1]
    
    # Store accuracy and recorder information
    Recorder <- UniqueRecorderwav2vec2[a]
    Features <- 'wav2vec2'
    
    AcousticSignals.umap <- umap::umap( TrainingData[, -c(41:44)],
                                        controlscale=TRUE,scale=3,n_neighbors=10)
    
    #TempCluster <- hdbscan(AcousticSignals.umap$layout[,1:2], minPts = 20)
    TempCluster <- apcluster::apcluster(
      apcluster::negDistMat(r = 2),# q=0,
      AcousticSignals.umap$layout[,1:2],
      maxits = 5000,
      convits = 500,
      nonoise = T
    )
    N.cluster <- length(unique(TempCluster@idx))
    N.Individual <- length(unique(TrainingData$Individual))
    NMI.val <- NMI(TempCluster@idx,TrainingData$Individual)
    Tempwav2vec2Row <- cbind.data.frame(AccuracyVal, Recorder,Features,N.cluster,N.Individual,NMI.val)
    
    RandomizationAccuracywav2vec2 <- rbind.data.frame(RandomizationAccuracywav2vec2, Tempwav2vec2Row)
    
    # Print and write RandomizationAccuracywav2vec2 to a CSV file
    print(Tempwav2vec2Row)
    write.csv(RandomizationAccuracywav2vec2, 'data/randomization_affinity/RandomizationAccuracywav2vec2meansd.csv')
  }
}

# AcousticIndices Randomization ------------------------------------------------------

# Read AcousticIndicesPlaybacks.csv
AcousticIndicesPlaybacks <- read.csv('data/AcousticIndicesDFPlayback.csv')

# Extract the recorder information from the 'Recording' column
AcousticIndicesPlaybacks$Recorder <- str_split_fixed(AcousticIndicesPlaybacks$RecorderID, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderAcousticIndices <- unique(AcousticIndicesPlaybacks$Recorder)

# Convert 'Individual' column to a factor
AcousticIndicesPlaybacks$Individual <- as.factor(AcousticIndicesPlaybacks$Individual)

# Initialize an empty data frame for randomization accuracy
RandomizationAccuracyAcousticIndices <- data.frame()

# Loop over each unique recorder
for (a in 1:length(UniqueRecorderAcousticIndices)) {
  
  # Subset the data for the current recorder
  AcousticIndicesPlaybacksSingleRecorder <- subset(AcousticIndicesPlaybacks, Recorder == UniqueRecorderAcousticIndices[a])
  
  # Perform randomization 20 times
  for (b in 1:20) {
    set.seed(b)
    
    # Split the data into training and test sets
    TrainingN <- 0.8 * nrow(AcousticIndicesPlaybacksSingleRecorder)
    TrainingSamples <- sample(seq(1:nrow(AcousticIndicesPlaybacksSingleRecorder)), TrainingN, replace = FALSE)
    TrainingData <- droplevels(AcousticIndicesPlaybacksSingleRecorder[TrainingSamples, ])
    TestData <- droplevels(AcousticIndicesPlaybacksSingleRecorder[-TrainingSamples, ])
    
    # Build a random forest model using the training data
    ml.model.rf.AcousticIndices.original <- randomForest::randomForest(
      x = TrainingData[, -c(5:8)],
      y = TrainingData$Individual,
      ntree = 500,
      random_state = 0,
      keep.forest = TRUE
    )
    
    # Make predictions on the test data
    TestPredictions <- predict(ml.model.rf.AcousticIndices.original, newdata = TestData[, -c(5:8)])
    
    # Compute accuracy of the predictions
    ConfMatrix <- confusionMatrix(TestData$Individual, TestPredictions)
    AccuracyVal <- ConfMatrix$overall[1]
    
    # Store accuracy and recorder information
    Recorder <- UniqueRecorderAcousticIndices[a]
    Features <- 'AcousticIndices'
    AcousticSignals.umap <- umap::umap( TrainingData[, -c(5:8)],
                                        controlscale=TRUE,scale=3,n_neighbors=10)
    
    #TempCluster <- hdbscan(AcousticSignals.umap$layout[,1:2], minPts = 20)
    TempCluster <- apcluster::apcluster(
      apcluster::negDistMat(r = 2),# q=0,
      AcousticSignals.umap$layout[,1:2],
      maxits = 5000,
      convits = 500,
      nonoise = T
    )
    N.cluster <- length(unique(TempCluster@idx))
    N.Individual <- length(unique(TrainingData$Individual))
    NMI.val <- NMI(TempCluster@idx,TrainingData$Individual)
    TempAcousticIndicesRow <- cbind.data.frame(AccuracyVal, Recorder,Features,N.cluster,N.Individual,NMI.val)
    
    RandomizationAccuracyAcousticIndices <- rbind.data.frame(RandomizationAccuracyAcousticIndices, TempAcousticIndicesRow)
    
    # Print and write RandomizationAccuracyAcousticIndices to a CSV file
    print(TempAcousticIndicesRow)
    write.csv(RandomizationAccuracyAcousticIndices, 'data/randomization_affinity/RandomizationAccuracyAcousticIndices.csv')
  }
}


# Plots for randomization -------------------------------------------------

# Load required libraries
library(ggplot2)
library(cowplot)
library(viridis)

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
file_names <- dir('data/randomization_affinity/', full.names = TRUE)
AllPlaybacksRand <- do.call(rbind, lapply(file_names, read.csv))
AllPlaybacksRand$AccuracyVal <- AllPlaybacksRand$AccuracyVal * 100
AllPlaybacksRand$Recorder <- as.factor(AllPlaybacksRand$Recorder)
levels(AllPlaybacksRand$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                       'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')
AllPlaybacksRand$Features <- as.factor(AllPlaybacksRand$Features)
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




# Random forest features --------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)
library(varImp)


# Read AcousticIndicesPlaybacks.csv
AcousticIndicesPlaybacks <- read.csv('data/AcousticIndicesDFPlayback.csv')

# Extract the recorder information from the 'Recording' column
AcousticIndicesPlaybacks$Recorder <- str_split_fixed(AcousticIndicesPlaybacks$RecorderID, pattern = '_', n = 2)[, 1]

# Get unique recorders
UniqueRecorderAcousticIndices <- unique(AcousticIndicesPlaybacks$Recorder)

# Convert 'Individual' column to a factor
AcousticIndicesPlaybacks$Individual <- as.factor(AcousticIndicesPlaybacks$Individual)

#Random Forest Modelling
model <- randomForest::randomForest(
  x = AcousticIndicesPlaybacks[, -c(5:8)],
  y = AcousticIndicesPlaybacks$Individual,
  ntree = 500,
  random_state = 0,
  importance = TRUE
)


#Evaluate variable importance
importance(model)
varImpPlot(model)

ggpubr::ggbarplot(data=AcousticIndicesPlaybacks,x='Individual',y='ACI')
