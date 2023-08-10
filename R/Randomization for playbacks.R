# Load packages
library(ggpubr)
library(umap)
library(randomForest)
library(stringr)
library(caret)
library(dbscan)
library(aricode)

# NOTE: code is currently set to run affinity propagation clustering, to run 'hdbscan' uncomment 
# TempCluster <- hdbscan(AcousticSignals.umap$layout[,1:2], minPts = 20) for all feature types
# When using apcluster use 'CTRL + F' to replace 'TempCluster$cluster' with 'TempCluster@idx' 
# When using hdbscan use 'CTRL + F' to replace 'TempCluster@idx' with 'TempCluster$cluster'

# MFCC Randomization ------------------------------------------------------

# Read MFCCPlaybacks.csv
MFCCPlaybacks <- read.csv('data/features/MFCCPlaybacks.csv')

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
file_names <- dir('data/features/BirdNET', full.names = T) #where you have your files

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
file_names <- dir('data/features/VGGish/', full.names = T)#where you have your files

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
wav2vec2Playbacks <- read.csv('data/features/wav2vecmeansdDFPlayback.csv')

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
AcousticIndicesPlaybacks <- read.csv('data/features/AcousticIndicesDFPlayback.csv')

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


