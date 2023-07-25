library(dplyr)
library(stringr)
library(ggplot2)
set.seed(13)

BirdNetFiles <- 
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample",
             full.names = T)

BirdNetFilesShort <- 
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample",
             full.names = F)

RecorderID <- str_split_fixed(BirdNetFilesShort,pattern = '_',n=2)[,1]

CallID <- str_split_fixed(BirdNetFilesShort,'_',n=4)[,4]
CallID <- str_split_fixed(CallID,'_snr',n=2)[,1]

IndividualID <- paste(str_split_fixed(CallID,'_',n=3)[,1],
                    str_split_fixed(CallID,'_',n=3)[,2],sep='_')

RecordingID <- paste(str_split_fixed(BirdNetFilesShort,'_',n=4)[,1],
                     str_split_fixed(BirdNetFilesShort,'_',n=4)[,2],
                     str_split_fixed(BirdNetFilesShort,'_',n=4)[,3],
                     sep='_')

Recorder <- str_split_fixed(BirdNetFilesShort,'_',n=4)[,1]

CombinedBirdNetFeatures <- data.frame()

for(a in 1:length(BirdNetFiles)){ # 
  print(a)
  Individual <- IndividualID[a]
  print(Individual)
  TempDF <-read.table(BirdNetFiles[a])
  CommaSplit <- strsplit(TempDF$V3,split = ',')
  
  CombinedValsDF <- data.frame()
  for(b in 1:length(CommaSplit)){
   TempVec <- CommaSplit[[b]]
   CombinedVals <- data.frame()
   CombinedVals <- rbind.data.frame(CombinedVals,TempVec)
   colnames(CombinedVals) <- paste('var_', seq(1,length(TempVec),1),sep='' )
   CombinedValsDF <- rbind.data.frame(CombinedValsDF,CombinedVals)
  }
  
  CombinedValsDF <- 
    CombinedValsDF %>% mutate_if(is.character,as.numeric)
 
  CombinedValsDFMean <- t(apply(CombinedValsDF,2,mean))
  CombinedValsSD <- t(apply(CombinedValsDF,2,sd))
  CombinedValsMeanSD <-  cbind.data.frame(CombinedValsDFMean,CombinedValsSD)
  NewDataFrame <- data.frame()
  
  NewDataFrame <- rbind.data.frame(NewDataFrame,CombinedValsMeanSD)
  colnames(NewDataFrame) <-  paste('var_', seq(1,ncol(NewDataFrame),1),sep='' )
  NewDataFrame$CallID <- CallID[a]
  NewDataFrame$Individual <- Individual
  NewDataFrame$Recorder  <- Recorder[a]
  NewDataFrame$Duration <- max(TempDF$V2)
  NewDataFrame$RecordingID <-RecordingID[a]
  CombinedBirdNetFeatures <- rbind.data.frame(CombinedBirdNetFeatures,NewDataFrame)
  write.csv(CombinedBirdNetFeatures,'/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/data/CombinedBirdNetFeaturesPlayback.csv',row.names = F)
  }



CombinedBirdNetFeatures$Individual <- as.factor(CombinedBirdNetFeatures$Individual)

# Randomization
RandomizationAccuracyBirdNEToriginal <- list()
for(a in 1:20){
  print(a)
  set.seed(a)  
  TrainingN <- 0.8*nrow(CombinedBirdNetFeatures )  
  
  TrainingSamples <-sample(seq(1:nrow(CombinedBirdNetFeatures )), TrainingN,replace=FALSE)
  
  TrainingData <- droplevels(CombinedBirdNetFeatures [TrainingSamples,])
  TestData <- droplevels(CombinedBirdNetFeatures [-TrainingSamples,])
  
  
  ml.model.rf.BirdNET.original <-
    randomForest::randomForest(x =  TrainingData[,-c(2048:2051,2053)], y =  TrainingData$Individual,
                               ntree = 500, random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.BirdNET.original,newdata=TestData[,-c(2048:2051)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyBirdNEToriginal[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyBirdNEToriginal))
sd(unlist(RandomizationAccuracyBirdNEToriginal))

AcousticSignals.umap <-
  umap::umap(CombinedBirdNetFeatures [,-c(2048:2051,2053)],
             #labels=as.factor(BirdNET$Validation),
             controlscale=TRUE,scale=3,n_neighbors=10)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],CombinedBirdNetFeatures $Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


BirdNETPercentCorrect <- 
  round(median(unlist(RandomizationAccuracyBirdNEToriginal)),3)


BirdNETScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                 y = "Dim.2",
                                 color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('BirdNET',BirdNETPercentCorrect*100, '% Correct'))

BirdNETScatter


Time <- str_split_fixed(CombinedBirdNetFeatures$RecordingID,pattern = '_',n=3)[,3]
plot.for.AcousticSignals$Time <- Time
plot.for.AcousticSignals$Site <- str_split_fixed(CombinedBirdNetFeatures$Individual,pattern = '_',n=3)[,1]

ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                  y = "Dim.2",
                  color="Site")+guides(color='none')
