library(dplyr)
library(stringr)
library(ggplot2)
set.seed(13)

BirdNetFiles <- 
  list.files("data/BirdNETembeddings",
             full.names = T)

BirdNetFilesShort <- 
  list.files("data/BirdNETembeddings",
             full.names = F)

CombinedBirdNetFeatures <- data.frame()

CallID <- str_split_fixed(BirdNetFilesShort,'.bird',n=2)[,1]
Individual <- str_split_fixed(CallID,'[.]',n=2)[,1]
TrainingData <- 'original'
for(a in 1:length(BirdNetFiles)){
  CallID <- str_split_fixed(BirdNetFilesShort[a],'.bird',n=2)[,1]
  Individual <- str_split_fixed(CallID,'[.]',n=2)[,1]
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
 
  CombinedValsDFMean <- colMeans((CombinedValsDF))
  
  NewDataFrame <- data.frame()
  
  NewDataFrame <- rbind.data.frame(NewDataFrame,CombinedValsDFMean)
  colnames(NewDataFrame) <-  paste('var_', seq(1,ncol(NewDataFrame),1),sep='' )
  NewDataFrame$CallID <- CallID
  NewDataFrame$Individual <- Individual
  NewDataFrame$TrainingData  <- TrainingData
  NewDataFrame$Duration <- max(TempDF$V2)
  print(NewDataFrame)
  CombinedBirdNetFeatures <- rbind.data.frame(CombinedBirdNetFeatures,NewDataFrame)
  }



CombinedBirdNetFeatures$Individual <- paste(str_split_fixed(CombinedBirdNetFeatures$Individual,
                                                      pattern = '_',n=3)[,1],
                                            str_split_fixed(CombinedBirdNetFeatures$Individual,
                                                             pattern = '_',n=3)[,2],sep='_')

CombinedBirdNetFeatures <- 
  subset(CombinedBirdNetFeatures, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

CombinedBirdNetFeatures <- droplevels(CombinedBirdNetFeatures)

CombinedBirdNetFeatures$Individual <- as.factor(CombinedBirdNetFeatures$Individual)

(unique(CombinedBirdNetFeatures$Individual))

ml.model.rf.birdnet.mean <-
  randomForest::randomForest(x = CombinedBirdNetFeatures[,-c(1025:1028)], y = CombinedBirdNetFeatures$Individual)


1-min(ml.model.rf.birdnet.mean$err.rate[,1])


AcousticSignals.umap <-
  umap::umap(
    CombinedBirdNetFeatures[,-c(1025:1028)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   CombinedBirdNetFeatures$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

BirdnetPercentCorrecMeant <- 
  round(1-min(ml.model.rf.birdnet.mean$err.rate[,1]),3)



BirdNetScatterMean <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNet',BirdnetPercentCorrecMeant*100, '% Correct'))

BirdNetScatterMean

write.csv(CombinedBirdNetFeatures,'data/CombinedBirdNetFeaturesV2.csv',row.names = F)




# BirdNetNoise noise -----------------------------------------------------------

CombinedBirdNetNoiseFeatures <- data.frame()


  BirdNetNoiseFiles <-   list.files("data/BirdNETembeddings6dB",
                                    full.names = T)
  
  BirdNetNoiseFilesShort <-    list.files("data/BirdNETembeddings6dB",
                                          full.names = F)
  
  CallID <- str_split_fixed(BirdNetNoiseFilesShort,'.bird',n=2)[,1]
  Individual <- str_split_fixed(CallID,'[.]',n=2)[,1]
  TrainingData <- 'noise6b'
  
  for(a in 1:length(BirdNetNoiseFiles)){
    print(a)
    CallID <- str_split_fixed(BirdNetNoiseFilesShort[a],'.bird',n=2)[,1]
    Individual <- str_split_fixed(CallID,'[.]',n=2)[,1]
    TempDF <-read.table(BirdNetNoiseFiles[a])
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
    
    CombinedValsDFMean <- colMeans((CombinedValsDF))
    
    NewDataFrame <- data.frame()
    
    NewDataFrame <- rbind.data.frame(NewDataFrame,CombinedValsDFMean)
    colnames(NewDataFrame) <-  paste('var_', seq(1,ncol(NewDataFrame),1),sep='' )
    NewDataFrame$CallID <- CallID
    NewDataFrame$Individual <- Individual
    NewDataFrame$TrainingData  <- TrainingData
    NewDataFrame$Duration <- max(TempDF$V2)
    #print(NewDataFrame)
    CombinedBirdNetNoiseFeatures <- rbind.data.frame(CombinedBirdNetNoiseFeatures,NewDataFrame)
  }
  




CombinedBirdNetNoiseFeatures$Individual <- paste(str_split_fixed(CombinedBirdNetNoiseFeatures$Individual,
                                                            pattern = '_',n=3)[,1],
                                            str_split_fixed(CombinedBirdNetNoiseFeatures$Individual,
                                                            pattern = '_',n=3)[,2],sep='_')

CombinedBirdNetNoiseFeatures <- 
  subset(CombinedBirdNetNoiseFeatures, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

CombinedBirdNetNoiseFeatures <- droplevels(CombinedBirdNetNoiseFeatures)

CombinedBirdNetNoiseFeatures$Individual <- as.factor(CombinedBirdNetNoiseFeatures$Individual)

(unique(CombinedBirdNetNoiseFeatures$Individual))

ml.model.rf.BirdNetNoise.mean <-
  randomForest::randomForest(x = CombinedBirdNetNoiseFeatures[,-c(1025:1028)], y = CombinedBirdNetNoiseFeatures$Individual)


1-min(ml.model.rf.BirdNetNoise.mean$err.rate[,1])


AcousticSignals.umap <-
  umap::umap(
    CombinedBirdNetNoiseFeatures[,-c(1025:1028)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   CombinedBirdNetNoiseFeatures$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

BirdNetNoisePercentCorrecMeant <- 
  round(1-min(ml.model.rf.BirdNetNoise.mean$err.rate[,1]),3)



BirdNetNoiseScatterMean <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNet Noise',BirdNetNoisePercentCorrecMeant*100, '% Correct'))

BirdNetNoiseScatterMean

write.csv(CombinedBirdNetNoiseFeatures,'data/CombinedBirdNetNoiseFeatures.csv',row.names = F)


