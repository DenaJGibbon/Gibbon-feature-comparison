library(dplyr)
library(stringr)
library(ggplot2)
set.seed(13)

BirdNetFeaturelist <- 
  list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/BirdNetEmbeddingsVocalIndividual/",
             full.names = T)

BirdNetFeaturelistshort <- 
  list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/BirdNetEmbeddingsVocalIndividual/",
             full.names = F)

CombinedBirdNetFeatures <- data.frame()

for(z in 1:length(BirdNetFeaturelist)){

BirdNetFiles <-   list.files(BirdNetFeaturelist[z],full.names = T)
BirdNetFilesShort <-   list.files(BirdNetFeaturelist[z],full.names = F)
CallID <- str_split_fixed(BirdNetFilesShort[z],'.bird',n=2)[,1]
Individual <- str_split_fixed(CallID,'[.]',n=2)[,1]
TrainingData <- BirdNetFeaturelistshort[z]
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
  randomForest::randomForest(x = CombinedBirdNetFeatures[,-c(321:324)], y = CombinedBirdNetFeatures$Individual)


1-min(ml.model.rf.birdnet.mean$err.rate[,1])


AcousticSignals.umap <-
  umap::umap(
    CombinedBirdNetFeatures[,-c(321:324)],
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

write.csv(CombinedBirdNetFeatures,'data/CombinedBirdNetFeatures.csv',row.names = F)



# Three secs ------------------------------------------------------------

CombinedBirdNetFeaturesBy3sec <- data.frame()

  BirdNetFiles <-   list.files(BirdNetFeaturelist[z],full.names = T)
  BirdNetFilesShort <-   list.files(BirdNetFeaturelist[z],full.names = F)
  #TrainingData <- BirdNetFeaturelistshort[z]
  for(a in 1:length(BirdNetFiles)){
    print(a)
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
    
   
    CombinedValsDF$CallID <- CallID
    CombinedValsDF$Individual <- Individual
    CombinedValsDF$TrainingData  <- TrainingData
    CombinedValsDF$Duration <- max(TempDF$V2)
    CombinedBirdNetFeaturesBy3sec <- rbind.data.frame(CombinedBirdNetFeaturesBy3sec,CombinedValsDF)
  }
  

nrow(CombinedBirdNetFeaturesBy3sec)

CombinedBirdNetFeaturesBy3sec$Individual <- paste(str_split_fixed(CombinedBirdNetFeaturesBy3sec$Individual,
                                                            pattern = '_',n=3)[,1],
                                            str_split_fixed(CombinedBirdNetFeaturesBy3sec$Individual,
                                                            pattern = '_',n=3)[,2],sep='_')

CombinedBirdNetFeaturesBy3sec$Individual <- as.factor(CombinedBirdNetFeaturesBy3sec$Individual)

ml.model.rf.birdnet <-
  randomForest::randomForest(x = CombinedBirdNetFeaturesBy3sec[,-c(321:324)], y = CombinedBirdNetFeaturesBy3sec$Individual)


1-min(ml.model.rf.birdnet$err.rate[,1])

AcousticSignals.umap <-
  umap::umap(
    CombinedBirdNetFeaturesBy3sec[,-c(321:324)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   CombinedBirdNetFeaturesBy3sec$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

# Remove outlier for now
# plot.for.AcousticSignals <- 
#   subset(plot.for.AcousticSignals, Dim.1 < -20)

BirdnetPercentCorrect <- 
  round(1-min(ml.model.rf.birdnet$err.rate[,1]),2)

BirdNetScatter <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNet',BirdnetPercentCorrect*100, '% Correct'))

BirdNetScatter
