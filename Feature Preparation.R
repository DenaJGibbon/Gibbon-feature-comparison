library(dplyr)
library(stringr)
library(ggplot2)

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
  TempDF <-read.table(BirdNetFilesShort[a])
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
  CombinedValsDFSD <- sapply(CombinedValsDF, sd)
  CombinedVec <- c(CombinedValsDFMean,CombinedValsDFSD)
  NewDataFrame <- data.frame()
  
  NewDataFrame <- rbind.data.frame(NewDataFrame,CombinedVec)
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

CombinedBirdNetFeatures$Individual <- as.factor(CombinedBirdNetFeatures$Individual)

ml.model.svm <-
  e1071::svm(
    CombinedBirdNetFeatures[,-c(641,642,643,644)],
    CombinedBirdNetFeatures$Individual,
    kernel = "radial",
    #gamma = tune.rad$best.parameters$gamma,
    #cost = tune.rad$best.parameters$cost,
    cross = 25,
    probability = TRUE
  )

ml.model.svm$tot.accuracy

length(unique(CombinedBirdNetFeatures$Individual))

AcousticSignals.umap <-
  umap::umap(
    CombinedBirdNetFeatures[,-c(641,642,643,644)],
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


ggpubr::ggscatter(
    data = plot.for.AcousticSignals,
    x = "Dim.1",
    y = "Dim.2",
    color  = "class"
  ) +
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")
  


# Zero padding ------------------------------------------------------------

CombinedBirdNetFeaturesBy3sec <- data.frame()

for(z in 1:length(BirdNetFeaturelist)){
  
  BirdNetFiles <-   list.files(BirdNetFeaturelist[z],full.names = T)
  BirdNetFilesShort <-   list.files(BirdNetFeaturelist[z],full.names = F)
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
    
   
    CombinedValsDF$CallID <- CallID
    CombinedValsDF$Individual <- Individual
    CombinedValsDF$TrainingData  <- TrainingData
    CombinedValsDF$Duration <- max(TempDF$V2)
    CombinedBirdNetFeaturesBy3sec <- rbind.data.frame(CombinedBirdNetFeaturesBy3sec,CombinedValsDF)
  }
  
}


nrow(CombinedBirdNetFeaturesBy3sec)

# CombinedBirdNetFeaturesBy3sec <- 
#   CombinedBirdNetFeaturesBy3sec %>% mutate_if(is.character,as.numeric)


CombinedBirdNetFeaturesBy3sec$Individual <- paste(str_split_fixed(CombinedBirdNetFeaturesBy3sec$Individual,
                                                            pattern = '_',n=3)[,1],
                                            str_split_fixed(CombinedBirdNetFeaturesBy3sec$Individual,
                                                            pattern = '_',n=3)[,2],sep='_')

CombinedBirdNetFeaturesBy3sec$Individual <- as.factor(CombinedBirdNetFeaturesBy3sec$Individual)

ml.model.rf <-
  randomForest::randomForest(x = CombinedBirdNetFeaturesBy3sec[,-c(321:324)], y = CombinedBirdNetFeaturesBy3sec$Individual)


ml.model.svm <-
  e1071::svm(
    CombinedBirdNetFeaturesBy3sec[,-c(321:324)],
    CombinedBirdNetFeaturesBy3sec$Individual,
    kernel = "radial",
    #gamma = tune.rad$best.parameters$gamma,
    #cost = tune.rad$best.parameters$cost,
    cross = 25,
    probability = TRUE
  )

ml.model.svm$tot.accuracy

length(unique(CombinedBirdNetFeatures$Individual))


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


ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")

