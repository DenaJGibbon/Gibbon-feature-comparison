library(caret)
library(ggpubr)

# Randomization MFCCs -----------------------------------------------------
mfcc.output.df <- read.csv('data/mfcc.output.df.csv')
mfcc.output.df$Individual <- as.factor(mfcc.output.df$Individual)
mfcc.output.df$Site <- str_split_fixed(mfcc.output.df$Individual,
                                       pattern = '_',n=2)[,1]
table(mfcc.output.df$Site)
length(unique(mfcc.output.df$Individual ))

# Randomization MFCC 
RandomizationAccuracyMFCC <- list()
for(a in 1:20){
  set.seed(a)  
  print(a)
  TrainingN <- 0.8*nrow(mfcc.output.df)  
  
  TrainingSamples <-sample(seq(1:nrow(mfcc.output.df)), TrainingN,replace=FALSE)
  
  TrainingData <- mfcc.output.df[TrainingSamples,]
  TestData <- mfcc.output.df[-TrainingSamples,]
  
  
  ml.model.rf.mfcc. <-
    randomForest::randomForest(x =  TrainingData[,-c(1)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.mfcc.,newdata=TestData[,-c(1)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)

  
  AccuracyVal <- ConfMatrix$overall[1]
  
  RandomizationAccuracyMFCC[[a]] <- c(AccuracyVal)
} 


mean(unlist(RandomizationAccuracyMFCC))
sd(unlist(RandomizationAccuracyMFCC))

mfcc.noise.output.df <- read.csv('data/mfcc.noise6db.output.df.csv')
mfcc.noise.output.df$Individual <- as.factor(mfcc.noise.output.df$Individual)

# Randomization MFCC noise
RandomizationAccuracyMFCCNoise <- list()
for(a in 1:20){
  set.seed(a) 
  TrainingN <- 0.8*nrow(mfcc.noise.output.df)  
  
  TrainingSamples <-sample(seq(1:nrow(mfcc.noise.output.df)), TrainingN,replace=FALSE)
  
  TrainingData <- mfcc.noise.output.df[TrainingSamples,]
  TestData <- mfcc.noise.output.df[-TrainingSamples,]
  
  
  ml.model.rf.mfcc.noise <-
    randomForest::randomForest(x =  TrainingData[,-c(1)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.mfcc.noise,newdata=TestData[,-c(1)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyMFCCNoise[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyMFCCNoise))
sd(unlist(RandomizationAccuracyMFCCNoise))


# Randomization BirdNet -----------------------------------------------------
BirdNetoutput.df <- read.csv('data/CombinedBirdNetFeaturesV2.csv')
BirdNetoutput.df$Individual <- as.factor(BirdNetoutput.df$Individual)

# Randomization BirdNet 
RandomizationAccuracyBirdNet <- list()
for(a in 1:20){
  set.seed(a)  
  print(a)
  TrainingN <- 0.8*nrow(BirdNetoutput.df)  
  
  TrainingSamples <-sample(seq(1:nrow(BirdNetoutput.df)), TrainingN,replace=FALSE)
  
  TrainingData <- BirdNetoutput.df[TrainingSamples,]
  TestData <- BirdNetoutput.df[-TrainingSamples,]
  
  
  ml.model.rf.BirdNet. <-
    randomForest::randomForest(x =  TrainingData[,-c(1025:1027)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.BirdNet.,newdata=TestData[,-c(1025:1027)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyBirdNet[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyBirdNet))
sd(unlist(RandomizationAccuracyBirdNet))


BirdNet.noise.output.df <- read.csv('data/CombinedBirdNetNoiseFeaturesV2.csv')

BirdNet.noise.output.df$Individual <- 
  as.factor(BirdNet.noise.output.df$Individual)

# Randomization BirdNet noise
RandomizationAccuracyBirdNetNoise <- list()
for(a in 1:20){
  set.seed(a)  
  print(a)
  TrainingN <- 0.8*nrow(BirdNet.noise.output.df)  
  
  TrainingSamples <-sample(seq(1:nrow(BirdNet.noise.output.df)), TrainingN,replace=FALSE)
  
  TrainingData <- BirdNet.noise.output.df[TrainingSamples,]
  TestData <- BirdNet.noise.output.df[-TrainingSamples,]
  
  
  ml.model.rf.BirdNet.noise <-
    randomForest::randomForest(x =  TrainingData[,-c(1025:1027)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.BirdNet.noise,newdata=TestData[,-c(1025:1027)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyBirdNetNoise[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyBirdNetNoise))
sd(unlist(RandomizationAccuracyBirdNetNoise))

# Randomization VGGish -----------------------------------------------------
VGGishoutput.df <- read.csv('data/VGGishDFMeanSDoriginal.csv')
VGGishoutput.df$Individual <- as.factor(VGGishoutput.df$Individual )

# Randomization VGGish 
RandomizationAccuracyVGGish <- list()
for(a in 1:20){
  set.seed(a)  
  TrainingN <- 0.8*nrow(VGGishoutput.df)  
  
  TrainingSamples <-sample(seq(1:nrow(VGGishoutput.df)), TrainingN,replace=FALSE)
  
  TrainingData <- VGGishoutput.df[TrainingSamples,]
  TestData <- VGGishoutput.df[-TrainingSamples,]
  
  
  ml.model.rf.VGGish. <-
    randomForest::randomForest(x =  TrainingData[,-c(129)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.VGGish.,newdata=TestData[,-c(129)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyVGGish[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyVGGish))
sd(unlist(RandomizationAccuracyVGGish))

VGGish.noise.output.df <- read.csv('data/VGGishDFMeanSD6dB.csv')
VGGish.noise.output.df$Individual <- as.factor(VGGish.noise.output.df$Individual )

# Randomization VGGish noise
RandomizationAccuracyVGGishNoise <- list()
for(a in 1:20){
  set.seed(a)  
  print(a)
  TrainingN <- 0.8*nrow(VGGish.noise.output.df)  
  
  TrainingSamples <-sample(seq(1:nrow(VGGish.noise.output.df)), TrainingN,replace=FALSE)
  
  TrainingData <- VGGish.noise.output.df[TrainingSamples,]
  TestData <- VGGish.noise.output.df[-TrainingSamples,]
  
  
  ml.model.rf.VGGish.noise <-
    randomForest::randomForest(x =  TrainingData[,-c(129)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.VGGish.noise,newdata=TestData[,-c(129)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyVGGishNoise[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyVGGishNoise))
sd(unlist(RandomizationAccuracyVGGishNoise))

# Randomization Acoustic Indices -----------------------------------------------------
AcousticIndicesoutput.df <- read.csv('data/AcousticIndicesDForiginal.csv')
AcousticIndicesoutput.df$Individual <- as.factor(AcousticIndicesoutput.df$Individual)

# Randomization AcousticIndices 
RandomizationAccuracyAcousticIndices <- list()
for(a in 1:20){
  set.seed(a)  
  TrainingN <- 0.8*nrow(AcousticIndicesoutput.df)  
  
  TrainingSamples <-sample(seq(1:nrow(AcousticIndicesoutput.df)), TrainingN,replace=FALSE)
  
  TrainingData <- AcousticIndicesoutput.df[TrainingSamples,]
  TestData <- AcousticIndicesoutput.df[-TrainingSamples,]
  
  
  ml.model.rf.AcousticIndices. <-
    randomForest::randomForest(x =  TrainingData[,-c(6)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.AcousticIndices.,newdata=TestData[,-c(6)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyAcousticIndices[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyAcousticIndices))
sd(unlist(RandomizationAccuracyAcousticIndices))

AcousticIndices.noise.output.df <- read.csv('data/AcousticIndicesNoiseDForiginal.csv')
AcousticIndices.noise.output.df$Individual <- as.factor(AcousticIndices.noise.output.df$Individual)
# Randomization AcousticIndices noise
RandomizationAccuracyAcousticIndicesNoise <- list()
for(a in 1:20){
  set.seed(a)  
  print(a)
  TrainingN <- 0.8*nrow(AcousticIndices.noise.output.df)  
  
  TrainingSamples <-sample(seq(1:nrow(AcousticIndices.noise.output.df)), TrainingN,replace=FALSE)
  
  TrainingData <- AcousticIndices.noise.output.df[TrainingSamples,]
  TestData <- AcousticIndices.noise.output.df[-TrainingSamples,]
  
  
  ml.model.rf.AcousticIndices.noise <-
    randomForest::randomForest(x =  TrainingData[,-c(6)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.AcousticIndices.noise,newdata=TestData[,-c(6)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyAcousticIndicesNoise[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyAcousticIndicesNoise))
sd(unlist(RandomizationAccuracyAcousticIndicesNoise))


# Combine and create boxplot ----------------------------------------------------------

ClassificationAccuracy <- unlist(RandomizationAccuracyMFCC)
Feature <- rep('MFCC',20)
Noise <- rep('No Noise Added',20)
MFCCaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyMFCCNoise)
Feature <- rep('MFCC',20)
Noise <- rep('Noise Added',20)
MFCCNoiseaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyBirdNet)
Feature <- rep('BirdNet',20)
Noise <- rep('No Noise Added',20)
BirdNetaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyBirdNetNoise)
Feature <- rep('BirdNet',20)
Noise <- rep('Noise Added',20)
BirdNetNoiseaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyVGGish)
Feature <- rep('VGGish',20)
Noise <- rep('No Noise Added',20)
VGGishaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyVGGishNoise)
Feature <- rep('VGGish',20)
Noise <- rep('Noise Added',20)
VGGishNoiseaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyAcousticIndices)
Feature <- rep('AcousticIndices',20)
Noise <- rep('No Noise Added',20)
AcousticIndicesaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyAcousticIndicesNoise)
Feature <- rep('AcousticIndices',20)
Noise <- rep('Noise Added',20)
AcousticIndicesNoiseaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)


CombinedFeatures <- rbind.data.frame(MFCCaccuracy,MFCCNoiseaccuracy,BirdNetaccuracy,BirdNetNoiseaccuracy,
                                     VGGishaccuracy,VGGishNoiseaccuracy,AcousticIndicesaccuracy,AcousticIndicesNoiseaccuracy)

CombinedFeatures$Feature <- as.factor(CombinedFeatures$Feature)

CombinedFeatures$Feature <- factor(CombinedFeatures$Feature, 
                                   levels=c("BirdNet","MFCC","VGGish","AcousticIndices"))


ggboxplot(data = CombinedFeatures, x='Feature',y='ClassificationAccuracy',fill='Feature',facet.by = 'Noise' )+
  guides(fill='none')




# MFCC UMAP Plot ----------------------------------------------------------

AcousticSignals.umap <-
  umap::umap(mfcc.output.df[,-c(1,179)],
             #labels=as.factor(MFCC$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 5 )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],mfcc.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


MFCCPercentCorrect <- 
  round(median(unlist(RandomizationAccuracyMFCC)),3)


MFCCScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                 y = "Dim.2",
                                 color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCC',MFCCPercentCorrect*100, '% Correct'))

MFCCScatter


AcousticSignals.umap <-
  umap::umap(mfcc.noise.output.df[,-c(1)],
             #labels=as.factor(mfcc.noise$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 5 )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],mfcc.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


mfcc.noisePercentCorrect <- 
  round(mean(unlist(RandomizationAccuracyMFCCNoise)),3)


mfcc.noiseScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                       y = "Dim.2",
                                       color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCC Noise',mfcc.noisePercentCorrect*100, '% Correct'))

mfcc.noiseScatter



# BirdNet UMAP Plot -------------------------------------------------------
AcousticSignals.umap <-
  umap::umap(
    BirdNetoutput.df[,-c(1025:1027)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3, n_neighbors = 5
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   BirdNetoutput.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

BirdNetPercentCorrecMeant <- 
  round(median(unlist(RandomizationAccuracyBirdNet)),3)

BirdNetScatterMean <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values =  matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNet ',BirdNetPercentCorrecMeant*100, '% Correct'))

BirdNetScatterMean


AcousticSignals.umap <-
  umap::umap(
    BirdNet.noise.output.df[,-c(1025:1027)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3, n_neighbors = 5
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   BirdNet.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

BirdNetNoisePercentCorrecMeant <- 
  round(median(unlist(RandomizationAccuracyBirdNetNoise)),3)

BirdNetNoiseScatterMean <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values =  matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNet Noise',BirdNetNoisePercentCorrecMeant*100, '% Correct'))

BirdNetNoiseScatterMean


# VGGish UMAP Plot --------------------------------------------------------

AcousticSignals.umap <-
  umap::umap(VGGishoutput.df[,-c(129)],
             #labels=as.factor(VGGishDFMeanSD$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 5)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishDFMeanSD$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")

VGGishPercentCorrect <- 
  round( median( unlist(RandomizationAccuracyVGGish)),3)

VGGishScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                   y = "Dim.2",
                                   color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish',VGGishPercentCorrect*100, '% Correct'))

VGGishScatter


plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishDFNoiseMeanSD$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")

VGGishNoisePercentCorrect <- 
 round( median( unlist(RandomizationAccuracyVGGishNoise)),3)


VGGishScatterNoise <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                        y = "Dim.2",
                                        color='Class')+guides(color='none')+
  scale_color_manual(values =  matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish Noise',VGGishNoisePercentCorrect*100, '% Correct'))

VGGishScatterNoise


# Acoustic Indices UMAP ---------------------------------------------------

AcousticSignals.umap <-
  umap::umap(AcousticIndicesoutput.df[,-c(6)],
             controlscale=TRUE,scale=3, n_neighbors = 5)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],AcousticIndicesoutput.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesPercentCorrect <- 
  round( median(unlist(RandomizationAccuracyAcousticIndices)),3)


AcousticIndicesScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values =matlab::jet.colors(length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('Acoustic Indices',AcousticIndicesPercentCorrect*100, '% Correct'))

AcousticIndicesScatter


## Noise
AcousticSignals.umap <-
  umap::umap(AcousticIndices.noise.output.df[,-c(6)],
             controlscale=TRUE,scale=3, n_neighbors = 5)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],AcousticIndices.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesNoisePercentCorrect <- 
  round( median(unlist(RandomizationAccuracyAcousticIndicesNoise)),3)


AcousticIndicesNoiseScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('Acoustic Indices',AcousticIndicesNoisePercentCorrect*100, '% Correct'))

AcousticIndicesNoiseScatter


# Combine all plots together ----------------------------------------------

cowplot::plot_grid( BirdNetScatterMean,
                    MFCCScatter,
                   VGGishScatter,
                   AcousticIndicesScatter,
                   nrow = 2)


cowplot::plot_grid(BirdNetNoiseScatterMean,
                   mfcc.noiseScatter,
                   VGGishScatterNoise,
                   AcousticIndicesNoiseScatter,
                   nrow = 2)

