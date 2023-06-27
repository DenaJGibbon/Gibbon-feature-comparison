library(caret)
library(ggpubr)
library(stringr)

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


# Randomization BirdNET -----------------------------------------------------
BirdNetoutput.df <- read.csv('data/CombinedBirdNetFeaturesV2.csv')
BirdNetoutput.df$Individual <- as.factor(BirdNetoutput.df$Individual)

# Randomization BirdNet 
RandomizationAccuracyBirdNet <- list()
for(a in c(1:8,10:21)){
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
Feature <- rep('BirdNET',20)
Noise <- rep('No Noise Added',20)
BirdNetaccuracy <- cbind.data.frame(ClassificationAccuracy,Feature,Noise)

ClassificationAccuracy <- unlist(RandomizationAccuracyBirdNetNoise)
Feature <- rep('BirdNET',20)
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


CombinedFeatures <- rbind.data.frame(MFCCaccuracy,BirdNetaccuracy,
                                     VGGishaccuracy,AcousticIndicesaccuracy)

CombinedFeatures$Feature <- as.factor(CombinedFeatures$Feature)

CombinedFeatures$Feature <- factor(CombinedFeatures$Feature, 
                                   levels=c("MFCC","BirdNET","VGGish","AcousticIndices"))

BoxplotOriginal <- ggboxplot(data = CombinedFeatures, x='Feature',y='ClassificationAccuracy',fill='Feature',facet.by = 'Noise' )+
  guides(fill='none')+ ylab('Classification Accuracy')+ylim(0,1)

CombinedFeaturesNoise <- rbind.data.frame(MFCCNoiseaccuracy,BirdNetNoiseaccuracy,
                                       VGGishNoiseaccuracy,AcousticIndicesNoiseaccuracy)

CombinedFeaturesNoise$Feature <- as.factor(CombinedFeaturesNoise$Feature)

CombinedFeaturesNoise$Feature <- factor(CombinedFeaturesNoise$Feature, 
                                   levels=c("MFCC","BirdNET","VGGish","AcousticIndices"))

BoxplotNoise <- ggboxplot(data = CombinedFeaturesNoise, x='Feature',y='ClassificationAccuracy',fill='Feature',facet.by = 'Noise' )+
  guides(fill='none')+ ylab('Classification Accuracy')+ylim(0,1)
  

cowplot::plot_grid(BoxplotOriginal,BoxplotNoise,labels = c('A)','B)'), label_x = 0
                   )


# MFCC UMAP Plot ----------------------------------------------------------

AcousticSignals.umap <-
  umap::umap(mfcc.output.df[,-c(1,179)],
             #labels=as.factor(MFCC$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 25 )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],mfcc.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


MFCCPercentCorrect <- 
  round(mean(unlist(RandomizationAccuracyMFCC)),3)


MFCCScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                 y = "Dim.2",
                                 color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCC',MFCCPercentCorrect*100, '% Correct'))

MFCCScatter


AcousticSignals.umap <-
  umap::umap(mfcc.noise.output.df[,-c(1)],
             #labels=as.factor(mfcc.noise$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 25 )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],mfcc.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


mfcc.noisePercentCorrect <- 
  round(mean(unlist(RandomizationAccuracyMFCCNoise)),3)


mfcc.noiseScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                       y = "Dim.2",
                                       color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCC Noise',mfcc.noisePercentCorrect*100, '% Correct'))

mfcc.noiseScatter



# BirdNet UMAP Plot -------------------------------------------------------
AcousticSignals.umap <-
  umap::umap(
    BirdNetoutput.df[,-c(1025:1027)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3, n_neighbors = 25
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   BirdNetoutput.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

BirdNetPercentCorrecMeant <- 
  round(mean(unlist(RandomizationAccuracyBirdNet)),3)

BirdNetScatterMean <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values =  viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNET',BirdNetPercentCorrecMeant*100, '% Correct'))

BirdNetScatterMean


AcousticSignals.umap <-
  umap::umap(
    BirdNet.noise.output.df[,-c(1025:1027)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3, n_neighbors = 25
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   BirdNet.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

BirdNetNoisePercentCorrecMeant <- 
  round(mean(unlist(RandomizationAccuracyBirdNetNoise)),3)

BirdNetNoiseScatterMean <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color  = "class"
) +
  scale_color_manual(values =  viridis::viridis (length(
    unique(plot.for.AcousticSignals$class)
  ))) + guides(color="none")+ggtitle( paste('BirdNET Noise',BirdNetNoisePercentCorrecMeant*100, '% Correct'))

BirdNetNoiseScatterMean


# VGGish UMAP Plot --------------------------------------------------------

AcousticSignals.umap <-
  umap::umap(VGGishoutput.df[,-c(129)],
             #labels=as.factor(VGGishDFMeanSD$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishoutput.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")

VGGishPercentCorrect <- 
  round( mean( unlist(RandomizationAccuracyVGGish)),3)

VGGishScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                   y = "Dim.2",
                                   color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish',VGGishPercentCorrect*100, '% Correct'))

VGGishScatter


AcousticSignals.umap <-
  umap::umap(VGGish.noise.output.df[,-c(129)],
             #labels=as.factor(VGGishDFMeanSD$Validation),
             controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGish.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")

VGGishNoisePercentCorrect <- 
 round( mean( unlist(RandomizationAccuracyVGGishNoise)),3)


VGGishScatterNoise <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                        y = "Dim.2",
                                        color='Class')+guides(color='none')+
  scale_color_manual(values =  viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish Noise',VGGishNoisePercentCorrect*100, '% Correct'))

VGGishScatterNoise


# Acoustic Indices UMAP ---------------------------------------------------

AcousticSignals.umap <-
  umap::umap(AcousticIndicesoutput.df[,-c(6)],
             controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],AcousticIndicesoutput.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesPercentCorrect <- 
  round( mean(unlist(RandomizationAccuracyAcousticIndices)),3)


AcousticIndicesScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values =viridis::viridis(length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('Acoustic Indices',AcousticIndicesPercentCorrect*100, '% Correct'))

AcousticIndicesScatter


## Noise
AcousticSignals.umap <-
  umap::umap(AcousticIndices.noise.output.df[,-c(6)],
             controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],AcousticIndices.noise.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


AcousticIndicesNoisePercentCorrect <- 
  round( mean(unlist(RandomizationAccuracyAcousticIndicesNoise)),3)


AcousticIndicesNoiseScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('Acoustic Indices',AcousticIndicesNoisePercentCorrect*100, '% Correct'))

AcousticIndicesNoiseScatter


# Combine all plots together ----------------------------------------------

cowplot::plot_grid(MFCCScatter,
                   BirdNetScatterMean,
                   VGGishScatter,
                   AcousticIndicesScatter,
                   nrow = 2,
                   labels = c('A)','B)','C)','D)'),
                   label_x = 0)


cowplot::plot_grid(mfcc.noiseScatter,
                   BirdNetNoiseScatterMean,
                   VGGishScatterNoise,
                   AcousticIndicesNoiseScatter,
                   nrow = 2,
                   labels = c('A)','B)','C)','D)'),
                   label_x = 0)


# Site-level plots --------------------------------------------------------
AcousticSignalsMFCC.umap <-
  umap::umap( mfcc.noise.output.df[,-c(1,179)],
              controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignalsMFCC <-
  cbind.data.frame(AcousticSignalsMFCC.umap$layout[,1:2],mfcc.noise.output.df$Individual)

colnames(plot.for.AcousticSignalsMFCC) <-
  c("Dim.1", "Dim.2","Class")

plot.for.AcousticSignalsMFCC$Site <- str_split_fixed(plot.for.AcousticSignalsMFCC$Class,pattern = '_',n=2)[,1]
plot.for.AcousticSignalsMFCC$Site  <- substr(plot.for.AcousticSignalsMFCC$Site, start=1,stop=2)
plot.for.AcousticSignalsMFCC$Site  <- recode_factor(plot.for.AcousticSignalsMFCC$Site, VJ = "SA")

plot.for.AcousticSignalsMFCC$Site  <- factor(plot.for.AcousticSignalsMFCC$Site, levels=c( "CR", "DK", "DV", "IC", "KB", "MB","SA"))

MFCCSite <-ggpubr::ggscatter(data = plot.for.AcousticSignalsMFCC,x = "Dim.1",
                  y = "Dim.2",
                  color='Site',alpha=0.25)+
  scale_color_manual(values = viridis::viridis (length(unique(plot.for.AcousticSignalsMFCC$Site)))) +ggtitle( ('MFCC by Site'))


AcousticSignalsBirdNET.umap <-
  umap::umap( BirdNet.noise.output.df[,-c(1025:1027)],
             controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignalsBirdNET <-
  cbind.data.frame(AcousticSignalsBirdNET.umap$layout[,1:2],BirdNet.noise.output.df$Individual)

colnames(plot.for.AcousticSignalsBirdNET) <-
  c("Dim.1", "Dim.2","Class")

plot.for.AcousticSignalsBirdNET$Site <- str_split_fixed(plot.for.AcousticSignalsBirdNET$Class,pattern = '_',n=2)[,1]
plot.for.AcousticSignalsBirdNET$Site  <- substr(plot.for.AcousticSignalsBirdNET$Site, start=1,stop=2)
plot.for.AcousticSignalsBirdNET$Site  <- recode_factor(plot.for.AcousticSignalsBirdNET$Site, VJ = "SA")

plot.for.AcousticSignalsBirdNET$Site  <- factor(plot.for.AcousticSignalsBirdNET$Site, levels=c( "CR", "DK", "DV", "IC", "KB", "MB","SA"))

BirdNETsite <- ggpubr::ggscatter(data = plot.for.AcousticSignalsBirdNET,x = "Dim.1",
                  y = "Dim.2",
                  color='Site',alpha=0.25)+
  scale_color_manual(values = viridis::viridis (length(unique(plot.for.AcousticSignalsBirdNET$Site)))) +ggtitle( ('BirdNET by Site'))


AcousticSignalsVGGish.umap <-
  umap::umap( VGGish.noise.output.df[,-c(129)],
              controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignalsVGGish <-
  cbind.data.frame(AcousticSignalsVGGish.umap$layout[,1:2],VGGish.noise.output.df$Individual)

colnames(plot.for.AcousticSignalsVGGish) <-
  c("Dim.1", "Dim.2","Class")

plot.for.AcousticSignalsVGGish$Site <- str_split_fixed(plot.for.AcousticSignalsVGGish$Class,pattern = '_',n=2)[,1]
plot.for.AcousticSignalsVGGish$Site  <- substr(plot.for.AcousticSignalsVGGish$Site, start=1,stop=2)
plot.for.AcousticSignalsVGGish$Site  <- recode_factor(plot.for.AcousticSignalsVGGish$Site, VJ = "SA")

plot.for.AcousticSignalsVGGish$Site  <- factor(plot.for.AcousticSignalsVGGish$Site, levels=c( "CR", "DK", "DV", "IC", "KB", "MB","SA"))

VGGishsite <- ggpubr::ggscatter(data = plot.for.AcousticSignalsVGGish,x = "Dim.1",
                  y = "Dim.2",
                  color='Site',alpha=0.25)+
  scale_color_manual(values = viridis::viridis (length(unique(plot.for.AcousticSignalsVGGish$Site)))) +ggtitle( ('VGGish by Site'))


AcousticSignalsIndices.umap <-
  umap::umap(AcousticIndicesoutput.df[,-c(6)],
             controlscale=TRUE,scale=3, n_neighbors = 25)

plot.for.AcousticSignalsIndices <-
  cbind.data.frame(AcousticSignalsIndices.umap$layout[,1:2],AcousticIndices.noise.output.df$Individual)

colnames(plot.for.AcousticSignalsIndices) <-
  c("Dim.1", "Dim.2","Class")

plot.for.AcousticSignalsIndices$Site <- str_split_fixed(plot.for.AcousticSignalsIndices$Class,pattern = '_',n=2)[,1]
plot.for.AcousticSignalsIndices$Site  <- substr(plot.for.AcousticSignalsIndices$Site, start=1,stop=2)
plot.for.AcousticSignalsIndices$Site  <- recode_factor(plot.for.AcousticSignalsIndices$Site, VJ = "SA")

plot.for.AcousticSignalsIndices$Site  <- factor(plot.for.AcousticSignalsIndices$Site, levels=c( "CR", "DK", "DV", "IC", "KB", "MB","SA"))

IndiceSite <- ggpubr::ggscatter(data = plot.for.AcousticSignalsIndices,x = "Dim.1",
                  y = "Dim.2",
                  color='Site',alpha=0.25)+
  scale_color_manual(values = viridis::viridis (length(unique(plot.for.AcousticSignalsIndices$Site)))) +ggtitle( ('Acoustic IndicesNoise by Site'))


cowplot::plot_grid(MFCCSite,
                   BirdNETsite,
                   VGGishsite,
                   IndiceSite,
                   nrow = 2,
                   labels = c('A)','B)','C)','D)'),
                   label_x = 0)
