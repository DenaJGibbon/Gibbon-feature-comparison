library(plyr)
library(stringr)
library(ggpubr)
library(apcluster)
library(tuneR)

TempfilesVGGishVGGish<-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/VGGish",full.names = T,
             recursive = T)

# VGGish Mean SD ----------------------------------------------------------
VGGishDFMeanSD <- data.frame()
for(i in 1401:length(TempfilesVGGish)){
  print(i)
  Tempcsv <- read.table(TempfilesVGGish[i], sep = ',')
  Duration <- nrow(Tempcsv)
  n.slash  <- str_count(TempfilesVGGish[i], pattern = "/")[1] + 1
  
  Temp.name <- str_split_fixed(TempfilesVGGish[i],pattern = "/",n=n.slash)[,n.slash]
  Class <- str_split_fixed(Temp.name,pattern = '.snr',n=2)[,1]
  
  Individual <- paste(str_split_fixed(Class,
                                      pattern = '_',n=6)[,4],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,5],sep='_')
  
  RecorderID <- paste(str_split_fixed(Class,
                                      pattern = '_',n=6)[,1],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,2],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,3],sep='_')
  
  TempcsvMean <- t(colMeans(Tempcsv))
  TempcsvSD <- t(apply(Tempcsv,2,sd))
  
  TempRow <- cbind.data.frame(TempcsvMean,TempcsvSD)
  

  colnames(TempRow) <- paste('V',seq(1,ncol(TempRow),1),sep='')
  
  TempRow$Individual <- Individual
  TempRow$Duration <- Duration
  TempRow$RecorderID <- RecorderID
  VGGishDFMeanSD <- rbind.data.frame(VGGishDFMeanSD,TempRow )
  write.csv(VGGishDFMeanSD,'data/VGGishDFMeanSDPlayback.csv',row.names = F)
}

VGGishDFMeanSD$Individual <- as.factor( VGGishDFMeanSD$Individual)

# Randomization
RandomizationAccuracyVGGishoriginal <- list()
for(a in 1:20){
  set.seed(a)  
  TrainingN <- 0.8*nrow(VGGishDFMeanSD)  
  
  TrainingSamples <-sample(seq(1:nrow(VGGishDFMeanSD)), TrainingN,replace=FALSE)
  
  TrainingData <- droplevels(VGGishDFMeanSD[TrainingSamples,])
  TestData <- droplevels(VGGishDFMeanSD[-TrainingSamples,])
  
  
  ml.model.rf.VGGish.original <-
    randomForest::randomForest(x =  TrainingData[,-c(257:259)], y =  TrainingData$Individual,
                               ntree = 500, random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.VGGish.original,newdata=TestData[,-c(257:259)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  print(AccuracyVal)
  RandomizationAccuracyVGGishoriginal[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyVGGishoriginal))
sd(unlist(RandomizationAccuracyVGGishoriginal))


AcousticSignals.umap <-
  umap::umap(VGGishDFMeanSD[,-c(257:259)],
             #labels=as.factor(VGGish$Validation),
             controlscale=TRUE,scale=3,n_neighbors=10)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishDFMeanSD$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


VGGishPercentCorrect <- 
  round(median(unlist(RandomizationAccuracyVGGishoriginal)),3)


VGGishScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                 y = "Dim.2",
                                 color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('VGGish',VGGishPercentCorrect*100, '% Correct'))

VGGishScatter



