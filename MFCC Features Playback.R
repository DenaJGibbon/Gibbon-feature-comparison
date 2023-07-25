library(gibbonR)
library(caret)

# Set input directory
input.dir <- "/Volumes/DJC 1TB/VocalIndividualityClips/SoundFilesDownsample"

MFCCFiles <-
  list.files(input.dir, full.names = T, pattern = '.wav')

MFCCFilesShort <-
  list.files(input.dir, full.names = F, pattern = '.wav')

CallID <- str_split_fixed(MFCCFilesShort,'_',n=4)[,4]
CallID <- str_split_fixed(CallID,'_snr',n=2)[,1]

IndividualID <- paste(str_split_fixed(CallID,'_',n=3)[,1],
                      str_split_fixed(CallID,'_',n=3)[,2],sep='_')

CallID <- str_split_fixed(MFCCFilesShort,'.snr',n=2)[,1]
RecordingID <- paste(str_split_fixed(CallID,'_',n=4)[,1],
                     str_split_fixed(CallID,'_',n=4)[,2],
                     str_split_fixed(CallID,'_',n=4)[,3],
                     sep='_')
n.windows <- 9
num.cep <- 12

mfcc.vector.df <- data.frame()
#for (a in 1:length(MFCCFiles)) {
  for (a in 1:1160) {
  print(a)
  short.wav <- readWave(MFCCFiles[a])
  wav.dur <- duration(short.wav)
  win.time <- wav.dur / n.windows
  
  # Calculate MFCCs
  melfcc.output <-
    tuneR::melfcc(
      short.wav,
      minfreq = 400,
      hoptime = win.time,
      maxfreq = 1600,
      numcep = num.cep,
      wintime = win.time
    )
  
  MFCCMean <-  t(apply(melfcc.output,2,mean))
  MFCCSD <- t(apply(melfcc.output,2,sd))
  # Calculate delta cepstral coefficients
  deltas.output <- tuneR::deltas(deltas.output)
  DeltaMean <-  t(apply(deltas.output,2,mean))
  DeltaSD <-  t(apply(deltas.output,2,sd))
  Individual <- IndividualID[a]
  Recording <- RecordingID[a]
  # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
  TempMFCCRow <- cbind.data.frame(MFCCMean,MFCCSD,wav.dur,Individual,Recording)
  mfcc.vector.df <- rbind.data.frame(mfcc.vector.df,TempMFCCRow)
  write.csv(mfcc.vector.df,'data/MFCCPlaybacks.csv',row.names = F)
}


mfcc.vector.df $Individual <- as.factor( mfcc.vector.df $Individual)

# Randomization
RandomizationAccuracyMFCCoriginal <- list()
for(a in 1:20){
  set.seed(a)  
  TrainingN <- 0.8*nrow(mfcc.vector.df )  
  
  TrainingSamples <-sample(seq(1:nrow(mfcc.vector.df )), TrainingN,replace=FALSE)
  
  TrainingData <- droplevels(mfcc.vector.df [TrainingSamples,])
  TestData <- droplevels(mfcc.vector.df [-TrainingSamples,])
  
  
  ml.model.rf.mfcc.original <-
    randomForest::randomForest(x =  TrainingData[,-c(26)], y =  TrainingData$Individual,
                               ntree = 500, random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.mfcc.original,newdata=TestData[,-c(49,50)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyMFCCoriginal[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyMFCCoriginal))
sd(unlist(RandomizationAccuracyMFCCoriginal))


AcousticSignals.umap <-
  umap::umap(mfcc.vector.df [,-c(26)],
             #labels=as.factor(MFCC$Validation),
             controlscale=TRUE,scale=3,n_neighbors=10)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],mfcc.vector.df $Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


MFCCPercentCorrect <- 
  round(median(unlist(RandomizationAccuracyMFCCoriginal)),3)


MFCCScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values = matlab::jet.colors (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCC',MFCCPercentCorrect*100, '% Correct'))

MFCCScatter

#write.csv(mfcc.vector.df ,'data/mfcc.vector.df .csv',row.names = F)

# Perform dimensionality reduction using UMAP on the MFCC data
AcousticSignals.umap <- umap::umap(MFCCPlaybacks[, -c(26:28)], controlscale = TRUE, scale = 3, n_neighbors = 10)

# Combine UMAP coordinates and individual labels
plot.for.AcousticSignals <- cbind.data.frame(AcousticSignals.umap$layout[, 1:2], MFCCPlaybacks$Individual)
colnames(plot.for.AcousticSignals) <- c("Dim.1", "Dim.2", "Class")

# Compute median percent correct from randomization
MFCCPercentCorrect <- round(median(unlist(RandomizationAccuracyMFCC$AccuracyVal)), 3)

# Generate scatter plot using ggpubr
png(filename = paste('data/randomization/MFCC', UniqueRecorderMFCC[a], '.png'))
MFCCScatter <- ggpubr::ggscatter(
  data = plot.for.AcousticSignals,
  x = "Dim.1",
  y = "Dim.2",
  color = 'Class'
) + guides(color = 'none') +
  scale_color_manual(
    values = matlab::jet.colors(length(unique(plot.for.AcousticSignals$Class)))
  ) +
  guides(color = "none") +
  ggtitle(paste('MFCC', UniqueRecorderMFCC[a], MFCCPercentCorrect * 100, '% Correct'))

# Save and display the scatter plot
MFCCScatter
graphics.off()

