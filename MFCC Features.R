library(gibbonR)

# Set input directory
input.dir <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles"

call.timing.list <-
  list.files(input.dir, full.names = T, pattern = '.wav')

call.timing.list.short <-
  list.files(input.dir, full.names = F, pattern = '.wav')

n.windows <- 9
num.cep <- 12

mfcc.vector.list <- list()
for (a in 1:length(call.timing.list)) {
  print(a)
  short.wav <- readWave(call.timing.list[a])
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
  
  # Calculate delta cepstral coefficients
  deltas.output <- tuneR::deltas(melfcc.output)
  
  # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
  mfcc.vector <-
    c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)
  mfcc.vector.list[[a]] <- mfcc.vector
}

mfcc.output <- mfcc.vector.list

Individual <- paste(str_split_fixed(call.timing.list.short,
                                    pattern = '_',n=3)[,1],
                    str_split_fixed(call.timing.list.short,
                                    pattern = '_',n=3)[,2],sep='_')


mfcc.output.df <- do.call(rbind.data.frame, mfcc.output)



colnames(mfcc.output.df) <-
  seq(from = 1,
      to = ncol(mfcc.output.df),
      by = 1)

mfcc.output.df <- cbind.data.frame(Individual, mfcc.output.df)

mfcc.output.df <- 
  subset(mfcc.output.df, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

mfcc.output.df <- droplevels(mfcc.output.df)


mfcc.output.df$Individual <- as.factor( mfcc.output.df$Individual)

# Randomization
RandomizationAccuracyMFCCoriginal <- list()
for(a in 1:20){
  set.seed(a)  
  TrainingN <- 0.8*nrow(mfcc.output.df)  
  
  TrainingSamples <-sample(seq(1:nrow(mfcc.output.df)), TrainingN,replace=FALSE)
  
  TrainingData <- mfcc.output.df[TrainingSamples,]
  TestData <- mfcc.output.df[-TrainingSamples,]
  
  
  ml.model.rf.mfcc.original <-
    randomForest::randomForest(x =  TrainingData[,-c(1)], y =  TrainingData$Individual,
                               ntree = 500, random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.mfcc.original,newdata=TestData[,-c(1)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyMFCCoriginal[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyMFCCoriginal))
sd(unlist(RandomizationAccuracyMFCCoriginal))


AcousticSignals.umap <-
  umap::umap(mfcc.output.df[,-c(1)],
             #labels=as.factor(MFCC$Validation),
             controlscale=TRUE,scale=3)

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

#write.csv(mfcc.output.df,'data/mfcc.output.df.csv',row.names = F)


# Noise -------------------------------------------------------------------

library(gibbonR)

# Set input directory
input.dir <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/output6db"

call.timing.list <-
  list.files(input.dir, full.names = T, pattern = '.wav')

call.timing.list.short <-
  list.files(input.dir, full.names = F, pattern = '.wav')

n.windows <- 9
num.cep <- 12

mfcc.noise.vector.list <- list()
for (a in 1:length(call.timing.list)) {
  print(a)
  short.wav <- readWave(call.timing.list[a])
  wav.dur <- duration(short.wav)
  win.time <- wav.dur / n.windows
  
  # Calculate mfcc.noises
  melfcc.output <-
    tuneR::melfcc(
      short.wav,
      minfreq = 400,
      hoptime = win.time,
      maxfreq = 1600,
      numcep = num.cep,
      wintime = win.time
    )
  
  # Calculate delta cepstral coefficients
  deltas.output <- tuneR::deltas(melfcc.output)
  
  # Ensure only same number of time windows are used for mfcc.noise and delta coefficients Also append .wav duration
  mfcc.noise.vector <-
    c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)
  mfcc.noise.vector.list[[a]] <- mfcc.noise.vector
}

mfcc.noise.output <- mfcc.noise.vector.list

Individual <- paste(str_split_fixed(call.timing.list.short,
                                    pattern = '_',n=3)[,1],
                    str_split_fixed(call.timing.list.short,
                                    pattern = '_',n=3)[,2],sep='_')


mfcc.noise.output.df <- do.call(rbind.data.frame, mfcc.noise.output)



colnames(mfcc.noise.output.df) <-
  seq(from = 1,
      to = ncol(mfcc.noise.output.df),
      by = 1)

mfcc.noise.output.df <- cbind.data.frame(Individual, mfcc.noise.output.df)

mfcc.noise.output.df <- 
  subset(mfcc.noise.output.df, ! Individual %in% c('DK_04','DV_04','DV_11','KB_01','MB_08','SAFA_01','SAFA_03','SAFA_09'))

mfcc.noise.output.df <- droplevels(mfcc.noise.output.df)


mfcc.noise.output.df$Individual <- as.factor( mfcc.noise.output.df$Individual)


# Randomization
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




AcousticSignals.umap <-
  umap::umap(mfcc.noise.output.df[,-c(1)],
             #labels=as.factor(mfcc.noise$Validation),
             controlscale=TRUE,scale=3)

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


#write.csv(mfcc.noise.output.df,'data/mfcc.noise6db.output.df.csv',row.names = F)



