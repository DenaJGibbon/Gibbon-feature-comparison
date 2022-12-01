
# Randomization MFCCs -----------------------------------------------------
mfccoutput.df <- read.csv('data/mfcc.output.df.csv')

# Randomization MFCC 
RandomizationAccuracyMFCC <- list()
for(a in 1:20){
  set.seed(a)  
  TrainingN <- 0.8*nrow(mfccoutput.df)  
  
  TrainingSamples <-sample(seq(1:nrow(mfccoutput.df)), TrainingN,replace=FALSE)
  
  TrainingData <- mfccoutput.df[TrainingSamples,]
  TestData <- mfccoutput.df[-TrainingSamples,]
  
  
  ml.model.rf.mfcc. <-
    randomForest::randomForest(x =  TrainingData[,-c(1)], y =  TrainingData$Individual,
                               random_state = 0,keep.forest=TRUE)
  
  TestPredictions <- predict(ml.model.rf.mfcc.,newdata=TestData[,-c(1)])
  
  ConfMatrix <-confusionMatrix(TestData$Individual, TestPredictions)
  AccuracyVal <- ConfMatrix$overall[1]
  RandomizationAccuracyMFCC[[a]] <- AccuracyVal
} 

mean(unlist(RandomizationAccuracyMFCC))
sd(unlist(RandomizationAccuracyMFCC))

mfcc.noise.output.df <- read.csv('data/mfcc.noise6db.output.df.csv')

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
