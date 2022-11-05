library(plyr)
library(stringr)
library(ggpubr)
library(apcluster)
library(tuneR)

Tempfiles <-
  list.files("/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/vggish_embeddings_original",full.names = T,
             recursive = T)

VGGishDF <- data.frame()

for(i in 1:length(Tempfiles)){
  print(i)
  Tempcsv <- read.delim(Tempfiles[i])

  n.slash  <- str_count(Tempfiles[i], pattern = "/")[1] + 1

  Temp.name <- str_split_fixed(Tempfiles[i],pattern = "/",n=n.slash)[,n.slash]
  Class <- str_split_fixed(Temp.name,pattern = '.csv',n=2)[,1]
  
  Individual <- paste(str_split_fixed(Class,
                                    pattern = '_',n=3)[,1],
                                                    str_split_fixed(Class,
                                                                    pattern = '_',n=3)[,2],sep='_')
  
  colnames(Tempcsv) <- paste('V',seq(1,128,1),sep='')
  
  
  RowSeq <- c(seq(1,nrow(Tempcsv),2),nrow(Tempcsv))
  
  New3secDF <- data.frame()
  for(a in 1: (length(RowSeq)-1) ){
    TempRows <- Tempcsv[RowSeq[a]:RowSeq[a+1],]
   
     TempRows <- 
      TempRows %>% mutate_if(is.character,as.numeric)
    
    NewRow <- colMeans(TempRows)
  
    New3secDF <- rbind.data.frame(New3secDF,NewRow)
  }
  
  colnames(New3secDF) <- paste('V',seq(1,128,1),sep='')
  
  New3secDF <- cbind.data.frame(Class,New3secDF)
  VGGishDF <- rbind.data.frame(VGGishDF,Temp.rw.class )
}


VGGishDF$Individual <- paste(str_split_fixed(VGGishDF$Class,
                                    pattern = '_',n=3)[,1],
                    str_split_fixed(VGGishDF$Class,
                                    pattern = '_',n=3)[,2],sep='_')


AcousticSignals.umap <-
  umap::umap(VGGishDF[,-c(1,130)],
             #labels=as.factor(VGGishDF$Validation),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],VGGishDF$Class)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                  y = "Dim.2",
                  color='Class')+guides(color='none')



# Ml comparison

svm.model.vggish <-
  e1071::svm(
    VGGishDF[,-c(1,2,3,259)],
    as.factor(VGGishDF$Class),
    kernel = "radial",
    type = "C-classification",
    cross = 5,
    probability = TRUE
  )

svm.model.vggish$tot.accuracy

subset.directory <-
  '/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/FemaleGibbonsSwiftHQ/'

source('R/MFCCFunction.R')
ValidationDF <- read.csv('gibbon.validation.dfHQfullstructure.csv')
ValidationDF <- droplevels(subset(ValidationDF,target.signal=='y'))

for(a in 1:nrow(ValidationDF)){
  TempRow <- ValidationDF[a,]
  TempWave <- readWave(TempRow$full.name)
  WaveName <- str_replace(TempRow$full.name,'FemaleGibbonsSwift','FemaleGibbonsSwiftHQ')
  writeWave(TempWave,filename = WaveName)
}


trainingdata <- MFCCFunctionMeanSD(input.dir= subset.directory, min.freq = 400, max.freq = 1600)

traningdatanames <- list.files(subset.directory,
                               full.names = F)

trainingdata$Class <- str_split_fixed(traningdatanames,pattern = '.wav',n=2)[,1]


trainingdata$class <- as.factor(trainingdata$class)

# svm.model.mfcc <-
#   e1071::svm(
#     trainingdata[,-c(1)],
#     as.factor(trainingdata$class),
#     kernel = "radial",
#     type = "C-classification",
#     cross = 25,
#     probability = TRUE
#   )
#
# svm.model.mfcc$tot.accuracy

AcousticSignalsMFCC.umap <-
  umap::umap(trainingdata[,-c(1,51)],
             #labels=as.factor(trainingdata$Class),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignalsMFCC <-
  cbind.data.frame(AcousticSignalsMFCC.umap$layout[,1:2],
                   trainingdata$class)

colnames(plot.for.AcousticSignalsMFCC) <-
  c("Dim.1", "Dim.2", "Class")

plot.for.AcousticSignalsMFCC$Class <- as.factor(plot.for.AcousticSignalsMFCC$Class)


ggpubr::ggscatter(data = plot.for.AcousticSignalsMFCC,x = "Dim.1",
                  y = "Dim.2",
                  color  = "Class", shape='Class')



# Unsupervised clustering -------------------------------------------------

AcousticSignalsAP <-
  apcluster::apcluster(negDistMat(r=2),q= 0.1,
                       trainingdata[,-c(1,51)],
                       maxits=100000,convits=10000)



AcousticSignals.umap <-
  umap::umap(trainingdata[,-c(1,51)],
             labels=as.factor( as.numeric(AcousticSignalsAP@idx)),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],#VGGishDF$PercentClass,
                   as.factor( as.numeric(AcousticSignalsAP@idx)))

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Cluster")

plot.for.AcousticSignals$Cluster <- as.factor(plot.for.AcousticSignals$Cluster)

ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                  y = "Dim.2",
                  color='Cluster')

table(plot.for.AcousticSignals$Validation,plot.for.AcousticSignals$Cluster)



source('R/AffinityBiplotAddSpectrograms.R')
set.seed(13)
AffinityBiplotAddSpectrograms(input.dir.Focal='data/FemaleGibbonsSwiftHQ',
                              output.dir.Focal = 'data/FemaleGibbonsSwiftHQ/Thumbnails/',
                              min.freq = 400, max.freq = 1500,main='Female Gibbon Calls',class='fixed')

source('R/UMAPBiplotAddSpectrograms.R')

UMAPBiplotAddSpectrograms(input.dir.Focal='data/FemaleGibbonsSwiftHQ',
                              output.dir.Focal = 'data/FemaleGibbonsSwiftHQ/Thumbnails/',
                              min.freq = 400, max.freq = 1500,main='Female Gibbon Calls',class='fixed')


source('R/AffinityBiplotAddSpectrograms.R')
source('R/UMAPBiplotAddSpectrograms.R')
source('R/MFCCFunctionMeanSD.R')
set.seed(13)
AffinityBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives',
                              output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/Thumbnails/',
                              min.freq = 400, max.freq = 1500,main='gibbonR detections',class='fixed')


UMAPBiplotAddSpectrograms(input.dir.Focal='/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives',
                              output.dir.Focal = '/Users/denaclink/Desktop/RStudio Projects/gibbonID/TrueFalsePositives/ThumbnailsUMAP/',
                              min.freq = 400, max.freq = 1500,main='gibbonR detections')

