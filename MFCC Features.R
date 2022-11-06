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

ml.model.rf.mfcc <-
  randomForest::randomForest(x =  mfcc.output.df[,-c(1)], y =  mfcc.output.df$Individual)

1-min(ml.model.rf.mfcc$err.rate[,1])


AcousticSignals.umap <-
  umap::umap(mfcc.output.df[,-c(1)],
             #labels=as.factor(MFCC$Validation),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],mfcc.output.df$Individual)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2","Class")


MFCCPercentCorrect <- 
  round(1-min(ml.model.rf.mfcc$err.rate[,1]),3)


MFCCScatter <- ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                                            y = "Dim.2",
                                            color='Class')+guides(color='none')+
  scale_color_manual(values = viridis::viridis (length(
    unique(plot.for.AcousticSignals$Class)
  ))) + guides(color="none")+ggtitle( paste('MFCC',MFCCPercentCorrect*100, '% Correct'))

MFCCScatter


write.csv(mfcc.output.df,'data/mfcc.output.df.csv',row.names = F)
