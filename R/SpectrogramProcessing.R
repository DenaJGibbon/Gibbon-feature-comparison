# Load required libraries
library(seewave)
library(tuneR)


AllFiles <- list.files('/Volumes/DJC 1TB/VocalIndividualityClips/SoundFiles',full.names = T)
AllFilesShort <- list.files('/Volumes/DJC 1TB/VocalIndividualityClips/SoundFiles',full.names = F)
Times <- substr(str_split_fixed(AllFilesShort,pattern = '_',n=5)[,3],start=1,stop=4)
TempName <- str_split_fixed(AllFilesShort,pattern = '_',n=4)[,4]
TempName <- str_split_fixed(TempName,pattern = '.snr',n=2)[,1]
Dates <- str_split_fixed(AllFilesShort,pattern = '_',n=4)[,2]

SpecPaths <- AllFiles[which(TempName=='MB_01_564.01' & Times== "0600" & Dates =="20190824")]

RecorderNames <- c('M1 (0 m)','M2 (50 m)','M3 (100 m)','M4 (150 m)',
  'M5 (200 m)','M6 (250 m)','M7 (300 m)')

png('spectrograms1.png',width = 1600, height = 1800, res= 200,pointsize = 14)
par(mfrow=c(4,2))
for(a in 1:length(SpecPaths)){
  TempWav <-  readWave(SpecPaths[a])
  
  phonTools::spectrogram(TempWav@left, fs=TempWav@samp.rate,maxfreq = 2000,
                         windowlength = 15 )
  
  # Add a label as the plot title
  plot_title <- RecorderNames[a]
  title(main = plot_title)
  
}

dev.off()


