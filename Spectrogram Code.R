library(phonTools)
set.seed(20)

input.dir <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles"

call.timing.list <-
  list.files(input.dir, full.names = T, pattern = '.wav')

call.timing.list.short <-
  list.files(input.dir, full.names = F, pattern = '.wav')

which.spectrograms <- round(runif(4,1,1146),0)

FilePaths <- call.timing.list[which.spectrograms]


# Set input directory
input.dir.noise <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles/output6db"

call.timing.list.noise <-
  list.files(input.dir.noise, full.names = T, pattern = '.wav')

call.timing.list.short.noise <-
  list.files(input.dir.noise, full.names = F, pattern = '.wav')

FilePathsNoise <- call.timing.list.noise[which.spectrograms]

SpecPaths <- c(FilePaths,FilePathsNoise)

SpecPaths <- SpecPaths[ c(1,5,2,6,3,7,4,8)]

pdf('spectrograms1.pdf',width=10,height=15)
par(mfrow=c(4,2))
for(a in 1:length(SpecPaths)){
 TempWav <-  readWave(SpecPaths[a])
  
 phonTools::spectrogram(TempWav@left, fs=TempWav@samp.rate,maxfreq = 2000,
                        windowlength = 10 )
 
}

dev.off()
  
  
  
  
  