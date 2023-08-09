#### R code adapted from Clink et al. Evidence of vocal performance constraints in a female non-human primate 

# Load required libraries
library(tuneR)
library(seewave)
library(ggplot2)
library(ggpubr)
library(plyr)
library(stringr)


#### Signal-to-noise ratio calculation ####

# Create index of calls for the loop
input.dir.new <- "/Volumes/DJC 1TB/VocalIndividualityClips/SoundFilesSNR"
L.snr = list.files(input.dir.new, pattern="*.wav", full.names=T)
L.snr

L.snr.short <- list.files(input.dir.new, pattern="*.wav", full.names=F)
RecorderInfo <- str_split_fixed(L.snr.short,pattern = '_',n=2)[,1]
uniqueRecorderInfo <- unique(RecorderInfo)

# Loop through different recorders to calculate SNR
for( i in 1:length(uniqueRecorderInfo) ){
  
  L.snr.sub <- L.snr[which(RecorderInfo == uniqueRecorderInfo[i])]
  
  # Create empty list to store SNR values
  snrdf <- data.frame()
  
  # Loop to find candidate noise from individual recording of gibbon great calls
  for (j in 1:length(L.snr.sub)) { 
    tryCatch({
      # Read in two-minute .wav file
      snr.file <- L.snr.sub[[j]]
      snr.wav <- readWave(snr.file)
      print(paste("processing", snr.file))
      
      # Filter the .wav file to focus on the gibbon frequency range
      w.dn.filt <- fir(snr.wav, from=450, to=1800, output="Wave") 
      w.dn.filt <- normalize(w.dn.filt, unit="16")
      
      # Create 1-sec bins 
      bin.seq <- seq(from=0, to=duration(snr.wav), by=1)
      length(bin.seq)
      
      # Create a list of all the 1-sec bins for SNR analysis
      bin.seq.length <- length(bin.seq) - 1
      subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
        extractWave(w.dn.filt, from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
                    xunit=c("time"), plot=F, output="Wave"))
      
      # Calculate noise for each 1 sec bin for the longer recording
      noise.list <- list()
      for (k in 1:length(subsamps.1sec)) { 
        # Read in .wav file 
        wav <- subsamps.1sec[[k]]
        
        bin.seq.smaller <- seq(from=0, to=48000, by=480)
        bin.seq.length.small <- length(bin.seq.smaller) - 1
        subsamps.short <- lapply(1:bin.seq.length.small, function(i) 
          extractWave(wav, from=as.numeric(bin.seq.smaller[i]), 
                      to=as.numeric(bin.seq.smaller[i+1]), xunit=c("samples"),
                      plot=F, output="Wave"))
        
        # Calculate sum of squares for each 10 ms long sample
        sum.square.list <- list()
        for (f in 1:length(subsamps.short)){
          wav.temp <- subsamps.short[[f]]
          sum.square.list[[f]] <- sum(wav.temp@left^2)
        }
        
        # Find median value for each 10 ms clip
        noise.list[[k]] <- median(unlist(sum.square.list))
      }
      
      # Now have a list with 120 values corresponding to the median value for each 10 ms subsample
      # that are candidate noise; use the 10th percentile of the distribution for noise estimate
      noise.value <- quantile(unlist(noise.list), c(0.10))
      signal.value <- quantile(unlist(noise.list), c(0.75))
      bins.minus.noise <- signal.value - noise.value
      SNR.dB <- 10 * log10(bins.minus.noise / noise.value)
      print(SNR.dB)
      
      # Combine recording id and noise estimate value
      # NOTE: depending on file path name the second numerical value [[7]] may need to be changed
      great.call <- str_split_fixed(L.snr.sub[[j]], pattern="/", n=6)[[6]]
      great.call <- gsub(".wav", "", great.call)
      
      # Add to list of noise values
      file_name <- paste('snr_df/', uniqueRecorderInfo[i], '_snrdf.csv', sep='')
      recorder <- uniqueRecorderInfo[i]
      wavname <- snr.file
      temprow <- cbind.data.frame(great.call, SNR.dB, recorder, wavname)
      snrdf <- rbind.data.frame(snrdf, temprow)
      write.csv(snrdf, file_name, row.names = F)
    }, error=function(e){
      cat("ERROR :", conditionMessage(e), "\n")
    })
  }
}


SNR_file_names <- dir('snr_df/', full.names = T) # Directory containing the saved SNR files

# Read all SNR files and combine them into a single data frame
AllPlaybacksSNR <- do.call(rbind, lapply(SNR_file_names, read.csv))

# Extract time information from great.call column
AllPlaybacksSNR$time <- str_split_fixed(AllPlaybacksSNR$great.call, pattern='_', n=4)[, 3]
AllPlaybacksSNR$time <- substr(AllPlaybacksSNR$time, 1, 4)

# Convert Recorder column to factor and relabel levels
AllPlaybacksSNR$Recorder <- as.factor(AllPlaybacksSNR$recorder)
levels(AllPlaybacksSNR$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                      'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

# Create boxplots for SNR analysis
ggboxplot(data = AllPlaybacksSNR, x = 'Recorder', y = "SNR.dB", outlier.shape = NA) + ylab('SNR (dB re 20 μPa)')

