# Load required libraries for audio file analysis and manipulation
library(seewave)
library(tuneR)

# List the full paths of all files in the specified directory
AllFiles <- list.files('/Volumes/DJC 1TB/VocalIndividualityClips/SoundFiles', full.names = TRUE)

# List the filenames (without paths) from the specified directory
AllFilesShort <- list.files('/Volumes/DJC 1TB/VocalIndividualityClips/SoundFiles', full.names = FALSE)

# Extract times from the filenames; assumes filenames have time encoded at a specific location after splitting with '_'
Times <- substr(str_split_fixed(AllFilesShort, pattern = '_', n=5)[,3], start=1, stop=4)

# Extract names temporarily from the filenames; again based on a specific structure after splitting with '_'
TempName <- str_split_fixed(AllFilesShort, pattern = '_', n=4)[,4]
TempName <- str_split_fixed(TempName, pattern = '.snr', n=2)[,1]

# Extract dates from the filenames; assumes filenames have dates encoded at a specific location after splitting with '_'
Dates <- str_split_fixed(AllFilesShort, pattern = '_', n=4)[,2]

# Filter out specific files based on a combination of name, time, and date criteria
SpecPaths <- AllFiles[which(TempName == 'MB_01_564.01' & Times == "0600" & Dates == "20190824")]

# Specify the names of the recorders used in the study
RecorderNames <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                   'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

# Set up a new PNG device for plotting, with defined width, height, resolution, and point size
png('spectrograms1.png', width = 1600, height = 1800, res= 200, pointsize = 14)

# Define a 4x2 plotting layout
par(mfrow=c(4,2))

# Loop through the filtered audio file paths to generate spectrograms
for(a in 1:length(SpecPaths)){
  # Read the current audio file using the tuneR library
  TempWav <- readWave(SpecPaths[a])
  
  # Generate a spectrogram for the audio file using phonTools library
  phonTools::spectrogram(TempWav@left, fs = TempWav@samp.rate, maxfreq = 2000, windowlength = 15)
  
  # Label the generated spectrogram with the corresponding recorder name
  plot_title <- RecorderNames[a]
  title(main = plot_title)
}

# Close the PNG device, effectively saving the plot to 'spectrograms1.png'
dev.off()
