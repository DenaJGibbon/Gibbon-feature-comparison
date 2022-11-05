library(soundecology)

# Set input directory
input.dir <- "/Users/denaclink/Library/CloudStorage/GoogleDrive-djc426@cornell.edu/.shortcut-targets-by-id/0B-Zf1l3eDDLjd2g5RHJEZlA5Sms/AllGreatCallWaveFiles"

call.timing.list <-
  list.files(input.dir, full.names = T, pattern = '.wav')

call.timing.list.short <-
  list.files(input.dir, full.names = F, pattern = '.wav')

subsamps <- lapply(1:length(call.timing.list),
                   function(i)
                     readWave(call.timing.list[[i]]))
