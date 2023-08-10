
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

This is the repository for: ‘Mel-frequency cepstral coefficients
outperform embeddings from pre-trained convolutional neural networks
under noisy conditions for discrimination tasks of individual gibbons’
(Lakdari et al, under review). The goal of the paper is to compare
different approaches of feature extraction for individual discrimination
of gibbon female calls. Acoustic data can be downloaded at:
10.5281/zenodo.8205685.

Feature extraction was done in both R and Python, and analyses for
publication were done in R.

# Feature/embedding extraction

### MFCCs

MFCCs are calculated using the ‘Processing features for randomization.R’
R script.

### BirdNET

Follow the installation instructions here:
<https://github.com/kahst/BirdNET-Analyzer>. Then use the ‘BirdNET
Terminal Script’. Then run the ‘Processing features for randomization.R’
R script to convert BirdNET embeddings into the format needed for
analyses.

### VGGish

Follow installation instructions:
<https://github.com/tensorflow/models/blob/master/research/audioset/vggish/README.md>.
Then use the ‘VGGish Terminal Script’. Then run the ‘Processing features
for randomization.R’ R script to convert VGGish embeddings into the
format needed for analyses.

### Wav2Vec2

Wav2Vec2 embeddings are caluclated using the ‘Wav2Vec2_Features.py’
Python script.

### Acoustic indices

Acoustic indices are calculated using the ‘Processing features for
randomization.R’ R script.

# SNR calculation

SNR calculation is done on sound clips that have an extra 2-s on either
side of the call using the ‘SNR Calculation’ R script.

# Supervised classification and unsupervised clustering

Use the ‘Randomization for playbacks.R’ script to randomly divide data
for each feature and distance category using a 80/20 split. This script
uses the processed data for each feature located in the ‘data/features’
folder.

# Creating plots for publication

See ‘Script to recreate figures.R’ to recreate all figures in
publication.

``` r
library(ggpubr)
#> Loading required package: ggplot2
library(umap)
library(stringr)
library(cowplot)
#> 
#> Attaching package: 'cowplot'
#> The following object is masked from 'package:ggpubr':
#> 
#>     get_legend
library(viridis)
#> Loading required package: viridisLite

# Plotting randomization results -------------------------------------------------

# Load and process data from the 'data/randomization_hdbscan/' directory
file_names <- dir('data/randomization_hdbscan/', full.names = TRUE) # List all file names in the directory

# Combine data from all files into a single data frame using read.csv and do.call(rbind, ...)
AllPlaybacksRand <- do.call(rbind, lapply(file_names, read.csv))

# Convert 'AccuracyVal' column to percentage scale
AllPlaybacksRand$AccuracyVal <- AllPlaybacksRand$AccuracyVal * 100

# Convert factors for 'Recorder' and 'Features' columns to meaningful labels
AllPlaybacksRand$Recorder <- as.factor(AllPlaybacksRand$Recorder)
levels(AllPlaybacksRand$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                       'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

AllPlaybacksRand$Features <- as.factor(AllPlaybacksRand$Features)
levels(AllPlaybacksRand$Features) <- c('Acoustic Indices', 'BirdNET', 'MFCCs',
                                       'VGGish', 'Wav2Vec2')

# Create ggplot2 plot 'PlotA' for Classification Accuracy by Features and Recorder
PlotA <- ggboxplot(data = AllPlaybacksRand,
                   color = 'Recorder', fill = 'Recorder', y = 'AccuracyVal', x = 'Features') +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Classification Accuracy')

# Create ggplot2 plot 'PlotBHDBSCAN' for Normalized Mutual Information (HDBSCAN) by Features and Recorder
PlotBHDBSCAN <- ggboxplot(data = AllPlaybacksRand,
                          color = 'Recorder', fill = 'Recorder', y = 'NMI.val', x = 'Features') +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Normalized Mutual Information (HDBSCAN)')

# Calculate the deviation between 'N.Individual' and 'N.cluster' columns and add it as 'Deviation' column
AllPlaybacksRand$Deviation <- AllPlaybacksRand$N.Individual - AllPlaybacksRand$N.cluster

# Create ggplot2 plot 'PlotCHDBSCAN' for Number of Clusters (HDBSCAN) by Features and Recorder
PlotCHDBSCAN <- ggboxplot(data = AllPlaybacksRand,
                          color = 'Recorder', fill = 'Recorder', y = 'N.cluster', x = 'Features', outlier.shape = NA) +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Number of clusters (HDBSCAN)') +
  geom_hline(yintercept = 12, linetype = 'dashed') + ylim(0, 25) + xlab('')

# Create a cowplot grid with PlotA and PlotB, labeled as 'A' and 'B', respectively, in 2 rows
cowplot::plot_grid(PlotA, PlotBHDBSCAN, labels = c('A', 'B'), nrow = 2, label_x = 0.95)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r

# Repeat the process for the 'data/randomization_affinity/' directory
# Get the list of all file names in the 'data/randomization_affinity/' directory, with their full paths
file_names <- dir('data/randomization_affinity/', full.names = TRUE)

# Read each CSV file in the list and combine all of them into a single data frame
AllPlaybacksRand <- do.call(rbind, lapply(file_names, read.csv))

# Convert the 'AccuracyVal' column values to percentage by multiplying with 100
AllPlaybacksRand$AccuracyVal <- AllPlaybacksRand$AccuracyVal * 100

# Convert the 'Recorder' column to a factor data type
AllPlaybacksRand$Recorder <- as.factor(AllPlaybacksRand$Recorder)

# Rename the factor levels of the 'Recorder' column to more descriptive labels
levels(AllPlaybacksRand$Recorder) <- c('M1 (0 m)', 'M2 (50 m)', 'M3 (100 m)', 'M4 (150 m)',
                                       'M5 (200 m)', 'M6 (250 m)', 'M7 (300 m)')

# Convert the 'Features' column to a factor data type
AllPlaybacksRand$Features <- as.factor(AllPlaybacksRand$Features)

# Rename the factor levels of the 'Features' column to more descriptive labels
levels(AllPlaybacksRand$Features) <- c('Acoustic Indices', 'BirdNET', 'MFCCs',
                                       'VGGish', 'Wav2Vec2')

# Create ggplot2 plot 'PlotBaffinity' for Normalized Mutual Information (affinity) by Features and Recorder
PlotBaffinity <- ggboxplot(data = AllPlaybacksRand,
                           color = 'Recorder', fill = 'Recorder', y = 'NMI.val', x = 'Features') +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Normalized Mutual Information (affinity)') + xlab('')

# Calculate the deviation between 'N.Individual' and 'N.cluster' columns and add it as 'Deviation' column
AllPlaybacksRand$Deviation <- AllPlaybacksRand$N.Individual - AllPlaybacksRand$N.cluster

# Create ggplot2 plot 'PlotCaffinity' for Number of Clusters (affinity) by Features and Recorder
PlotCaffinity <- ggboxplot(data = AllPlaybacksRand,
                           color = 'Recorder', fill = 'Recorder', y = 'N.cluster', x = 'Features', outlier.shape = NA) +
  scale_color_manual(values = viridis::viridis(7)) +
  scale_fill_manual(values = viridis::viridis(7)) +
  ylab('Number of clusters (affinity)') +
  geom_hline(yintercept = 12, linetype = 'dashed') + ylim(0, 25) + xlab('')

# Create a cowplot grid with PlotBaffinity and PlotBHDBSCAN, labeled as 'A' and 'B', respectively, in 2 rows
cowplot::plot_grid(PlotBaffinity, PlotBHDBSCAN, labels = c('A', 'B'), nrow = 2, label_x = 0.95)
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r

# Create a cowplot grid with PlotCaffinity and PlotCHDBSCAN, labeled as 'A' and 'B', respectively, in 2 rows
cowplot::plot_grid(PlotCaffinity, PlotCHDBSCAN, labels = c('A', 'B'), nrow = 2, label_x = 0.95)
```

![](README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->
