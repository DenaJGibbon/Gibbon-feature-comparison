
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
