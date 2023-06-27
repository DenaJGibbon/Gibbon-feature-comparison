
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Female Gibbon Calls Classification

This code performs classification on female gibbon calls using various
acoustic signal features, including MFCCs, BirdNET outputs, VGGish
embeddings, and Acoustic Indices. The classification accuracy is
evaluated both with and without the addition of noise.

## Data Preparation

Before running the classification, ensure that the required data files
are available in the specified paths.

### MFCCs

The MFCC data is loaded from the ‘mfcc.output.df.csv’ file. The dataset
is preprocessed to include individual and site information. Summary
statistics of the data are displayed, including the number of unique
individuals.

### Randomization of MFCCs

The MFCC data is randomly split into training and test sets. Random
Forest classification models are trained on the training set and
evaluated on the test set. This process is repeated 20 times, each time
with a different random seed. The classification accuracy for each
iteration is stored in the ‘RandomizationAccuracyMFCC’ list.

### MFCCs with Noise

Similar to the previous step, this section performs randomization of
MFCCs with the addition of noise. The MFCC data with noise is loaded
from the ‘mfcc.noise6db.output.df.csv’ file. Random Forest models are
trained and evaluated, and the classification accuracy for each
iteration is stored in the ‘RandomizationAccuracyMFCCNoise’ list.

### BirdNET Outputs

The BirdNET output data is loaded from the
‘CombinedBirdNetFeaturesV2.csv’ file. The dataset is preprocessed to
include individual information. The number of unique individuals is
displayed.

### Randomization of BirdNET Outputs

The BirdNET output data is randomly split into training and test sets.
Random Forest models are trained and evaluated, and the classification
accuracy for each iteration is stored in the
‘RandomizationAccuracyBirdNet’ list.

### BirdNET Outputs with Noise

Similar to the previous step, this section performs randomization of
BirdNET outputs with the addition of noise. The BirdNET output data with
noise is loaded from the ‘CombinedBirdNetNoiseFeaturesV2.csv’ file.
Random Forest models are trained and evaluated, and the classification
accuracy for each iteration is stored in the
‘RandomizationAccuracyBirdNetNoise’ list.

### VGGish Embeddings

The VGGish embedding data is loaded from the
‘VGGishDFMeanSDoriginal.csv’ file. The dataset is preprocessed to
include individual information. The number of unique individuals is
displayed.

### Randomization of VGGish Embeddings

The VGGish embedding data is randomly split into training and test sets.
Random Forest models are trained and evaluated, and the classification
accuracy for each iteration is stored in the
‘RandomizationAccuracyVGGish’ list.

### VGGish Embeddings with Noise

Similar to the previous step, this section performs randomization of
VGGish embeddings with the addition of noise. The VGGish embedding data
with noise is loaded from the ‘VGGishDFMeanSD6dB.csv’ file. Random
Forest models are trained and evaluated, and the classification accuracy
for each iteration is stored in the ‘RandomizationAccuracyVGGishNoise’
list.

### Acoustic Indices

The Acoustic Indices data is loaded from the
‘AcousticIndicesDForiginal.csv’ file. The dataset is preprocessed to
include individual information. The number of unique individuals is
displayed.

### Randomization of Acoustic Indices

The Acoustic Indices data is randomly split into training and test sets.
Random Forest models are trained and evaluated, and the classification
accuracy for each iteration is stored in the
‘RandomizationAccuracyAcousticIndices’ list.

### Acoustic Indices with Noise

Similar to the previous step, this section performs randomization of
Acoustic Indices with the addition of noise. The Acoustic Indices data
with noise is loaded from the ‘AcousticIndicesDF6dB.csv’ file. Random
Forest models are trained and evaluated, and the classification accuracy
for each iteration is stored in the
‘RandomizationAccuracyAcousticIndicesNoise’ list.

## Results

The classification accuracy results for each feature set, both with and
without added noise, are stored in the corresponding lists. Boxplots are
created to visualize the distribution of classification accuracy across
different feature sets. Additionally, a UMAP plot is generated to
visualize the MFCC features in a two-dimensional space.

Please refer to the code comments and documentation within the script
for further details and instructions.
