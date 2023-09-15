
# Metadata for all data included in the repository.

<table>
<thead>
<tr>
<th style="text-align:left;">
FileLocation
</th>
<th style="text-align:left;">
Description
</th>
<th style="text-align:left;">
Date
</th>
<th style="text-align:left;">
DateType
</th>
<th style="text-align:left;">
Summary
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
data/features
</td>
<td style="text-align:left;">
A folder containing the different feature sets for each .wav file, along
with recorder ID that include location, time, and date. There is also a
column for individual ID
</td>
<td style="text-align:left;">
Fri Sep 15 08:48:03 2023
</td>
<td style="text-align:left;">
Folder
</td>
<td style="text-align:left;">
The folder contains .csv files for acoustic indices, BirdNET, MFCCs,
VGGIsh, and Wav2Vec2. For VGGIsh and BirdNET the .csv files are divided
by recorder location.
</td>
</tr>
<tr>
<td style="text-align:left;">
data/MB Playbacks 50 m.csv
</td>
<td style="text-align:left;">
A .csv file containing the GPS coordinates of the recorders
</td>
<td style="text-align:left;">
Fri Sep 15 08:48:03 2023
</td>
<td style="text-align:left;">
.csv
</td>
<td style="text-align:left;">
This file contains the GPS coordinates of each recorder M01-M09
</td>
</tr>
<tr>
<td style="text-align:left;">
data/randomization_affinity
</td>
<td style="text-align:left;">
Contains a .csv files for each feature type
</td>
<td style="text-align:left;">
Fri Sep 15 08:48:03 2023
</td>
<td style="text-align:left;">
Folder
</td>
<td style="text-align:left;">
This file contains the classification accuracy, recorder, number of
clusters returned by affinity propagation clustering, and normalized
mutual information value
</td>
</tr>
<tr>
<td style="text-align:left;">
data/randomization_hdbscan
</td>
<td style="text-align:left;">
Contains a .csv files for each feature type
</td>
<td style="text-align:left;">
Fri Sep 15 08:48:03 2023
</td>
<td style="text-align:left;">
Folder
</td>
<td style="text-align:left;">
This file contains the classification accuracy, recorder, number of
clusters returned by hdbscan, and normalized mutual information value
</td>
</tr>
<tr>
<td style="text-align:left;">
data/snr_df
</td>
<td style="text-align:left;">
Contains a .csv files for each recorder location
</td>
<td style="text-align:left;">
Fri Sep 15 08:48:03 2023
</td>
<td style="text-align:left;">
Folder
</td>
<td style="text-align:left;">
This file contains the recording ID, signal-to-noise ratio, recorder,
and wave file path.
</td>
</tr>
</tbody>
</table>
