# This is to run in the terminal to extract BirdNET embeddings
# Get the environment set up
cd /Users/denaclink/BirdNET-Analyzer
conda activate birdnet-analyzer 
python embeddings.py --i "/Volumes/Clink Data Backup/Great Calls All/AllGreatCallsWaveFilesOnly/" --o "/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/BirdNETembeddings" 

python embeddings.py --i "/Users/denaclink/Library/CloudStorage/GoogleDrive-denajane13@gmail.com/My Drive/AllGreatCallsWaveFiles6dB" --o "/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/BirdNETembeddings6dB" 
