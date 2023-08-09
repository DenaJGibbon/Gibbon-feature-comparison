# Follow the installation instructions here: https://github.com/kahst/BirdNET-Analyzer

# After installation get the environment set up (run this in the terminal)
cd /Users/denaclink/BirdNET-Analyzer
conda activate birdnet-analyzer 

# Run the example to ensure it works 
python analyze.py

# Extract embeddings using the following command line code 
python embeddings.py --i '/Volumes/DJC 1TB/VocalIndividualityClips/SoundFiles/' --o '/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample/' --threads 4 --batchsize 16

