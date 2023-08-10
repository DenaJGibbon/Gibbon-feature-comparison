# Follow installation instructions: https://github.com/tensorflow/models/blob/master/research/audioset/vggish/README.md

# All this needs to be run from the terminal after following the installation instructions

# Change directory to location of VGGish model
cd '/Users/denaclink/Desktop/VSCode Repos/vggish-female-features/'

# Activate the VGGish environment
source vggish/bin/activate

# Extract VGGish embeddings from the clips in --sound_dir
python vggishfeaturetest.py --sound_dir '/Volumes/DJC 1TB/VocalIndividualityClips/SoundFilesDownsample/' --tfrecord_dir '/Volumes/DJC 1TB/VocalIndividualityClips/VGGish/'
