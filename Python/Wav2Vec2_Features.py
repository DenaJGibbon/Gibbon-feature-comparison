# Import required libraries
import torch
import numpy as np
import os
import soundfile as sf
from transformers import AutoFeatureExtractor, AutoModelForAudioClassification
import pandas as pd

# Load the trained model
model = AutoModelForAudioClassification.from_pretrained("facebook/Wav2Vec2-Base-960h")

# Audio feature extractor
feature_extractor = AutoFeatureExtractor.from_pretrained("facebook/Wav2Vec2-Base-960h")

# Directory where the audio files are located
audio_dir = '/Volumes/DJC 1TB/VocalIndividualityClips/SoundFilesDownsample'
output_dir = '/Volumes/DJC 1TB/VocalIndividualityClips/wav2vec2featuresmeansd'  # directory to save the features

# Make sure the output directory exists
os.makedirs(output_dir, exist_ok=True)

# Get all .wav files in the directory
audio_files = [f for f in os.listdir(audio_dir) if os.path.isfile(os.path.join(audio_dir, f)) and f.endswith('.wav')]

# Create lists to store mean and standard deviation values for each audio clip
mean_list = []
std_list = []

# Process each audio file
for file in audio_files:
    # Load the audio
    audio, samplerate = sf.read(os.path.join(audio_dir, file))

    # Calculate the duration of the audio file
    duration = len(audio) / samplerate

    # Resample to 16000 Hz if necessary
    if samplerate != 16000:
        audio = librosa.resample(audio, samplerate, 16000)
        samplerate = 16000

    # Split audio into 1-second chunks
    chunk_size = 16000  # 1 second at 16000 Hz
    num_chunks = len(audio) // chunk_size
    audio_chunks = np.array_split(audio, num_chunks)

    # Initialize lists to store mean and std for each chunk
    chunk_mean_list = []
    chunk_std_list = []

    # Process each chunk
    for chunk in audio_chunks:
        # Preprocess
        inputs = feature_extractor([chunk], sampling_rate=samplerate, max_length=chunk_size, truncation=True)

        # Remember to move your inputs to the same device as your model.
        inputs = {name: torch.tensor(val).to(model.device) for name, val in inputs.items()}

        # Get the model's base transformer output
        with torch.no_grad():
            outputs = model.base_model(**inputs)

        # Extract the last hidden states (features)
        features = outputs.last_hidden_state

        # Pool the sequence of hidden states into a single vector
        pooled_features = features.mean(dim=1).cpu().numpy()[0]

        # Calculate the mean and standard deviation for the current chunk
        chunk_mean = np.mean(pooled_features)
        chunk_std = np.std(pooled_features)

        chunk_mean_list.append(chunk_mean)
        chunk_std_list.append(chunk_std)

    # Store the mean and std values for the audio clip
    mean_list.append(chunk_mean_list)
    std_list.append(chunk_std_list)

    # Create a DataFrame with feature values and standardized column names
    df = pd.DataFrame({'mean': chunk_mean_list, 'std': chunk_std_list})

    # Add the file name and duration as extra columns
    df['file'] = file
    df['duration'] = duration

    # Save the DataFrame to a CSV file
    output_file = os.path.join(output_dir, f'{os.path.splitext(file)[0]}_features.csv')
    df.to_csv(output_file, index=False)

# Create a summary DataFrame with mean and standard deviation values for each audio clip
summary_df = pd.DataFrame({'file': audio_files, 'mean': mean_list, 'std': std_list})
summary_output_file = os.path.join(output_dir, 'summary_features.csv')
summary_df.to_csv(summary_output_file, index=False)
