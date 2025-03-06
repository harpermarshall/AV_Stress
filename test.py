import os
from psychopy import sound

# Define file paths
audio_files = {
    "red": "C:/Users/WallaceLab/Documents/GitHub/AV_Stress/Sounds/red_converted.wav",
    "blue": "C:/Users/WallaceLab/Documents/GitHub/AV_Stress/Sounds/blue_converted.wav"
}

# Check if files exist
for color, path in audio_files.items():
    exists = os.path.exists(path)
    print(f"{color} sound file exists: {exists} ({path})")

    if exists:
        try:
            snd = sound.Sound(path)
            print(f"✅ Successfully loaded {color} sound.")
            snd.play()
        except Exception as e:
            print(f"❌ Error loading {color} sound: {e}")
