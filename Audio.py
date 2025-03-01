import os
import asyncio
import edge_tts
from pydub import AudioSegment
from pydub.generators import Sine

# Define the project folder path
project_folder = os.path.expanduser("~/Desktop/Project 1")
os.makedirs(project_folder, exist_ok=True)  # Ensure the folder exists

# Create Speech Using Edge-TTS
async def create_audio(word, filename, speed=1.2):
    """
    Generates speech using Microsoft Edge-TTS with a customizable speed factor.
    
    :param word: The word to be spoken
    :param filename: Output file name
    :param speed: Speed factor (e.g., 1.2 makes speech 20% faster)
    """
    file_path = os.path.join(project_folder, filename)

    # Ensure the rate is formatted correctly with a '+' sign
    rate_value = int((speed - 1) * 100)
    rate = f"+{rate_value}%" if rate_value >= 0 else f"{rate_value}%"

    # Configure Edge-TTS
    tts = edge_tts.Communicate(text=word, voice="en-US-AriaNeural", rate=rate)
    
    # Save audio
    await tts.save(file_path)
    
    # Load the file and measure duration
    audio = AudioSegment.from_file(file_path)
    duration_ms = len(audio)  # Get duration in milliseconds
    print(f"Created '{word}' audio with speed factor {speed} ({rate}) -> {file_path} (Duration: {duration_ms} ms)")

# Function to create beep sound
def create_beep(filename, frequency=1000, duration=500):
    """
    Generates a beep sound at a given frequency and duration.
    
    :param filename: Name of the output file (must end in .mp3 or .wav)
    :param frequency: Frequency of the beep in Hz (default 1000Hz)
    :param duration: Duration of the beep in milliseconds (default 500ms)
    """
    file_path = os.path.join(project_folder, filename)
    
    # Generate a sine wave beep
    beep = Sine(frequency).to_audio_segment(duration=duration)
    
    # Export as an MP3 file
    beep.export(file_path, format="mp3")

    # Load the file and measure duration
    audio = AudioSegment.from_file(file_path)
    duration_ms = len(audio)  # Get duration in milliseconds
    print(f"Created beep sound -> {file_path} (Duration: {duration_ms} ms)")

# Run the Edge-TTS speech synthesis
async def main():
    # Generate "blue" and "red" with adjusted speed
    await create_audio("blue", "blue.mp3", speed=1.2)  # Adjust speed for correct duration
    await create_audio("red", "red.mp3", speed=1.2)

    # Generate a beep sound
    create_beep("beep.mp3", frequency=1000, duration=500)

    print(f"All audio files saved in: {project_folder}")

# Run the asyncio event loop
asyncio.run(main())