from psychopy import visual, core, event, gui, sound
from psychopy import prefs
prefs.hardware['audioLib'] = ['PTB']
import random
import csv
import os

# 🔷 Function to Get Participant Info & Handle File Overwriting
def get_participant_info():
    """
    Asks for participant information, prevents overwriting files, 
    and returns the participant number and CSV filename.
    """
    while True:
        dlg = gui.Dlg(title="Participant Information")
        dlg.addField("Participant Number:")
        dlg.show()

        if not dlg.OK:
            print("Experiment canceled.")
            core.quit()

        participant_number = dlg.data[0].strip()
        if not participant_number.isnumeric():
            print("\n🚩 Invalid input. Enter a numeric participant number (ex. 001).\n")
            continue

        participant_number = f"P{int(participant_number):03d}"
        data_folder = "AV_Stress_Data"
        os.makedirs(data_folder, exist_ok=True)
        csv_filename = os.path.abspath(os.path.join(data_folder, f"experiment_results_{participant_number}.csv"))

        # Warn if file exists
        if os.path.isfile(csv_filename):
            overwrite_warning = gui.Dlg(title="⚠️ WARNING: File Exists!")
            overwrite_warning.addText(f"File for Participant {participant_number} already exists:\n📂 {csv_filename}")
            overwrite_warning.addField("Confirm Overwrite", choices=["No, enter new number", "Yes, overwrite"])
            overwrite_warning.show()
            if overwrite_warning.data[0] == "No, enter new number":
                continue  # Ask for a new number

        # Final confirmation
        confirm_dlg = gui.Dlg(title="Confirm Participant Info")
        confirm_dlg.addText(f"Is this correct?\n📂 {csv_filename}")
        confirm_dlg.addField("Confirm", choices=["Yes", "No"])
        confirm_dlg.show()
        if confirm_dlg.data[0] == "Yes":
            print(f"\n✅ File will be saved as: {csv_filename}\n")
            return participant_number, csv_filename
        else:
            print("\nRe-entering participant number...\n")

# 🔷 Function to Show Written Instructions
def show_instructions(win, text, duration):
    instructions = visual.TextStim(
        win, text=text, color="white", height=30, 
        wrapWidth=700, font="Arial Unicode MS"
    )
    continue_text = visual.TextStim(win, text="*Press space bar to begin*",
                                    color="white", height=20, italic=True, pos=(0, -250))
    
    instructions.draw()
    win.flip()
    core.wait(duration)
    instructions.draw()
    continue_text.draw()
    win.flip()

    event.clearEvents(eventType='keyboard')
    while "space" not in event.getKeys():
        pass
    win.flip()

# 🔷 Function to Run Trials
def run_trials(win, participant_number, csv_filename, block_num, iti_range, total_trials, trial_types, trial4 = False):

    if trial4:
        # If trial4 is True, set the decreasing ITI range
        iti_start = iti_range[0]
        iti_end = 0.375  # Desired lower bound for trial4
    total_trial_types = len(trial_types)
    trials_per_type = total_trials // total_trial_types  # // rounds down to nearest int
    response_keys = ["r", "b"]  # Response keys
    fixation = visual.TextStim(win, text="+", color="white", height=40)

    # ✅ Preload audio files
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.mp3",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.mp3"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    file_exists = os.path.exists(csv_filename)
    with open(csv_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["Participant", "Block", "Trial", "Type", "Visual", "Audio", "Response", "RT", "Correct", "ITI"])

        trials = []
        # ✅ Generate trials
        for trial_type in trial_types:
            colors = ["red"] * (trials_per_type // 2) + ["blue"] * (trials_per_type // 2)
            color_iterator = iter(colors)  # Create an iterator to assign colors in order

            for _ in range(trials_per_type):
                if trial_type == "V":
                    color = next(color_iterator)
                    audio = None
                elif trial_type == "A":
                    color = None
                    audio = preloaded_sounds[next(color_iterator)]
                elif trial_type == "AVC":
                    color = next(color_iterator)
                    audio = preloaded_sounds[color]
                elif trial_type == "AVI":
                    color = next(color_iterator)
                    incongruent_color = "red" if color == "blue" else "blue"
                    audio = preloaded_sounds[incongruent_color]
                trials.append({"type": trial_type, "visual": color, "audio": audio})

        # ✅ Shuffle trials **within each block**
        random.shuffle(trials)

        for i, trial in enumerate(trials):
            print(f"  🔹 Trial {i+1}: {trial}")  # Debugging print to check each trial

            if trial["audio"]:
                trial["audio"].stop()

            # Show fixation cross
            fixation.draw()
            win.flip()
            core.wait(0.5)  # Fixation for 500ms

            event.clearEvents(eventType='keyboard')
            clock = core.Clock()

            # Prepare visual stimulus
            if trial["visual"]:
                circle = visual.Circle(win, radius=50, fillColor=trial["visual"], lineColor=None)
                circle.draw()

            # Prepare auditory stimulus
            beep = trial["audio"]

            # ✅ Ensure **perfect synchronization**
            stimulus_start_time = core.getTime()

            if beep:
                beep.play()

            win.flip()  # Show visual stimulus

            # ✅ Debugging timestamps for verification
            print(f"🎵 Audio started at: {stimulus_start_time:.3f} sec")
            print(f"🎨 Visual stimulus appeared at: {core.getTime():.3f} sec")

            # Collect response
            response = event.waitKeys(maxWait=2.0, keyList=response_keys, timeStamped=clock)
            win.flip()

            key, rt = response[0] if response else ("No Response", None)

            correct = None
            if trial["type"] in ["V", "A", "AVC"]:
                expected_response = "b" if (trial["visual"] == "blue" or (trial["audio"] and "blue" in trial["audio"].fileName)) else "r"
                correct = key == expected_response
            elif trial["type"] in ["AVI"]:
                correct = "NA"

            # ✅ Adjust ITI based on trial4 condition
            if trial4:
                progress = i / (len(trials) - 1)  # Progress from 0 to 1 across trials
                iti = iti_start - progress * (iti_start - iti_end)
            else:
                iti = random.uniform(iti_range[0], iti_range[1])

            # ✅ Save data ensuring no repetition of blocks
            writer.writerow([
                participant_number,
                block_num,
                i + 1,
                trial["type"],
                trial["visual"] if trial["visual"] is not None else "NA",
                os.path.basename(trial["audio"].fileName) if trial["audio"] else "NA",
                key if key is not None else "NA",
                rt if rt is not None else "NA",
                correct if correct is not None else "NA",
                round(iti, 3)
            ])

            print(f"  ✅ Trial {i+1} completed. ITI: {round(iti, 3)}s")

            core.wait(iti)  # Inter-Trial Interval

# 🔷 Function to Run Practice
def run_practice(win, iti_range):

    trial_types = ["A", "V"]
    total_trials = 6
    total_trial_types = 2
    trials_per_type = total_trials // total_trial_types # // rounds down to nearest int
    fixation = visual.TextStim(win, text="+", color="white", height=40)

    # ✅ Preload audio files
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.mp3",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.mp3"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    trials = []
    # ✅ Generate trials
    for trial_type in trial_types:
        # Create a balanced color list (half red, half blue) and shuffle
        colors = ["red", "blue"]

        for _ in range(trials_per_type):
            random.shuffle(colors)
            if trial_type == "V":
                color = colors[0]
                audio = None
            elif trial_type == "A":
                color = None
                audio = preloaded_sounds[colors[0]] 
            trials.append({"type": trial_type, "visual": color, "audio": audio}) 

        # ✅ Shuffle trials **within each block**
        random.shuffle(trials)

        for i, trial in enumerate(trials):
            # Show fixation cross
            fixation.draw()
            win.flip()
            core.wait(0.5)  # Fixation for 500ms
            event.clearEvents(eventType='keyboard')
            clock = core.Clock()
            if trial["visual"]:
                circle = visual.Circle(win, radius=50, fillColor=trial["visual"], lineColor=None)
                circle.draw()
            beep = trial["audio"]
            # ✅ Ensure **perfect synchronization**
            stimulus_start_time = core.getTime()  # Get timestamp before showing stimuli
            if beep:
                beep.play()  # Play audio just before flipping screen
            win.flip()  # Show visual stimulus
            iti = random.uniform(iti_range[0], iti_range[1])
            core.wait(iti)  # Inter-Trial Interval
            
# 🔶 Get Participant Info
participant_number, csv_filename = get_participant_info()

# 🔶 Initialize PsychoPy Window
win = visual.Window(size=(800, 600), color="black", units="pix")

# 🔶 Practice
show_instructions(win, "In this experiment, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "Respond as quickly and accurately as possible.", 1)
show_instructions(win,"click the RED button\n when you percieve RED\n\n"
                  "click the BLUE\n when you percieve BLUE\n\n", 1)
### Include Practice Trials ###
show_instructions(win, "Great job! now you will be moving on to the real task.\n\n"
                  "Respond as quickly and accurately as possible", 1)

# 🔶 RUN TRIALS
run_trials(win, participant_number, csv_filename, block_num=1, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "one minute break...\n\n", 2)
run_trials(win, participant_number, csv_filename, block_num=2, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "one minute break...", 2)
run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "one minute break...", 2)
run_trials(win, participant_number, csv_filename, block_num=4, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"], trial4=True)
show_instructions(win, "Great job! You have completed the task\n\n", 
                  "Thank you for participating!", 2)

# 🔶 Close the Experiment
win.close()
core.quit()