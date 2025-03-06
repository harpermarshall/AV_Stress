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

    # Preload audio files
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
        # Generate trials
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

        # Shuffle trials **within each block**
        random.shuffle(trials)

        for i, trial in enumerate(trials):
            print(f"  🔹 Trial {i+1}: {trial}")  # Debugging print to check each trial

            if trial["audio"]:
                trial["audio"].stop() # Stops sound from previous audio trials

            # Show fixation cross
            fixation.draw()
            win.flip()
            core.wait(0.5)  # Fixation for 500ms

            event.clearEvents(eventType='keyboard')
            clock = core.Clock()

            # Prepare visual stimulus
            if trial["visual"]:
                circle = visual.Circle(win, radius=50, fillColor=trial["visual"], lineColor=None)
            # Prepare auditory stimulus
            beep = trial["audio"]

            # Get time just before stimulus presentation
            stimulus_start_time = core.getTime()

            # Play auditory stimulus immediately
            if beep:
                beep.play()
                print(f"🎵 Audio started at: {stimulus_start_time:.3f} sec")
            
            # Introduce 300 ms delay before showing visual stimulus
            core.wait(0.3)

            # Now display the visual stimulus
            if trial["visual"]:
                circle.draw()
                win.flip()
                visual_onset_time = core.getTime()  # Get actual visual onset time
                print(f"🎨 Visual stimulus appeared at: {visual_onset_time:.3f} sec")
            else:
                visual_onset_time = stimulus_start_time  # If no visual stimulus, keep same onset

            # Start response collection from the correct time
            response = event.waitKeys(maxWait=2.0, keyList=response_keys, timeStamped=clock)
            win.flip()

            # If no response...
            if response is None:
                key, rt = "No Response", None
                if trial4:  # Only show feedback if it's a trial 4 condition
                    feedback_text = visual.TextStim(win, text="Too Slow", color="red", height=40)
                    feedback_text.draw()
                    win.flip()
                    core.wait(1)  # Display feedback for 1 second
            else:
                key, rt = response[0]
                # **Correct RT calculation: subtract from visual onset**
                rt = rt + stimulus_start_time - visual_onset_time 

            # Determine correctness
            correct = None
            if trial["type"] in ["V", "A", "AVC"]:
                expected_response = "b" if (trial["visual"] == "blue" or (trial["audio"] and "blue" in trial["audio"].fileName)) else "r"
                correct = key == expected_response
            elif trial["type"] in ["AVI"]:
                correct = "NA"

            # Adjust ITI based on trial4 condition
            if trial4:
                progress = i / (len(trials) - 1)  # Progress from 0 to 1 across trials
                iti = iti_start - progress * (iti_start - iti_end)
            else:
                iti = random.uniform(iti_range[0], iti_range[1])

            # Save data ensuring no repetition of blocks
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
    fixation = visual.TextStim(win, text="+", color="white", height=40)

    # Preload audio files
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.mp3",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.mp3"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    # Hard-coded practice trials
    practice_trials = [
        {"type": "A", "visual": None, "audio": preloaded_sounds["red"]},  # Auditory Red
        {"type": "A", "visual": None, "audio": preloaded_sounds["red"]},
        {"type": "A", "visual": None, "audio": preloaded_sounds["red"]},
        {"type": "A", "visual": None, "audio": preloaded_sounds["blue"]},  # Auditory Blue
        {"type": "A", "visual": None, "audio": preloaded_sounds["blue"]},
        {"type": "A", "visual": None, "audio": preloaded_sounds["blue"]},
        {"type": "V", "visual": "red", "audio": None},  # Visual Red
        {"type": "V", "visual": "red", "audio": None},
        {"type": "V", "visual": "red", "audio": None},
        {"type": "V", "visual": "blue", "audio": None},  # Visual Blue
        {"type": "V", "visual": "blue", "audio": None},
        {"type": "V", "visual": "blue", "audio": None},
    ]

    # Shuffle the hard-coded practice trials
    random.shuffle(practice_trials)

    # Run trials in randomized order
    for i, trial in enumerate(practice_trials):
        # Show fixation cross
        fixation.draw()
        win.flip()
        core.wait(0.5)  # Fixation for 500ms
        event.clearEvents(eventType='keyboard')
        clock = core.Clock()

        # Display visual stimulus if applicable
        if trial["visual"]:
            circle = visual.Circle(win, radius=50, fillColor=trial["visual"], lineColor=None)
            circle.draw()

        # Play auditory stimulus if applicable
        beep = trial["audio"]
        stimulus_start_time = core.getTime()  # Get timestamp before showing stimuli
        if beep:
            beep.play()  # Play audio just before flipping screen

        win.flip()  # Show visual stimulus

        # ITI (Inter-Trial Interval)
        iti = random.uniform(iti_range[0], iti_range[1])
        core.wait(iti)

# 🔷 Function to Run Post-Experiment Questionnaire
def run_post_experiment_questionnaire(win, participant_number, csv_filename):
    """
    Presents a post-experiment Likert-scale questionnaire and records responses in the same CSV file.
    """
    questions = [
        "I felt pressured to respond quickly during the task.",
        "I found the task mentally demanding.",
        "As the experiment progressed, my reaction time improved.",  # Reverse-coded for stress impact
        "In the fourth block, I felt significantly more stressed than in previous blocks.",
        "When both sound and visual stimuli were presented, I relied more on the visual information.",
        "When both sound and visual stimuli were presented, I relied more on the auditory information.",
        "I found it difficult to ignore the sound when focusing on the visual task.",
        "I found it difficult to ignore the visual stimulus when focusing on the sound.",
        "The incongruent (mismatched) trials were harder than the congruent trials.",
        "I felt confident in my responses throughout the experiment."
    ]

    scale_labels = "1 = Strongly Disagree   2 = Disagree   3 = Neutral   4 = Agree   5 = Strongly Agree"

    responses = []  # Store participant responses

    for question in questions:
        # Create and display question text
        question_text = visual.TextStim(win, text=question, color="white", height=25, wrapWidth=750, pos=(0, 50))
        scale_text = visual.TextStim(win, text=scale_labels, color="white", height=20, wrapWidth=750, pos=(0, -50))
        
        # Display question
        question_text.draw()
        scale_text.draw()
        win.flip()

        # Wait for response (keys 1-5)
        response = None
        clock = core.Clock() # Start timing

        while response not in ["1", "2", "3", "4", "5"]:
            response = event.waitKeys(keyList=["1", "2", "3", "4", "5"])[0]  # Only record the key, no timestamp
        
        responses.append(response)  # Save response

        # Ensure a **minimum** of 7 seconds has passed
        elapsed_time = clock.getTime()
        remaining_time = 7 - elapsed_time
        if remaining_time > 0:
            core.wait(remaining_time)

        # Show "Press SPACE to continue" after 7 seconds
        continue_text = visual.TextStim(win, text="Press SPACE to continue", color="white", height=20, pos=(0, -100))
        continue_text.draw()
        question_text.draw()
        scale_text.draw()
        win.flip()

        # Wait for space bar
        event.waitKeys(keyList=["space"])

    # Save responses to the existing experiment CSV file
    with open(csv_filename, "a", newline="") as file:
        writer = csv.writer(file)
        writer.writerow(["Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"])
        writer.writerow([participant_number] + responses)

    # Display "Thank You" Message
    thanks = visual.TextStim(win, text="Thank you for completing the questionnaire!", color="white", height=30)
    thanks.draw()
    win.flip()
    core.wait(2)  # Display for 2 seconds

# 🔶 Get Participant Info
participant_number, csv_filename = get_participant_info()

# 🔶 Initialize PsychoPy Window
win = visual.Window(size=(800, 600), color="black", units="pix")

# 🔶 Practice
"""
show_instructions(win, "In this experiment, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "Respond as quickly and accurately as possible.", 1)
show_instructions(win,"click the RED button\n when you percieve RED\n\n"
                  "click the BLUE\n when you percieve BLUE\n\n", 1)
run_practice(win, (1, 1.2))
show_instructions(win, "Great job! now you will be moving on to the real task.\n\n"
                  "Respond as quickly and accurately as possible", 1)
"""
# 🔶 RUN TRIALS
"""
run_trials(win, participant_number, csv_filename, block_num=1, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "You may now take up to a one minute break...\n\n", 2)
show_instructions(win, "In the next block, you will either SEE a color, HEAR a color, or both.\n\n"
                  "This time, please respond to what you SEE, ignore what you hear.\n\n", 1)
show_instructions(win, "For example, if you SEE a RED circle but HEAR the word BLUE, the correct response is RED"
### MAYBE INCLUDE A PRACTICE ROUND WITH FEEDBACK ###
                  "Respond as quickly and accurately as possible.\n\n", 1)
run_trials(win, participant_number, csv_filename, block_num=2, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "You may now take up to a one minute break...\n\n", 2)
run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.25, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "You may now take up to a one minute break...\n\n", 2)
run_trials(win, participant_number, csv_filename, block_num=4, iti_range=(0.375, 1.5), total_trials=120, trial_types=["V", "A", "AVC", "AVI"], trial4=True)
show_instructions(win, "Great job! You have completed the task\n\n", 
                  "Now you will move on to a brief questionaire", 2)
"""
run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.25, 1.5), total_trials=8, trial_types=["AVI"])
# run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.25, 1.5), total_trials=8, trial_types=["V", "A", "AVC", "AVI"])

# run_post_experiment_questionnaire(win, participant_number, csv_filename)

# 🔶 Close the Experiment
win.close()
core.quit()