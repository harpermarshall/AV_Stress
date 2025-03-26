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
        csv_filename = os.path.abspath(os.path.join(data_folder, f"AV_Stress_Results_{participant_number}.csv"))

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
        win, text=text, color="white", height=45, 
        wrapWidth=1400, font="Arial Unicode MS"
    )
    continue_text = visual.TextStim(win, text="press space bar to continue",
                                    color="white", height=30, italic=True, pos=(0, -350))
    
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

def get_ready(win, text):
    """Displays a 'Get Ready' message followed by a 3-2-1 countdown."""
    
    # Create text stimulus
    text_stim = visual.TextStim(win, text=text, height=45, font="Arial Unicode MS")

    # Display the initial message for 4 seconds
    text_stim.draw()
    win.flip()
    core.wait(2)  

    # Countdown: 3, 2, 1
    for num in ["3", "2", "1"]:
        text_stim.text = num
        text_stim.draw()
        win.flip()
        core.wait(1)

    # Clear the screen after countdown
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
            if 'escape' in event.getKeys():
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()

            print(f"  🔹 Trial {i+1}: {trial}")  # Debugging print to check each trial

            if trial["audio"]:
                trial["audio"].stop() # Stops sound from previous audio trials

            event.clearEvents(eventType='keyboard')

            # Prepare visual stimulus
            if trial["visual"]:
                circle = visual.Circle(win, radius=75, fillColor=trial["visual"], lineColor=None)
            
            # Prepare auditory stimulus
            beep = trial["audio"]

            # Get time just before stimulus presentation
            audio_onset_time = core.getTime()

            # Play auditory stimulus immediately
            if beep:
                beep.play()
                print(f"🎵 Audio started at: {audio_onset_time:.3f} sec")
            
            # Introduce 300 ms delay before showing visual stimulus
            core.wait(0.3)
            # Start clock for RT recordings
            clock = core.Clock()

            # Remove the fixation cross just before stimulus onset
            fixation.autoDraw = False
            win.flip()

            # Now display the visual stimulus
            if trial["visual"]:
                circle.draw()
                win.flip()
                visual_onset_time = core.getTime()  # Get actual visual onset time
                print(f"🎨 Visual stimulus appeared at: {visual_onset_time:.3f} sec")

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
                    core.wait(0.6)  # Display feedback for 0.6 second
            else:
                key, rt = response[0]

            # Turn fixation cross on continuously
            fixation.autoDraw = True
            win.flip()

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

        # End of block — turn off fixation cross
    fixation.autoDraw = False
    win.flip()  # Clear the screen visually

# 🔷 Function to Run Practice
def run_practice(win, iti_range, total_trials, trial_types):
    response_keys = ["r", "b"]  # Response keys
    fixation = visual.TextStim(win, text="+", color="white", height=40)

    # Preload audio files
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.mp3",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.mp3"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    # Generate trials
    trials = []
    total_trial_types = len(trial_types)
    trials_per_type = total_trials // total_trial_types  # // rounds down to nearest int

    for trial_type in trial_types:
        colors = ["red"] * (trials_per_type // 2) + ["blue"] * (trials_per_type // 2)
        color_iterator = iter(colors)  # Assign colors in order

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

    # **Shuffle trials ONCE before running**
    random.shuffle(trials)

    # Run trials
    for i, trial in enumerate(trials):
        if 'escape' in event.getKeys():
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()
        
        print(f"🔹 Practice Trial {i+1}: {trial}")  # Debugging print

        if trial["audio"]:
            trial["audio"].stop()  # Stop previous sound

        # Show fixation cross
        fixation.draw()
        win.flip()
        core.wait(0.5)  # Fixation for 500ms

        event.clearEvents(eventType='keyboard')
        clock = core.Clock()

        # Prepare visual stimulus
        if trial["visual"]:
            circle = visual.Circle(win, radius=75, fillColor=trial["visual"], lineColor=None)

        # Prepare auditory stimulus
        beep = trial["audio"]

        # Start timing
        audio_onset_time = core.getTime()

        # Play auditory stimulus immediately
        if beep:
            beep.play()
            print(f"🎵 Audio started at: {audio_onset_time:.3f} sec")

        # Introduce 300 ms delay before showing visual stimulus
        core.wait(0.3)

        # Now display the visual stimulus
        if trial["visual"]:
            circle.draw()
            win.flip()
            visual_onset_time = core.getTime()  # Get actual visual onset time
            print(f"🎨 Visual stimulus appeared at: {visual_onset_time:.3f} sec")

        # Collect response
        response = event.waitKeys(maxWait=2.0, keyList=response_keys)
        win.flip()

        # Default values
        key, correct = "No Response", None
        if response:
            key = response[0]
            
        # Determine correctness
        if trial["type"] in ["V", "A", "AVC"]:
            expected_response = "b" if (trial["visual"] == "blue" or (trial["audio"] and "blue" in trial["audio"].fileName)) else "r"
            correct = key == expected_response

            if key == "No Response":
                correct = None
            else:
                correct = key == expected_response

            # Provide feedback
            if correct is None:
                feedback_text = visual.TextStim(win, text="Too Slow!", color="red", height=40)
            elif correct:
                feedback_text = visual.TextStim(win, text="✓ Correct", color="green", height=40)
            else:
                feedback_text = visual.TextStim(win, text="✗ Incorrect", color="red", height=40)

        feedback_text.draw()
        win.flip()
        core.wait(0.6)  # Show feedback for 0.6 second

        # **ITI should be here, after feedback**
        iti = random.uniform(iti_range[0], iti_range[1])
        core.wait(iti)  # Inter-Trial Interval

        print(f"✅ Practice Trial {i+1} completed. ITI: {round(iti, 3)}s")

# 🔷 Function to Run Post-Experiment Questionnaire
def run_post_experiment_questionnaire(win, participant_number, csv_filename):
    questions = [
        "Overall, I felt pressured to respond quickly during the task.",
        "In the LAST BLOCK, I felt MORE stressed than in previous blocks.",
        "I found the task mentally demanding.",
        "As the experiment progressed, I believe my performance IMPROVED.",  
        "When both sound and visual stimuli were presented, I relied more on the VISUAL information.",
        "When both sound and visual stimuli were presented, I relied more on the AUDITORY information.",
        "I was presented with MORE RED stimuli than blue stimuli throughout the experiment.",
        "I was presented with MORE BLUE stimuli than red stimuli throughout the experiment.",
        "The incongruent (mismatched) trials were harder than the congruent trials.",
        "I felt confident in my responses throughout the experiment."
    ]

    responses = []  # Store all responses

    # Define font and scale layout
    font_style = "Arial"
    scale_positions = [-600, -300, 0, 300, 600]
    labels = ["Strongly\nDisagree", "Disagree", "Neutral", "Agree", "Strongly\nAgree"]

    for question in questions:
        # Instructions (static)
        instruction_text = visual.TextStim(win, text="Use the keyboard (1-5) to select an answer.",
                                           font=font_style, color="lightgray", height=30, pos=(0, 380), bold=True)

        # Warning message (above the question, initially empty)
        warning_message = visual.TextStim(win, text="", font=font_style,
                                          color="red", height=30, pos=(0, 310), wrapWidth=1000, bold=True)

        # Question text (large and bold)
        question_text = visual.TextStim(win, text=question, font=font_style,
                                        color="white", height=50, wrapWidth=1400, pos=(0, 240), bold=True)

        # Scale line
        scale_line = visual.Line(win, start=(scale_positions[0], 0), end=(scale_positions[-1], 0),
                                 lineColor="lightgray", lineWidth=6)

        # Selection dots and labels
        dots = [visual.Circle(win, radius=30, fillColor="gray", lineColor="white", pos=(scale_positions[i], 0))
                for i in range(5)]
        dot_labels = [visual.TextStim(win, text=str(i+1), font=font_style, color="white",
                                      height=40, pos=(scale_positions[i], 60)) for i in range(5)]
        labels_text = [visual.TextStim(win, text=labels[i], font=font_style, color="lightgray",
                                       height=35, pos=(scale_positions[i], -100)) for i in range(5)]

        # "Press SPACE" message (hidden initially)
        continue_text = visual.TextStim(win, text="Press SPACE to confirm your response",
                                        font=font_style, color="white", height=40, pos=(0, -300))

        # Display initial screen
        instruction_text.draw()
        warning_message.draw()
        question_text.draw()
        scale_line.draw()
        for dot, dot_label, label in zip(dots, dot_labels, labels_text):
            dot.draw()
            dot_label.draw()
            label.draw()
        win.flip()

        # Start timing
        clock = core.Clock()
        selected_index = None  # Track selection
        space_prompt_shown = False  # Flag for "Press SPACE to continue"
        allow_space = False  # SPACE cannot be pressed until after delay

        while True:
            elapsed_time = clock.getTime()

            # Show "Press SPACE" message after 3 seconds
            if elapsed_time >= 3 and not space_prompt_shown:
                space_prompt_shown = True
                allow_space = True  # Now SPACE can be used

            # Always redraw everything
            instruction_text.draw()
            warning_message.draw()
            question_text.draw()
            scale_line.draw()
            for dot, dot_label, label in zip(dots, dot_labels, labels_text):
                dot.draw()
                dot_label.draw()
                label.draw()

            if space_prompt_shown:  # Ensure "Press SPACE" appears after delay
                continue_text.draw()

            win.flip()

            # Wait for valid key press
            keys = event.waitKeys(keyList=["1", "2", "3", "4", "5", "space", "escape"])

            if "escape" in keys:
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()

            if "space" in keys:
                if not allow_space:  # Prevent SPACE before 3 seconds
                    continue  
                if selected_index is None:  # If no selection, show warning
                    warning_message.text = "Select an option before continuing!"
                else:
                    responses.append(str(selected_index + 1))  # Save response
                    break  # Exit loop when response is recorded

            elif keys[0] in ["1", "2", "3", "4", "5"]:
                new_index = int(keys[0]) - 1

                # Reset previous selection
                if selected_index is not None:
                    dots[selected_index].fillColor = "gray"
                    dots[selected_index].lineColor = "white"

                # Update selection
                selected_index = new_index  
                dots[selected_index].fillColor = "green"  # Highlight selected response
                dots[selected_index].lineColor = "green"

                # Clear warning message when selection is made
                warning_message.text = ""

    # Save responses to CSV
    with open(csv_filename, "a", newline="") as file:
        writer = csv.writer(file)
        writer.writerow(["Participant"] + [f"Q{i+1}" for i in range(len(questions))])  # Header row
        writer.writerow([participant_number] + responses)  # Responses row

    # Display "Thank You" Message
    thanks = visual.TextStim(win, text="Thank you for completing the questionnaire!",
                             font=font_style, color="white", height=55, bold=True)
    thanks.draw()
    win.flip()
    core.wait(2)  # Display for 2 seconds

# 🔶 Get Participant Info
participant_number, csv_filename = get_participant_info()

# 🔶 Initialize PsychoPy Window
win = visual.Window(fullscr=True, color="black", units="pix")

""" 🔶 Practice
show_instructions(win, "In this experiment, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "Respond as quickly and accurately as possible.", 1)
show_instructions(win,"Press the RED button\n when you percieve RED\n\n"
                  "Press the BLUE\n when you percieve BLUE\n\n", 1)
get_ready(win, "Get Ready!\nPractice will begin in...") 
run_practice(win, iti_range=(1.75, 2), total_trials=12, trial_types=["V", "A"])
show_instructions(win, "Great job! now you will be moving on to the real task.\n\n"
                  "Respond as quickly and accurately as possible", 1)"""
get_ready(win, "Get Ready!\nTask will begin in...")                

# 🔶 RUN TRIALS
"""run_trials(win, participant_number, csv_filename, block_num=1, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "You may now take a brief break...\n\n"
                  "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed", 2)
show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "Respond as quickly and accurately as possible.", 1)
show_instructions(win,"Press the RED button\n when you percieve RED\n\n"
                  "Press the BLUE\n when you percieve BLUE\n\n", 1)
get_ready(win, "Get Ready!\nTask will begin in...")                 
run_trials(win, participant_number, csv_filename, block_num=2, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "You may now take a brief break...\n\n"
                  "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed", 2)
show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "Respond as quickly and accurately as possible.", 1)
show_instructions(win,"Press the RED button\n when you percieve RED\n\n"
                  "Press the BLUE\n when you percieve BLUE\n\n", 1)"""
get_ready(win, "Get Ready!\nTask will begin in...") 
run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "You may now take a brief break...\n\n"
                  "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed", 2)
show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "Respond as quickly and accurately as possible.", 1)
show_instructions(win,"Press the RED button\n when you percieve RED\n\n"
                  "Press the BLUE\n when you percieve BLUE\n\n", 1)
get_ready(win, "Get Ready!\nTask will begin in...") 
run_trials(win, participant_number, csv_filename, block_num=4, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"], trial4=True)
show_instructions(win, "Great job! You have completed the main portion of the task\n\n" 
                  "Now you will move on to a brief survey", 2)

# 🔶 RUN QUESTIONNAIRE
show_instructions(win, "This survey consists of 10 statements.\n\n"
                  "Please use the keyboard to rate your agreement with each statement on a scale from 1 to 5.\n\n", 1)
show_instructions(win,  "Press 1 if you strongly disagree, 5 if you strongly agree,\nor 2-4 for responses in between.", 1)
run_post_experiment_questionnaire(win, participant_number, csv_filename)

# FOR TROUBLE SHOOTING 
# show_instructions(win,"Press the RED button\n when you percieve RED\n\n"
#                 "Press the BLUE\n when you percieve BLUE\n\n", 1)
# get_ready(win, "Get Ready!\nTask will begin in...")      
# run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.75, 2), total_trials=8, trial_types=["AVI"])
# run_trials(win, participant_number, csv_filename, block_num=4, iti_range=(1.75, 2), total_trials=8, trial_types=["V", "A", "AVC", "AVI"])
# run_practice(win, iti_range=(1.25, 1.5), total_trials=12, trial_types=["V", "A"])
# run_post_experiment_questionnaire(win, participant_number, csv_filename)

# 🔶 Close the Experiment
win.close()
core.quit()