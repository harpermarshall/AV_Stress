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
    # Use a non-blocking wait loop to avoid OS event bug instead of "core.wait(duration)"
    wait_clock = core.Clock()
    while wait_clock.getTime() < duration:
        if event.getKeys(['escape']):
            print("Escape key pressed during wait! Exiting...")
            win.close()
            core.quit()
        pass
    instructions.draw()
    continue_text.draw()
    win.flip()

    event.clearEvents(eventType='keyboard')
    while True:
        keys = event.getKeys()
        if 'escape' in keys:
            print("Escape key pressed! Exiting...")
            win.close()
            core.quit()
        elif 'space' in keys:
            break

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
        iti_end = 0  # Desired lower bound for trial4
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
                trial["audio"].stop()  # Stops sound from previous audio trials

            # Clear events at the start of the trial
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
            #core.wait(0.04) #uncomment if visual stimuli appear too early
            
            # Remove the fixation cross just before stimulus onset
            fixation.autoDraw = False
            win.flip()

            # Now display the visual stimulus
            if trial["visual"]:
                circle.draw()
                win.flip()
                visual_onset_time = core.getTime()  # Get actual visual onset time
                print(f"🎨 Visual stimulus appeared at: {visual_onset_time:.3f} sec")

            # Clear any residual key events immediately after the visual stimulus is shown
            event.clearEvents(eventType='keyboard')

            # Start the RT clock after clearing events
            clock = core.Clock()
            start_time = clock.getTime()

            # Start response collection from the correct time
            response = None
            key, rt = "No Response", None

            while response is None and clock.getTime() - start_time < .850:
                keys = event.getKeys(timeStamped=clock)
                for k in keys:
                    key_name, key_rt = k
                    if key_name.lower() in response_keys:  # handles 'r' or 'b' (case-insensitive)
                        response = (key_name.lower(), key_rt)
                        break
                    elif key_name == "escape":
                        print("Escape key pressed! Exiting...")
                        win.close()
                        core.quit()
                core.wait(0.01)

            # Handle response outcome
            if response is not None:
                key, rt = response
            else:
                if trial4:
                    feedback_text = visual.TextStim(win, text="Too Slow", color="red", height=40)
                    feedback_text.draw()
                    win.flip()
                    core.wait(0.6)  # Display feedback for 0.6 second

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

        # Collect response safely
        clock = core.Clock()
        response = None
        key, rt, correct = "No Response", None, None
        start_time = clock.getTime()

        while response is None and clock.getTime() - start_time < 0.850:
            keys = event.getKeys(timeStamped=clock)
            for k in keys:
                key_name, key_rt = k
                if key_name.lower() in response_keys:
                    response = (key_name.lower(), key_rt)
                    break
                elif key_name == "escape":
                    print("Escape key pressed! Exiting...")
                    win.close()
                    core.quit()
            core.wait(0.01)

        # Evaluate response
        if response is not None:
            key, rt = response

        # Determine correctness
        if trial["type"] in ["V", "A", "AVC"]:
            expected_response = "b" if (trial["visual"] == "blue" or (trial["audio"] and "blue" in trial["audio"].fileName)) else "r"
            correct = key == expected_response

            # Provide feedback
            if rt is None:
                feedback_text = visual.TextStim(win, text="Too Slow!", color="red", height=40)
            elif correct:
                feedback_text = visual.TextStim(win, text="✓ Correct", color="green", height=40)
            else:
                feedback_text = visual.TextStim(win, text="✗ Incorrect", color="red", height=40)

            feedback_text.draw()
            win.flip()
            core.wait(0.6)

        # **ITI should be here, after feedback**
        iti = random.uniform(iti_range[0], iti_range[1])
        core.wait(iti)  # Inter-Trial Interval

        print(f"✅ Practice Trial {i+1} completed. ITI: {round(iti, 3)}s")

# 🔷 Function to Run Experiment Questionnaire
def run_experiment_questionnaire(win, participant_number, questions, block_num):
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
        selected_index = None
        space_prompt_shown = False
        allow_space = False
        done = False

        while not done:
            elapsed_time = clock.getTime()

            # Show "Press SPACE to continue" only if both conditions are met
            if elapsed_time >= 3 and selected_index is not None:
                space_prompt_shown = True
                allow_space = True

            # Always redraw everything
            instruction_text.draw()
            warning_message.draw()
            question_text.draw()
            scale_line.draw()
            for dot, dot_label, label in zip(dots, dot_labels, labels_text):
                dot.draw()
                dot_label.draw()
                label.draw()

            if space_prompt_shown:
                continue_text.draw()

            win.flip()

            # Check keys
            keys = event.getKeys()
            for key in keys:
                if key == "escape":
                    print("Escape key pressed! Exiting...")
                    win.close()
                    core.quit()

                elif key == "space":
                    if allow_space:
                        responses.append(str(selected_index + 1))
                        done = True
                        break
                    else:
                        warning_message.text = "Wait 3 seconds AND make a selection before continuing."

                elif key in ["1", "2", "3", "4", "5"]:
                    new_index = int(key) - 1

                    # Reset previous selection
                    if selected_index is not None:
                        dots[selected_index].fillColor = "gray"
                        dots[selected_index].lineColor = "white"

                    # Update selection
                    selected_index = new_index
                    dots[selected_index].fillColor = "green"
                    dots[selected_index].lineColor = "green"
                    warning_message.text = ""

    # Save responses to a per-participant survey CSV
    data_folder = "AV_Stress_Data"
    os.makedirs(data_folder, exist_ok=True)
    survey_filename = os.path.join(data_folder, f"AV_Stress_Survey_{participant_number}.csv")
    file_exists = os.path.exists(survey_filename)

    with open(survey_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["Participant", "Block"] + [f"Q{i+1}" for i in range(len(questions))])  # Header
        writer.writerow([participant_number, block_num] + responses)  # One row per block

block_questions = [
    "1. I felt alert and focused during this section.",
    "2. I felt motivated to do well during this section.",
    "3. I felt frustrated or irritated during this section.",
    "4. I found this section emotionally draining.",
    "5. I felt stressed or tense during this section.",
    "6. I felt overwhelmed by the demands of this section.",
    "7. I was concerned about how well I was doing during this section.",
    "8. I found myself distracted during this section.",
    "9. I was worried I was making too many mistakes in this section",
    "10. I used a specific strategy to help me respond during this entirety of this section.",
    "11. This section felt more difficult than the others.",
    "12. I found this section enjoyable.",
    "13. I changed my strategy during the section.",
    "14. I relied more on the AUDIO than the visual information in this section.",
    "15. I relied more on the VISUAL than the audio information in this section.",
]

# RUNNING EXPERIMENT ----------------------------------------------------------------------------------------------------------------------------------------------

# 🔶 Get Participant Info
participant_number, csv_filename = get_participant_info()

# 🔶 Initialize PsychoPy Window
win = visual.Window(fullscr=True, color="black", units="pix")

"""# 🔶 Practice
show_instructions(win, "This experiment will be broken up into 4 main sections with a short survey after each section.\n\n"
                  "You will have the option to take a brief break after each survey.", 8)
show_instructions(win, "In the main portion of this experiment, you will either:\n\nSEE a colored circle,\nHEAR the name of a color,\nor BOTH.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "In between trials, keep your eyes on the fixation cross in the center of the screen", 9)
show_instructions(win,"Press the RED button\n when you percieve RED.\n\n"
                  "Press the BLUE button\n when you percieve BLUE.\n\n"
                  "You can practice selecting the correct color in this short practice section.", 8)
get_ready(win, "Get Ready!\nPractice will begin in...") 
run_practice(win, iti_range=(1.75, 2), total_trials=12, trial_types=["V", "A"])
show_instructions(win, "Great job!\n\nIn the real task, you will not be told if your responses are correct or incorrect like you saw in the practice.", 8)
show_instructions(win, "Now you will be moving on to the real task.\n\n"
                  "Just like the practice, you will have less than a second to respond after the color is presented.\n\n"
                  "Respond as quickly and accurately as possible.", 6)
show_instructions(win,"Press the RED button\n when you percieve RED.\n\n"
                  "Press the BLUE button\n when you percieve BLUE.", 3) """                

# 🔶 RUN TRIALS
        # block 1
get_ready(win, "Get Ready!\nSection 1 will begin in...")
run_trials(win, participant_number, csv_filename, block_num=1, iti_range=(1.75, 2), total_trials=120, trial_types=["AVC", "AVI"])
show_instructions(win, "Great job, you competed Section 1! You will now move on to a brief survey.", 3)
show_instructions(win, "This survey consists of 15 statements.\n\n"
                  "Please use the keyboard to rate your agreement with each statement on a scale from 1 to 5.\n\n", 
                  "Answer based off of your experience in Section 1 ONLY", 8)
show_instructions(win,  "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 4)
run_experiment_questionnaire(win, participant_number, block_questions, block_num=1)
show_instructions(win, "You may now take a brief break...\n\n"
                  "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 7)
        # block 2
show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "You will have less than a second to respond after the color is presented.\n\n"
                  "Respond as quickly and accurately as possible.", 6)
show_instructions(win,"Press the RED button\n when you percieve RED.\n\n"
                  "Press the BLUE button\n when you percieve BLUE.", 3)
get_ready(win, "Get Ready!\nSection 2 will begin in...")                 
run_trials(win, participant_number, csv_filename, block_num=2, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "Great job, you completed Section 2! You will now move on to another 15 question survey.", 2)
show_instructions(win, "Answer based off of your experience in Section 2 ONLY", 2)
show_instructions(win,  "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 4)
run_experiment_questionnaire(win, participant_number, block_questions, block_num=2)
show_instructions(win, "You may now take a brief break...\n\n"
                  "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 7)
        # block 3
show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "You will have less than a second to respond after the color is presented.\n\n"
                  "Respond as quickly and accurately as possible.", 6)
show_instructions(win,"Press the RED button\n when you percieve RED.\n\n"
                  "Press the BLUE button\n when you percieve BLUE.", 3)
get_ready(win, "Get Ready!\nSection 3 will begin in...") 
run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"])
show_instructions(win, "Great job, you completed Section 3! You will now move on to another 15 question survey.", 2)
show_instructions(win, "Answer based off of your experience in Section 3 ONLY", 8)
show_instructions(win, "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 4)
run_experiment_questionnaire(win, participant_number, block_questions, block_num=3)
show_instructions(win, "You may now take a brief break...\n\n"
                  "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 7)
        # block 4
show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                  "Your task is to press the button that matches the perceived color.\n\n"
                  "You will have less than a second to respond after the color is presented.\n\n"
                  "Respond as quickly and accurately as possible.", 6)
show_instructions(win,"Press the RED button\n when you percieve RED.\n\n"
                  "Press the BLUE button\n when you percieve BLUE.", 3)
get_ready(win, "Get Ready!\nSection 4 will begin in...") 
run_trials(win, participant_number, csv_filename, block_num=4, iti_range=(1.75, 2), total_trials=120, trial_types=["V", "A", "AVC", "AVI"], trial4=True)
show_instructions(win, "Great job, you completed Section 4! You will now move on to your last 15 question survey.", 2)
show_instructions(win, "Answer based off of your experience in Section 4 ONLY", 8)
show_instructions(win, "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 4)
run_experiment_questionnaire(win, participant_number, block_questions, block_num=4)
show_instructions(win, "You have now completed the experiment!\n",
                  "Thank you so much for participating!", 3) 

# FOR TROUBLE SHOOTING 
# show_instructions(win,"Press the RED button\n when you percieve RED\n\n"
#                 "Press the BLUE\n when you percieve BLUE\n\n", 1)
# get_ready(win, "Get Ready!\nTask will begin in...")      
# run_trials(win, participant_number, csv_filename, block_num=3, iti_range=(1.75, 2), total_trials=120, trial_types=["AVC"])
# run_trials(win, participant_number, csv_filename, block_num=4, iti_range=(1.00, 1.25), total_trials=120, trial_types=["V", "A", "AVC", "AVI"], trial4 = True)
# run_practice(win, iti_range=(1.25, 1.5), total_trials=12, trial_types=["V", "A"])
# run_experiment_questionnaire(win, participant_number, block_questions, block_num=4)

# 🔶 Close the Experiment
win.close()
core.quit()

