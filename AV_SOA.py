from psychopy import visual, core, event, gui, sound
from psychopy import prefs
prefs.hardware['audioLib'] = ['PTB']
import random
import csv
import os

# 🔷 Function to Get Participant Info & Handle File Overwriting
def get_participant_info():
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
        base_data_folder = "AV_SOA_Data"
        participant_folder = os.path.join(base_data_folder, participant_number)

        # 🚨 Check if folder exists
        warning_text = ""
        if os.path.exists(participant_folder):
            warning_text = f"\n\n⚠️ WARNING: Folder for Participant {participant_number} already exists!\nIt may contain previously saved data."

        os.makedirs(participant_folder, exist_ok=True)

        confirm_dlg = gui.Dlg(title="Confirm Participant Info")
        confirm_dlg.addText(f"Is this correct?\n📂 Folder: {participant_folder}{warning_text}")
        confirm_dlg.addField("Confirm", choices=["Yes", "No"])
        confirm_dlg.show()

        if confirm_dlg.data[0] == "Yes":
            print(f"\n✅ Data will be saved in: {participant_folder}\n")
            return participant_number
        else:
            print("\nRe-entering participant number...\n")

# 🔷 Function to Show Written Instructions
def show_instructions(win, text, duration):
    instructions = visual.TextStim(
        win, text=text, color="white", height=45, 
        wrapWidth=1400, font="Arial Unicode MS"
    )
    continue_text = visual.TextStim(win, text="press space bar to continue",
                                    color="white", height=30, italic=True, pos=(0, -390))
    
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

# ♦️ Function to Run Single Trial (called within run_trials)
def run_single_trial(trial, stim_offset, win, response_keys):
    event.clearEvents(eventType='keyboard')

    rt_clock = core.Clock()  # Start RT clock immediately

    circle = visual.Circle(win, radius=75, fillColor=trial["visual"], lineColor=None) if trial["visual"] else None
    beep = trial["audio"]

    response, rt = None, None 

    if stim_offset <= 0:
        if trial["visual"]:
            circle.draw()
            win.flip()
            visual_onset_time = core.getTime()
            rt_clock.reset()

            event.clearEvents(eventType='keyboard')
            response = None
            rt = None

            print(f"🎨 Visual onset: {visual_onset_time:.3f} sec")

        wait_clock = core.Clock()
        while wait_clock.getTime() < abs(stim_offset) and response is None:
            keys = event.getKeys(timeStamped=rt_clock)
            for k in keys:
                key_name, key_rt = k
                if key_name in response_keys:
                    response = key_name
                    rt = key_rt
                    break
                elif key_name == "escape":
                    print("Escape key pressed! Exiting...")
                    win.close()
                    core.quit()
            core.wait(0.01)

        if trial["audio"] and response is None:
            beep.play()
            audio_onset_time = core.getTime()
            print(f"🎵 Audio onset: {audio_onset_time:.3f} sec")

    elif stim_offset > 0:
        if trial["audio"]:
            beep.play()
            audio_onset_time = core.getTime()
            rt_clock.reset()

            event.clearEvents(eventType='keyboard')
            response = None
            rt = None

            print(f"🎵 Audio onset: {audio_onset_time:.3f} sec")

        wait_clock = core.Clock()
        while wait_clock.getTime() < stim_offset and response is None:
            keys = event.getKeys(timeStamped=rt_clock)
            for k in keys:
                key_name, key_rt = k
                if key_name in response_keys:
                    response = key_name
                    rt = key_rt
                    break
                elif key_name == "escape":
                    print("Escape key pressed! Exiting...")
                    win.close()
                    core.quit()
            core.wait(0.01)

        if trial["visual"] and response is None:
            circle.draw()
            win.flip()
            visual_onset_time = core.getTime()
            print(f"🎨 Visual onset: {visual_onset_time:.3f} sec")

    while response is None:
        keys = event.getKeys(timeStamped=rt_clock)
        for k in keys:
            key_name, key_rt = k
            if key_name in response_keys:
                response = key_name
                rt = key_rt
                break
            elif key_name == "escape":
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()
        core.wait(0.01)

    # Clip negative RT if somehow happens
    if rt is not None and rt < 0:
        rt = 0.0

    correct = None
    if trial["type"] in ["V", "A", "AVC"]:
        expected_response = "b" if (trial["visual"] == "blue" or (trial["audio"] and "blue" in trial["audio"].fileName)) else "r"
        correct = (response == expected_response)
    elif trial["type"] == "AVI":
        correct = "NA"

    return response, rt, correct

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

        if trial["audio"]:
            trial["audio"].stop()  # Stop previous sound

        event.clearEvents(eventType='keyboard')
        clock = core.Clock()

        # Stop fixation cross before stimuli are presented
        fixation.autoDraw = False
        win.flip()

        # Prepare visual stimulus
        if trial["visual"]:
            circle = visual.Circle(win, radius=75, fillColor=trial["visual"], lineColor=None)

        # Prepare auditory stimulus
        beep = trial["audio"]

        # Start timing
        audio_onset_time = core.getTime()

        # Play auditory stimulus
        if beep:
            beep.play()
            print(f"🎵 Audio started at: {audio_onset_time:.3f} sec")

        # Display the visual stimulus
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
            core.wait(0.4)
            # Start new fixation cross with ITI wait
            fixation.draw()
            win.flip()
            core.wait(random.uniform(*iti_range))

# 🔷 Function to Run Trials
def run_trials(win, participant_number, block_num, iti_range, stim_offset, total_trials):

    base_data_folder = "AV_SOA_Data"
    participant_folder = os.path.join(base_data_folder, participant_number)
    os.makedirs(participant_folder, exist_ok=True)
    csv_filename = os.path.join(participant_folder, f"AV_Trials_Results_{participant_number}.csv")

    response_keys = ["r", "b"]
    fixation = visual.TextStim(win, text="+", color="white", height=40)

    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.mp3",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.mp3"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    file_exists = os.path.exists(csv_filename)
    with open(csv_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["Participant", "Block", "Trial", "Type", "Visual", "Audio", "Response", "RT", "Correct", "Visual Delay"])

        trials = []
        proportions = {"A": 0.3, "V": 0.3, "AVC": 0.3, "AVI": 0.1}
        trial_counts = {ttype: int(total_trials * prop) for ttype, prop in proportions.items()}
        
        # Adjust for rounding errors to ensure total trials adds up correctly
        while sum(trial_counts.values()) < total_trials:
            for ttype in ["A", "V", "AVC", "AVI"]:
                if sum(trial_counts.values()) < total_trials:
                    trial_counts[ttype] += 1
        
        for trial_type, count in trial_counts.items():
            colors = ["red"] * (count // 2) + ["blue"] * (count // 2)
            # In case of an odd count, randomly add one extra color
            if count % 2 != 0:
                colors.append(random.choice(["red", "blue"]))
            random.shuffle(colors)
            color_iterator = iter(colors)
        
            for _ in range(count):
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

        random.shuffle(trials)

        for i, trial in enumerate(trials):
            if 'escape' in event.getKeys():
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()

            print(f"  🔹 Trial {i+1}: {trial}")

            response, rt, correct = run_single_trial(trial, stim_offset, win, response_keys)

            writer.writerow([
                participant_number,
                block_num,
                i + 1,
                trial["type"],
                trial["visual"] if trial["visual"] is not None else "NA",
                os.path.basename(trial["audio"].fileName) if trial["audio"] else "NA",
                response if response is not None else "NA",
                rt if rt is not None else "NA",
                correct if correct is not None else "NA",
                stim_offset
            ])

            # After response, immediately begin ITI
            fixation.autoDraw = True
            iti = random.uniform(iti_range[0], iti_range[1])
            wait_clock = core.Clock()
            while wait_clock.getTime() < iti:
                win.flip()
                core.wait(0.01)
            fixation.autoDraw = False
            win.flip()

# 🔷 Function for SOA Test
def run_soa_test(win, participant_number, iti_range=(1.9,2.1), soa_values_ms=[-200, -150, -100, -50, 0, 50, 100, 150, 200, 300, 400]):
    """
    Runs an SOA test block with 3 repetitions of each SOA x congruency x color combo,
    fully randomized across all 120 trials. Records only essential trial data.
    """
    fixation = visual.TextStim(win, text="+", color="white", height=40)
    response_keys = ["s", "a"]
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.mp3",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.mp3"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    # Save to SOA-specific file
    if isinstance(participant_number, int):
        participant_number = f"P{int(participant_number):03d}"
    base_data_folder = "AV_SOA_Data"
    participant_folder = os.path.join(base_data_folder, participant_number)
    os.makedirs(participant_folder, exist_ok=True)
    soa_filename = os.path.join(participant_folder, f"AV_SOA_Results_{participant_number}.csv")
    file_exists = os.path.exists(soa_filename)

    with open(soa_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["Participant", "Block", "Trial", "Type", "Visual", "Audio", "SOA_ms", "Response"])

        trials = []
        trial_types = ["AVC"]
        colors = ["red", "blue"]
        reps = 2  # repeat every combination 2 times

        for _ in range(reps):
            for soa in soa_values_ms:
                for ttype in trial_types:
                    for color in colors:
                        trial = {"type": ttype, "visual": color, "soa": soa}
                        if ttype == "AVC":
                            trial["audio"] = preloaded_sounds[color]
                        trials.append(trial)

        random.shuffle(trials)

        for i, trial in enumerate(trials):
            if 'escape' in event.getKeys():
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()

            if trial["audio"]:
                trial["audio"].stop()
            event.clearEvents(eventType='keyboard')

            if trial["visual"]:
                circle = visual.Circle(win, radius=75, fillColor=trial["visual"], lineColor=None)

            beep = trial["audio"]
            soa_sec = trial["soa"] / 1000.0

            if trial["soa"] < 0:
                # Show visual first
                if trial["visual"]:
                    circle.draw()
                    win.flip()
                    visual_onset = core.getTime()
                    print(f"🎨 Visual appeared at: {visual_onset:.3f} sec (SOA: {trial['soa']} ms)")
                core.wait(abs(soa_sec))  # Wait before playing audio
                if beep:
                    beep.play()
                    print(f"🎵 Audio started at: {core.getTime():.3f} sec")
            else:
                # Play audio first
                if beep:
                    beep.play()
                    print(f"🎵 Audio started at: {core.getTime():.3f} sec")
                core.wait(soa_sec)
                if trial["visual"]:
                    circle.draw()
                    win.flip()
                    visual_onset = core.getTime()
                    print(f"🎨 Visual appeared at: {visual_onset:.3f} sec (SOA: {trial['soa']} ms)")

            # Start RT recording after visual with enhanced key processing
            response = None
            while response is None:
                try:
                    raw_keys = event.getKeys()
                    keys = []
                    for k in raw_keys:
                        try:
                            keys.append(str(k[0]) if isinstance(k, tuple) else str(k))
                        except Exception as inner_e:
                            print("Nested key conversion error during SOA test:", inner_e, k)
                except Exception as e:
                    print("Top-level key processing error during SOA test:", e)
                    keys = []

                for k in keys:
                    if k in response_keys:
                        response = k
                        break
                    elif k == "escape":
                        print("Escape key pressed! Exiting...")
                        win.close()
                        core.quit()
                core.wait(0.01)

            key = response if response else "No Response"

            fixation.autoDraw = False
            win.flip()

            writer.writerow([
                participant_number,
                "SOA_Test",
                i + 1,
                trial["type"],
                trial["visual"],
                os.path.basename(trial["audio"].fileName) if trial["audio"] else "NA",
                trial["soa"],
                key
            ])

            print(f"  ✅ SOA Trial {i+1} complete.")

            # Show fixation during ITI
            fixation.draw()
            win.flip()
            core.wait(random.uniform(*iti_range))

    fixation.autoDraw = False
    win.flip()

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
    base_data_folder = "AV_SOA_Data"
    participant_folder = os.path.join(base_data_folder, participant_number)
    os.makedirs(participant_folder, exist_ok=True)
    survey_filename = os.path.join(participant_folder, f"AV_Strategy_Survey_{participant_number}.csv")
    file_exists = os.path.exists(survey_filename)

    with open(survey_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["Participant", "Block"] + [f"Q{i+1}" for i in range(len(questions))])  # Header
        writer.writerow([participant_number, block_num] + responses)  # One row per block

block_questions = [
    "1. The AUDIO cues influenced my responses more than the visual cues.",
    "2. The VISUAL cues influenced my responses more than the audio cues.",
    "3. I felt like I had a single, specific strategy throughout the entirety of this section.",
    "4. I had a strategy, but I feel like my strategy changed during this section.",
    "5. I do not feel like I had a specific strategy during this section.",
]

# 🔷 Function to Run Experiment ----------------------------------------------------------------------------------------------------------------------------------------
def run_full_experiment():
    
    # 🔶 Get Participant Info
    participant_number = get_participant_info()

    # 🔶 Initialize PsychoPy Window
    win = visual.Window(fullscr=True, color="black", units="pix")

    # SOA Test 
    show_instructions(win, 
        "Before we begin the main task, you'll complete a short section where you'll judge the TIMING between what you SEE and what you HEAR.",
        3)
    show_instructions(win,
        "In each trial, you will either:\n\n"
        "SEE a BLUE circle and HEAR the word 'BLUE' spoken out loud or\n"
        "SEE a RED circle and HEAR the word 'RED' spoken out loud.\n\n"
        "Your job is to decide whether the visual and audio started at the SAME TIME or at DIFFERENT TIMES.",
        14)
    show_instructions(win,
        "Use the white 'S' and 'A' buttons on the button box to respond:\n"
        "Press 'S' if they occurred together (SYNCHRONOUS).\n"
        "Press 'A' if one came before the other (ASYNCHRONOUS).\n\n"
        "There is NO TIME LIMIT — respond at your own pace.", 
        14)
    show_instructions(win, 
        "To recap:\n\n"
        "Press 'S' if the visual and audio cues happened TOGETHER.\n"
        "Press 'A' if one cue came BEFORE the other.\n\n"
        "You can take your time to respond — there is no time limit in this section.", 
        9)
    show_instructions(win, 
        "The next screen will show a short countdown to help you get ready.\n\n"
        "After each response, there will be a brief pause before the next trial begins.", 
        6)
    get_ready(win, "Get Ready!\nThis task will begin in...")
    run_soa_test(win, participant_number)

    # Randomize visual delays for blocks 1–5
    stim_offsets = [0, 0.05, 0.1, 0.15, 0.2]
    random.shuffle(stim_offsets)

    # Establish number of total trials for run_trials in block 1-5
    total_trials = 80

    # 🔶 Practice
    show_instructions(win, 
        "The rest of this experiment will be broken up into 5 main sections with a short survey after each section.\n\n"
        "You will have the option to take a brief break after each survey.", 
        8)
    show_instructions(win, 
        "In the main portion of this experiment, you will either:\n\nSEE a colored circle,\nHEAR the name of a color,\nor BOTH.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "In between trials, keep your eyes on the fixation cross in the center of the screen", 
        9)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.\n\n"
        "However, this part of the experiment is timed.\n"
        "You will have less than a second to respond\n\n"
        "You can practice selecting the correct color in this short practice section.", 
        12)
    get_ready(win, "Get Ready!\nPractice will begin in...") 
    run_practice(win, iti_range=(1, 1.25), total_trials=8, trial_types=["V", "A"])
    show_instructions(win, 
        "Great job!\n\n"
        "In the real task, you will not be told if your responses are correct or incorrect like you saw in the practice.", 
        8)

    # 🔶 RUN TRIALS
            # block 1
    show_instructions(win, 
        "Now you will be moving on to the real task.\n\n"
        "Just like the practice, you will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win, 
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)                
    get_ready(win, "Get Ready!\nSection 1 will begin in...")
    run_trials(win, participant_number, block_num=1, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[0])
    show_instructions(win, 
        "Great job, you completed Section 1! You will now move on to a brief survey.", 
        3)
    show_instructions(win, 
        "This survey consists of 5 statements.\n\n"
        "Please use the keyboard to rate your agreement with each statement on a scale from 1 to 5.\n\n" 
        "Answer based on your experience in Section 1 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=1)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

            # block 2
    show_instructions(win, 
        "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "You will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)
    get_ready(win, "Get Ready!\nSection 2 will begin in...")                 
    run_trials(win, participant_number, block_num=2, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[1])
    show_instructions(win, 
        "Great job, you completed Section 2! You will now move on to another 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 2 ONLY", 
        2)
    show_instructions(win,  
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=2)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

            # block 3
    show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                    "Your task is to press the button that matches the perceived color.\n\n"
                    "You will have less than a second to respond after the color is presented.\n\n"
                    "Respond as quickly and accurately as possible.", 6)
    show_instructions(win,"Press the RED button\n when you perceive RED.\n\n"
                    "Press the BLUE button\n when you perceive BLUE.", 3)
    get_ready(win, "Get Ready!\nSection 3 will begin in...") 
    run_trials(win, participant_number, block_num=3, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[2])
    show_instructions(win, 
        "Great job, you completed Section 3! You will now move on to another 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 3 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=3)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)
            # block 4
    show_instructions(win, 
        "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "You will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)
    get_ready(win, "Get Ready!\nSection 4 will begin in...") 
    run_trials(win, participant_number, block_num=4, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[3])
    show_instructions(win, 
        "Great job, you completed Section 3! You will now move on to another 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 4 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=4)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

            # block 5
    show_instructions(win, 
        "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "You will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)
    get_ready(win, "Get Ready!\nSection 5 will begin in...") 
    run_trials(win, participant_number, block_num=5, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[4])
    show_instructions(win, 
        "Great job, you completed Section 5! You will now move on to your last 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 5 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=5)
    show_instructions(win, 
        "Congratulations! You have completed the main portion of the experiment.\nYou may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)
    show_instructions(win, 
        "You have now completed the experiment!\n"
        "Thank you so much for participating!", 
        3)  
    
    # 🔶 Close the Experiment
    win.close()
    core.quit()

# 🔷 Function to Run Trials Only ----------------------------------------------------------------------------------------------------------------------------------------
def run_trials_only():

    # 🔶 Get Participant Info
    participant_number = get_participant_info()

    # 🔶 Initialize PsychoPy Window
    win = visual.Window(fullscr=True, color="black", units="pix")

    # Randomize visual delays for blocks 1–5
    stim_offsets = [0, 0.05, 0.1, 0.15, 0.2]
    random.shuffle(stim_offsets)

    # Establish number of total trials for run_trials in block 1-5
    total_trials = 80

    # 🔶 RUN TRIALS
            # block 1
    show_instructions(win, 
        "Now you will be moving on to the real task.\n\n"
        "Just like the practice, you will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win, 
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)                
    get_ready(win, "Get Ready!\nSection 1 will begin in...")
    run_trials(win, participant_number, block_num=1, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[0])
    show_instructions(win, 
        "Great job, you completed Section 1! You will now move on to a brief survey.", 
        3)
    show_instructions(win, 
        "This survey consists of 5 statements.\n\n"
        "Please use the keyboard to rate your agreement with each statement on a scale from 1 to 5.\n\n" 
        "Answer based on your experience in Section 1 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=1)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

            # block 2
    show_instructions(win, 
        "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "You will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)
    get_ready(win, "Get Ready!\nSection 2 will begin in...")                 
    run_trials(win, participant_number, block_num=2, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[1])
    show_instructions(win, 
        "Great job, you completed Section 2! You will now move on to another 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 2 ONLY", 
        2)
    show_instructions(win,  
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=2)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

            # block 3
    show_instructions(win, "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
                    "Your task is to press the button that matches the perceived color.\n\n"
                    "You will have less than a second to respond after the color is presented.\n\n"
                    "Respond as quickly and accurately as possible.", 6)
    show_instructions(win,"Press the RED button\n when you perceive RED.\n\n"
                    "Press the BLUE button\n when you perceive BLUE.", 3)
    get_ready(win, "Get Ready!\nSection 3 will begin in...") 
    run_trials(win, participant_number, block_num=3, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[2])
    show_instructions(win, 
        "Great job, you completed Section 3! You will now move on to another 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 3 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=3)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)
            # block 4
    show_instructions(win, 
        "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "You will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)
    get_ready(win, "Get Ready!\nSection 4 will begin in...") 
    run_trials(win, participant_number, block_num=4, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[3])
    show_instructions(win, 
        "Great job, you completed Section 3! You will now move on to another 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 4 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=4)
    show_instructions(win, 
        "You may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

            # block 5
    show_instructions(win, 
        "Like before, you will either SEE a color, HEAR a color, or both.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "You will have less than a second to respond after the color is presented.\n\n"
        "Respond as quickly and accurately as possible.", 
        6)
    show_instructions(win,
        "Press the RED button\n when you perceive RED.\n\n"
        "Press the BLUE button\n when you perceive BLUE.", 
        3)
    get_ready(win, "Get Ready!\nSection 5 will begin in...") 
    run_trials(win, participant_number, block_num=5, iti_range=(1.75, 2), total_trials=total_trials, stim_offset=stim_offsets[4])
    show_instructions(win, 
        "Great job, you completed Section 5! You will now move on to your last 5-question survey.", 
        2)
    show_instructions(win, 
        "Answer based on your experience in Section 5 ONLY", 
        8)
    show_instructions(win, 
        "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
        4)
    run_experiment_questionnaire(win, participant_number, block_questions, block_num=5)
    show_instructions(win, 
        "Congratulations! You have completed the main portion of the experiment.\nYou may now take a brief break...\n\n"
        "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
        7)

    show_instructions(win, 
        "You have now completed the experiment!\n"
        "Thank you so much for participating!", 
        3)  
    
    # 🔶 Close the Experiment
    win.close()
    core.quit()

# 🔶 RUN EXPERIMENT
#run_full_experiment()

# 🔶 RUN EXPERIMENT
#run_trials_only()

# DUMMY MODE -----------------------------------------------------------------------------------------------------------------------------------------------------------
win = visual.Window(fullscr=True, color="black", units="pix")
get_ready(win, "Get Ready!\nTask will begin in...")
#run_practice(win, iti_range=(1.25, 1.5), total_trials=8, trial_types=["V", "A"])
run_trials(win, "P999", block_num=1, iti_range=(1, 1.25), total_trials=20, stim_offset=.5)
#run_trials(win, "P999", block_num=2, iti_range=(1, 1.25), total_trials=5, stim_offset=0.05)
#run_trials(win, "P999", block_num=3, iti_range=(1, 1.25), total_trials=5, stim_offset=0.1)
#run_trials(win, "P999", block_num=4, iti_range=(1, 1.25), total_trials=5, stim_offset=0.15)
#run_trials(win, "P999", block_num=5, iti_range=(1, 1.25), total_trials=5, stim_offset=0.20)
#run_soa_test(win, "P999")
#run_experiment_questionnaire(win, "P999", block_questions, block_num=1)
win.close()
core.quit()