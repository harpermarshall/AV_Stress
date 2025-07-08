### SEMANTIC CONGRUENCY TASK ###

from psychopy import visual, core, event, gui, sound
from psychopy import prefs
prefs.hardware['audioLib'] = ['PTB']
import random
import csv
import os

auto_response = False  # Set to True to simulate response after 0.5s

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
        base_data_folder = "SCT_Data"
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
        core.wait(0.01)

    win.flip()

def get_ready(win, text):
    """Displays a 'Get Ready' message followed by a 3-2-1 countdown."""
    
    # Create text stimulus
    text_stim = visual.TextStim(win, text=text, height=45, font="Arial Unicode MS")

    # Display the initial message for 4 seconds
    text_stim.draw()
    win.flip()
    wait_clock = core.Clock()
    while wait_clock.getTime() < 2:
        if event.getKeys(['escape']):
            print("Escape key pressed during wait! Exiting...")
            win.close()
            core.quit()
        core.wait(0.01)  

    # Countdown: 3, 2, 1
    for num in ["3", "2", "1"]:
        text_stim.text = num
        text_stim.draw()
        win.flip()
        core.wait(1)

    # Clear the screen after countdown
    win.flip()

# ♦️ Function to Run Single Trial
def run_single_trial(win, trial, stim_offset, response_keys):

    event.clearEvents(eventType='keyboard')
    response, rt = None, None

    # Prepare stimuli
    circle = visual.Circle(win, radius=75, fillColor=trial["visual"], lineColor=None) if trial["visual"] else None
    beep   = trial["audio"]

    rt_clock = core.Clock()
    visual_onset = audio_onset = None

    is_multisensory = trial["type"] in ["AVC", "AVI"]

    # ── Case A: Unisensory  ───────────────────────────────────────────────────────
    if not is_multisensory:
        # Unisensory: present visual OR play audio immediately
        if trial["type"] == "V" and circle:
            circle.draw()
            win.flip()
            rt_clock.reset()
            visual_onset = core.getTime()
            print(f"🎨 Visual onset at {visual_onset:.3f} sec")
        elif trial["type"] == "A" and beep:
            beep.play()
            rt_clock.reset()
            audio_onset = core.getTime()
            print(f"🎵 Audio onset at {audio_onset:.3f} sec")
    else:
        # ── Case B: Visual leads or is synchronous ────────────────────────────────
        if stim_offset <= 0:
            # 1) Show visual & start clock
            if circle:
                circle.draw()
                win.flip()
                rt_clock.reset()
                visual_onset = core.getTime()
                print(f"🎨 Visual onset at {visual_onset:.3f} sec")
            # 2) Wait the remaining SOA on multisensory trials, polling for response
            soa_timer = core.Clock()
            while soa_timer.getTime() < abs(stim_offset) and response is None:
                keys = event.getKeys(timeStamped=rt_clock)
                for k in keys:
                    key_name, key_rt = k
                    if key_name in response_keys:
                        response, rt = key_name, key_rt
                        break
                    elif key_name == "escape":
                        print("Escape key pressed! Exiting...")
                        win.close()
                        core.quit()
                core.wait(0.01)
            # 3) Play audio if needed
            if beep:
                beep.play()
                audio_onset = core.getTime()
                print(f"🎵 Audio onset at {audio_onset:.3f} sec")

        # ── Case C: Audio leads ──────────────────────────────────────────────────
        else:
            # 1) Play audio & start clock
            if beep:
                beep.play()
                rt_clock.reset()
                audio_onset = core.getTime()
                print(f"🎵 Audio onset at {audio_onset:.3f} sec")
            # 2) Wait the remaining SOA on multisensory trials, polling for response
            soa_timer = core.Clock()
            while soa_timer.getTime() < stim_offset and response is None:
                keys = event.getKeys(timeStamped=rt_clock)
                for k in keys:
                    key_name, key_rt = k
                    if key_name in response_keys:
                        response, rt = key_name, key_rt
                        break
                    elif key_name == "escape":
                        print("Escape key pressed! Exiting...")
                        win.close()
                        core.quit()
                core.wait(0.01)
            # 3) Show visual
            if circle:
                circle.draw()
                win.flip()
                visual_onset = core.getTime()
                print(f"🎨 Visual onset at {visual_onset:.3f} sec")

    # now poll for response until participant responds
    while response is None:
        keys = event.getKeys(timeStamped=rt_clock)
        for k in keys:
            key_name, key_rt = k
            if key_name in response_keys:
                response, rt = key_name, key_rt
                break
            elif key_name == "escape":
                print("Escape key pressed! Exiting...")
                win.close()
                core.quit()
        if auto_response and rt_clock.getTime() > 0.5:
            response, rt = "NA", 0.5
            break
        core.wait(0.01)
    win.flip()  # clear all stimuli after response

    # Fix impropperly recorded RTs
    if rt is not None and rt < 0:
        rt = 0.0

    # Compute correctness
    if trial["type"] in ["V","A","AVC"]:
        expected = "f" if (trial["visual"]=="blue" or (trial["audio"] and "blue" in trial["audio"].fileName)) else "a"
        correct = (response == expected)
    else:
        correct = "NA"

    return response, rt, correct

# 🔷 Function to Run Practice
def run_practice(win, total_trials, trial_types):
    response_keys = ["a", "f"]  # Response keys
    fixation = visual.TextStim(win, text="+", color="white", height=40)
    iti_range=(1, 1.25)

    # Preload audio files
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.wav",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.wav"
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

    # **Shuffle trials before running**
    random.shuffle(trials)

    # Run trials
    for i, trial in enumerate(trials):
        # Allow quitting
        if 'escape' in event.getKeys():
            print("Escape key pressed! Exiting...")
            win.close()
            core.quit()

        # Run the trial using your unified function (no SOA, visual-only = 0 offset)
        response, rt, correct = run_single_trial(win, trial, 0, response_keys)

        # Provide feedback
        if response is None:
            feedback_text = visual.TextStim(win, text="Too Slow!", color="red", height=40)
        elif correct:
            feedback_text = visual.TextStim(win, text="✓ Correct", color="green", height=40)
        else:
            feedback_text = visual.TextStim(win, text="✗ Incorrect", color="red", height=40)
        feedback_text.draw()
        win.flip()
        core.wait(0.4)

        # Draw fixation and wait ITI
        fixation.draw()
        win.flip()
        core.wait(random.uniform(*iti_range))

# 🔷 Function to Run Trials
def run_trials(win, participant_number, block_num, stim_offset, total_trials):

    base_data_folder = "SCT_Data"
    participant_folder = os.path.join(base_data_folder, participant_number)
    os.makedirs(participant_folder, exist_ok=True)
    csv_filename = os.path.join(participant_folder, f"SCT_trials_{participant_number}.csv")

    response_keys = ["a", "f"]
    fixation = visual.TextStim(win, text="+", color="white", height=40)
    iti_range=(1, 1.25)

    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.wav",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.wav"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    file_exists = os.path.exists(csv_filename)
    with open(csv_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["participant_number", "block", "trial", "modality", "visual", "audio", "response", "rt", "correct", "offset"])

        trials = []
        proportions = {"A": 0.25, "V": 0.25, "AVC": 0.25, "AVI": 0.25}
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

            response, rt, correct = run_single_trial(win, trial, stim_offset, response_keys)

            writer.writerow([
                participant_number,
                block_num,
                i + 1,
                trial["type"],
                trial["visual"] if trial["visual"] is not None else "NA",
                os.path.basename(trial["audio"].fileName) if trial["audio"] else "NA",
                "R" if response == "a" else "B" if response == "f" else "NA",
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
def run_soa_test(win, participant_number, block_num, total_trials):

    fixation = visual.TextStim(win, text="+", color="white", height=40)
    response_keys = ["s", "d"]
    audio_files = {
        "red": "/Users/harpermarshall/Desktop/Project 1/sounds/red.wav",
        "blue": "/Users/harpermarshall/Desktop/Project 1/sounds/blue.wav"
    }
    preloaded_sounds = {color: sound.Sound(path) if os.path.exists(path) else None for color, path in audio_files.items()}

    # List of stimulus offset values to be pulled from in SOA task
    stim_offset_soa=[-1.0, -0.800, -0.05001, -0.03334, -0.01667, 0, 0.01667, 0.03334, 0.05001, 0.800, 1.0]

    # Save to SOA-specific file
    if isinstance(participant_number, int):
        participant_number = f"P{int(participant_number):03d}"
    base_data_folder = "SCT_Data"
    participant_folder = os.path.join(base_data_folder, participant_number)
    os.makedirs(participant_folder, exist_ok=True)
    soa_filename = os.path.join(participant_folder, f"SCT_soa_{participant_number}.csv")
    file_exists = os.path.exists(soa_filename)

    with open(soa_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(["participant_number", "block", "trial", "modality", "visual", "audio", "soa_", "response"])

        trials = []
        colors = ["red", "blue"]

        # Separate sync vs async SOAs
        if 0 in stim_offset_soa:
            sync_soa = 0
            async_soas = [s for s in stim_offset_soa if s != 0]
        else:
            sync_soa = None
            async_soas = stim_offset_soa

        # Trial counts
        n_total = total_trials
        n_sync = n_total // 2 if sync_soa is not None else 0
        n_async = n_total - n_sync
        n_per_async = n_async // (len(async_soas) * len(colors))  # per color per async SOA
        n_per_sync = n_sync // len(colors) if sync_soa is not None else 0  # per color for sync

        # Generate sync trials
        if sync_soa is not None:
            for _ in range(n_per_sync):
                for color in colors:
                    trial = {"type": "AVC", "visual": color, "soa": 0}
                    trial["audio"] = preloaded_sounds[color]
                    trials.append(trial)

        # Generate async trials
        for soa in async_soas:
            for _ in range(n_per_async):
                for color in colors:
                    trial = {"type": "AVC", "visual": color, "soa": soa}
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
            soa_sec = trial["soa"]

            if trial["soa"] < 0:
                # Show visual first
                if trial["visual"]:
                    circle.draw()
                    win.flip()
                core.wait(abs(soa_sec))  # Wait before playing audio
                if beep:
                    beep.play()
            else:
                # Play audio first
                if beep:
                    beep.play()
                core.wait(soa_sec)
                if trial["visual"]:
                    circle.draw()
                    win.flip()

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
                block_num,
                i + 1,
                trial["type"],
                trial["visual"] if trial["visual"] is not None else "NA",
                os.path.basename(trial["audio"].fileName) if trial["audio"] else "NA",
                trial["soa"],
                "S" if key.lower() == "s" else "A"
            ])

            print(f"  ✅ SOA Trial {i+1} complete.")

            # Show fixation during ITI
            fixation.draw()
            win.flip()
            iti_range=(1.9,2.1)
            core.wait(random.uniform(*iti_range))

    fixation.autoDraw = False
    win.flip()

# 🔷 Function to Run Experiment Questionnaire
def run_survey(win, participant_number, questions, block_num):
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

            core.wait(0.01)

    # Save responses to a per-participant survey CSV
    base_data_folder = "SCT_Data"
    participant_folder = os.path.join(base_data_folder, participant_number)
    os.makedirs(participant_folder, exist_ok=True)
    survey_filename = os.path.join(participant_folder, f"SCT_survey_{participant_number}.csv")
    file_exists = os.path.exists(survey_filename)

    with open(survey_filename, "a", newline="") as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow([
                "participant_number", 
                "block", 
                "audio_bias", 
                "visual_bias", 
                "one_strat", 
                "change_strat", 
                "no_strat"])
        writer.writerow([participant_number, block_num] + responses)  # One row per block

block_questions = [
    "1. The AUDIO cues influenced my responses more than the visual cues.",
    "2. The VISUAL cues influenced my responses more than the audio cues.",
    "3. I felt like I had a single, specific strategy throughout the entirety of this section.",
    "4. I had a strategy, but I feel like my strategy changed at least once during this section.",
    "5. I do not feel like I had a specific strategy during this section.",
]

# 🔷 Function to Run Experiment ----------------------------------------------------------------------------------------------------------------------------------------
def run_full_experiment():
    
    # 🔶 Get Participant Info
    participant_number = get_participant_info()

    # 🔶 Initialize PsychoPy Window
    win = visual.Window(fullscr=True, color="black", units="pix", checkTiming=False)

    # Trial Counts
    tt_SOA = 80
    tt_main = 64

    # SOA Test – Section 1
    show_instructions(win, 
        "Before we begin the main task, you'll complete TWO short sections where you'll judge the TIMING between what you SEE and what you HEAR.",
        3)
    show_instructions(win,
        "In each trial, you will either:\n\n"
        "SEE a BLUE circle and HEAR the word 'BLUE' spoken out loud or\n"
        "SEE a RED circle and HEAR the word 'RED' spoken out loud.\n\n"
        "Your job is to decide whether the visual and audio started at the SAME TIME or at DIFFERENT TIMES.",
        8)
    show_instructions(win,
        "Use the white 'S' and 'A' buttons on the button box to respond:\n"
        "Press 'S' if they occurred together (SYNCHRONOUS).\n"
        "Press 'A' if one came before the other (ASYNCHRONOUS).\n\n"
        "There is NO TIME LIMIT — respond at your own pace.", 
        8)
    show_instructions(win, 
        "To recap:\n\n"
        "Press 'S' if the visual and audio cues happened TOGETHER.\n"
        "Press 'A' if one cue came BEFORE the other.\n\n"
        "You can take your time to respond — there is no time limit in this section.", 
        7)
    show_instructions(win, 
        "The next screen will show a short countdown to help you get ready.\n\n"
        "After each response, there will be a brief pause before the next trial begins.", 
         5)
    get_ready(win, "Get Ready!\nSOA Task 1 will begin in...")
    run_soa_test(win, participant_number, block_num=1, total_trials=tt_SOA)

    # SOA Test – Section 2
    show_instructions(win, 
        "Now you'll repeat the timing judgment task once more.\n\n"
        "Again, decide whether the visual and auditory cues were SYNCHRONOUS or ASYNCHRONOUS.\n\n"
        "There is no time limit, so take your time to respond.",
        7)
    get_ready(win, "Get Ready!\nSOA Task 2 will begin in...")
    run_soa_test(win, participant_number, block_num=2, total_trials=tt_SOA)

    # Randomize visual delays for blocks 1–7
    stim_offsets = [-0.05001, -0.03334, -0.01667, 0, 0.01667, 0.03334, 0.05001]
    random.shuffle(stim_offsets)

    # 🔶 Practice
    show_instructions(win, 
        "The rest of this experiment will be broken up into 7 main sections with a short survey after each section.\n\n"
        "You will have the option to take a brief break after each survey.", 
        7)
    show_instructions(win, 
        "In this portion of this experiment, you will either:\n\nSEE a colored circle,\nHEAR the name of a color,\nor BOTH.\n\n"
        "Your task is to press the button that matches the perceived color.\n\n"
        "In between trials, keep your eyes on the fixation cross in the center of the screen.", 
        9)
    show_instructions(win,
        "Press the RED button when you perceive RED.\n\n"
        "Press the BLUE button when you perceive BLUE.\n\n"
        "You are about to start the Practice Section."
        "Although you are not timed, please respond as quickly and accurately as possible.", 
        9)
    get_ready(win, "Get Ready!\nPractice will begin in...") 
    run_practice(win, total_trials=12, trial_types=["V", "A"])
    show_instructions(win, 
        "Great job!\n\n"
        "In the real task, you will not be told if your responses are correct or incorrect like you saw in the practice.", 
        6)

    # Shared instructions for main blocks
    show_instructions(win, 
        "The next 7 sections will be identical. Your job is to:\n\n"
        "Press the RED button when you perceive RED.\n"
        "Press the BLUE button when you perceive BLUE.\n\n"
        "When you are ready, please continue to begin Section 1.", 
        8)

    # 🔶 RUN TRIALS: 7 blocks
    for block_num in range(1, 8):
        get_ready(win, f"Get Ready!\nSection {block_num} will begin in...")
        run_trials(win, participant_number, block_num=block_num, total_trials=tt_main, stim_offset=stim_offsets[block_num - 1])
        show_instructions(win, 
            f"Great job, you completed Section {block_num}! You will now move on to a brief survey.", 
            3)
        show_instructions(win, 
            f"Answer based on your experience in Section {block_num} ONLY.\n\n"
            "Use the number keys to rate your agreement with each statement.", 
            4)
        show_instructions(win, 
            "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
            3)
        run_survey(win, participant_number, block_questions, block_num=block_num)
        
        if block_num < 7:
            show_instructions(win, 
                "You may now take a brief break...\n\n"
                "Feel free to stand up and stretch.\nWhenever you are ready to begin the next Section, press the space bar.", 
                10)

    show_instructions(win, 
        "Congratulations! You have completed the entire experiment.\n\n"
        "Thank you so much for participating!", 
        4)
    
    # 🔶 Close the Experiment
    win.close()
    core.quit()

# 🔷 Function to Run Trials Only ----------------------------------------------------------------------------------------------------------------------------------------
def run_trials_only():

    # 🔶 Get Participant Info
    participant_number = get_participant_info()

    # 🔶 Initialize PsychoPy Window
    win = visual.Window(fullscr=True, color="black", units="pix", checkTiming=False)

    # Trial Counts
    tt_main = 64

    # Randomize visual delays for blocks 1–7
    stim_offsets = [-0.05001, -0.03334, -0.01667, 0, 0.01667, 0.03334, 0.05001]
    random.shuffle(stim_offsets)

    # Show brief instructions to start
    show_instructions(win, 
        "The next 7 sections will be identical. Your job is to:\n\n"
        "Press the RED button when you perceive RED.\n"
        "Press the BLUE button when you perceive BLUE.\n\n"
        "When you are ready, please continue to begin Section 1.", 
        8)
    
    # 🔶 RUN TRIALS: 7 blocks
    for block_num in range(1, 8):
        get_ready(win, f"Get Ready!\nSection {block_num} will begin in...")
        run_trials(win, participant_number, block_num=block_num, total_trials=tt_main, stim_offset=stim_offsets[block_num - 1])
        show_instructions(win, 
            f"Great job, you completed Section {block_num}! You will now move on to a brief survey.", 
            3)
        show_instructions(win, 
            f"Answer based on your experience in Section {block_num} ONLY.\n\n"
            "Use the number keys to rate your agreement with each statement.", 
            4)
        show_instructions(win, 
            "Press 1 if you strongly disagree,\n5 if you strongly agree,\nor 2-4 for responses in between.", 
            3)
        run_survey(win, participant_number, block_questions, block_num=block_num)
        
        if block_num < 7:
            show_instructions(win, 
                "You may now take a brief break...\n\n"
                "Feel free to stand up and stretch.\nWhenever you are ready, press the space bar to proceed.", 
                10)

    show_instructions(win, 
        "Congratulations! You have completed the entire experiment.\n\n"
        "Thank you so much for participating!", 
        4)
    
    # 🔶 Close the Experiment
    win.close()
    core.quit()

# 🔶 RUN EXPERIMENT
run_full_experiment()

# 🔶 RUN EXPERIMENT
#run_trials_only()

# DUMMY MODE -----------------------------------------------------------------------------------------------------------------------------------------------------------
#win = visual.Window(fullscr=True, color="black", units="pix", checkTiming=False)
#get_ready(win, "Get Ready!\nTask will begin in...")
#run_practice(win, total_trials=8, trial_types=["V", "A"])
#run_trials(win, "P999", block_num=1, total_trials=20, stim_offset=.5)
#run_trials(win, "P999", block_num=2, total_trials=5, stim_offset=0.05)
#run_trials(win, "P999", block_num=3, total_trials=5, stim_offset=0.1)
#run_trials(win, "P999", block_num=4, total_trials=5, stim_offset=0.15)
#run_trials(win, "P999", block_num=5, total_trials=5, stim_offset=0.20)
#run_soa_test(win, "P999", block_num=1, total_trials=40)
#run_survey(win, "P999", block_questions, block_num=1)
#win.close()
#core.quit()