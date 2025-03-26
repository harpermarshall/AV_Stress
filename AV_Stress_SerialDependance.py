import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from tabulate import tabulate

# -------------------------------------------------------------------
"""LOAD AND PREPARE DATA"""

# Load CSV (skip bad lines from Likert part for now)
df = pd.read_csv("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Pilot_Data/AV_Stress_PilotData_P001.csv", on_bad_lines='skip')

# Filter out trials without RTs (just in case) and make a working copy
df_clean = df[df['RT'].notna()].copy()

# Ensure trials are ordered by Block and Trial
df_clean = df_clean.sort_values(by=['Block', 'Trial']).reset_index(drop=True)

# Create common lag columns for serial dependence analyses
df_clean['Prev_Visual']   = df_clean['Visual'].shift(1)
df_clean['Prev_Audio']    = df_clean['Audio'].shift(1)
df_clean['Prev_Response'] = df_clean['Response'].shift(1)
df_clean['Prev_Correct']  = df_clean['Correct'].shift(1)
df_clean['Prev_Type']     = df_clean['Type'].shift(1)  # This line is added to fix the error

# Create lag-2 columns
df_clean['Visual_Lag2'] = df_clean['Visual'].shift(2)
df_clean['Audio_Lag2']  = df_clean['Audio'].shift(2)

# Add trial index (for time-on-task analyses)
df_clean['Trial_Index'] = df_clean.index

print("Data loaded and prepared.\n")

# -------------------------------------------------------------------
"""BIAS TOWARD PREVIOUS COLOR"""

# For incorrect responses only, check if the current response matches the previous visual/audio stimulus
df_incorrect = df_clean[df_clean['Correct'] == False].copy()

df_incorrect['Match_Prev_Visual'] = df_incorrect.apply(
    lambda row: str(row['Response']).lower() == str(row['Prev_Visual'])[0].lower() if pd.notna(row['Prev_Visual']) else False, axis=1)

df_incorrect['Match_Prev_Audio'] = df_incorrect.apply(
    lambda row: str(row['Response']).lower() == str(row['Prev_Audio'])[0].lower() if pd.notna(row['Prev_Audio']) else False, axis=1)

visual_bias_rate = df_incorrect['Match_Prev_Visual'].mean()
audio_bias_rate = df_incorrect['Match_Prev_Audio'].mean()

print("Bias toward previous color:")
print(f"Incorrect responses matching previous VISUAL stimulus: {visual_bias_rate:.2%}")
print(f"Incorrect responses matching previous AUDIO stimulus: {audio_bias_rate:.2%}\n")

# -------------------------------------------------------------------
"""BIAS TOWARD 2 TRIALS BACK"""

# For incorrect trials, check if the response matches the visual/audio stimulus from 2 trials back
df_incorrect['Match_Visual_Lag2'] = df_incorrect['Response'].str.lower() == df_incorrect['Visual_Lag2'].str[0].str.lower()
df_incorrect['Match_Audio_Lag2'] = df_incorrect['Response'].str.lower() == df_incorrect['Audio_Lag2'].str[0].str.lower()

print("Bias toward 2 trials back:")
print(f"Lag-2 Visual Match (Incorrects): {df_incorrect['Match_Visual_Lag2'].mean():.2%}")
print(f"Lag-2 Audio Match (Incorrects): {df_incorrect['Match_Audio_Lag2'].mean():.2%}\n")

# -------------------------------------------------------------------
"""BIAS IF PREVIOUS 2 TRIALS WERE THE SAME"""

prev1 = df_clean['Visual'].shift(1)
prev2 = df_clean['Visual'].shift(2)

df_clean['Prev_Two_Visual_Same'] = (prev1 == prev2) & prev1.notna()

df_clean['Match_Prev_Two_Visual'] = df_clean.apply(
    lambda row: row['Response'].lower() == str(prev1[row.name])[0].lower() if row['Prev_Two_Visual_Same'] else False,
    axis=1
)

print("Bias if previous 2 trials were the same:")
print(f"Match Prev-Two-Visual Same: {df_clean['Match_Prev_Two_Visual'].mean():.2%}\n")

# -------------------------------------------------------------------
"""RT DRIFT OVER TRIALS OR BLOCKS"""

plt.figure(figsize=(8,4))
plt.plot(df_clean['Trial_Index'], df_clean['RT'], marker='o', linestyle='-', alpha=0.7)
plt.xlabel("Trial Index")
plt.ylabel("RT (s)")
plt.title("RT Drift Over Trials")
plt.tight_layout()
plt.show()

# -------------------------------------------------------------------
"""INFLUENCE OF INTER-TRIAL INTERVAL (ITI) ON RT"""

iti_rt_corr = df_clean[['ITI', 'RT']].corr().iloc[0, 1]
print(f"Correlation between ITI and RT: {iti_rt_corr:.3f}")

# Optionally, bin ITI into quantiles and compute average RT per bin
df_clean['ITI_bin'] = pd.qcut(df_clean['ITI'], 4, duplicates='drop')
iti_group = df_clean.groupby('ITI_bin')['RT'].mean()
print("Average RT by ITI bin:")
print(iti_group)
print("\n")

# -------------------------------------------------------------------
"""CONDITION TRANSITION EFFECTS ON RT & ACCURACY"""

# Create a column indicating if the current trial's condition is the same as the previous trial's
df_clean['Same_Condition'] = df_clean['Type'] == df_clean['Prev_Type']

same_cond = df_clean[df_clean['Same_Condition'] == True]
diff_cond = df_clean[df_clean['Same_Condition'] == False]

mean_rt_same    = same_cond['RT'].mean()
mean_rt_diff    = diff_cond['RT'].mean()
error_rate_same = 1 - same_cond['Correct'].mean()
error_rate_diff = 1 - diff_cond['Correct'].mean()

print("Condition Transition Effects:")
print(f"Same Condition - Mean RT: {mean_rt_same:.3f}, Error Rate: {error_rate_same:.2%}")
print(f"Different Condition - Mean RT: {mean_rt_diff:.3f}, Error Rate: {error_rate_diff:.2%}\n")

# -------------------------------------------------------------------
"""RESPONSE TRANSITION EFFECTS (REPEAT VS. SWITCH)"""

df_clean['Repeat_Response'] = df_clean['Response'].str.lower() == df_clean['Prev_Response'].str.lower()

repeat_resp = df_clean[df_clean['Repeat_Response'] == True]
switch_resp = df_clean[df_clean['Repeat_Response'] == False]

mean_rt_repeat    = repeat_resp['RT'].mean()
mean_rt_switch    = switch_resp['RT'].mean()
error_rate_repeat = 1 - repeat_resp['Correct'].mean()
error_rate_switch = 1 - switch_resp['Correct'].mean()

print("Response Transition Effects:")
print(f"Repeat Response - Mean RT: {mean_rt_repeat:.3f}, Error Rate: {error_rate_repeat:.2%}")
print(f"Switch Response - Mean RT: {mean_rt_switch:.3f}, Error Rate: {error_rate_switch:.2%}\n")

# -------------------------------------------------------------------
"""EFFECT OF PREVIOUS ERROR ON CURRENT PERFORMANCE"""

after_correct = df_clean[df_clean['Prev_Correct'] == True]
after_error   = df_clean[df_clean['Prev_Correct'] == False]

mean_rt_after_correct = after_correct['RT'].mean()
mean_rt_after_error   = after_error['RT'].mean()
accuracy_after_correct = after_correct['Correct'].mean()
accuracy_after_error   = after_error['Correct'].mean()

print("Effect of Previous Error on Current Performance:")
print(f"After Correct - Mean RT: {mean_rt_after_correct:.3f}, Accuracy: {accuracy_after_correct:.2%}")
print(f"After Error   - Mean RT: {mean_rt_after_error:.3f}, Accuracy: {accuracy_after_error:.2%}\n")

# -------------------------------------------------------------------
"""RT AUTOCORRELATION"""

max_lag = 10
autocorr_results = {}
for lag in range(1, max_lag + 1):
    autocorr_results[lag] = df_clean['RT'].autocorr(lag=lag)

print("RT Autocorrelation by Lag:")
for lag, corr in autocorr_results.items():
    print(f"Lag {lag}: {corr:.3f}")
print("\n")

plt.figure(figsize=(6,4))
plt.bar(list(autocorr_results.keys()), list(autocorr_results.values()))
plt.xlabel("Lag")
plt.ylabel("Autocorrelation")
plt.title("RT Autocorrelation")
plt.tight_layout()
plt.show()

# Blank line after plot output
print("\n")

# -------------------------------------------------------------------
"""INFLUENCE OF PREVIOUS NON-AVI TRIAL ON SUBSEQUENT AVI RESPONSE"""

# For non-AVI trials (A, V, AVC), compute the expected response from the visual stimulus.
df_clean['Expected_Response'] = df_clean.apply(
    lambda row: str(row['Visual'])[0].lower() if row['Type'] in ['A', 'V', 'AVC'] and pd.notna(row['Visual']) else None,
    axis=1
)

# The column 'Prev_Type' is already created above.
df_clean['Prev_Expected'] = df_clean['Expected_Response'].shift(1)

# Filter transitions where the current trial is AVI and the previous trial is non-AVI
transitions = df_clean[(df_clean['Type'] == 'AVI') & (df_clean['Prev_Type'].isin(['A', 'V', 'AVC']))].copy()

# For these transitions, check if the AVI response matches the previous expected response
transitions['Match_Prev_Expected'] = transitions.apply(
    lambda row: row['Response'].lower() == row['Prev_Expected'] if pd.notna(row['Prev_Expected']) else False,
    axis=1
)

# Group by the previous trial's condition to compare bias effects
transition_summary = transitions.groupby('Prev_Type').agg(
    count=('Match_Prev_Expected', 'count'),
    match_rate=('Match_Prev_Expected', 'mean')
).reset_index()

print("Influence of Previous Non-AVI Trial on AVI Response:")
print(tabulate(transition_summary, headers="keys", tablefmt="fancy_grid", showindex=False))
print("\n")