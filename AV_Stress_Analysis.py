import pandas as pd
import itertools
from scipy.stats import ttest_ind
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from statsmodels.stats.anova import AnovaRM


# Load CSV (skip bad lines from Likert part for now)
df = pd.read_csv("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Pilot_Data/AV_Stress_PilotData_P002.csv", on_bad_lines='skip')

# Filter out trials without RTs (just in case)
df_clean = df[df['RT'].notna()]

# Group by condition Type and summarize
summary = df_clean.groupby('Type').agg(
    mean_RT=('RT', 'mean'),
    std_RT=('RT', 'std'),
    accuracy=('Correct', lambda x: x.mean() if x.notna().any() else None),
    count=('RT', 'count')
).reset_index()

# Display the summary
print(summary)

# Define a function to assign significance markers
def significance_marker(p_val):
    if p_val < 0.001:
        return "***"
    elif p_val < 0.01:
        return "**"
    elif p_val < 0.05:
        return "*"
    else:
        return ""

# Create an empty list to store results
results = []

# Get unique condition types (e.g., A, V, AVC, AVI)
conditions = df_clean['Type'].unique()

# Loop through every pair of conditions and perform a t-test
for cond1, cond2 in itertools.combinations(conditions, 2):
    rt1 = df_clean[df_clean['Type'] == cond1]['RT']
    rt2 = df_clean[df_clean['Type'] == cond2]['RT']
    t_stat, p_val = ttest_ind(rt1, rt2, equal_var=False)  # Welch's t-test
    marker = significance_marker(p_val)
    results.append([f"{cond1} vs {cond2}", f"{t_stat:.2f}", f"{p_val:.3f}{marker}" ])

# Create a DataFrame with the results and custom column names
results_df = pd.DataFrame(results, columns=["Comparison", "t-statistic", "p-value"])

# Print the results in a table format
print(results_df)

# Look at effect size
def cohen_d(x, y):
    # Calculate pooled standard deviation
    nx, ny = len(x), len(y)
    pooled_std = np.sqrt(((nx-1)*np.std(x, ddof=1)**2 + (ny-1)*np.std(y, ddof=1)**2) / (nx+ny-2))
    return (np.mean(x) - np.mean(y)) / pooled_std

for cond1, cond2 in itertools.combinations(conditions, 2):
    rt1 = df_clean[df_clean['Type'] == cond1]['RT']
    rt2 = df_clean[df_clean['Type'] == cond2]['RT']
    d = cohen_d(rt1, rt2)
    print(f"{cond1} vs {cond2}: Cohen's d = {d:.2f}")

"""# USE FOR MULTIPLE PARTICIPANTS
anova = AnovaRM(df_clean, depvar='RT', subject='Participant', within=['Type'], aggregate_func=np.mean).fit()
print(anova)"""

# Visualize Data
plt.figure(figsize=(8, 6))
sns.boxplot(x="Type", y="RT", data=df_clean, palette="Set2")
plt.title("Reaction Time by Condition")
plt.xlabel("Condition")
plt.ylabel("Reaction Time (s)")
plt.show()

