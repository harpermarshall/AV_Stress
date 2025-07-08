########################################
###      LOAD LIBRARIES & SETUP      ###
########################################

# Load a set of helpful packages:
# tidyverse: a bundle of tools for reading, cleaning, and reshaping data
# rstatix: extra functions for basic statistics
# ggsignif: makes it easy to add stars or lines showing significance on plots
library(tidyverse)
library(rstatix)
library(ggsignif)

# 1. TELL R WHERE YOUR DATA LIVES
# Change R’s “current folder” to the one that has all your participant folders
setwd("/Users/harpermarshall/Desktop/Project 1/SCT_Data/")

# 2. FIND ALL THE PARTICIPANT FOLDERS
# list.dirs() gets every subfolder right under the current folder
# recursive = FALSE means “just one level down, not nested folders”
# discard() removes any folder whose name starts with "X" or is exactly "P999"
participant_folders <- list.dirs(path = ".", recursive = FALSE) %>%
  discard(
    ~ str_starts(basename(.x), "X")   # drop folders that start with “X”
    || basename(.x) == "P999"         # or are named “P999”
  )

# Show which folders we will work with
print(participant_folders)

# 4. MAKE A HELPER FUNCTION TO LOAD AND COMBINE FILES
# This function takes a file-name prefix (like "SCT_soa")
# then:
#  - looks inside each participant folder for any CSV that starts with that prefix
#  - reads all those CSVs into R
#  - stacks them on top of each other to make one big table
load_combined_data <- function(filename_prefix) {
  # 4a. Find all matching CSV file paths
  file_paths <- participant_folders %>%
    map( ~ list.files(
      path = .x,                                       # search in this folder
      pattern = paste0("^", filename_prefix, ".*\\.csv$"),  # name starts with prefix and ends in .csv
      full.names = TRUE                                # give full path, not just file name
    )) %>%
    flatten_chr()                                      # turn list of lists into one simple character vector
  
  # 4b. Read each CSV and combine into one data frame
  combined_df <- file_paths %>%
    map_dfr(read_csv)                                  # map over each path, read it, and bind rows together
}

# 5. USE THE FUNCTION TO LOAD YOUR THREE DATASETS
soa_df    <- load_combined_data("SCT_soa")     # SOA (timing) data from each participant
trials_df <- load_combined_data("SCT_trials")  # trial‐by‐trial reaction time data
survey_df <- load_combined_data("SCT_survey")  # survey answers collected after each block

# 6. MERGE TRIAL DATA WITH SURVEY DATA
# We want one table with both trial info and the matching survey answers
trials_with_survey_df <- trials_df %>%
  left_join(
    survey_df,
    by = c("participant_number", "block"),  # join on these two columns
    relationship = "many-to-many"           # fine if blocks repeat per person
  )

# 7. CHECK HOW MANY TRIALS EACH PARTICIPANT DID IN EACH CONDITION
# count() tallies trials for each participant and modality
# pivot_wider() spreads those counts into separate columns for A, V, AVC, AVI
# then we add a Total column
trials_with_survey_df %>%
  count(participant_number, modality) %>%
  pivot_wider(
    names_from = modality,
    values_from = n,
    values_fill = 0    # fill missing cells with zero
  ) %>%
  mutate(
    Total = A + V + AVC + AVI
  ) %>%
  print()

# trouble shoot by participant (e.g. "P016") if needed to figure out what data is missing/where
#trials_with_survey_df %>%
  #filter(participant_number == "P016") %>%      # keep only P016
  #count(block, modality) %>%                    # count rows for each block × modality
  #pivot_wider(                                   # spread modalities into their own columns
    #names_from  = modality,
    #values_from = n,
    #values_fill = 0                              # fill missing combos with 0
  #) %>%
  #mutate(
    #Total = A + V + AVC + AVI                    # add up across modalities
  #) %>%
  #arrange(block)                                 # sort by block

# 8. FLAG “OUTLIER” RESPONSE TIMES
# Within each participant & condition:
#  - compute each RT’s z-score (how far from that person’s average in SD units)
#  - mark any trial as an outlier if its z-score is bigger than 3 in absolute value
outlier_counts <- trials_with_survey_df %>%
  group_by(participant_number, modality) %>%
  mutate(
    rt_z      = (rt - mean(rt, na.rm = TRUE)) / sd(rt, na.rm = TRUE),
    is_outlier = abs(rt_z) > 3
  ) %>%
  summarise(
    total_trials     = n(),                              # how many trials
    outliers         = sum(is_outlier, na.rm = TRUE),    # how many outliers
    percent_outliers = round(100 * outliers / total_trials, 2),
    .groups = "drop"
  )

# Show the full outlier summary
print(outlier_counts, n = Inf)

# 9. QUICK COUNTS BEFORE WE CLEAN
# Count how many trials are marked wrong, too fast (<250 ms), or too slow (>2000 ms)

cat("🔍 Total trials BEFORE filter:", nrow(trials_with_survey_df), "\n")
cat("🧹 Trials marked incorrect:", sum(trials_with_survey_df$correct == FALSE, na.rm = TRUE), "\n")
cat("• Too fast (<250 ms):", sum(trials_with_survey_df$rt < 0.250, na.rm = TRUE), "\n")
cat("• Too slow (>2000 ms):", sum(trials_with_survey_df$rt > 2, na.rm = TRUE), "\n")

# 10. CLEAN AND PREPARE THE DATA
#  - convert text columns into numbers
#  - change seconds to milliseconds for easier filtering
#  - turn “modality” into a true category
#  - drop any trials outside of 250–2000 ms
clean_df <- trials_with_survey_df %>%
  mutate(
    offset           = as.numeric(offset),        # make sure offset is numeric
    offset_corrected = offset * 1000,             # convert seconds → ms
    modality         = factor(modality, levels = c("A","V","AVC","AVI")),
    rt               = as.numeric(rt),
    rt_corrected     = rt * 1000,
    correct          = as.logical(correct)
  ) %>%
  filter(rt_corrected >= 250, rt_corrected <= 2000)

# 11. OPTIONAL: DROP ALL WRONG TRIALS
# If you want to remove trials where correct == FALSE, uncomment the next line
#clean_df <- clean_df %>% filter(is.na(correct) | correct == TRUE)

# 12. SEE HOW MANY TRIALS GOT REMOVED, BY CONDITION
removed_trials <- bind_rows(
  trials_with_survey_df %>% mutate(stage = "before"),
  clean_df              %>% mutate(stage = "after")
) %>%
  group_by(stage, participant_number, modality) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from  = stage,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(removed = before - after) %>%
  select(participant_number, modality, removed) %>%
  pivot_wider(
    names_from  = modality,
    values_from = removed,
    values_fill = 0
  ) %>%
  mutate(Total_removed = A + V + AVC + AVI) %>%
  bind_rows(
    summarise(.,
              participant_number = "Total",
              across(where(is.numeric), sum)
    )
  )

# Print the removal summary
print(removed_trials)

# 13. QUICK COUNTS BEFORE AFTER CLEAN
# Count how many trials are marked wrong, too fast (<250 ms), or too slow (>2000 ms) after cleaning

cat("🔍 Total trials BEFORE filter:", nrow(clean_df), "\n")
cat("🧹 Trials marked incorrect:", sum(clean_df$correct == FALSE, na.rm = TRUE), "\n")
cat("• Too fast (<250 ms):", sum(clean_df$rt < 0.250, na.rm = TRUE), "\n")
cat("• Too slow (>2000 ms):", sum(clean_df$rt > 2, na.rm = TRUE), "\n")

# 14. SAVE YOUR CLEAN DATA
# Write the cleaned table out to a new CSV file for future use
#write_csv(clean_df, "All_Trials_With_Survey_17All.csv")
