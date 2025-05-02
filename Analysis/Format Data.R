################################
### LOOKING AT THE NEW DATA ####
################################

library(tidyverse)
library(rstatix)
library(ggsignif)

# Set working directory to the AV_SOA_Data folder
setwd("/Users/harpermarshall/Desktop/Project 1/AV_SOA_Data/")

# Get list of participant folders (e.g., P001, P002, ...)
participant_folders <- list.dirs(path = ".", recursive = FALSE) %>%
  discard(~ str_starts(basename(.x), "X") || basename(.x) == "P999")

print(participant_folders)

# Function to load and combine files by type
load_combined_data <- function(filename_prefix) {
  file_paths <- participant_folders %>%
    map(~ list.files(path = .x, pattern = paste0("^", filename_prefix, ".*\\.csv$"), full.names = TRUE)) %>%
    flatten_chr()
  
  combined_df <- file_paths %>%
    map_dfr(read_csv)
  
  return(combined_df)
}

# Load all three datasets across participants
soa_results_df     <- load_combined_data("AV_SOA_Results")
stroop_results_df  <- load_combined_data("AV_Stroop_Results")
survey_df          <- load_combined_data("AV_Strategy_Survey")

trial_counts <- stroop_results_df %>%
  count(Participant, Type) %>%
  pivot_wider(names_from = Type, values_from = n, values_fill = 0) %>%
  mutate(Total = A + V + AVC + AVI)

print(trial_counts)

# Ensure Visual_Delay is not treated as a list or NA
stroop_delay_lookup <- stroop_results_df %>%
  select(Participant, Block, `Visual Delay`) %>%
  distinct()  # one delay per participant+block combo

# Join the delay info onto the survey data
survey_with_delay <- survey_df %>%
  left_join(stroop_delay_lookup, by = c("Participant", "Block"))

survey_with_delay <- survey_with_delay %>%
  rename(
    Audio_Bias     = Q1,
    Visual_Bias    = Q2,
    One_Strat      = Q3,
    Strat_Change   = Q4,
    No_Strat       = Q5,
  )

###########################################
### CLEAN STROOP TRIALS & ADD SURVEY ######
###########################################

# Step 1: Keep Visual Delay as numeric, clean and recode
stroop_clean <- stroop_results_df %>%
  filter(!is.na(RT), Response != "no response") %>%  # remove invalid trials
  rename(Offset_ms = `Visual Delay`) %>%  # Keep numeric delay as Offset_ms
  mutate(
    Correct = as.logical(Correct),
    Offset_ms = as.numeric(Offset_ms) * 1000,
    RT = as.numeric(RT),
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI")),
    
    # Correct RT: only add offset to non-visual trials
    RT_corrected = if_else(Type == "V", RT * 1000, RT * 1000 + Offset_ms),
    
    # Create a factor version of Offset_ms for plotting
    Offset = factor(as.character(Offset_ms), levels = c("0", "50", "100", "150", "200"))
  ) %>%
  filter((Type %in% c("A", "V", "AVC") & RT >= 0.2 & RT <= 0.8) | Type == "AVI")

# Step 2: Merge survey data
stroop_with_survey <- stroop_clean %>%
  left_join(survey_with_delay, by = c("Participant", "Block"))

# Remove unnecessary columns
stroop_with_survey <- stroop_with_survey %>%
  select(-Offset, -`Visual Delay`)

# Step 3: Save cleaned & corrected data
write_csv(stroop_with_survey, "All_Stroop_Trials_With_Survey.csv")

