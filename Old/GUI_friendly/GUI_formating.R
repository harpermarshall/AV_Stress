library(tidyverse)
library(readr)

library(tidyverse)

# Set working directory
setwd("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Data/")

# Load the CSV and convert format
gui_ready_df <- read_csv("All_Stroop_Trials_With_Survey_NoOffsets.csv") %>%
  filter(Type %in% c("A", "V", "AVI")) %>%
  mutate(
    participant_number = as.integer(str_remove(Participant, "^P0*")),
    modality = recode(Type, A = 1, V = 2, AVI = 3),
    reaction_time = RT_corrected
  ) %>%
  select(participant_number, modality, reaction_time)

# Save new GUI-ready file
write_csv(gui_ready_df, "GUI_Formatted_Data(AVI).csv")
