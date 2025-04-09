library(tidyverse)

# Set working directory
setwd("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Data/")

# Load and combine participants 2–5 from AV_Stress_Data folder
file_list <- list.files(pattern = "AV_Stress_Results_P00[2-5]\\.csv", 
                        full.names = TRUE)
print(file_list)  # Optional: check the list

# Combine and clean the data
df_all <- file_list %>%
  map_dfr(~ read_csv(.x)) %>%
  filter(Block %in% c(2),                     # Keep only blocks 2 & 3
         Type %in% c("A", "V", "AVI")) %>%       # Keep only A, V, AVC trials
  mutate(
    modality = recode(Type, A = 1, V = 2, AVI = 3),     # Recode modality
    reaction_time = RT * 1000                           # Convert seconds to ms
  ) %>%
  rename(
    participant_number = Participant
  ) %>%
  select(participant_number, Block, Trial, modality, Visual, Audio,
         Response, reaction_time, Correct, ITI)         # Reorder/keep desired columns

# To save df as CSV file
library(readr)
write_csv(df_all, "AV_Stress_PilotData_Delays_AVI2.csv")
