library(tidyverse)
library(readr)

# SET WORKING DIRECTORY
setwd("/Users/harpermarshall/Desktop/Project 1/SCT_Data/")

library(readr)
library(dplyr)
library(stringr)

excluded_participants <- c()

gui_ready_df <- read_csv("All_Trials_With_Survey_21True.csv") %>%
  filter(
    modality %in% c("A", "V", "AVC"),
    offset_corrected == 0,
    rt_corrected <= 1000,           # drop trials over 800 ms
    !participant_number %in% excluded_participants
  ) %>%
  mutate(
    participant_number = as.integer(str_remove(participant_number, "^P0*")),
    modality           = recode(modality, A = 1, V = 2, AVC = 3),
    reaction_time      = rt_corrected
  ) %>%
  select(participant_number, modality, reaction_time)

# Save new GUI-ready file
write_csv(gui_ready_df, "GUI_Formatted_Data(20AVC0TEST).csv")
