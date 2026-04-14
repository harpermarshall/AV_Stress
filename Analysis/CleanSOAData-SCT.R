############################################
###   CLEAN SOA DATA FROM MASTER CSV     ###
############################################

# ----------------------------
# LOAD LIBRARIES
# ----------------------------
#library(tidyverse)
#library(rstatix)
#library(ggsignif)

# ----------------------------
# USER SETTINGS
# ----------------------------
data_dir <- "/Users/harpermarshall/Desktop/Project 1/SCT_Data"

trials_df <- read_csv(file.path(data_dir, "All_SOA_MASTER.csv"), show_col_types = FALSE) %>%
  filter(!participant_number %in% exclude_participants)


