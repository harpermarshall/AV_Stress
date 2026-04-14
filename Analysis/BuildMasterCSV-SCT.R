########################################
###     BUILD MASTER CSV FILES       ###
########################################

# ----------------------------
# LIBRARIES
# ----------------------------
library(tidyverse)

# ----------------------------
# USER SETTINGS
# ----------------------------
data_dir <- "/Users/harpermarshall/Desktop/Project 1/SCT_Data"

drop_folder_prefix <- "X"
drop_folder_exact  <- "P999"

master_trials_csv <- "All_Trials_MASTER.csv"
master_soa_csv    <- "All_SOA_MASTER.csv"


# ----------------------------
# PARTICIPANT FOLDERS
# ----------------------------
participant_folders <- list.dirs(
  path = data_dir,
  recursive = FALSE,
  full.names = TRUE
) %>%
  discard(
    ~ str_starts(basename(.x), drop_folder_prefix) ||
      basename(.x) == drop_folder_exact
  )

cat("Participant folders found:", length(participant_folders), "\n")
print(basename(participant_folders))


# ----------------------------
# HELPER FUNCTIONS
# ----------------------------
load_combined_data <- function(filename_prefix, participant_folders) {
  
  file_paths <- participant_folders %>%
    map(~ list.files(
      path = .x,
      pattern = paste0("^", filename_prefix, ".*\\.csv$"),
      full.names = TRUE
    )) %>%
    flatten_chr()
  
  if (length(file_paths) == 0) {
    stop(paste0("No files found for prefix: ", filename_prefix))
  }
  
  map_dfr(file_paths, read_csv, show_col_types = FALSE)
}


# ----------------------------
# MASTER TRIALS CSV
# ----------------------------
trials_df_master <- load_combined_data("SCT_trials", participant_folders) %>%
  mutate(
    participant_number = as.character(participant_number),
    modality           = factor(as.character(modality), levels = c("A", "V", "AVC", "AVI")),
    
    # overwrite seconds → milliseconds
    rt     = suppressWarnings(as.numeric(rt)) * 1000,
    offset = suppressWarnings(as.numeric(offset)) * 1000,

    correct            = as.logical(correct)
  ) %>%
  relocate(participant_number, modality, offset, rt, correct, .before = 1) %>%
  arrange(participant_number)


# Check all trial data uploaded propperly 
trials_df_master %>%
  count(participant_number, modality) %>%
  pivot_wider(
    names_from  = modality,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Total = A + V + AVC + AVI) %>%
  bind_rows(
    summarise(
      .,
      participant_number = "Total",
      across(where(is.numeric), sum)
    )
  ) %>%
  print(n = Inf)

write_csv(trials_df_master, file.path(data_dir, master_trials_csv))
cat("Saved:", file.path(data_dir, master_trials_csv), "\n")


# ----------------------------
# MASTER SOA CSV
# ----------------------------
soa_df_master <- load_combined_data("SCT_soa", participant_folders) %>%
  mutate(
    participant_number = as.character(participant_number),
    
    # overwrite seconds → milliseconds
    soa   = round(suppressWarnings(as.numeric(soa_)) * 1000, 2),
    
    block = suppressWarnings(as.numeric(block))
  ) %>%
  select(-soa_) %>%   # drop the old column
  arrange(participant_number)

# Check all SOA data uploaded propperly 
soa_df_master %>%
  count(participant_number, soa) %>%
  pivot_wider(
    names_from  = soa,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(
    summarise(
      .,
      participant_number = "Total",
      across(where(is.numeric), sum)
    )
  ) %>%
  print(n = Inf, width = Inf)

write_csv(soa_df_master, file.path(data_dir, master_soa_csv))
cat("Saved:", file.path(data_dir, master_soa_csv), "\n")
