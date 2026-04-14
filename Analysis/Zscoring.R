data_dir <- "/Users/harpermarshall/Desktop/Project 1/SCT_Data"

df <- read_csv(file.path(data_dir, "All_Trials_MASTER.csv"), show_col_types = FALSE) 

participant_medians <- df %>%
  filter(modality %in% c("A", "V", "AVC")) %>%
  group_by(participant_number, modality, offset) %>%
  summarise(
    median_rt = median(rt, na.rm = TRUE),
    .groups = "drop"
  )

participant_modality_medians <- participant_medians %>%
  group_by(participant_number, modality) %>%
  summarise(
    median_rt = median(median_rt),
    .groups = "drop"
  )

participant_zscores <- participant_modality_medians %>%
  group_by(modality) %>%
  mutate(
    z_median_rt = (median_rt - mean(median_rt)) / sd(median_rt)
  ) %>%
  ungroup()

participant_zscores %>%
  arrange(z_median_rt) %>%
  print(n = Inf)



