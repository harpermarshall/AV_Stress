clean_trials_df %>%
  filter(modality %in% c("A", "V", "AVC")) %>%
  group_by(modality, offset) %>%
  summarise(
    trials = n(),
    correct_trials = sum(correct),
    accuracy = mean(correct),
    .groups = "drop"
  ) %>%
  arrange(modality, offset) %>%
  print(n = Inf)
