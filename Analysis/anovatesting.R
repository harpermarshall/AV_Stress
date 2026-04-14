clean_trials_df %>%
  filter(modality %in% c("A", "V")) %>%
  group_by(participant_number, offset, modality) %>%
  summarise(mean_rt = mean(rt), .groups = "drop") %>%
  group_by(offset, modality) %>%
  summarise(
    group_mean = mean(mean_rt),
    group_sd   = sd(mean_rt),
    .groups = "drop"
  )

library(ez)

anova_df <- clean_trials_df %>%
  filter(modality %in% c("A", "V")) %>%
  group_by(participant_number, offset, modality) %>%
  summarise(mean_rt = mean(rt), .groups = "drop")

ezANOVA(
  data = anova_df,
  dv = mean_rt,
  wid = participant_number,
  within = .(offset, modality),
  detailed = TRUE
)


anova_df <- clean_trials_df %>%
  filter(modality %in% c("A", "V")) %>%
  group_by(participant_number, offset, modality) %>%
  summarise(mean_rt = mean(rt), .groups = "drop") %>%
  mutate(offset = factor(offset))

library(afex)

aov_ez(
  id = "participant_number",
  dv = "mean_rt",
  within = c("offset", "modality"),
  data = anova_df
)


library(rstatix)

visual_anova <- clean_trials_df %>%
  filter(modality == "V") %>%
  group_by(participant_number, offset) %>%
  summarise(mean_rt = mean(rt), .groups = "drop") %>%
  anova_test(
    dv = mean_rt,
    wid = participant_number,
    within = offset
  )

get_anova_table(visual_anova)
