clean_trials_df <- clean_trials_df %>%
  mutate(
    stim_color = case_when(
      toupper(response) == "R" ~ "red",
      toupper(response) == "B" ~ "blue",
      TRUE ~ NA_character_
    )
  )

table(clean_trials_df$stim_color, useNA = "ifany")

plots_by_color <- run_race_model_by_color(clean_trials_df, color_col = "stim_color")

print(plots_by_color[["red"]])
print(plots_by_color[["blue"]])



#LINE GRAPH - no differences

df_plot <- clean_trials_df %>%
  mutate(
    stim_color = case_when(
      toupper(response) == "R" ~ "red",
      toupper(response) == "B" ~ "blue",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(stim_color))

onset_levels <- c("-50.01","-33.34","-16.67","0.00","16.67","33.34","50.01")

median_summary_modality_color <- df_plot %>%
  mutate(
    modality = factor(modality, levels = c("A","V","AVC","AVI")),
    onset_chr = factor(formatC(offset, format = "f", digits = 2), levels = onset_levels)
  ) %>%
  group_by(stim_color, modality, onset_chr) %>%
  summarise(median_rt = median(rt, na.rm = TRUE), .groups = "drop")

bargraph_by_color <- ggplot(
  median_summary_modality_color,
  aes(x = modality, y = median_rt, fill = onset_chr)
) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  scale_fill_manual(values = onset_colors, breaks = names(onset_colors), name = "Onset (ms)") +
  facet_wrap(~ stim_color, nrow = 1) +
  labs(
    title = "Median RT by Modality and Onset (split by response color)",
    x = "Trial Modality",
    y = "Median Response Time (ms)"
  ) +
  coord_cartesian(ylim = c(300, 700)) +
  THEME +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "grey60"),
    axis.line.y = element_line(color = "grey60"),
    axis.ticks  = element_line(color = "grey60")
  )

print(bargraph_by_color)

# LINEAR MIXED EFFECTS MODEL

library(lme4)
library(lmerTest)   # gives p-values

df_lmm <- clean_trials_df %>%
  mutate(
    stim_color = factor(response, levels = c("R","B"),
                        labels = c("red","blue")),
    modality   = factor(modality, levels = c("A","V","AVC","AVI")),
    participant_number = factor(participant_number)
  )

df_lmm <- df_lmm %>%
  mutate(log_rt = log(rt))

m1 <- lmer(
  log_rt ~ stim_color * modality + (1 | participant_number),
  data = df_lmm
)

anova(m1)
