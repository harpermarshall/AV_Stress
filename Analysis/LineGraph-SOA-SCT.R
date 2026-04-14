#########################################################
###   Line Graph: Individual Perceived Simultaneity   ###
#########################################################

# ----------------------------
# PREP DATA
# ----------------------------
soa_summary <- clean_SOA_df %>%
  group_by(participant_number, soa_factor) %>%
  summarise(
    proportion_simultaneous = mean(simultaneous, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(soa_factor) %>%
  summarise(
    mean_proportion = mean(proportion_simultaneous, na.rm = TRUE),
    se = sd(proportion_simultaneous, na.rm = TRUE) / sqrt(n()),
    n_participants = n(),
    .groups = "drop"
  )

# ----------------------------
# PLOT
# ----------------------------
linegraph_soa <- ggplot(soa_summary, aes(x = soa_factor, y = mean_proportion, group = 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray70") +
  geom_line(linewidth = 0.6, color = "#60110C") +
  geom_point(size = 2, color = "#60110C") +
  geom_errorbar(
    aes(ymin = mean_proportion - se, ymax = mean_proportion + se),
    width = 0.25, color = "#60110C"
  ) +
  scale_y_continuous(
    name = "Mean Proportion Judged Simultaneous",
    limits = c(0, 1)
  ) +
  scale_x_discrete(
    name = "Stimulus Onset Asynchrony (ms)",
    labels = function(x) round(as.numeric(as.character(x)), 2)
  ) +
  labs(title = "Average Perceived Simultaneity by SOA") +
  THEME +
  theme(
    axis.line.x = element_line(color = "grey60"),
    axis.line.y = element_line(color = "grey60")
  )

print(linegraph_soa)
