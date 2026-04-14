#########################################################
###   Line Graph: Individual Perceived Simultaneity   ###
#########################################################

# ----------------------------
# PLOT
# ----------------------------
linegraph_ind_soa <- ggplot(
  clean_SOA_df %>%
    group_by(participant_number, soa_factor) %>%
    summarise(
      proportion_simultaneous = mean(simultaneous, na.rm = TRUE),
      n_trials = n(),
      .groups = "drop"
    ),
  aes(
    x = soa_factor,
    y = proportion_simultaneous,
    group = 1
  )
) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "gray"
  ) +
  geom_line(
    linewidth = 1,
    color = "#9e91c3"
  ) +
  geom_point(
    size = 2,
    color = "#9e91c3"
  ) +
  scale_y_continuous(
    name = "Proportion Judged Simultaneous",
    limits = c(0, 1)
  ) +
  scale_x_discrete(
    name = "Stimulus Onset Asynchrony (SOA)"
  ) +
  facet_wrap(
    ~ participant_number,
    ncol = 4
  ) +
  labs(
    title = "Perceived Simultaneity by SOA per Participant"
  ) +
  THEME

print(linegraph_ind_soa)
