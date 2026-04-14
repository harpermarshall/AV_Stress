###########################################################################
###   Line Graph: Per-Participant Median RTs for AVC Trials by Offset   ###
###########################################################################

# ----------------------------
# PLOT
# ----------------------------
linegraph_ind_rtxoffset <- clean_trials_df %>%
  filter(modality == "AVC") %>%
  group_by(participant_number, offset) %>%
  summarise(median_rt = median(rt, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(
    x = offset,
    y = median_rt,
    group = participant_number,
    color = participant_number
  )) +
  geom_line(alpha = 0.7, linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(name = "Visual Offset (ms)",
                     breaks = sort(unique(clean_trials_df$offset))) +
  scale_y_continuous(name = "Response Time (ms)",
                     breaks = seq(300, 800, 100),
                     minor_breaks = seq(300, 800, 50)) +
  coord_cartesian(ylim = c(300, 800)) +
  labs(title = "Per-Participant Median RTs for AVC Trials by Offset", color = "Participant") +
  THEME

print(linegraph_ind_rtxoffset)
