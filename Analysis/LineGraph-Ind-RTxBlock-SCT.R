##########################################################################
###   Line Graph: Per-Participant Median RTs for AVC Trials by Block   ###
##########################################################################

# ----------------------------
# PLOT
# ----------------------------
linegraph_ind_rtxblock <- ggplot(clean_trials_df %>%
    filter(modality == "AVC") %>%
    group_by(participant_number, block) %>%
    summarise(median_rt = median(rt, na.rm = TRUE), .groups = "drop"),
  aes(
    x = block,
    y = median_rt,
    group = participant_number,
    color = participant_number
  )
) +
  geom_line(alpha = 0.6, linewidth = 0.9) +
  geom_point(size = 2, alpha = 0.8) +
  
  # Group-level median (computed inline)
  geom_line(
    data = clean_trials_df %>%
      filter(modality == "AVC") %>%
      group_by(block) %>%
      summarise(group_median_rt = median(rt, na.rm = TRUE), .groups = "drop"),
    aes(x = block, y = group_median_rt),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1.6
  ) +
  geom_point(
    data = clean_trials_df %>%
      filter(modality == "AVC") %>%
      group_by(block) %>%
      summarise(group_median_rt = median(rt, na.rm = TRUE), .groups = "drop"),
    aes(x = block, y = group_median_rt),
    inherit.aes = FALSE,
    color = "black",
    size = 3
  ) +
  
  scale_x_continuous(name = "Block Number", breaks = 1:7) +
  scale_y_continuous(
    name = "Response Time (ms)",
    breaks = seq(300, 800, 100),
    minor_breaks = seq(300, 800, 50)
  ) +
  coord_cartesian(ylim = c(300, 800)) +
  labs(
    title = "Per-Participant Median RTs for AVC Trials by Block",
    color = "Participant"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12),
    panel.grid.minor.x = element_blank()
  )

print(linegraph_ind_rtxblock)
