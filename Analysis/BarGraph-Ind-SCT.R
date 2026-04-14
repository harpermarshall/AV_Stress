###############################################################
### Individual Bar Graphs: Median RT by Modality per Offset ###
###############################################################

# ----------------------------
# COLOR SETTINGS
# ----------------------------
# Map colors from chosen pallet
offset_map <- c(
  "-50.01" = "A",
  "-33.34" = "B",
  "-16.67" = "C",
  "0"      = "D",
  "0.00"   = "D",
  "16.67"  = "E",
  "33.34"  = "F",
  "50.01"  = "G"
)

offset_colors <- setNames(unname(COLORS[offset_map]), names(offset_map))


# ----------------------------
# PLOT
# ----------------------------
median_summary_participant <- clean_trials_df %>%
  group_by(participant_number, modality, offset) %>%
  summarise(
    median_rt = median(rt, na.rm = TRUE),
    .groups = "drop"
  )

bargraph_ind <- ggplot(median_summary_participant, aes(x = modality, y = median_rt, fill = factor(offset))) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  facet_wrap(~ participant_number) +
  scale_fill_manual(values = offset_colors, name = "Offset (ms)") +
  coord_cartesian(ylim = c(300, 800)) +
  labs(
    title = "Median RT by Trial Modality and Offset (Per Participant)",
    x = "Trial Modality",
    y = "Median RT (ms)"
  ) +
  THEME +
  theme(
    panel.grid.major.x = element_blank(),
  )

print (bargraph_ind)
