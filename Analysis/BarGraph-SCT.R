############################################################
###      Bar Graph: Median RT by Modality per Onset     ###
############################################################

# ----------------------------
# COLOR SETTINGS
# ----------------------------
# Map colors from chosen pallet
onset_map <- c(
  "-50.01" = "A",
  "-33.34" = "B",
  "-16.67" = "C",
  "0"      = "D",
  "0.00"   = "D",
  "16.67"  = "E",
  "33.34"  = "F",
  "50.01"  = "G"
)

onset_colors <- setNames(unname(COLORS[onset_map]), names(onset_map))

# ----------------------------
# PLOT
# ----------------------------
onset_levels <- c("-50.01","-33.34","-16.67","0.00","16.67","33.34","50.01")

median_summary_modality <- clean_trials_df %>%
  mutate(
    modality = factor(modality, levels = c("A","V","AVC","AVI")),
    onset_chr = factor(
      formatC(offset, format = "f", digits = 2),
      levels = onset_levels
    )
  ) %>%
  group_by(modality, onset_chr) %>%
  summarise(median_rt = median(rt, na.rm = TRUE), .groups = "drop")

bargraph <- ggplot(median_summary_modality,
                   aes(x = modality, y = median_rt, fill = onset_chr)) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  scale_fill_manual(values = onset_colors, breaks = names(onset_colors), name = "Onset (ms)") +
  labs(
    title = "Median Response Time by\nTrial Modality and Stimulus Onset",
    x = "Trial Modality",
    y = "Median Response Time (ms)",
  ) +
  coord_cartesian(ylim = c(0, 800)) +
  THEME +
  theme(
    panel.grid.major.x = element_blank(),
    
    # make axes grey instead of black
    axis.line.x = element_line(color = "grey60"),
    axis.line.y = element_line(color = "grey60"),
    
    # (optional but usually desired for consistency)
    axis.ticks = element_line(color = "grey60"),
  )

print(bargraph)

