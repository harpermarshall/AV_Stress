############################################################
###   Bar Graph: Median RT by Modality (per participant)  ###
############################################################

# ----------------------------
# COLOR SETTINGS (unchanged)
# ----------------------------
onset_map <- c(
  "-50.01" = "A",
  "-33.34" = "B",
  "-16.67" = "C",
  "0"      = "D",
  "0.00"   = "D",
  "16.67"  = "E",
  "33.34"  = "F",
  "50.01"  = "G",
  "collapsed" = "D"
)

onset_colors <- setNames(unname(COLORS[onset_map]), names(onset_map))
onset_levels <- c("-50.01","-33.34","-16.67","0.00","16.67","33.34","50.01","collapsed")

# ----------------------------
# SUMMARIZE DATA (ADD participant_number)
# ----------------------------
median_summary_modality_pp <- clean_trials_df %>%
  mutate(
    modality = factor(modality, levels = c("A","V","AVC","AVI")),
    onset_chr = case_when(
      modality %in% c("A","V") ~ "collapsed",
      TRUE ~ formatC(offset, format = "f", digits = 2)
    ),
    onset_chr = factor(onset_chr, levels = onset_levels)
  ) %>%
  group_by(participant_number, modality, onset_chr) %>%
  summarise(
    median_rt = median(rt, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# PLOT (FACET BY PARTICIPANT)
# ----------------------------
bargraph_pp <- ggplot(
  median_summary_modality_pp,
  aes(x = modality, y = median_rt, fill = onset_chr)
) +
  geom_col(position = position_dodge(0.6), width = 0.6) +
  scale_fill_manual(
    values = onset_colors,
    breaks = setdiff(onset_levels, "collapsed"),
    name = "Onset (ms)"
  ) +
  labs(
    title = "Median Response Times (Per Participant)",
    x = "Trial Modality",
    y = "Median Response Time (ms)"
  ) +
  coord_cartesian(ylim = c(200, 800)) +
  THEME +
  theme(
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "grey60"),
    axis.line.y = element_line(color = "grey60"),
    axis.ticks  = element_line(color = "grey60")
  ) +
  facet_wrap(~ participant_number)

print(bargraph_pp)

ggsave(
  filename = "BarGraph_MedianRT_PerParticipant_Facets.png",
  plot = bargraph_pp,
  width = 7.0,
  height = 4.0,
  units = "in",
  dpi = 600,
  device = "png",
  bg = "white"
)
