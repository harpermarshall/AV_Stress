#######################################################
###      COMBINED MEDIAN AND DISTRIBUTION GRAPH     ###
#######################################################

library(ggtext)


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
      ifelse(offset == 0, "0", formatC(offset, format = "f", digits = 2)),
      levels = c("-50.01","-33.34","-16.67","0","16.67","33.34","50.01")
    )
  ) %>%
  group_by(modality, onset_chr) %>%
  summarise(median_rt = median(rt, na.rm = TRUE), .groups = "drop")

bargraph <- ggplot(median_summary_modality,
                   aes(x = modality, y = median_rt, fill = onset_chr)) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  scale_fill_manual(values = onset_colors, breaks = names(onset_colors), name = "SOA (ms)") +
  labs(
    title = "Median Response Time by Modality and SOA",
    x = "Trial Modality",
    y = "Response Time (ms)",
  ) +
  coord_cartesian(ylim = c(300, 700)) +
  THEME +
  theme(
    plot.title = element_textbox_simple(
      width = unit(0.75, "npc"),   # <- narrower title box
      margin = margin(b = 8),
      halign = 0.5,                  # left aligned
      face = "bold",
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title.position = "plot"
  )

print(bargraph)

##############################################################################

# ----------------------------
# PLOT
# ----------------------------
# Create box plots for each onset
boxplot <- ggplot(
  clean_trials_df,
  aes(
    x = modality,
    y = rt,
    color = factor(offset),
    group = interaction(modality, offset)
  )
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
    alpha = 0.4
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.6),
    alpha = 0.5,
    outlier.shape = NA,
    width = 0.5
  ) +
  scale_color_manual(
    values = onset_colors,
    guide = "none"    # 🔒 hard kill legend at scale level
  ) +
  scale_y_continuous(
    name = "Response Time (ms)",
    breaks = seq(200, 2000, 200),
    minor_breaks = seq(200, 2000, 50),
    limits = c(200, 2000)
  ) +
  labs(
    title = "Response Time Distributions by Modality and SOA",
    x = "Trial Modality"
  ) +
  THEME +
  theme(
    plot.title = element_textbox_simple(
      width = unit(0.75, "npc"),
      margin = margin(b = 8),
      halign = 0.5,
      face = "bold"
    ),
    plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",   # 🔒 backup kill
    axis.title.x = element_blank(),
    axis.text.x  = element_blank()
  )

print(boxplot)

##############################################################################

combined_rt_figure <-
  (boxplot / plot_spacer() / bargraph) +
  plot_layout(
    guides = "collect",
    heights = c(1, 0.15, 1)
  ) &
  theme(
    legend.position = "right",
    legend.box.just = "center",
    legend.justification = "center"
  )

print(combined_rt_figure)
