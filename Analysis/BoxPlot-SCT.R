######################################################
###        Box Plot: RT by Modality per Onset     ###
######################################################

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
# Create box plots for each onset
boxplot <- ggplot(clean_trials_df,
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
  scale_color_manual(values = onset_colors) +
  scale_y_continuous(
    name = "Response Time (ms)",
    breaks = seq(200, 2000, 200),
    minor_breaks = seq(200, 2000, 50),
    limits = c(200, 2000)
  ) +
  labs(
    title = "Response Time by Trial Modality and SOA",
    x = "Trial Modality",
    color = "SOA"
  ) +
  THEME +
  theme(
    panel.grid.major.x = element_blank(),
  )

print(boxplot)
