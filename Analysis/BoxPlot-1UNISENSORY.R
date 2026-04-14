######################################################
###     Box Plot: RT by Modality per Onset         ###
###       (Unisensory conditions collapsed)        ###
######################################################

# ----------------------------
# COLOR SETTINGS
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
  "collapsed" = "D"   # same purple as 0.00
)

onset_colors <- setNames(unname(COLORS[onset_map]), names(onset_map))

# ----------------------------
# PREP DATA
# ----------------------------
plot_df <- clean_trials_df %>%
  mutate(
    onset_chr = case_when(
      modality %in% c("A","V") ~ "collapsed",
      TRUE ~ formatC(offset, format = "f", digits = 2)
    ),
    onset_chr = factor(
      onset_chr,
      levels = c("-50.01","-33.34","-16.67","0.00","16.67","33.34","50.01","collapsed")
    )
  )

# ----------------------------
# PLOT
# ----------------------------
boxplot <- ggplot(
  plot_df,
  aes(
    x = modality,
    y = rt,
    color = onset_chr,
    group = interaction(modality, onset_chr)
  )
) +
  
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.3,
      dodge.width = 1
    ),
    alpha = 0.4,
    size = 0.5,
    stroke = 0.2
  ) +
  
  geom_boxplot(
    position = position_dodge2(width = 0.6, preserve = "single"),
    alpha = 0.5,
    outlier.shape = NA,
    width = 1,
    linewidth = 0.2   # ← controls box outline thickness
  ) +
  
  scale_color_manual(
    values = onset_colors,
    breaks = setdiff(names(onset_colors), "collapsed")
  ) +
  
  scale_y_continuous(
    name = "Response Time (ms)",
    breaks = seq(200, 2000, 200),
    minor_breaks = seq(200, 2000, 50),
    limits = c(200, 2000)
  ) +
  
  labs(
    title = "Response Times",
    x = "Trial Modality",
    color = "SOA"
  ) +
  
  THEME +
  
  theme(
    panel.grid.major.x = element_blank()
  )

print(boxplot)

ggsave(
  filename = "BoxPlot.png",
  plot = boxplot,
  width = 3.35,
  height = 2.75,
  units = "in",
  dpi = 600,
  device = "png",
  bg = "white"
)

