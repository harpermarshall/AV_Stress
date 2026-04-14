######################################################
###  Bar Graph: RTs by Modality per Participant    ###
######################################################

# ----------------------------
# COLOR SETTINGS
# ----------------------------
# Map colors from chosen pallet
bargraph_ind_colors_map <- c(
  "A"   = "C",
  "AVC" = "D",
  "V"   = "F"
)
bargraph_ind_colors <- setNames(unname(COLORS[bargraph_ind_colors_map]), names(bargraph_ind_colors_map))


# ----------------------------
# PLOT SETTINGS
# ----------------------------
dodge_w <- 0.6
bar_w   <- 0.6
soa_levels <- c("-50.01", "-33.34", "-16.67", "0.00", "16.67", "33.34", "50.01")


# ----------------------------
# DATA ORGANIZATION
# ----------------------------
bar_df <- clean_trials_df %>%
  filter(modality %in% c("A", "V", "AVC")) %>%
  group_by(participant_number, offset, modality) %>%
  summarise(median_rt = median(rt, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    offset_f = factor(sprintf("%.2f", offset), levels = soa_levels)
  )

group_medians <- clean_trials_df %>%
  filter(modality %in% c("A", "V", "AVC")) %>%
  group_by(offset, modality) %>%
  summarise(group_median_rt = median(rt, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    offset_f = factor(sprintf("%.2f", offset), levels = soa_levels)
  )

group_medians_seg <- group_medians %>%
  mutate(
    modality = factor(modality, levels = c("A", "AVC", "V")),
    offset_f = factor(offset_f, levels = soa_levels),
    
    x_base = as.numeric(offset_f),
    
    # manual dodge offsets (A, AVC, V)
    dodge_offset = case_when(
      modality == "A"   ~ -dodge_w/3,
      modality == "AVC" ~  dodge_w/3,
      modality == "V"   ~  0
    ),
    
    x_center = x_base + dodge_offset,
    x_start  = x_center - bar_w/6,
    x_end    = x_center + bar_w/6
  )


# ----------------------------
# PLOT 
# ----------------------------
bargraph_ind_rtxmodality <-
  ggplot(bar_df, aes(x = offset_f, y = median_rt, fill = modality)) +
  geom_col(
    position = position_dodge(width = dodge_w),
    width = bar_w,
    alpha = 0.45  # <-- more translucent so the dark median line shows
  ) +
  # "median tick" inside each bar (group median)
  geom_segment(
    data = group_medians_seg,
    aes(
      x = x_start, xend = x_end,
      y = group_median_rt, yend = group_median_rt,
      color = modality
    ),
    linewidth = 0.8,      # thinner line
    lineend = "butt",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = bargraph_ind_colors) +
  scale_color_manual(values = bargraph_ind_colors, guide = "none") +
  scale_y_continuous(
    name = "Median RT (ms)",
    breaks = seq(300, rt_max, 100)
  ) +
  scale_x_discrete(name = "Visual Offset (ms)", drop = FALSE) +
  coord_cartesian(ylim = c(300, rt_max)) +
  facet_wrap(~ participant_number, ncol = 4) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Median RTs for A, V, and AVC Trials by Participant",
    fill = "Trial Modality"
  ) +
  THEME +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1.4, "lines"),
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold"),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4)
  )

print(bargraph_ind_rtxmodality)
