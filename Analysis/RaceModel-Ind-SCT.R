# ----------------------------
# PER-PARTICIPANT CDF TABLES (no averaging)
# ----------------------------
compute_race_model_tables_by_participant <- function(clean_trials_df) {
  
  offset_levels <- offsets
  offset_levels <- as.numeric(offset_levels)
  offset_label_levels <- paste0(offset_levels, " ms Onset")
  
  clean_trials_df <- clean_trials_df %>%
    mutate(
      offset = as.numeric(offset),
      offset_label = factor(paste0(offset, " ms Onset"), levels = offset_label_levels),
      participant_number = as.factor(participant_number)
    )
  
  # ECDFs computed within participant × offset
  cdf_by_subj <- clean_trials_df %>%
    group_by(participant_number, offset, offset_label) %>%
    group_modify(~{
      dat <- .x
      ecdf_A   <- ecdf(dat$rt[dat$modality == "A"])
      ecdf_V   <- ecdf(dat$rt[dat$modality == "V"])
      ecdf_AVC <- ecdf(dat$rt[dat$modality == "AVC"])
      
      tibble(
        rt_bin = bins,
        A      = ecdf_A(bins),
        V      = ecdf_V(bins),
        AVC    = ecdf_AVC(bins),
        Miller = pmin(ecdf_A(bins) + ecdf_V(bins), 1)
      )
    }) %>%
    ungroup()
  
  cdf_wide <- cdf_by_subj %>%
    mutate(diff = AVC - Miller)
  
  cdf_wide
}

# ----------------------------
# PER-PARTICIPANT CDF TABLES (no averaging)
# ----------------------------
compute_race_model_tables_by_participant <- function(clean_trials_df) {
  
  offset_levels <- offsets
  offset_levels <- as.numeric(offset_levels)
  offset_label_levels <- paste0(offset_levels, " ms Onset")
  
  clean_trials_df <- clean_trials_df %>%
    mutate(
      offset = as.numeric(offset),
      offset_label = factor(paste0(offset, " ms Onset"), levels = offset_label_levels),
      participant_number = as.factor(participant_number)
    )
  
  # ECDFs computed within participant × offset
  cdf_by_subj <- clean_trials_df %>%
    group_by(participant_number, offset, offset_label) %>%
    group_modify(~{
      dat <- .x
      ecdf_A   <- ecdf(dat$rt[dat$modality == "A"])
      ecdf_V   <- ecdf(dat$rt[dat$modality == "V"])
      ecdf_AVC <- ecdf(dat$rt[dat$modality == "AVC"])
      
      tibble(
        rt_bin = bins,
        A      = ecdf_A(bins),
        V      = ecdf_V(bins),
        AVC    = ecdf_AVC(bins),
        Miller = pmin(ecdf_A(bins) + ecdf_V(bins), 1)
      )
    }) %>%
    ungroup()
  
  cdf_wide <- cdf_by_subj %>%
    mutate(diff = AVC - Miller)
  
  cdf_wide
}

# ----------------------------
# PLOT: ΔCDF (AVC - Miller) PER PARTICIPANT (rows) × OFFSET (cols)
# ----------------------------
make_plot_delta_cdf_by_participant <- function(clean_trials_df) {
  
  cdf_wide <- compute_race_model_tables_by_participant(clean_trials_df)
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  # Positive-violation shading (clip at 0)
  cdf_clean <- cdf_wide %>%
    arrange(participant_number, offset_label, rt_bin) %>%
    filter(!is.na(diff), !is.na(rt_bin))
  
  viol_clip <- cdf_clean %>%
    mutate(ymin = 0, ymax = pmax(diff, 0))
  
  # AUC per participant × offset
  auc_df <- cdf_clean %>%
    group_by(participant_number, offset_label) %>%
    summarise(
      AUC_pos = sum(
        (lead(rt_bin) - rt_bin) * (pmax(diff, 0) + pmax(lead(diff), 0)) / 2,
        na.rm = TRUE
      ),
      AUC_neg = sum(
        (lead(rt_bin) - rt_bin) * (pmax(-diff, 0) + pmax(-lead(diff), 0)) / 2,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  # Max ΔCDF per participant × offset (for optional labels/dots)
  lab_pos <- cdf_clean %>%
    group_by(participant_number, offset_label) %>%
    slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(auc_df, by = c("participant_number", "offset_label"))
  
  # If you want labels only on specific offsets, keep your existing vector:
  label_offsets_keep <- c(
    "-50.01 ms Onset",
    "0 ms Onset",
    "-16.67 ms Onset"
  )
  
  lab_pos_text <- lab_pos %>%
    filter(offset_label %in% label_offsets_keep)
  
  lab_pos_dots <- lab_pos_text
  
  ggplot() +
    geom_hline(yintercept = 0, color = "#333333", linewidth = 0.6, alpha = 0.7) +
    geom_ribbon(
      data = viol_clip,
      aes(x = rt_bin, ymin = ymin, ymax = ymax),
      fill = cond_colors[["AVC"]],
      alpha = 0.35,
      na.rm = TRUE
    ) +
    geom_line(
      data = cdf_clean,
      aes(x = rt_bin, y = diff),
      color = cond_colors[["AVC"]],
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    geom_point(
      data = lab_pos_dots,
      aes(x = rt_bin, y = diff),
      size = 2,
      color = "#333333",
      shape = 21,
      fill = "#333333",
      stroke = 0,
      na.rm = TRUE
    ) +
    geom_text(
      data = lab_pos_text,
      aes(
        x = rt_bin, y = diff,
        label = sprintf("Max ΔCDF = %.3f\nAUC = %.1f", diff, AUC_pos)
      ),
      inherit.aes = FALSE,
      vjust = -0.9,
      hjust = 0.5,
      color = "#333333",
      size = 2.5,
      lineheight = 0.95,
      fontface = "plain",
      na.rm = TRUE
    ) +
    labs(
      x = "Response Time (ms)",
      y = "ΔCDF"
    ) +
    facet_grid(
      rows = vars(participant_number),
      cols = vars(offset_label),
      switch = "y"
    ) +
    scale_x_continuous(breaks = x_breaks, name = "Response Time (ms)") +
    scale_y_continuous(
      breaks = seq(-0.3, 0.2, by = 0.2),
      limits = c(-0.3, 0.2),
      oob = scales::squish,
      labels = function(x) sprintf("%.1f", ifelse(abs(x) < 1e-12, 0, x)),
      name = "ΔCDF"
    ) +
    THEME +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y  = element_text(size = 6)
    )
}

df_plot <- filter_clean_trials_df(clean_trials_df)

p_delta_by_subj <- make_plot_delta_cdf_by_participant(df_plot) +
  geom_vline(xintercept = -Inf, linewidth = 0.6)

print(p_delta_by_subj)