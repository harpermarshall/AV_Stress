#############################
###        Race Model     ###
#############################

# ----------------------------
# COLOR + LINE SETTINGS
# ----------------------------
# Map colors from chosen pallet
cond_colors_map <- c(
  "A"   = "C",
  "AVC" = "D",
  "V"   = "F"
  )
cond_colors <- setNames(unname(COLORS[cond_colors_map]), names(cond_colors_map))
cond_colors <- c(cond_colors, "Miller" = "#333333")


linetypes <- c(
  "A"   = "solid",
  "AVC" = "solid",
  "V"   = "solid",
  "Miller" = "dashed"
)

cond_order <- c("Miller", "AVC", "V", "A")


# ----------------------------
# SHARED COMPUTATIONS
# ----------------------------
# Load Data
filter_clean_trials_df <- function(clean_trials_df) {
  clean_trials_df %>%
    filter(
      modality %in% modalities_keep,
      rt <= rt_max,
      offset %in% offsets
    )
}

# Build subject-averaged CDF + wide table
compute_race_model_tables <- function(clean_trials_df) {
  
  # ---- FORCE facet order (left -> right) using your `offsets` vector ----
  offset_levels <- offsets                   # e.g., c(-50.01, -33.34, -16.67, 0, 16.67, 33.34, 50.01)
  offset_levels <- as.numeric(offset_levels) # just in case
  offset_label_levels <- paste0(offset_levels, " ms Onset")
  
  clean_trials_df <- clean_trials_df %>%
    mutate(offset = as.numeric(offset))
  
  cdf_by_subj <- clean_trials_df %>%
    group_by(offset, participant_number) %>%
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
  
  cdf_avg_long <- cdf_by_subj %>%
    group_by(offset, rt_bin) %>%
    summarise(across(c(A, V, AVC, Miller), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(
      cols = c(A, V, AVC, Miller),
      names_to  = "condition",
      values_to = "CDF"
    ) %>%
    mutate(
      offset_label = factor(
        paste0(offset, " ms Onset"),
        levels = offset_label_levels
      )
    )
  
  cdf_wide <- cdf_avg_long %>%
    select(offset, offset_label, rt_bin, condition, CDF) %>%
    pivot_wider(names_from = condition, values_from = CDF)
  
  list(cdf_avg_long = cdf_avg_long, cdf_wide = cdf_wide)
}


# ----------------------------
# PLOT 1: Subject-averaged CDF with violation shading + max marker
# ----------------------------
make_plot_cdf_shaded <- function(clean_trials_df) {
  
  tabs <- compute_race_model_tables(clean_trials_df)
  cdf_avg_long <- tabs$cdf_avg_long
  cdf_wide     <- tabs$cdf_wide
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  violation_ribbon <- cdf_wide %>%
    mutate(is_violation = AVC > Miller) %>%
    filter(is_violation) %>%
    transmute(
      offset_label,
      rt_bin,
      ymin = Miller,
      ymax = AVC
    )
  
  max_violations <- cdf_wide %>%
    mutate(diff = AVC - Miller) %>%
    group_by(offset_label) %>%
    slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  ggplot(
    cdf_avg_long,
    aes(x = rt_bin, y = CDF, color = condition, linetype = condition)
  ) +
    geom_ribbon(
      data = violation_ribbon,
      aes(x = rt_bin, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = cond_colors[["AVC"]],
      alpha = 0.35
    ) +
    geom_line(linewidth = 0.4) +
    #geom_point(
      #data = max_violations,
      #aes(x = rt_bin, y = AVC),
      #inherit.aes = FALSE,
      #size = 2,
      #shape = 21,
      #fill = "#333333",
      #color = "#333333",
      #stroke = 0
    #) +
    #geom_text(
      #data = max_violations,
      #aes(x = rt_bin, y = AVC, label = sprintf("+%.2f", AVC - Miller)),
      #inherit.aes = FALSE,
      #vjust = -0.9,
      #hjust = 0.5,
      #color = "#333333",
      #size = 2.5,          # smaller = more “paper”
      #lineheight = 0.95,
      #fontface = "plain"
    #) #+
    scale_color_manual(values = cond_colors, breaks = cond_order) +
    scale_linetype_manual(values = linetypes, breaks = cond_order) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(name = "Response Time (ms)", breaks = x_breaks) +
    scale_y_continuous(name = "Cumulative Probability", limits = c(0, 1)) +
    labs(
      title = "Subject-Averaged Race Model CDF",
      color = "Condition",
      linetype = "Condition"
    ) +
    THEME +
    theme(
      # remove all gridlines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


# ----------------------------
# PLOT 2: ΔCDF (AVC - Miller) with positive shading + max label + AUC
# ----------------------------
make_plot_delta_cdf <- function(clean_trials_df) {
  tabs <- compute_race_model_tables(clean_trials_df)
  cdf_wide <- tabs$cdf_wide %>%
    mutate(diff = AVC - Miller)
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  auc_df <- cdf_wide %>%
    arrange(offset_label, rt_bin) %>%
    group_by(offset_label) %>%
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
  
  cdf_clean <- cdf_wide %>%
    arrange(offset_label, rt_bin) %>%
    filter(!is.na(diff), !is.na(rt_bin))
  
  viol_clip <- cdf_clean %>%
    mutate(ymin = 0, ymax = pmax(diff, 0))
  
  lab_pos <- cdf_wide %>%
    filter(!is.na(diff)) %>%
    group_by(offset_label) %>%
    slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    left_join(auc_df, by = "offset_label")
  
  # ---- offsets that KEEP internal labels (and now ALSO keep dots) ----
  label_offsets_keep <- c(
    "-50.01 ms Onset",
    "0 ms Onset",
    "-16.67 ms Onset"
  )
  
  lab_pos_text <- lab_pos %>%
    filter(offset_label %in% label_offsets_keep)
  
  # dots should exist ONLY where labels exist
  lab_pos_dots <- lab_pos_text
  # -------------------------------------------------------------------
  
  max_abs <- max(abs(cdf_wide$diff), na.rm = TRUE)
  y_lim <- c(-1.05 * max_abs, 1.05 * max_abs)
  
  ggplot() +
    geom_hline(yintercept = 0, color = "#333333", linewidth = 0.6, alpha = 0.7) +
    geom_ribbon(
      data = viol_clip,
      aes(x = rt_bin, ymin = ymin, ymax = ymax, group = offset_label),
      fill = cond_colors[["AVC"]],
      alpha = 0.35,
      na.rm = TRUE
    ) +
    geom_line(
      data = cdf_wide,
      aes(x = rt_bin, y = diff, group = offset_label),
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
      y = "ΔCDF (Proportion)"
    ) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(breaks = x_breaks, name = "Response Time (ms)") +
    scale_y_continuous(
      breaks = seq(-0.3, 0.2, by = 0.1),
      limits = c(-0.3, 0.2),
      oob = scales::squish,
      labels = function(x) sprintf("%.1f", ifelse(abs(x) < 1e-12, 0, x)),
      name = "ΔCDF"
    ) +
    THEME +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ----------------------------
# CREATE PLOTS
# ----------------------------
df_plot <- filter_clean_trials_df(clean_trials_df)

p1 <- make_plot_cdf_shaded(df_plot) +
  geom_vline(xintercept = -Inf, linewidth = 0.6) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p2 <- make_plot_delta_cdf(df_plot) +
  geom_vline(xintercept = -Inf, linewidth = 0.6) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )

combined <- (p1 / p2) +
  plot_layout(heights = c(0.8, 1))

print(combined)

