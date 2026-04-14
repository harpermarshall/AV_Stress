# ============================================================
# Libraries
# ============================================================
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# SETTINGS (edit these manually at the top)
# ============================================================

# filtering
offsets <- c(0)
modalities_keep <- c("A", "V", "AVC")
rt_max <- 800

# binning / axes
bins <- seq(200, 800, by = 10)
x_break_by <- 100

# choose theme preset (pick ONE)
# STYLE <- poster_light
# STYLE <- poster_dark
# STYLE <- paper_light
# STYLE <- paper_dark


# ============================================================
# Theme + style factory
# ============================================================
make_style <- function(mode = c("paper", "poster"), bg = c("light", "dark")) {
  mode <- match.arg(mode)
  bg   <- match.arg(bg)
  
  base_size <- if (mode == "poster") 26 else 11
  axis_text <- if (mode == "poster") 26 else 10
  axis_title<- if (mode == "poster") 30 else 12
  title_sz  <- if (mode == "poster") 36 else 14
  sub_sz    <- if (mode == "poster") 26 else 11
  strip_sz  <- if (mode == "poster") 28 else 11
  leg_title <- if (mode == "poster") 28 else 11
  leg_text  <- if (mode == "poster") 24 else 10
  title_mb  <- if (mode == "poster") 6 else 4
  sub_mb    <- if (mode == "poster") 10 else 6
  
  if (bg == "dark") {
    bg_fill   <- "#161521"
    grid_maj  <- "#2A2938"
    grid_min  <- "#2A2938"
    txt       <- "white"
    
    cols <- c(
      "A"      = "#C95C5C",
      "V"      = "#6BBCE0",
      "AVC"    = "#9BD59B",
      "Miller" = "#D9D9D9"
    )
    annot_col <- "#D9D9D9"
  } else {
    bg_fill   <- "white"
    grid_maj  <- if (mode == "poster") "#CCCCCC" else "#D0D0D0"
    grid_min  <- if (mode == "poster") "#E0E0E0" else "#E6E6E6"
    txt       <- "#333333"
    
    cols <- c(
      "A"      = "#A63C3C",
      "V"      = "#4BA3C3",
      "AVC"    = "#8FBC8F",
      "Miller" = "#333333"
    )
    annot_col <- "#333333"
  }
  
  linetypes <- c("A"="solid", "V"="solid", "AVC"="solid", "Miller"="dashed")
  cond_order <- c("Miller", "AVC", "V", "A")
  
  th <- theme_minimal(base_size = base_size, base_family = "Arial") +
    theme(
      plot.background  = element_rect(fill = bg_fill, color = NA),
      panel.background = element_rect(fill = bg_fill, color = NA),
      panel.grid.major = element_line(color = grid_maj),
      panel.grid.minor = element_line(color = grid_min),
      
      axis.text  = element_text(color = txt, size = axis_text),
      axis.title = element_text(color = txt, size = axis_title),
      
      plot.title = element_text(
        hjust = 0.5, size = title_sz, face = "bold", color = txt,
        margin = margin(b = title_mb)
      ),
      plot.subtitle = element_text(
        hjust = 0.5, size = sub_sz, color = txt,
        margin = margin(t = 0, b = sub_mb)
      ),
      
      strip.text = element_text(face = "bold", color = txt, size = strip_sz),
      
      legend.position = "right",
      legend.title = element_text(face = "bold", color = txt, size = leg_title),
      legend.text  = element_text(color = txt, size = leg_text),
      legend.background = element_blank()
    )
  
  list(
    theme = th,
    cols = cols,
    linetypes = linetypes,
    cond_order = cond_order,
    annot_col = annot_col
  )
}

# ============================================================
# Four explicit styles (named exactly as you asked)
# ============================================================
poster_light <- make_style("poster", "light")
poster_dark  <- make_style("poster", "dark")
paper_light  <- make_style("paper",  "light")
paper_dark   <- make_style("paper",  "dark")

# pick one:
STYLE <- paper_dark


# ============================================================
# Data loader
# ============================================================
filter_clean_trials_df <- function(clean_trials_df) {
  clean_trials_df %>%
    filter(
      modality %in% modalities_keep,
      rt <= rt_max,
      offset %in% offsets
    )
}

# ============================================================
# Shared computation: build subject-averaged CDF + wide table
# ============================================================
compute_race_model_tables <- function(clean_trials_df) {
  
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
    mutate(offset_label = paste0(offset, " ms Onset"))
  
  cdf_wide <- cdf_avg_long %>%
    select(offset, offset_label, rt_bin, condition, CDF) %>%
    pivot_wider(names_from = condition, values_from = CDF)
  
  list(cdf_avg_long = cdf_avg_long, cdf_wide = cdf_wide)
}

# ============================================================
# Plot 1: Subject-averaged CDF with violation shading + max marker
# ============================================================
make_plot_cdf_shaded <- function(clean_trials_df, style) {
  tabs <- compute_race_model_tables(clean_trials_df)
  cdf_avg_long <- tabs$cdf_avg_long
  cdf_wide     <- tabs$cdf_wide
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  violation_ribbon <- cdf_wide %>%
    filter(AVC > Miller) %>%
    transmute(
      offset,
      offset_label,
      rt_bin,
      ymin = Miller,
      ymax = AVC
    )
  
  max_violations <- cdf_wide %>%
    mutate(diff = AVC - Miller) %>%
    group_by(offset, offset_label) %>%
    slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  ggplot() +
    geom_ribbon(
      data = violation_ribbon,
      aes(x = rt_bin, ymin = ymin, ymax = ymax),
      alpha = 0.35,
      fill = style$cols[["AVC"]]
    ) +
    geom_line(
      data = cdf_avg_long,
      aes(x = rt_bin, y = CDF, color = condition, linetype = condition),
      linewidth = 1.2
    ) +
    geom_point(
      data = max_violations,
      aes(x = rt_bin, y = AVC),
      size = 3.5,
      color = style$annot_col,
      stroke = 0,
      shape = 21,
      fill = style$annot_col
    ) +
    geom_text(
      data = max_violations,
      aes(x = rt_bin, y = AVC, label = sprintf("+%.2f", AVC - Miller)),
      vjust = -1.0,
      color = style$annot_col,
      size = 6
    ) +
    labs(
      title = "Subject-Averaged Race Model CDF",
      x = "Response Time (ms)",
      y = "Cumulative Probability",
      color = "Condition",
      linetype = "Condition"
    ) +
    scale_color_manual(values = style$cols, breaks = style$cond_order) +
    scale_linetype_manual(values = style$linetypes, breaks = style$cond_order) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = c(0, 1)) +
    style$theme
}

# ============================================================
# Plot 2: ΔCDF (AVC - Miller) with positive shading + max label + AUC
# ============================================================
make_plot_delta_cdf <- function(clean_trials_df, style) {
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
  
  max_abs <- max(abs(cdf_wide$diff), na.rm = TRUE)
  y_lim <- c(-1.05 * max_abs, 1.05 * max_abs)
  
  ggplot() +
    geom_hline(yintercept = 0, color = style$annot_col, linewidth = 0.6, alpha = 0.7) +
    geom_ribbon(
      data = viol_clip,
      aes(x = rt_bin, ymin = ymin, ymax = ymax, group = offset_label),
      fill = style$cols[["AVC"]],
      alpha = 0.35,
      na.rm = TRUE
    ) +
    geom_line(
      data = cdf_wide,
      aes(x = rt_bin, y = diff, group = offset_label),
      color = style$cols[["AVC"]],
      linewidth = 1.1,
      na.rm = TRUE
    ) +
    geom_point(
      data = lab_pos,
      aes(x = rt_bin, y = diff),
      size = 3.5,
      color = style$annot_col,
      shape = 21,
      fill = style$annot_col,
      stroke = 0,
      na.rm = TRUE
    ) +
    geom_text(
      data = lab_pos,
      aes(
        x = rt_bin, y = diff,
        label = paste0(
          "Max ΔCDF=", sprintf("%.3f", diff),
          "\nAUC=", sprintf("%.1f", AUC_pos)
        )
      ),
      vjust = -1.10,
      color = style$annot_col,
      size = 6,
      lineheight = 0.95,
      check_overlap = TRUE,
      na.rm = TRUE
    ) +
    labs(
      title = "Race Model Difference (AVC − Miller)",
      x = "Response Time (ms)",
      y = "ΔCDF (Proportion)"
    ) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = y_lim) +
    style$theme +
    theme(legend.position = "none")
}

# ============================================================
# Run
# ============================================================
# ============================================================
# Libraries
# ============================================================
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# SETTINGS (edit these manually at the top)
# ============================================================

# filtering
offsets <- c(0)
modalities_keep <- c("A", "V", "AVC")
rt_max <- 800

# binning / axes
bins <- seq(200, 800, by = 10)
x_break_by <- 100

# choose theme preset (pick ONE)
# STYLE <- poster_light
# STYLE <- poster_dark
# STYLE <- paper_light
# STYLE <- paper_dark


# ============================================================
# Theme + style factory
# ============================================================
make_style <- function(mode = c("paper", "poster"), bg = c("light", "dark")) {
  mode <- match.arg(mode)
  bg   <- match.arg(bg)
  
  base_size <- if (mode == "poster") 26 else 11
  axis_text <- if (mode == "poster") 26 else 10
  axis_title<- if (mode == "poster") 30 else 12
  title_sz  <- if (mode == "poster") 36 else 14
  sub_sz    <- if (mode == "poster") 26 else 11
  strip_sz  <- if (mode == "poster") 28 else 11
  leg_title <- if (mode == "poster") 28 else 11
  leg_text  <- if (mode == "poster") 24 else 10
  title_mb  <- if (mode == "poster") 6 else 4
  sub_mb    <- if (mode == "poster") 10 else 6
  
  if (bg == "dark") {
    bg_fill   <- "#161521"
    grid_maj  <- "#2A2938"
    grid_min  <- "#2A2938"
    txt       <- "white"
    
    cols <- c(
      "A"      = "#C95C5C",
      "V"      = "#6BBCE0",
      "AVC"    = "#9BD59B",
      "Miller" = "#D9D9D9"
    )
    annot_col <- "#D9D9D9"
  } else {
    bg_fill   <- "white"
    grid_maj  <- if (mode == "poster") "#CCCCCC" else "#D0D0D0"
    grid_min  <- if (mode == "poster") "#E0E0E0" else "#E6E6E6"
    txt       <- "#333333"
    
    cols <- c(
      "A"      = "#A63C3C",
      "V"      = "#4BA3C3",
      "AVC"    = "#8FBC8F",
      "Miller" = "#333333"
    )
    annot_col <- "#333333"
  }
  
  linetypes <- c("A"="solid", "V"="solid", "AVC"="solid", "Miller"="dashed")
  cond_order <- c("Miller", "AVC", "V", "A")
  
  th <- theme_minimal(base_size = base_size, base_family = "Arial") +
    theme(
      plot.background  = element_rect(fill = bg_fill, color = NA),
      panel.background = element_rect(fill = bg_fill, color = NA),
      panel.grid.major = element_line(color = grid_maj),
      panel.grid.minor = element_line(color = grid_min),
      
      axis.text  = element_text(color = txt, size = axis_text),
      axis.title = element_text(color = txt, size = axis_title),
      
      plot.title = element_text(
        hjust = 0.5, size = title_sz, face = "bold", color = txt,
        margin = margin(b = title_mb)
      ),
      plot.subtitle = element_text(
        hjust = 0.5, size = sub_sz, color = txt,
        margin = margin(t = 0, b = sub_mb)
      ),
      
      strip.text = element_text(face = "bold", color = txt, size = strip_sz),
      
      legend.position = "right",
      legend.title = element_text(face = "bold", color = txt, size = leg_title),
      legend.text  = element_text(color = txt, size = leg_text),
      legend.background = element_blank()
    )
  
  list(
    theme = th,
    cols = cols,
    linetypes = linetypes,
    cond_order = cond_order,
    annot_col = annot_col
  )
}

# ============================================================
# Four explicit styles (named exactly as you asked)
# ============================================================
poster_light <- make_style("poster", "light")
poster_dark  <- make_style("poster", "dark")
paper_light  <- make_style("paper",  "light")
paper_dark   <- make_style("paper",  "dark")

# pick one:
STYLE <- paper_dark


# ============================================================
# Data loader
# ============================================================
filter_clean_trials_df <- function(clean_trials_df) {
  clean_trials_df %>%
    filter(
      modality %in% modalities_keep,
      rt <= rt_max,
      offset %in% offsets
    )
}

# ============================================================
# Shared computation: build subject-averaged CDF + wide table
# ============================================================
compute_race_model_tables <- function(clean_trials_df) {
  
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
    mutate(offset_label = paste0(offset, " ms Onset"))
  
  cdf_wide <- cdf_avg_long %>%
    select(offset, offset_label, rt_bin, condition, CDF) %>%
    pivot_wider(names_from = condition, values_from = CDF)
  
  list(cdf_avg_long = cdf_avg_long, cdf_wide = cdf_wide)
}

# ============================================================
# Plot 1: Subject-averaged CDF with violation shading + max marker
# ============================================================
make_plot_cdf_shaded <- function(clean_trials_df, style) {
  tabs <- compute_race_model_tables(clean_trials_df)
  cdf_avg_long <- tabs$cdf_avg_long
  cdf_wide     <- tabs$cdf_wide
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  violation_ribbon <- cdf_wide %>%
    filter(AVC > Miller) %>%
    transmute(
      offset,
      offset_label,
      rt_bin,
      ymin = Miller,
      ymax = AVC
    )
  
  max_violations <- cdf_wide %>%
    mutate(diff = AVC - Miller) %>%
    group_by(offset, offset_label) %>%
    slice_max(order_by = diff, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  ggplot() +
    geom_ribbon(
      data = violation_ribbon,
      aes(x = rt_bin, ymin = ymin, ymax = ymax),
      alpha = 0.35,
      fill = style$cols[["AVC"]]
    ) +
    geom_line(
      data = cdf_avg_long,
      aes(x = rt_bin, y = CDF, color = condition, linetype = condition),
      linewidth = 1.2
    ) +
    geom_point(
      data = max_violations,
      aes(x = rt_bin, y = AVC),
      size = 3.5,
      color = style$annot_col,
      stroke = 0,
      shape = 21,
      fill = style$annot_col
    ) +
    geom_text(
      data = max_violations,
      aes(x = rt_bin, y = AVC, label = sprintf("+%.2f", AVC - Miller)),
      vjust = -1.0,
      color = style$annot_col,
      size = 6
    ) +
    labs(
      title = "Subject-Averaged Race Model CDF",
      x = "Response Time (ms)",
      y = "Cumulative Probability",
      color = "Condition",
      linetype = "Condition"
    ) +
    scale_color_manual(values = style$cols, breaks = style$cond_order) +
    scale_linetype_manual(values = style$linetypes, breaks = style$cond_order) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = c(0, 1)) +
    style$theme
}

# ============================================================
# Plot 2: ΔCDF (AVC - Miller) with positive shading + max label + AUC
# ============================================================
make_plot_delta_cdf <- function(clean_trials_df, style) {
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
  
  max_abs <- max(abs(cdf_wide$diff), na.rm = TRUE)
  y_lim <- c(-1.05 * max_abs, 1.05 * max_abs)
  
  ggplot() +
    geom_hline(yintercept = 0, color = style$annot_col, linewidth = 0.6, alpha = 0.7) +
    geom_ribbon(
      data = viol_clip,
      aes(x = rt_bin, ymin = ymin, ymax = ymax, group = offset_label),
      fill = style$cols[["AVC"]],
      alpha = 0.35,
      na.rm = TRUE
    ) +
    geom_line(
      data = cdf_wide,
      aes(x = rt_bin, y = diff, group = offset_label),
      color = style$cols[["AVC"]],
      linewidth = 1.1,
      na.rm = TRUE
    ) +
    geom_point(
      data = lab_pos,
      aes(x = rt_bin, y = diff),
      size = 3.5,
      color = style$annot_col,
      shape = 21,
      fill = style$annot_col,
      stroke = 0,
      na.rm = TRUE
    ) +
    geom_text(
      data = lab_pos,
      aes(
        x = rt_bin, y = diff,
        label = paste0(
          "Max ΔCDF=", sprintf("%.3f", diff),
          "\nAUC=", sprintf("%.1f", AUC_pos)
        )
      ),
      vjust = -1.10,
      color = style$annot_col,
      size = 6,
      lineheight = 0.95,
      check_overlap = TRUE,
      na.rm = TRUE
    ) +
    labs(
      title = "Race Model Difference (AVC − Miller)",
      x = "Response Time (ms)",
      y = "ΔCDF (Proportion)"
    ) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(limits = y_lim) +
    style$theme +
    theme(legend.position = "none")
}

# ============================================================
# Run
# ============================================================
df_plot <- filter_clean_trials_df(clean_trials_df)

p1 <- make_plot_cdf_shaded(df_plot, STYLE)
p2 <- make_plot_delta_cdf(df_plot, STYLE)

p1
p2

