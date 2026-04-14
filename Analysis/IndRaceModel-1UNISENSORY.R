#############################
### INDIVIDUAL ΔCDF ONLY   ###
###   (NO DOTS / NO AUC)   ###
#############################

# ----------------------------
# COLORS (yours)
# ----------------------------
cond_colors_map <- c("A" = "C", "AVC" = "D", "V" = "F")
cond_colors <- setNames(unname(COLORS[cond_colors_map]), names(cond_colors_map))
cond_colors <- c(cond_colors, "Miller" = "#333333")

# ----------------------------
# FILTER (yours)
# ----------------------------
filter_clean_trials_df <- function(clean_trials_df) {
  clean_trials_df %>%
    dplyr::filter(
      modality %in% modalities_keep,
      offset %in% offsets
    )
}

# ----------------------------
# participant-level tables (keeps SOA logic + single averaged A/V)
# ----------------------------
compute_race_model_tables_individual <- function(clean_trials_df, pid) {
  
  offset_levels <- as.numeric(offsets)
  offset_label_levels <- paste0(offset_levels, " ms Onset")
  
  df_pid <- clean_trials_df %>%
    dplyr::mutate(offset = as.numeric(offset)) %>%
    dplyr::filter(participant_number == pid)
  
  # ONE averaged ECDF for A and ONE for V (across ALL offsets)
  ecdf_lookup_pid <- df_pid %>%
    dplyr::filter(modality %in% c("A","V")) %>%
    dplyr::group_by(modality) %>%
    dplyr::summarise(ecdf_fun = list(stats::ecdf(rt)), .groups = "drop") %>%
    tibble::deframe()
  
  if (!("A" %in% names(ecdf_lookup_pid)) || !("V" %in% names(ecdf_lookup_pid))) {
    stop(sprintf("Participant %s is missing A and/or V trials after filtering.", pid))
  }
  
  ecdf_A <- ecdf_lookup_pid[["A"]]
  ecdf_V <- ecdf_lookup_pid[["V"]]
  
  cdf_wide <- df_pid %>%
    dplyr::group_by(offset) %>%
    dplyr::group_modify(~{
      dat <- .x
      soa <- .y$offset[1]
      
      ecdf_AVC <- stats::ecdf(dat$rt[dat$modality == "AVC"])
      
      # exact same shift logic
      if (soa < 0) {
        Miller_vals <- pmin(ecdf_A(bins) + ecdf_V(bins + soa), 1)
      } else if (soa > 0) {
        Miller_vals <- pmin(ecdf_A(bins - soa) + ecdf_V(bins), 1)
      } else {
        Miller_vals <- pmin(ecdf_A(bins) + ecdf_V(bins), 1)
      }
      
      tibble::tibble(
        participant_number = pid,
        rt_bin = bins,
        AVC    = ecdf_AVC(bins),
        Miller = Miller_vals
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      offset_label = factor(
        paste0(offset, " ms Onset"),
        levels = offset_label_levels
      ),
      diff = AVC - Miller
    )
  
  cdf_wide
}

# ----------------------------
# ΔCDF ONLY (individual): (AVC - Miller), with positive shading only
# ----------------------------
make_plot_delta_cdf_individual <- function(clean_trials_df, pid) {
  
  cdf_wide <- compute_race_model_tables_individual(clean_trials_df, pid)
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  cdf_clean <- cdf_wide %>%
    dplyr::arrange(offset_label, rt_bin) %>%
    dplyr::filter(!is.na(diff), !is.na(rt_bin))
  
  viol_clip <- cdf_clean %>%
    dplyr::mutate(ymin = 0, ymax = pmax(diff, 0))
  
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.6, alpha = 0.7) +
    ggplot2::geom_ribbon(
      data = viol_clip,
      ggplot2::aes(x = rt_bin, ymin = ymin, ymax = ymax, group = offset_label),
      fill = cond_colors[["AVC"]],
      alpha = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = cdf_wide,
      ggplot2::aes(x = rt_bin, y = diff, group = offset_label),
      color = cond_colors[["AVC"]],
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = paste0("ΔCDF (Individual): ", pid),
      x = "Response Time (ms)",
      y = "ΔCDF (Proportion)"
    ) +
    ggplot2::facet_wrap(~ offset_label, nrow = 1) +
    ggplot2::scale_x_continuous(breaks = x_breaks, name = "Response Time (ms)") +
    ggplot2::scale_y_continuous(
      breaks = seq(-0.2, 0.2, by = 0.2),
      limits = c(-0.2, 0.2),
      oob = scales::squish,
      labels = function(x) sprintf("%.1f", ifelse(abs(x) < 1e-12, 0, x)),
      name = "ΔCDF"
    ) +
    THEME +
    ggplot2::theme(
      aspect.ratio = 1,
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ----------------------------
# RUN: one participant OR all participants
# ----------------------------
df_plot <- filter_clean_trials_df(clean_trials_df)

pids <- df_plot %>%
  dplyr::distinct(participant_number) %>%
  dplyr::pull(participant_number) %>%
  setdiff(exclude_participants)

# show one
print(make_plot_delta_cdf_individual(df_plot, pids[11]))

# save all
# dir.create("RaceModel_Individuals_DeltaOnly", showWarnings = FALSE)
# for (pid in pids) {
#   fig <- make_plot_delta_cdf_individual(df_plot, pid)
#   ggsave(
#     filename = file.path("RaceModel_Individuals_DeltaOnly", paste0("DeltaCDF_", pid, ".png")),
#     plot = fig, width = 14, height = 3.5, dpi = 300
#   )
# }


# ----------------------------
# MULTI-PARTICIPANT ΔCDF COMPARISON
#   (participants as rows, SOA as columns)
# ----------------------------

compute_race_model_tables_multiple <- function(clean_trials_df, pid_vec) {
  dplyr::bind_rows(lapply(pid_vec, function(pid) {
    compute_race_model_tables_individual(clean_trials_df, pid)
  }))
}

make_plot_delta_cdf_multiple <- function(clean_trials_df, pid_vec) {
  
  df_all <- compute_race_model_tables_multiple(clean_trials_df, pid_vec)
  
  x_breaks <- seq(0, max(bins, na.rm = TRUE), by = x_break_by)
  
  df_all_clean <- df_all %>%
    dplyr::arrange(participant_number, offset_label, rt_bin) %>%
    dplyr::filter(!is.na(diff), !is.na(rt_bin))
  
  viol_clip <- df_all_clean %>%
    dplyr::mutate(ymin = 0, ymax = pmax(diff, 0))
  
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, color = "#333333", linewidth = 0.6, alpha = 0.7) +
    ggplot2::geom_ribbon(
      data = viol_clip,
      ggplot2::aes(
        x = rt_bin, ymin = ymin, ymax = ymax,
        group = interaction(participant_number, offset_label)
      ),
      fill = cond_colors[["AVC"]],
      alpha = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = df_all_clean,
      ggplot2::aes(
        x = rt_bin, y = diff,
        group = interaction(participant_number, offset_label)
      ),
      color = cond_colors[["AVC"]],
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = "Individual ΔCDF",
      x = "Response Time (ms)",
      y = "ΔCDF (Proportion)"
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(participant_number),
      cols = ggplot2::vars(offset_label)
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks, name = "Response Time (ms)") +
    ggplot2::scale_y_continuous(
      breaks = seq(-0.2, 0.2, by = 0.2),
      limits = c(-0.2, 0.2),
      oob = scales::squish,
      labels = function(x) sprintf("%.1f", ifelse(abs(x) < 1e-12, 0, x)),
      name = "ΔCDF"
    ) +
    THEME +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ----------------------------
# EXAMPLES
# ----------------------------

# If you literally mean participant numbers 1 and 2:
p_compare <- pids
print(make_plot_delta_cdf_multiple(df_plot, p_compare))

# Or pick from your filtered set:
# p_compare <- pids[c(1,2)]
# print(make_plot_delta_cdf_multiple(df_plot, p_compare))