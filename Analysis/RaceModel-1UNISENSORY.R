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
      offset %in% offsets
    )
}

# Build subject-averaged CDF + wide table
compute_race_model_tables <- function(clean_trials_df) {
  
  # ---- FORCE facet order ----
  offset_levels <- offsets
  offset_levels <- as.numeric(offset_levels)
  offset_label_levels <- paste0(round(offset_levels), " ms Onset")
  
  clean_trials_df <- clean_trials_df %>%
    mutate(offset = as.numeric(offset))
  
  # ----------------------------
  # NEW: compute participant-level A and V ECDFs averaged across ALL offsets
  # ----------------------------
  ecdf_lookup <- clean_trials_df %>%
    filter(modality %in% c("A","V")) %>%
    group_by(participant_number, modality) %>%
    summarise(
      ecdf_fun = list(ecdf(rt)),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = modality, values_from = ecdf_fun)
  
  # ----------------------------
  # original structure preserved
  # ----------------------------
  cdf_by_subj <- clean_trials_df %>%
    group_by(offset, participant_number) %>%
    group_modify(~{
      
      dat <- .x
      pid <- .y$participant_number[1]
      soa <- .y$offset[1]
      
      # get averaged ECDFs
      ecdf_A <- ecdf_lookup$A[[ which(ecdf_lookup$participant_number == pid) ]]
      ecdf_V <- ecdf_lookup$V[[ which(ecdf_lookup$participant_number == pid) ]]
      
      # AVC still offset-specific (unchanged)
      ecdf_AVC <- ecdf(dat$rt[dat$modality == "AVC"])
      
      # SAME shift logic, but now using averaged A and V
      if (soa < 0) {
        
        Miller_vals <- pmin(
          ecdf_A(bins) +
            ecdf_V(bins + soa),
          1
        )
        
      } else if (soa > 0) {
        
        Miller_vals <- pmin(
          ecdf_A(bins - soa) +
            ecdf_V(bins),
          1
        )
        
      } else {
        
        Miller_vals <- pmin(
          ecdf_A(bins) +
            ecdf_V(bins),
          1
        )
      }
      
      tibble(
        rt_bin = bins,
        A      = ecdf_A(bins),
        V      = ecdf_V(bins),
        AVC    = ecdf_AVC(bins),
        Miller = Miller_vals
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
        paste0(round(offset), " ms Onset"),
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
      aspect.ratio = 1,   # ← ADD THIS LINE
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
    "-50 ms Onset",
    "0 ms Onset",
    "-17 ms Onset",
    "-33 ms Onset",
    "16 ms Onset"
  )
  
  lab_pos_text <- lab_pos %>%
    filter(offset_label %in% label_offsets_keep) %>%
    mutate(
      x_label = max(bins) * 0.55,   # 60% across panel
      y_label = max(diff, na.rm = TRUE) * 2.7  # near top
    )
  
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
        x = x_label,
        y = y_label,
        label = sprintf("Max ΔCDF = %.3f\nAUC = %.1f", diff, AUC_pos)
      ),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 1,
      size = 2,
      color = "#333333"
    ) +
    labs(
      x = "Response Time (ms)",
      y = "ΔCDF (Proportion)"
    ) +
    facet_wrap(~ offset_label, nrow = 1) +
    scale_x_continuous(breaks = x_breaks, name = "Response Time (ms)") +
    scale_y_continuous(
      breaks = seq(-0.2, 0.2, by = 0.1),
      limits = c(-0.2, 0.2),
      oob = scales::squish,
      labels = function(x) sprintf("%.1f", ifelse(abs(x) < 1e-12, 0, x)),
      name = "ΔCDF"
    ) +
    THEME +
    theme(
      aspect.ratio = 1,   # ← ADD THIS LINE
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
  plot_layout(heights = c(1, 1))

print(combined)

# ggsave(
#   filename = "NEWRaceModel.png",
#   plot = combined,
#   width = 8,
#   height = 3,
#   units = "in",
#   dpi = 600,
#   bg = "white"
# )




# ----------------------------
# PERMUTATION TEST (Gondan & Minakata, 2016)
# USING AVERAGED A AND V BASELINE
# ----------------------------
run_race_model_permutation_test <- function(clean_trials_df, n_perm = 5000) {
  
  clean_trials_df <- clean_trials_df %>%
    mutate(offset = as.numeric(offset))
  
  # ----------------------------
  # NEW: averaged A and V ECDF lookup (across all offsets)
  # ----------------------------
  ecdf_lookup <- clean_trials_df %>%
    filter(modality %in% c("A","V")) %>%
    group_by(participant_number, modality) %>%
    summarise(
      ecdf_fun = list(ecdf(rt)),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = modality, values_from = ecdf_fun)
  
  results <- list()
  
  for (soa in offsets) {
    
    dat_soa <- clean_trials_df %>%
      filter(offset == soa)
    
    participants <- unique(dat_soa$participant_number)
    
    diff_mat <- matrix(
      NA,
      nrow = length(participants),
      ncol = length(bins)
    )
    
    for (i in seq_along(participants)) {
      
      pid <- participants[i]
      
      dat_p <- dat_soa %>%
        filter(participant_number == pid)
      
      # averaged ECDFs (NEW)
      ecdf_A <- ecdf_lookup$A[[ which(ecdf_lookup$participant_number == pid) ]]
      ecdf_V <- ecdf_lookup$V[[ which(ecdf_lookup$participant_number == pid) ]]
      
      # AVC remains offset-specific
      ecdf_AVC <- ecdf(dat_p$rt[dat_p$modality == "AVC"])
      
      # SAME Miller shift logic
      if (soa < 0) {
        
        Miller <- pmin(
          ecdf_A(bins) +
            ecdf_V(bins + soa),
          1
        )
        
      } else if (soa > 0) {
        
        Miller <- pmin(
          ecdf_A(bins - soa) +
            ecdf_V(bins),
          1
        )
        
      } else {
        
        Miller <- pmin(
          ecdf_A(bins) +
            ecdf_V(bins),
          1
        )
      }
      
      diff_mat[i, ] <- ecdf_AVC(bins) - Miller
    }
    
    # observed Tmax
    observed_Tmax <- max(colMeans(diff_mat))
    
    # permutation distribution
    perm_Tmax <- numeric(n_perm)
    
    for (p in 1:n_perm) {
      
      signs <- sample(
        c(-1, 1),
        size = nrow(diff_mat),
        replace = TRUE
      )
      
      permuted <- diff_mat * signs
      
      perm_Tmax[p] <- max(colMeans(permuted))
    }
    
    p_value <- mean(perm_Tmax >= observed_Tmax)
    
    results[[as.character(soa)]] <- list(
      observed_Tmax = observed_Tmax,
      p_value = p_value
    )
  }
  
  return(results)
}

perm_results <- run_race_model_permutation_test(df_plot, n_perm = 20000)
print(perm_results)




