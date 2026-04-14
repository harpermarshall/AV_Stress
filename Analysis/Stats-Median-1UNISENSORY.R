#################################
### STATS: MEDIAN vs MODALITY ###
###  (A and V collapsed)       ###
#################################

# ----------------------------
# DISTINCT OFFSETS (only AV offsets matter)
# ----------------------------
offset_levels_median <- sort(unique(clean_trials_df$offset))

# ----------------------------
# PRECOMPUTE COLLAPSED A AND V MEDIANS
# ----------------------------
unisensory_medians <- clean_trials_df %>%
  filter(modality %in% c("A","V")) %>%
  group_by(participant_number, modality) %>%
  summarise(
    median_rt = median(rt, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# RM-ANOVA + PAIRWISE T-TESTS PER OFFSET
# ----------------------------
anova_results_by_offset_median    <- list()
pairwise_results_by_offset_median <- list()

for (off in offset_levels_median) {
  
  # multisensory medians at this offset
  multisensory_medians <- clean_trials_df %>%
    filter(offset == off, modality %in% c("AVC","AVI")) %>%
    group_by(participant_number, modality) %>%
    summarise(
      median_rt = median(rt, na.rm = TRUE),
      .groups = "drop"
    )
  
  # combine with collapsed unisensory medians
  df_offset_median <- bind_rows(
    unisensory_medians,
    multisensory_medians
  )
  
  # ensure factor order
  df_offset_median <- df_offset_median %>%
    mutate(
      modality = factor(modality, levels = c("A","V","AVC","AVI"))
    )
  
  # ----------------------------
  # RM-ANOVA
  # ----------------------------
  aov_res_median <- df_offset_median %>%
    anova_test(
      dv     = median_rt,
      wid    = participant_number,
      within = modality
    ) %>%
    as_tibble() %>%
    mutate(offset = off)
  
  anova_results_by_offset_median[[as.character(off)]] <- aov_res_median
  
  # ----------------------------
  # Post-hoc paired t-tests
  # ----------------------------
  pair_res_median <- df_offset_median %>%
    pairwise_t_test(
      median_rt ~ modality,
      paired = TRUE,
      p.adjust.method = p_adjust_method
    ) %>%
    mutate(offset = off)
  
  pairwise_results_by_offset_median[[as.character(off)]] <- pair_res_median
}

# ----------------------------
# COMBINE RESULTS
# ----------------------------
anova_summary_median    <- bind_rows(anova_results_by_offset_median)
pairwise_summary_median <- bind_rows(pairwise_results_by_offset_median)

cat("ANOVA SUMMARY – MEDIAN RT\n")
print(anova_summary_median, n = Inf, width = Inf)

cat("\nPAIRWISE T-TESTS – MEDIAN RT\n")
print(pairwise_summary_median, n = Inf, width = Inf)
