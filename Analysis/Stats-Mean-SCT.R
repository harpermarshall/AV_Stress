###############################
### STATS: MEAN vs MODALITY ###
###############################

# ----------------------------
# DISTINCT OFFSETS
# ----------------------------
offset_levels_mean <- sort(unique(clean_trials_df$offset))

# ----------------------------
# RM-ANOVA + PAIRWISE T-TESTS PER OFFSET (MEAN RT)
# ----------------------------
anova_results_by_offset_mean    <- list()
pairwise_results_by_offset_mean <- list()

for (off in offset_levels_mean) {
  
  df_offset_mean <- clean_trials_df %>%
    filter(offset == off, modality %in% c("A", "V", "AVC", "AVI")) %>%
    group_by(participant_number, modality) %>%
    summarise(
      mean_rt = mean(rt, na.rm = TRUE),
      .groups = "drop"
    )
  
  # RM-ANOVA
  aov_res_mean <- df_offset_mean %>%
    anova_test(
      dv     = mean_rt,
      wid    = participant_number,
      within = modality
    ) %>%
    as_tibble() %>%
    mutate(offset = off)
  
  anova_results_by_offset_mean[[as.character(off)]] <- aov_res_mean
  
  # Post-hoc paired t-tests
  pair_res_mean <- df_offset_mean %>%
    pairwise_t_test(
      mean_rt ~ modality,
      paired = TRUE,
      p.adjust.method = p_adjust_method
    ) %>%
    mutate(offset = off)
  
  pairwise_results_by_offset_mean[[as.character(off)]] <- pair_res_mean
}

anova_summary_mean    <- bind_rows(anova_results_by_offset_mean)
pairwise_summary_mean <- bind_rows(pairwise_results_by_offset_mean)

cat("ANOVA SUMMARY – MEAN RT\n")
print(anova_summary_mean, n = Inf, width = Inf)

cat("\nPAIRWISE T-TESTS – MEAN RT\n")
print(pairwise_summary_mean, n = Inf)