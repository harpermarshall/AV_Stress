########################################
###      LOAD LIBRARIES & SETUP      ###
########################################

# tidyverse: “one-stop shop” for data import, cleaning, and plotting
# dplyr: data-manipulation functions (already in tidyverse, but loaded explicitly)
# stringr: handy text (string) functions, e.g. lowercase or remove parts of words
# rstatix: simple wrappers for common statistical tests (paired t-tests, ANOVA)
# ggsignif: add significance stars or lines to ggplots
# patchwork: combine multiple ggplots into one layout
# lme4: fit linear mixed-effects models
library(tidyverse)
library(dplyr)
library(stringr)
library(rstatix)
library(ggsignif)
library(patchwork)
library(lme4)

# SET WORKING DIRECTORY
setwd("/Users/harpermarshall/Desktop/Project 1/SCT_Data/")

# LOAD DATASET
total_df <- read_csv("All_Trials_With_Survey_17True.csv") %>%
  # Make sure the “modality” column is treated as four categories in this specific order
  mutate(modality = factor(modality, levels = c("A","V","AVC","AVI")))

########################################
### STATS: PAIRED T-TESTS ON AVC MEAN ###
########################################

# 3. CALCULATE AVERAGE RT FOR EACH PARTICIPANT AT EACH OFFSET,
#    BUT ONLY FOR THE AVC CONDITION
avc_med <- total_df %>%
  filter(modality == "AVC") %>%              # keep only audio-visual congruent trials
  group_by(participant_number, offset_corrected) %>%
  summarise(
    mean_rt = mean(rt_corrected, na.rm = TRUE),  # average reaction time
    .groups = "drop"
  )

# 4. VIEW THAT TABLE
print(avc_med, n = Inf)

# 5. RUN PAIRED T-TESTS COMPARING EACH OFFSET AGAINST EACH OTHER
#    paired = TRUE means “within the same person”
avc_pairwise <- avc_med %>%
  pairwise_t_test(
    mean_rt ~ offset_corrected,  # formula: compare mean_rt across offsets
    paired = TRUE,
    p.adjust.method = "fdr"      # control for multiple comparisons
  )
print(avc_pairwise, n = Inf)

# This will show us whether the average RT for AVC trials at each offset is significantly different.
# If there are no significant differences here (as expected), that would indicate that the shifting stimulus
# timing by undetectable amounts did not impact AVC RT in any meaningful way.

########################################
### REPEATED-MEASURES ANOVA: MEAN RT ###
########################################

# 6. FIND ALL THE DISTINCT OFFSET VALUES
offset_levels <- sort(unique(total_df$offset_corrected))

# 7. PREPARE EMPTY LISTS TO STORE RESULTS
anova_results_by_offset    <- list()
pairwise_results_by_offset <- list()

# 8. LOOP OVER EACH OFFSET VALUE, DO:
for (off in offset_levels) {
  
  # a) SUBSET TO THAT OFFSET, ALL FOUR MODALITIES
  df_offset <- total_df %>%
    filter(offset_corrected == off, modality %in% c("A","V","AVC","AVI")) %>%
    group_by(participant_number, modality) %>%
    summarise(
      mean_rt = mean(rt_corrected, na.rm = TRUE),
      .groups = "drop"
    )
  
  # b) RUN A REPEATED-MEASURES ANOVA ON THOSE MEAN RTs
  aov_res <- df_offset %>%
    anova_test(
      dv = mean_rt,             # dependent variable
      wid = participant_number, # “within‐ID” = same person measured across conditions
      within = modality         # the repeated factor
    ) %>%
    as_tibble() %>%
    mutate(offset = off)        # tag results with this offset
  anova_results_by_offset[[as.character(off)]] <- aov_res
  
  # c) RUN PAIRED T-TESTS ACROSS THE FOUR MODALITIES FOR THAT OFFSET
  pair_res <- df_offset %>%
    pairwise_t_test(
      mean_rt ~ modality,
      paired = TRUE,
      p.adjust.method = "fdr"
    ) %>%
    mutate(offset = off)
  pairwise_results_by_offset[[as.character(off)]] <- pair_res
}

# 9. COMBINE ALL RESULTS INTO TWO BIG TABLES AND PRINT
anova_summary    <- bind_rows(anova_results_by_offset)
pairwise_summary <- bind_rows(pairwise_results_by_offset)

cat("ANOVA SUMMARY – MEAN RT\n")
print(anova_summary,    n = Inf, width = Inf)
print(pairwise_summary, n = Inf)

########################################
### HEATMAP: SIGNIFICANCE (MEAN RT)  ###
########################################

# 10. PREPARE DATA FOR HEATMAP
pairwise_heatmap <- pairwise_summary %>%
  mutate(
    comparison = paste(group1, "vs", group2),       # “A vs V”, etc.
    offset     = as.numeric(offset)
  )

# 11. PLOT A TILE HEATMAP WHERE FILL = –log10(p-value)
ggplot(pairwise_heatmap, aes(x = offset, y = comparison, fill = -log10(p.adj))) +
  geom_tile(color = "white") +  # each cell with white border
  geom_text(aes(label = round(p.adj, 3)), size = 3) +  # show the raw p.adj
  scale_fill_gradient(low = "#f0f9e8", high = "#0868ac", name = "-log10(p.adj)") +
  scale_x_continuous(breaks = sort(unique(pairwise_heatmap$offset))) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Pairwise t-test p-values by Offset (Mean RTs)",
    x     = "Visual Offset (ms)",
    y     = "Condition Comparison"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(hjust = 0.5))

##########################################
### REPEAT ANALYSIS FOR MEDIAN RT      ###
##########################################

# 12. RESET RESULT LISTS
# We’ll store each offset’s ANOVA and t-test results in these lists,
# so at the end we can glue them together into big tables.
anova_results_by_offset    <- list()
pairwise_results_by_offset <- list()

# 13. LOOP OVER EACH OFFSET, USING MEDIAN RT
# Why median?  It’s less sensitive to extreme RTs (“outliers”) than the mean.
for (off in offset_levels) {
  
  # 13a) SUBSET TO ONE OFFSET & ALL FOUR MODALITIES
  #     We’re asking: “At this particular timing offset, 
  #     do people respond differently to A vs V vs AVC vs AVI?”
  df_offset <- total_df %>%
    filter(
      offset_corrected == off,                 # keep only trials at this offset
      modality %in% c("A","V","AVC","AVI")      # keep our four key conditions
    ) %>%
    group_by(
      participant_number,                      # within each person …
      modality                                 # … and each condition …
    ) %>%
    summarise(
      median_rt = median(rt_corrected, na.rm = TRUE),  # calculate that person’s median RT
      .groups   = "drop"                                # drop grouping afterward
    )
  
  # 13b) RUN A REPEATED-MEASURES ANOVA ON MEDIAN RT
  #     *Why ANOVA?* We want to test if there’s any overall difference
  #     in median RT across the four modalities, *within the same participants*.
  #     A significant result means “some modalities differ from others” at this offset.
  aov_res <- df_offset %>%
    anova_test(
      dv     = median_rt,             # dependent variable: each person’s median RT
      wid    = participant_number,    # “within-ID”: same person measured across conditions
      within = modality               # the repeated factor we’re comparing
    ) %>%
    as_tibble() %>%
    mutate(offset = off)              # tag which offset this ANOVA came from
  
  # Save the ANOVA results in our list, keyed by the offset value
  anova_results_by_offset[[as.character(off)]] <- aov_res
  
  # 13c) FOLLOW-UP WITH PAIRED T-TESTS
  #     If the ANOVA says “yes, there’s a difference somewhere,”
  #     we do pairwise t-tests between each pair of modalities to see *which* differ.
  pair_res <- df_offset %>%
    pairwise_t_test(
      median_rt ~ modality,          # formula: compare median_rt across modalities
      paired           = TRUE,       # within-subject comparisons
      p.adjust.method  = "fdr"       # false-discovery rate to correct for multiple tests
    ) %>%
    mutate(offset = off)             # again tag with offset for later reference
  
  # Save the pairwise-test results
  pairwise_results_by_offset[[as.character(off)]] <- pair_res
}

# 14. COMBINE & PRINT ALL THE MEDIAN RT RESULTS
# We turn our lists of per-offset tables into two big tibbles and display them.
anova_summary    <- bind_rows(anova_results_by_offset)
pairwise_summary <- bind_rows(pairwise_results_by_offset)

cat("ANOVA SUMMARY – MEDIAN RT\n")
print(anova_summary,    n = Inf, width = Inf)

cat("\nPAIRWISE T-TESTS – MEDIAN RT\n")
print(pairwise_summary, n = Inf)

########################################
### HEATMAP: SIGNIFICANCE (MEDIAN)   ###
########################################

pairwise_heatmap <- pairwise_summary %>%
  mutate(
    comparison = paste(group1, "vs", group2),
    offset     = as.numeric(offset)
  )

ggplot(pairwise_heatmap, aes(x = offset, y = comparison, fill = -log10(p.adj))) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(p.adj, 3)), size = 3) +
  scale_fill_gradient(low = "#f0f9e8", high = "#0868ac", name = "-log10(p.adj)") +
  scale_x_continuous(breaks = sort(unique(pairwise_heatmap$offset))) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Pairwise t-test p-values by Offset (Median RTs)",
    x     = "Visual Offset (ms)",
    y     = "Condition Comparison"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title   = element_text(hjust = 0.5))

########################################
### LINEAR MIXED MODELS WITH STRATEGY ###
########################################

# 15. PREPARE AVERAGE RT BY PERSON × MODALITY × OFFSET
rt_summary_modality <- total_df %>%
  group_by(participant_number, modality, offset_corrected) %>%
  summarise(mean_rt = mean(rt_corrected, na.rm = TRUE), .groups = "drop")

# 16. EXTRACT UNIQUE STRATEGY SCORES (one_strat, change_strat, no_strat) PER PERSON × OFFSET
strategy_summary <- total_df %>%
  select(participant_number, offset_corrected, one_strat, change_strat, no_strat) %>%
  distinct()

# 17. MERGE THEM AND MAKE “AVC” THE REFERENCE LEVEL FOR MODE
rt_strategy_modality <- left_join(rt_summary_modality, strategy_summary,
                                  by = c("participant_number", "offset_corrected")) %>%
  mutate(modality = relevel(factor(modality, levels = c("A", "V", "AVC", "AVI")), ref = "AVC"))

# 18. EXAMPLE MODEL: one_strat × modality, WITH RANDOM INTERCEPT FOR EACH PERSON
model <- lmer(mean_rt ~ one_strat * modality + (1 | participant_number),
              data = rt_strategy_modality)
summary(model)

# 19. PLOT THAT INTERACTION
ggplot(rt_strategy_modality, aes(x = one_strat, y = mean_rt, color = modality)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect of one_strat on RT by Modality & Offset",
    x     = "one_strat Score",
    y     = "Mean RT (ms)"
  )

# 20–28. REPEAT LMMs FOR no_strat, change_strat, audio_bias, visual_bias...
#      You’d just swap the predictor in the formula above and re-run.
