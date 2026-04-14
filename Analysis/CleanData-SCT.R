##############################################
###   CLEAN TRIAL DATA FROM MASTER CSV     ###
##############################################

# ----------------------------
# LOAD LIBRARIES
# ----------------------------
#library(tidyverse)
#library(rstatix)
#library(ggsignif)

# ----------------------------
# USER SETTINGS
# ----------------------------
data_dir <- "/Users/harpermarshall/Desktop/Project 1/SCT_Data"

trials_df <- read_csv(file.path(data_dir, "All_Trials_MASTER.csv"), show_col_types = FALSE) %>%
  filter(!participant_number %in% exclude_participants)

soa_df <- read_csv(file.path(data_dir, "All_SOA_MASTER.csv"), show_col_types = FALSE) %>%
  filter(!participant_number %in% exclude_participants)

# ----------------------------
# CLEAN SOA DATA
# ----------------------------
clean_SOA_df <- soa_df %>%
  filter(!participant_number %in% exclude_participants) %>%
  mutate(
    simultaneous = case_when(
      response == "S" ~ 1,
      response == "A" ~ 0,
      TRUE ~ NA_real_
    ),
    soa_factor = factor(soa)  # Treat SOA as a categorical variable
  )


# ----------------------------
# TAG AND SUMMARIZE OUTLIERS WITHIN participant × modality
# ----------------------------
trials_tagged <- trials_df %>%
  mutate(rt = as.numeric(rt)) %>%
  group_by(participant_number, modality) %>%
  mutate(
    is_outlier = abs((rt - mean(rt, na.rm = TRUE)) / sd(rt, na.rm = TRUE)) > outlier_z,
    modality = factor(modality, levels = c("A", "V", "AVC", "AVI"))
  ) %>%
  ungroup()

outlier_counts <- trials_tagged %>%
  group_by(participant_number, modality) %>%
  summarise(
    total_trials     = n(),
    outliers         = sum(is_outlier),
    percent_outliers = round(100 * outliers / total_trials, 2),
    .groups = "drop"
  )

print(outlier_counts, n = Inf, width = Inf)


# ----------------------------
# CLEAN DATA
# ----------------------------
clean_trials_df <- trials_tagged %>%
  filter(rt >= rt_abs_min, rt <= rt_abs_max)

if (drop_incorrect) {
  clean_trials_df <- clean_trials_df %>%
    filter(is.na(correct) | correct == TRUE)
}

if (drop_outliers) {
  clean_trials_df <- clean_trials_df %>%
    filter(!is_outlier)
}


# ----------------------------
# REMOVED TRIALS SUMMARY (participant × modality)
# ----------------------------
removed_trials <- bind_rows(
  trials_df %>% mutate(stage = "before"),
  clean_trials_df %>% mutate(stage = "after")
) %>%
  count(participant_number, modality, stage, name = "n") %>%
  tidyr::complete(
    participant_number, modality,
    stage = c("before", "after"),
    fill = list(n = 0)
  ) %>%
  pivot_wider(
    names_from  = stage,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(removed = before - after) %>%
  select(participant_number, modality, removed) %>%
  pivot_wider(
    names_from  = modality,
    values_from = removed,
    values_fill = 0
  ) %>%
  mutate(Total_removed = rowSums(across(where(is.numeric))))

print(removed_trials, n = Inf, width = Inf)

# ----------------------------
# SUMMARY OF FINAL_DF
# ----------------------------
raw_flags <- trials_tagged %>%
  mutate(
    rt = as.numeric(rt),
    correct = as.logical(correct),
    
    too_fast = !is.na(rt) & rt < rt_abs_min,
    too_slow = !is.na(rt) & rt > rt_abs_max,
    rt_outside = too_fast | too_slow,
    
    incorrect = !is.na(correct) & correct == FALSE,
    
    removed_by_rt = rt_outside,
    removed_by_incorrect = drop_incorrect & incorrect,
    removed_by_outlier   = drop_outliers & is_outlier,
    
    removed_any = removed_by_rt | removed_by_incorrect | removed_by_outlier
  )

n_raw     <- nrow(trials_df)
n_clean   <- nrow(clean_trials_df)
n_removed <- n_raw - n_clean

pct <- function(x, denom) round(100 * x / denom, 2)

# mutually exclusive categories
n_rt_only  <- sum(raw_flags$removed_by_rt & !raw_flags$removed_by_incorrect, na.rm = TRUE)
n_inc_only <- sum(raw_flags$removed_by_incorrect & !raw_flags$removed_by_rt, na.rm = TRUE)
n_out_only <- sum(raw_flags$removed_by_outlier &
                    !raw_flags$removed_by_rt &
                    !raw_flags$removed_by_incorrect)

n_multiple <- sum(
  rowSums(raw_flags[, c("removed_by_rt",
                            "removed_by_incorrect",
                            "removed_by_outlier")]) > 1
)

n_kept <- sum(!raw_flags$removed_any)

# raw flags
n_too_fast  <- sum(raw_flags$too_fast, na.rm = TRUE)
n_too_slow  <- sum(raw_flags$too_slow, na.rm = TRUE)
n_incorrect <- sum(raw_flags$incorrect, na.rm = TRUE)

cat(
  "\n",
  "============================================================\n",
  "FINAL CLEANING SUMMARY: clean_SOA_df + clean_trials_df \n",
  "============================================================\n\n",
  
  "DATA SOURCE:\n",
  "  Master file: ", "All_Trials_Master", "\n\n",
  
  "CLEANING PARAMETERS:\n",
  "  Reaction time range: ", rt_abs_min, "–", rt_abs_max, " ms\n",
  "  Incorrect trials included? ",
  ifelse(drop_incorrect,
         "NO (incorrect removed; NA treated as correct)",
         "YES (incorrect retained)"), "\n",
  "  Outliers included? ",
  ifelse(drop_outliers,
         "NO (outliers removed)",
         "YES (outliers retained)"),
  "\n\n",
  
  "TRIAL COUNTS:\n",
  "  Trials before cleaning: ", n_raw, "\n",
  "  Trials after cleaning:  ", n_clean, "\n",
  "  Total trials removed:   ", n_removed,
  " (", pct(n_removed, n_raw), "%)\n\n",
  
  "REMOVAL BREAKDOWN (mutually exclusive):\n",
  "  RT exclusion only:               ",
  n_rt_only, " (", pct(n_rt_only, n_raw), "%)\n",
  "  Incorrect response only:         ",
  n_inc_only, " (", pct(n_inc_only, n_raw), "%)\n",
  if (drop_outliers)
    paste0(
      "  RT outlier only (|z| > ", outlier_z, "): ",
      n_out_only, " (", pct(n_out_only, n_raw), "%)\n"
    ) else "",
  "  Multiple exclusion criteria:     ",
  n_multiple, " (", pct(n_multiple, n_raw), "%)\n",
  "  Trials retained:                 ",
  n_kept, " (", pct(n_kept, n_raw), "%)\n\n",
  
  "DETAILS OF REMOVAL CRITERIA (raw data):\n",
  "  Too fast (<", rt_abs_min, " ms): ",
  n_too_fast, " (", pct(n_too_fast, n_raw), "%)\n",
  "  Too slow (>", rt_abs_max, " ms): ",
  n_too_slow, " (", pct(n_too_slow, n_raw), "%)\n",
  "  Incorrect responses (FALSE): ",
  n_incorrect, " (", pct(n_incorrect, n_raw), "%)\n",
  if (drop_outliers)
    paste0(
      "  Outliers (|z| > ", outlier_z, "): ",
      sum(raw_flags$is_outlier, na.rm = TRUE),
      " (", pct(sum(raw_flags$is_outlier, na.rm = TRUE), n_raw), "%)\n"
    ) else "",
  "\n",
  
  "OUTPUT:\n",
  "  Final dataset object: clean_trials_df\n\n",
  
  "NOTE:\n",
  "  prior to downstream analysis.\n",
  "  All timing variables are in milliseconds.\n",
  "  Percentages are relative to the raw trial count.\n",
  "  Outliers were defined within participant × modality.\n",
  "  This summary fully specifies preprocessing applied\n",
  "  prior to downstream analysis.\n",
  
  "============================================================\n\n",
  sep = ""
  
)

