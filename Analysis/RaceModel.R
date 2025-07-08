
# SET WORKING DIRECTORY
setwd("/Users/harpermarshall/Desktop/Project 1/SCT_Data/")

# LOAD DATASET
total_df <- read_csv("All_Trials_With_Survey_17.csv") %>%
  mutate(
    modality = factor(modality, levels = c("A","V","AVC","AVI"))
  )

########################################
###    RACE MODEL CDFs BY OFFSET    ###
########################################

library(tidyverse)

# 1. define your bins
bins <- seq(250, 800, by = 20)

# 2. helpers to compute binned-CDFs
compute_binned_cdf <- function(rts, bins) {
  sapply(bins, function(b) mean(rts <= b))
}

# 3. loop over each offset
offsets <- sort(unique(total_df$offset_corrected))

for (off in offsets) {
  
  df_off <- total_df %>% filter(offset_corrected == off)
  
  # 3a. compute CDFs for each modality at each bin
  cdf_df <- df_off %>%
    group_by(modality) %>%
    summarize(
      rt_list = list(rt_corrected),
      n       = n(),
      .groups = "drop"
    ) %>%
    mutate(
      bin_cdf = map(rt_list, ~ tibble(
        rt_bin = bins,
        cdf    = compute_binned_cdf(.x, bins)
      ))
    ) %>%
    select(modality, bin_cdf) %>%
    unnest(bin_cdf)
  
  # 3b. build Miller bound (A + V) at each bin
  miller_df <- cdf_df %>%
    filter(modality %in% c("A","V")) %>%
    pivot_wider(names_from = modality, values_from = cdf) %>%
    mutate(cdf = A + V) %>%
    select(rt_bin, cdf)
  
  # 3c. plot
  p <- ggplot() +
    geom_line(data = filter(cdf_df, modality == "A"),
              aes(rt_bin, cdf, color = "A"), linewidth = 0.5) +
    geom_line(data = filter(cdf_df, modality == "V"),
              aes(rt_bin, cdf, color = "V"), linewidth = 0.5) +
    geom_line(data = filter(cdf_df, modality == "AVC"),
              aes(rt_bin, cdf, color = "AVC"), linewidth = 0.5) +
    geom_line(data = miller_df,
              aes(rt_bin, cdf),
              color = "black", linetype = "dotted", linewidth = 0.5) +
    scale_color_manual(values = c("A"   = "red",
                                  "V"   = "blue",
                                  "AVC" = "green"),
                       name = "Modality") +
    labs(title = paste0("Group CDFs — Offset ", off, " ms"),
         x     = "RT bin (ms)",
         y     = "CDF") +
    theme_minimal()
  
  print(p)
}

