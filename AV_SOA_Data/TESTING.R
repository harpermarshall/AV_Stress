################################
### TRYING SOMETHING NEW ####
################################

library(tidyverse)
library(rstatix)
library(ggsignif)

# Set working directory to the AV_SOA_Data folder
setwd("/Users/harpermarshall/Desktop/Project 1/AV_SOA_Data/")

participant_folders <- list.dirs(path = ".", recursive = FALSE) %>%
  keep(~ !str_starts(basename(.x), "X") && basename(.x) != "P999")

print(participant_folders)

# Function to load and combine files by type
load_combined_data <- function(filename_prefix) {
  file_paths <- participant_folders %>%
    map(~ list.files(path = .x, pattern = paste0("^", filename_prefix, ".*\\.csv$"), full.names = TRUE)) %>%
    flatten_chr()
  
  combined_df <- file_paths %>%
    map_dfr(read_csv)
  
  return(combined_df)
}

# Load all three datasets across participants
soa_results_df     <- load_combined_data("AV_SOA_Results")
stroop_results_df  <- load_combined_data("AV_Stroop_Results")
survey_df          <- load_combined_data("AV_Strategy_Survey")

trial_counts <- stroop_results_df %>%
  count(Participant, Type) %>%
  pivot_wider(names_from = Type, values_from = n, values_fill = 0) %>%
  mutate(Total = A + V + AVC + AVI)

print(trial_counts)

# Ensure Visual_Delay is not treated as a list or NA
stroop_delay_lookup <- stroop_results_df %>%
  select(Participant, Block, `Visual Delay`) %>%
  distinct()  # one delay per participant+block combo

# Join the delay info onto the survey data
survey_with_delay <- survey_df %>%
  left_join(stroop_delay_lookup, by = c("Participant", "Block"))

survey_with_delay <- survey_with_delay %>%
  rename(
    Audio_Bias     = Q1,
    Visual_Bias    = Q2,
    One_Strat      = Q3,
    Strat_Change   = Q4,
    No_Strat       = Q5,
  )

################################
### CLEAN STROOP RT DATA ######
################################

# Clean and recode
stroop_clean <- stroop_results_df %>%
  mutate(
    Correct = as.logical(Correct),
    RT = as.numeric(RT),
    VisualOffset = as.factor('Visual Delay'),
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI"))
  ) %>%
  filter(
    (Type %in% c("A", "V", "AVC") & RT >= 0.2 & RT <= 0.8) | Type == "AVI"
  )

# Optional: Custom color palette for offsets
offset_colors <- c("0" = "#e6a53250", "50" = "#F8766D", "100" = "#00BFC4", "150" = "plum", "200" = "#466eb4")

##################################
### RT BOXPLOT BY OFFSET & TYPE ##
##################################

ggplot(stroop_clean, aes(x = Type, y = RT, color = Visual_Delay)) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
    alpha = 0.4
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.6),
    alpha = 0.5,
    outlier.shape = NA,
    width = 0.5
  ) +
  scale_color_manual(values = offset_colors, name = "Visual Delay") +
  theme_minimal() +
  labs(
    title = "RT by Trial Type and Visual Delay",
    x = "Trial Type",
    y = "RT (s)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12)
  )

#########################################
### MEDIAN RT BARPLOT BY TRIAL TYPE #
#########################################

median_offset_summary <- stroop_clean %>%
  group_by(Type, VisualDelay) %>%
  summarise(median_rt = median(RT, na.rm = TRUE), .groups = "drop")

ggplot(median_offset_summary, aes(x = Type, y = median_rt, fill = Visual_Delay)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  scale_fill_manual(values = offset_colors, name = "Visual Delay") +
  theme_minimal() +
  labs(
    title = "Median RT by Trial Type and Visual Delay",
    x = "Trial Type",
    y = "Median RT (s)"
  ) +
  coord_cartesian(ylim = c(0.3, NA)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12)
  )

#########################################
### MEDIAN RT BARPLOT BY VISUAL OFFSET #
#########################################

# Clean and recode Visual_Delay → VisualOffset
stroop_clean <- stroop_results_df %>%
  mutate(
    Correct = as.logical(Correct),
    RT = as.numeric(RT),
    VisualOffset = factor(
      as.character(`Visual Delay` * 1000),  # ✅ backticks, not quotes
      levels = c("0", "50", "100", "150", "200")
    ),
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI"))
  ) %>%
  filter(RT >= 0.2, RT <= 0.8)

# Optional: Custom color palette for offsets
offset_colors <- c("0" = "#e6a53250", "50" = "#F8766D", "100" = "#00BFC4", "150" = "plum", "200" = "#466eb4")

# Subset to AVC trials and calculate median RTs
avc_medians <- stroop_clean %>%
  filter(Type == "AVC") %>%
  group_by(VisualOffset) %>%
  summarise(median_rt = median(RT * 1000, na.rm = TRUE), .groups = "drop")  # <- multiply here

# Plot AVC-only median RT by VisualOffset
ggplot(avc_medians, aes(x = VisualOffset, y = median_rt, fill = VisualOffset)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = offset_colors, name = "Visual Offset") +
  scale_y_continuous(
    name = "Median RT (ms)",
    breaks = seq(300, 600, by = 20)  # Keep your 20 ms interval ticks
  ) +
  coord_cartesian(ylim = c(300, NA)) +  # ✅ Zoom without removing
  theme_minimal() +
  labs(
    title = "Median RT for AVC Trials by Visual Offset",
    x = "Visual Offset (ms)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12)
  )

############
### PLOT ###.    DOES NOT WORK RN 
############

stroop_clean %>%
  filter(Type == "AVC") %>%
  group_by(Participant, VisualOffset) %>%
  summarise(
    median_rt = median(RT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = VisualOffset, y = median_rt, group = Participant)) +
  geom_line(alpha = 0.4, color = "gray") +  # light lines connecting each participant's points
  geom_point(aes(color = VisualOffset), size = 2) +
  scale_color_manual(values = offset_colors) +
  scale_y_continuous(
    name = "Median RT (ms)",
    breaks = seq(300, 600, 20)
  ) +
  coord_cartesian(ylim = c(300, NA)) +
  theme_minimal() +
  labs(
    title = "Per-Participant Median RT (AVC) by Visual Offset",
    x = "Visual Offset (ms)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12)
  )

####################
### PLOT MEDIANS ###
####################

stroop_clean %>%
  filter(Type == "AVC") %>%
  group_by(Participant, VisualOffset) %>%
  summarise(median_rt = median(RT * 1000, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = VisualOffset, y = median_rt, group = Participant, color = Participant)) +
  geom_line(alpha = 0.7, linewidth = 1) +     # participant-colored lines
  geom_point(size = 2) +                # matching participant-colored dots
  scale_y_continuous(
    name = "Median RT (ms)",
    breaks = seq(200, 600, by = 20)
  ) +
  coord_cartesian(ylim = c(200, NA)) +
  theme_minimal() +
  labs(
    title = "Per-Participant Median RTs for AVC Trials",
    x = "Visual Offset (ms)",
    color = "Participant"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

##############################
### T-TESTS #########
##############################

avc_ttests <- stroop_clean %>%
  filter(Type == "AVC") %>%
  pairwise_t_test(
    RT ~ VisualOffset,
    paired = FALSE,
    p.adjust.method = "fdr"
  )

print(avc_ttests)

##############################
### ADDING STRATEGY! #########
##############################

survey_long <- survey_with_delay %>%
  pivot_longer(
    cols = c(Audio_Bias, Visual_Bias, One_Strat, Strat_Change, No_Strat),
    names_to = "Strategy_Question",
    values_to = "Rating"
  )

ggplot(survey_long, aes(x = Strategy_Question, y = Rating, fill = as.factor(`Visual Delay`))) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), 
              shape = 21, size = 2, alpha = 0.4) +
  scale_fill_brewer(palette = "Set2", name = "Visual Delay (s)") +
  theme_minimal() +
  labs(
    title = "Individual Strategy Ratings by Visual Delay",
    x = "Strategy Question",
    y = "Rating (1–5)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12, angle = 25, hjust = 1)
  )

#### MESSING AROUND ####
library(tidyverse)
library(scales)
library(colorspace)

# Step 1: Create base color for each participant
participants <- unique(survey_long$Participant)
participant_colors <- scales::hue_pal()(length(participants))
names(participant_colors) <- participants

# Step 2: Create shaded colors per participant per delay
survey_long <- survey_long %>%
  mutate(Strategy_Question = factor(
    Strategy_Question,
    levels = c("Audio_Bias", "Visual_Bias", "No_Strat", "One_Strat", "Strat_Change")
  ))

ggplot(survey_long, aes(x = Strategy_Question, y = Rating, group = Participant, color = Participant)) +
  geom_line(linewidth = 0.7, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ `Visual Delay`) +
  theme_minimal() +
  labs(
    title = "Strategy Ratings per Visual Delay",
    x = "Strategy Question",
    y = "Rating (1–5)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
    strip.text = element_text(size = 12)
  )

### MEDIANS BY TRIAL TYPE ###

library(patchwork)

trial_types <- c("A", "V", "AVC", "AVI")
plot_list <- list()

for (t in trial_types) {
  plot_list[[t]] <- stroop_clean %>%
    filter(Type == t) %>%
    group_by(Participant, VisualOffset) %>%
    summarise(median_rt = median(RT * 1000, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = VisualOffset, y = median_rt, group = Participant, color = Participant)) +
    geom_line(alpha = 0.7, linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(
      name = "Median RT (ms)",
      limits = c(240, 540),
      breaks = seq(250, 550, by = 50)
    ) +
    theme_minimal() +
    labs(
      title = paste("Per-Participant Median RTs for", t, "Trials"),
      x = "Visual Offset (ms)",
      color = "Participant"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
}

# Combine into one 2x2 grid
combined_plot <- (plot_list[["A"]] | plot_list[["V"]]) / (plot_list[["AVC"]] | plot_list[["AVI"]])

# Display it
combined_plot

### OVERLAY V AND AVI MEDIANS ###

# Step 1: Filter to A, V, AVC and compute per-participant medians
median_rt_overlay <- stroop_clean %>%
  filter(Type %in% c("A", "V", "AVC")) %>%
  group_by(Participant, VisualOffset, Type) %>%
  summarise(median_rt = median(RT * 1000, na.rm = TRUE), .groups = "drop")

# Step 2: Assign participant colors
participants <- unique(median_rt_overlay$Participant)
participant_colors <- scales::hue_pal()(length(participants))
names(participant_colors) <- participants

# Step 3: Plot
ggplot(median_rt_overlay, aes(x = VisualOffset, y = median_rt, group = interaction(Participant, Type))) +
  geom_line(aes(color = Participant, linetype = Type), linewidth = 1, alpha = 0.8) +
  geom_point(aes(color = Participant, shape = Type), size = 2) +
  scale_color_manual(values = participant_colors) +
  scale_shape_manual(values = c("AVC" = 17, "A" = 15, "V" = 16)) +  # triangle, square, circle
  scale_linetype_manual(values = c("AVC" = "solid", "A" = "dashed", "V" = "dotted")) +
  scale_y_continuous(name = "Median RT (ms)", breaks = seq(200, 600, 50)) +
  coord_cartesian(ylim = c(200, 600)) +
  theme_minimal() +
  labs(
    title = "Overlayed Median RTs for A, V, and AVC Trials by Participant",
    x = "Visual Offset (ms)",
    color = "Participant",
    linetype = "Trial Type",
    shape = "Trial Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 12)
  )

### AVI BIASING ###

# Step 1: Classify AVI responses by modality only
avi_response_summary <- stroop_clean %>%
  filter(Type == "AVI") %>%
  mutate(
    Audio = str_remove(tolower(Audio), "\\.mp3$"),
    Visual = tolower(Visual),
    Response_Color = case_when(
      tolower(Response) == "r" ~ "red",
      tolower(Response) == "b" ~ "blue",
      TRUE ~ NA_character_
    ),
    MatchType = case_when(
      Response_Color == Visual ~ "Visual",
      Response_Color == Audio ~ "Audio",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(MatchType %in% c("Visual", "Audio"))

# Step 2: Count responses per offset
avi_counts <- avi_response_summary %>%
  mutate(VisualOffset = as.factor(VisualOffset)) %>%
  group_by(Participant, VisualOffset, MatchType) %>%
  summarise(n = n(), .groups = "drop")

# Step 3: Fill in missing combinations (zeros)
avi_counts_complete <- avi_counts %>%
  complete(
    Participant,
    VisualOffset,
    MatchType = c("Visual", "Audio"),
    fill = list(n = 0)
  )

# Step 4: Plot stacked bars per participant
avi_counts_complete %>%
  group_by(Participant) %>%
  group_walk(~ {
    p <- ggplot(.x, aes(x = VisualOffset, y = n, fill = MatchType)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = paste("Participant:", .y),
        x = "Visual Offset (ms)",
        y = "Response Count",
        fill = "Match Type"
      ) +
      scale_fill_manual(values = c("Visual" = "skyblue", "Audio" = "tomato")) +
      theme_minimal(base_size = 14)
    
    print(p)
  })

