############################################
### LOAD LIBRARIES & SETUP ##############
############################################

library(tidyverse)
library(dplyr)
library(stringr)
library(rstatix)
library(ggsignif)
library(patchwork)

# Set working directory
setwd("/Users/harpermarshall/Desktop/Project 1/AV_SOA_Data/")

# Load the fully merged Stroop + Survey dataset
stroop_df <- read_csv("All_Stroop_Trials_With_Survey.csv")

# Convert relevant columns
stroop_df <- stroop_df %>%
  mutate(
    Offset = factor(Offset_ms, levels = c("0", "50", "100", "150", "200")),
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI"))
  )
    
# Change Unisensory trial offset to 0
#stroop_df <- stroop_df %>%
  #mutate(
    #Offset_ms = if_else(Type %in% c("A", "V"), 0, Offset_ms),
    #Offset = factor(as.character(Offset_ms), levels = c("0", "50", "100", "150", "200"))
  #)

# Make RT column ms
stroop_df <- stroop_df %>%
  mutate(RT = RT * 1000)

# Custom color palette for visual offsets
offset_colors <- c("0" = "#e6a53250", "50" = "#F8766D", "100" = "#00BFC4", "150" = "plum", "200" = "#466eb4")

############################################
### PLOT 1: Raw RT by Trial Type and Offset
############################################

ggplot(stroop_df, aes(x = Type, y = RT, color = Offset)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), alpha = 0.4) +
  geom_boxplot(position = position_dodge(width = 0.6), alpha = 0.5, outlier.shape = NA, width = 0.5) +
  scale_color_manual(values = offset_colors) +
  scale_y_continuous(
    name = "RT (ms)",
    breaks = seq(200, 900, by = 50),  # ✅ every 50 ms
    limits = c(200, 900)              # ✅ consistent min/max
  ) +
  theme_minimal() +
  labs(title = "RT by Trial Type and Offset", x = "Trial Type", color = "Offset") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12)
  )

############################################
### PLOT 2: Median RT by Trial Type and Offset
############################################

median_summary <- stroop_df %>%
  group_by(Type, Offset_ms) %>%
  summarise(median_rt = median(RT, na.rm = TRUE), .groups = "drop")
print(median_summary)

ggplot(median_summary, aes(x = Type, y = median_rt, fill = factor(Offset_ms))) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  scale_fill_manual(values = offset_colors) +
  theme_minimal() +
  labs(title = "Median RT by Trial Type and Offset", x = "Trial Type", y = "Median RT (ms)", fill = "Offset") +
  coord_cartesian(ylim = c(300, 700)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 12))

#############################################################
### PLOT 3: Median RT by Trial Type and Offset by Participant
#############################################################

# Step 1: Compute per-participant medians
median_summary <- stroop_df %>%
  group_by(Participant, Type, Offset_ms) %>%
  summarise(median_rt = median(RT_corrected, na.rm = TRUE), .groups = "drop")

# Step 2: Plot by Trial Type (x-axis), fill = Offset
ggplot(median_summary, aes(x = Type, y = median_rt, fill = factor(Offset_ms))) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  facet_wrap(~ Participant) +
  scale_fill_manual(values = offset_colors, name = "Offset (ms)") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Median RT Corrected by Trial Type and Offset (Per Participant)",
    x = "Trial Type",
    y = "Median RT (ms)"
  ) +
  coord_cartesian(ylim = c(300, 700)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    strip.text = element_text(size = 13)
  )

############################################
### PLOT 4: Per-Participant Median RTs (AVC only)
############################################

stroop_df %>%
  filter(Type == "AVC") %>%
  group_by(Participant, Offset) %>%
  summarise(median_rt = median(RT_corrected, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Offset, y = median_rt, group = Participant, color = Participant)) +
  geom_line(alpha = 0.7, linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(name = "Median RT (ms)", breaks = seq(300, 700, 20)) +
  coord_cartesian(ylim = c(300, 700)) +
  theme_minimal() +
  labs(title = "Per-Participant Median RTs for AVC Trials", x = "Offset (ms)", color = "Participant") +
  theme(plot.title = element_text(hjust = 0.5))

#######################################################
### TABLE COMPARING AVI ANSWERS TO SURVEY RESPONSES ###
#######################################################

avi_response_summary <- stroop_df %>%
  filter(Type == "AVI") %>%
  mutate(
    # Standardize to lowercase and strip ".mp3" from audio if present
    Audio_clean = str_remove(tolower(Audio), "\\.mp3$"),
    Visual_clean = tolower(Visual),
    Response_color = case_when(
      tolower(Response) == "r" ~ "red",
      tolower(Response) == "b" ~ "blue",
      TRUE ~ NA_character_
    ),
    MatchType = case_when(
      Response_color == Audio_clean ~ "Audio",
      Response_color == Visual_clean ~ "Visual",
      TRUE ~ "Neither"
    )
  ) %>%
  filter(MatchType %in% c("Audio", "Visual"))  # remove unclear responses

# Count and calculate proportions
avi_summary <- avi_response_summary %>%
  group_by(Participant, MatchType) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = MatchType, values_from = n, values_fill = 0) %>%
  mutate(
    Total = Audio + Visual,
    Prop_Audio = Audio / Total,
    Prop_Visual = Visual / Total
  )

# View result
print(avi_summary)

###########################
### PLOT 5: Survey Analysis
###########################

# Step 1: Prepare survey data (one row per Participant × Offset)
survey_by_offset <- stroop_df %>%
  select(Participant, Offset, Audio_Bias, Visual_Bias, One_Strat, Strat_Change, No_Strat) %>%
  distinct() %>%
  pivot_longer(
    cols = c(Audio_Bias, Visual_Bias, One_Strat, Strat_Change, No_Strat),
    names_to = "Strategy_Question",
    values_to = "Rating"
  )

# Step 2: Plot ratings per visual offset per question
ggplot(survey_by_offset, aes(x = Offset, y = Rating, group = Participant, color = Participant)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Strategy_Question, scales = "fixed") +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Survey Ratings by Offset and Strategy Question",
    x = "Visual Offset (ms)",
    y = "Rating (1–5)",
    color = "Participant"
  ) +
  theme(
    strip.text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12)
  )

############################################
### PLOT 6: Strategy Ratings Per Participant by Offset
############################################

# Convert strategy responses to long format
survey_long <- stroop_df %>%
  select(Participant, Offset, Block, Audio_Bias, Visual_Bias, One_Strat, Strat_Change, No_Strat) %>%
  distinct() %>%  # one row per Participant × Block
  pivot_longer(
    cols = c(Audio_Bias, Visual_Bias, One_Strat, Strat_Change, No_Strat),
    names_to = "Strategy_Question",
    values_to = "Rating"
  ) %>%
  mutate(
    Strategy_Question = factor(Strategy_Question, levels = c("Audio_Bias", "Visual_Bias", "No_Strat", "One_Strat", "Strat_Change"))
  )

# Plot: strategy rating lines per participant, faceted by offset
ggplot(survey_long, aes(x = Strategy_Question, y = Rating, group = Participant, color = Participant)) +
  geom_line(linewidth = 0.7, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ Offset) +
  theme_minimal() +
  labs(
    title = "Strategy Ratings per Participant by Offset",
    x = "Strategy Question",
    y = "Rating (1–5)"
  ) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 25, hjust = 1))

############################################
### STATS: Pairwise t-tests on AVC RTs by Offset
############################################

avc_ttests <- stroop_df %>%
  filter(Type == "AVC") %>%
  pairwise_t_test(
    RT ~ Offset,
    paired = FALSE,
    p.adjust.method = "fdr"
  )

print(avc_ttests)

############################################
### EXTRA: Overlayed Medians for A, V, AVC
############################################

median_rt_overlay <- stroop_df %>%
  filter(Type %in% c("A", "V", "AVC")) %>%
  group_by(Participant, Offset, Type) %>%
  summarise(median_rt = median(RT_corrected, na.rm = TRUE), .groups = "drop")

participant_colors <- scales::hue_pal()(length(unique(median_rt_overlay$Participant)))
names(participant_colors) <- unique(median_rt_overlay$Participant)

ggplot(median_rt_overlay, aes(x = Offset, y = median_rt, group = interaction(Participant, Type))) +
  geom_line(aes(color = Participant, linetype = Type), linewidth = 1, alpha = 0.8) +
  geom_point(aes(color = Participant, shape = Type), size = 2) +
  scale_color_manual(values = participant_colors) +
  scale_shape_manual(values = c("AVC" = 17, "A" = 15, "V" = 16)) +
  scale_linetype_manual(values = c("AVC" = "solid", "A" = "dashed", "V" = "dotted")) +
  scale_y_continuous(name = "Median RT (ms)", breaks = seq(300, 700, 50)) +
  coord_cartesian(ylim = c(300, 700)) +
  theme_minimal() +
  labs(
    title = "Overlayed Median RTs for A, V, and AVC Trials by Participant",
    x = "Offset (ms)",
    color = "Participant",
    linetype = "Trial Type",
    shape = "Trial Type"
  ) +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 12))
