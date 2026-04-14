########################################
###      LOAD LIBRARIES & SETUP      ###
########################################

# Tell R which folder has all your data files
setwd("/Users/harpermarshall/Desktop/Project 1/SCT_Data/")

# Read in the cleaned CSV of all trials + survey answers
# and make sure "modality" is treated as a category with levels A, V, AVC, AVI
total_df <- read_csv("All_Trials_With_Survey_21TrueFAST.csv") %>%
  mutate(
    modality = factor(modality, levels = c("A","V","AVC","AVI"))
  )

# Define a custom set of colors for each offset value (in milliseconds)
offset_colors <- c(
  "-50.01" = "#FEC495",
  "-33.34" = "#F99A3F",
  "-16.67" = "#C1292E",
  "0"      = "#60110C",
  "16.67"  = "#006A79",
  "33.34"  = "#BCE1E5",
  "50.01"  = "#DEEDEE"
)

#offset_colors <- c(
  #"-33.34" = "#C23B7A",
  #"-16.67" = "#8B1E3F",
  #"0"      = "#4A4A4A",
  #"16.67"  = "#008080",
  #"33.34"  = "#42BFBF",
  #"50.01"  = "#8ADBD2"

########################################
###        Raw RT by Modality        ###
########################################

# This plot shows all individual RTs (dots) plus boxplots, colored by offset
ggplot(total_df, 
       aes(
         x = modality,                      # category on x-axis
         y = rt_corrected,                  # RT in ms on y-axis
         color = factor(offset_corrected),  # color points by offset
         group = interaction(modality, offset_corrected)
       )
) +
  # jittered points so they don't all sit on top of each other
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
    alpha = 0.4
  ) +
  # semi-transparent boxplots, no outlier points shown
  geom_boxplot(
    position = position_dodge(width = 0.6),
    alpha = 0.5,
    outlier.shape = NA,
    width = 0.5
  ) +
  # use our custom colors for offsets
  scale_color_manual(values = offset_colors) +
  # y-axis from 200 to 2000 ms, major ticks every 100, minor every 50
  scale_y_continuous(
    name = "Response Time (ms)",
    breaks = seq(200, 2000, 100),
    minor_breaks = seq(200, 2000, 50),
    limits = c(200, 2000)
  ) +
  # labels and title
  labs(
    title = "Response Time by Trial Modality and Stimulus Offset",
    x = "Trial Modality",
    color = "Offset (ms)"
  ) +
  # a clean look
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12)
  )

# Same plot but with the mean RT marked as a white diamond outlined in black
ggplot(total_df, aes(x = modality, y = rt_corrected, color = factor(offset_corrected), group = interaction(modality, offset_corrected))) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6), alpha = 0.4) +
  geom_boxplot(position = position_dodge(width = 0.6), alpha = 0.5, outlier.shape = NA, width = 0.5) +
  # add mean point
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,   # diamond shape
    size = 2.5,
    fill = "white",
    color = "black",
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = offset_colors) +
  scale_y_continuous(name = "Response Time (ms)", breaks = seq(200, 2000, 100), minor_breaks = seq(200, 2000, 50), limits = c(200, 2000)) +
  labs(title = "Response Time by Trial Modality and Stimulus Offset", x = "Trial Modality", color = "Offset (ms)") +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(size = 12))

########################################
###      Median RT by Modality       ###
########################################

# Calculate the median RT for each modality × offset
median_summary <- total_df %>%
  group_by(modality, offset_corrected) %>%
  summarise(
    median_rt = median(rt_corrected, na.rm = TRUE),
    .groups = "drop"
  )

# Bar plot of those medians, colored by offset
# Bar plot of those medians, colored by offset
ggplot(median_summary, aes(x = modality, y = median_rt, fill = factor(offset_corrected))) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  scale_fill_manual(values = offset_colors) +
  labs(
    title = "Median Response Time by\nTrial Modality and Stimulus Onset",
    x = "Trial Modality",
    y = "Median Response Time (ms)",
    fill = "Onset (ms)"
  ) +
  coord_cartesian(ylim = c(300, 700)) +  # zoom in on typical RT range
  theme_minimal(base_size = 26, base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "#CCCCCC"),
    panel.grid.minor = element_line(color = "#E0E0E0"),
    axis.text = element_text(color = "#333333"),
    axis.title = element_text(color = "#333333"),
    plot.title = element_text(
      hjust = 0.5, size = 36, face = "bold", color = "#333333"
    ),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(color = "#333333"),
    legend.title = element_text(color = "#333333"),
    axis.text.x = element_text(size = 26),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30)
  )

########################################
###    Median RT by Participant      ###
########################################

# Now calculate median RT per person × modality × offset
# Optional: list participants to exclude (does NOT modify total_df)
exclude_participants <- c()  # or c() to include everyone

median_summary <- total_df %>%
  filter(!participant_number %in% exclude_participants) %>%
  group_by(participant_number, modality, offset_corrected) %>%
  summarise(
    median_rt = median(rt_corrected, na.rm = TRUE),
    .groups = "drop"
  )

# Same bar chart but split into one small panel for each participant
ggplot(median_summary, aes(x = modality, y = median_rt, fill = factor(offset_corrected))) +
  geom_col(position = position_dodge(0.6), width = 0.5) +
  facet_wrap(~ participant_number) +  # one plot per person
  scale_fill_manual(values = offset_colors, name = "Offset (ms)") +
  coord_cartesian(ylim = c(300, 800)) +
  labs(
    title = "Median RT by Trial Modality and Offset (Per Participant)",
    x = "Trial Modality",
    y = "Median RT (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    strip.text = element_text(size = 13)  # larger participant label
  )

########################################
###    AVC Median RTs by Offset      ###
########################################

# Focus only on the AVC trials
# Optional exclusion list (does NOT modify total_df)
exclude_participants <- c("P014")  # or c()

total_df %>%
  filter(
    modality == "AVC",
    !participant_number %in% exclude_participants
  ) %>%  
  group_by(participant_number, offset_corrected) %>%
  summarise(
    median_rt = median(rt_corrected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = offset_corrected,
    y = median_rt,
    group = participant_number,
    color = participant_number
  )) +
  geom_line(alpha = 0.7, linewidth = 1) +  # connect points with lines
  geom_point(size = 2) +
  scale_x_continuous(name = "Visual Offset (ms)", breaks = sort(unique(total_df$offset_corrected))) +
  scale_y_continuous(name = "Response Time (ms)", breaks = seq(300, 800, 100), minor_breaks = seq(300, 800, 50)) +
  coord_cartesian(ylim = c(300, 800)) +
  labs(title = "Per-Participant Median RTs for AVC Trials by Offset", color = "Participant") +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12),
    panel.grid.minor.x = element_blank()  # remove extra vertical grid lines
  )

########################################
###    AVC Median RTs by Block       ###
########################################

# Instead of offset, look by the block number
total_df %>%
  filter(modality == "AVC") %>%
  group_by(participant_number, block) %>%
  summarise(
    median_rt = median(rt_corrected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = block,
    y = median_rt,
    group = participant_number,
    color = participant_number
  )) +
  geom_line(alpha = 0.7, linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(name = "Block Number", breaks = 1:7) +
  scale_y_continuous(name = "Response Time (ms)", breaks = seq(300, 800, 100), minor_breaks = seq(300, 800, 50)) +
  coord_cartesian(ylim = c(300, 800)) +
  labs(title = "Per-Participant Median RTs for AVC Trials by Block", color = "Participant") +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12),
    panel.grid.minor.x = element_blank()
  )

########################################
###   Table: AVI vs Survey Responses ###
########################################

# Clean up AVI trials to figure out if the response matched audio or visual color
avi_response_summary <- total_df %>%
  filter(modality == "AVI") %>%
  mutate(
    # make audio text lowercase & remove file extensions
    audio_clean    = str_remove(tolower(audio), "\\.[a-z0-9]+$"),
    visual_clean   = tolower(visual),
    # convert 'r'/'b' to full words
    response_color = case_when(
      tolower(response) == "r" ~ "red",
      tolower(response) == "b" ~ "blue",
      TRUE                     ~ NA_character_
    ),
    # check whether they picked audio color, visual color, or neither
    matchmodality = case_when(
      response_color == audio_clean  ~ "Audio",
      response_color == visual_clean ~ "Visual",
      TRUE                           ~ "Neither"
    )
  )

# Count how many times they matched the audio vs visual color
avi_summary <- avi_response_summary %>%
  filter(matchmodality %in% c("Audio","Visual")) %>%
  group_by(participant_number, matchmodality) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = matchmodality, values_from = n, values_fill = 0) %>%
  mutate(
    total      = Audio + Visual,
    prop_audio = Audio / total,
    prop_visual = Visual / total
  )

# Show the table in the console
print(avi_summary)

########################################
###   Survey Ratings by Offset       ###
########################################

# Tidy the survey columns so each question is in its own row
survey_by_offset <- total_df %>%
  select(participant_number, offset_corrected, audio_bias, visual_bias, one_strat, change_strat, no_strat) %>%
  distinct() %>%
  pivot_longer(
    cols = c(audio_bias, visual_bias, one_strat, change_strat, no_strat),
    names_to = "Strategy_Question",
    values_to = "Rating"
  ) %>%
  mutate(offset_corrected = as.numeric(as.character(offset_corrected)))

# Plot each question’s ratings over offsets, separate panel per question
ggplot(survey_by_offset, aes(x = offset_corrected, y = Rating, group = participant_number, color = participant_number)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Strategy_Question, scales = "fixed") +
  scale_x_continuous(name = "Visual Offset (ms)", breaks = sort(unique(survey_by_offset$offset_corrected))) +
  scale_y_continuous(breaks = 1:5, limits = c(1,5)) +
  theme_minimal(base_size = 14) +
  labs(title = "Survey Ratings by Offset and Strategy Question", y = "Rating (1–5)", color = "Participant") +
  theme(strip.text = element_text(size = 13), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 12))

########################################
### Strategy Ratings by Participant  ###
########################################

# Same survey data but now we facet by offset instead of by question
survey_long <- total_df %>%
  select(participant_number, offset_corrected, block, audio_bias, visual_bias, one_strat, change_strat, no_strat) %>%
  distinct() %>%
  pivot_longer(
    cols = c(audio_bias, visual_bias, one_strat, change_strat, no_strat),
    names_to = "Strategy_Question",
    values_to = "Rating"
  ) %>%
  mutate(Strategy_Question = factor(Strategy_Question, levels = c("audio_bias","visual_bias","no_strat","one_strat","change_strat")))

ggplot(survey_long, aes(x = Strategy_Question, y = Rating, group = participant_number, color = participant_number)) +
  geom_line(linewidth = 0.7, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ offset_corrected) +
  theme_minimal() +
  labs(title = "Strategy Ratings per Participant by Offset", x = "Strategy Question", y = "Rating (1–5)") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 25, hjust = 1))

########################################
### Overlayed Median RTs per Modal   ###
########################################

# Gather median RTs for A, V, AVC trials
median_rt_overlay <- total_df %>%
  filter(modality %in% c("A","V","AVC")) %>%
  group_by(participant_number, offset_corrected, modality) %>%
  summarise(median_rt = median(rt_corrected, na.rm = TRUE), .groups = "drop")

# Plot lines for each modality, one small panel per participant
ggplot(median_rt_overlay, aes(x = offset_corrected, y = median_rt, group = modality)) +
  geom_line(aes(color = modality, linetype = modality), linewidth = 1, alpha = 0.8) +
  geom_point(aes(color = modality, shape = modality), size = 2) +
  scale_color_manual(values = c("AVC"="#9467bd","A"="#d62728","V"="#1f77b4")) +
  scale_shape_manual(values = c("AVC"=17,"A"=15,"V"=16)) +
  scale_linetype_manual(values = c("AVC"="solid","A"="dashed","V"="dotted")) +
  scale_y_continuous(name = "Median RT (ms)", breaks = seq(300,900,100)) +
  scale_x_continuous(name = "Visual Offset (ms)", breaks = sort(unique(median_rt_overlay$offset_corrected))) +
  coord_cartesian(ylim = c(300,900)) +
  facet_wrap(~ participant_number, ncol = 4) +
  theme_minimal(base_size = 14) +
  labs(title = "Median RTs for A, V, and AVC Trials by Participant", color = "Trial Modality", linetype = "Trial Modality", shape = "Trial Modality") +
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 10), strip.text = element_text(face = "bold"))

########################################
### Strategy vs Median RT Plots      ###
########################################

# Compute median RT per person × offset
rt_summary <- total_df %>%
  filter(!is.na(rt_corrected)) %>%
  group_by(participant_number, offset_corrected) %>%
  summarise(median_rt = median(rt_corrected, na.rm = TRUE), .groups = "drop")

# Get distinct strategy ratings per person × offset
strategy_summary <- total_df %>%
  select(participant_number, offset_corrected, one_strat, change_strat, no_strat) %>%
  distinct()

# Merge them together
rt_strategy <- left_join(rt_summary, strategy_summary, by = c("participant_number","offset_corrected"))

# Plot median RT vs one_strat for each offset
ggplot(rt_strategy, aes(x = one_strat, y = median_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Median RT by one_strat (per Offset)", x = "one_strat", y = "Median RT (ms)")

# Repeat for change_strat
ggplot(rt_strategy, aes(x = change_strat, y = median_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Median RT by change_strat (per Offset)", x = "change_strat", y = "Median RT (ms)")

# And for no_strat
ggplot(rt_strategy, aes(x = no_strat, y = median_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Median RT by no_strat (per Offset)", x = "no_strat", y = "Median RT (ms)")

########################################
### Strategy Bias: Audio vs Visual   ###
########################################

# Compute mean RT per person × modality × offset
rt_bias_summary <- total_df %>%
  filter(!is.na(rt_corrected)) %>%
  group_by(participant_number, modality, offset_corrected) %>%
  summarise(mean_rt = mean(rt_corrected, na.rm = TRUE), .groups = "drop")

# Get distinct bias scores per person × offset
bias_scores <- total_df %>%
  select(participant_number, offset_corrected, audio_bias, visual_bias) %>%
  distinct()

# Merge together
rt_bias <- left_join(rt_bias_summary, bias_scores, by = c("participant_number","offset_corrected"))

# Plot audio_bias vs RT for A trials
ggplot(rt_bias %>% filter(modality=="A"), aes(x = audio_bias, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method="lm", se=TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size=14) +
  labs(title="RT in A Trials vs audio_bias (by Offset)", x="audio_bias", y="Mean RT (ms)")

# Plot visual_bias vs RT for V trials
ggplot(rt_bias %>% filter(modality=="V"), aes(x = visual_bias, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method="lm", se=TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size=14) +
  labs(title="RT in V Trials vs visual_bias (by Offset)", x="visual_bias", y="Mean RT (ms)")

########################################
### AVC Mean RT vs Strategy Ratings  ###
########################################

# Mean RT for AVC only, per person × offset
AVC_rt_summary <- total_df %>%
  filter(!is.na(rt_corrected), modality == "AVC") %>%
  group_by(participant_number, offset_corrected) %>%
  summarise(mean_rt = mean(rt_corrected, na.rm = TRUE), .groups = "drop")

# Merge with strategy ratings
AVC_rt_strategy <- left_join(AVC_rt_summary, strategy_summary, by = c("participant_number","offset_corrected"))

# Plot one_strat vs mean RT (AVC)
ggplot(AVC_rt_strategy, aes(x = one_strat, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean RT (AVC only) by one_strat (per Offset)", x = "one_strat", y = "Mean RT (ms)")

# ... and similar for change_strat and no_strat
ggplot(AVC_rt_strategy, aes(x = change_strat, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean RT (AVC only) by change_strat (per Offset)", x = "change_strat", y = "Mean RT (ms)")

ggplot(AVC_rt_strategy, aes(x = no_strat, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean RT (AVC only) by no_strat (per Offset)", x = "no_strat", y = "Mean RT (ms)")

########################################
### AVI Mean RT vs Strategy Ratings  ###
########################################

# Mean RT for AVI only, per person × offset
AVI_rt_summary <- total_df %>%
  filter(!is.na(rt_corrected), modality == "AVI") %>%
  group_by(participant_number, offset_corrected) %>%
  summarise(mean_rt = mean(rt_corrected, na.rm = TRUE), .groups = "drop")

# Merge with strategy ratings
AVI_rt_strategy <- left_join(AVI_rt_summary, strategy_summary, by = c("participant_number","offset_corrected"))

# Plot one_strat vs mean RT (AVI)
ggplot(AVI_rt_strategy, aes(x = one_strat, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean RT (AVI only) by one_strat (per Offset)", x = "one_strat", y = "Mean RT (ms)")

# ... and similar for change_strat and no_strat
ggplot(AVI_rt_strategy, aes(x = change_strat, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean RT (AVI only) by change_strat (per Offset)", x = "change_strat", y = "Mean RT (ms)")

ggplot(AVI_rt_strategy, aes(x = no_strat, y = mean_rt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ offset_corrected) +
  theme_minimal(base_size = 14) +
  labs(title = "Mean RT (AVI only) by no_strat (per Offset)", x = "no_strat", y = "Mean RT (ms)")

########################################
### Accuracy by Modality & Offset    ###
########################################

# Uses clean_df form CleanDataSCT to re-add the incorrect trials

# For each modality × offset, calculate the proportion of correct trials
accuracy_summary <- clean_df %>%
  filter(!is.na(correct)) %>%
  group_by(modality, offset_corrected) %>%
  summarise(accuracy = mean(correct), .groups = "drop") %>%
  mutate(offset_corrected = as.numeric(offset_corrected))

ggplot(accuracy_summary, aes(x = offset_corrected, y = accuracy, color = modality, group = modality)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("A" = "#4F81BD", "AVC" = "darkgreen", "V" = "#C0504D")) +
  scale_x_continuous(name = "Visual Offset (ms)",
                     breaks = sort(unique(accuracy_summary$offset_corrected))) +
  scale_y_continuous(name = "Proportion Correct", limits = c(0.95, 1)) +
  theme_minimal(base_size = 14) +
  labs(title = "Accuracy by Modality and Visual Offset")

# BAR PLOT VERSION
ggplot(accuracy_summary, aes(x = factor(offset_corrected), 
                             y = accuracy, 
                             fill = modality)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           width = 0.7) +
  scale_fill_manual(values = c("A" = "#4F81BD",      # soft blue
                               "AVC" = "darkgreen",  # AVC stays green
                               "V" = "#C0504D")) +   # soft red
  scale_y_continuous(name = "Proportion Correct") + 
  coord_cartesian(ylim = c(0.90, 1)) +   # <-- ZOOMS in without dropping bars
  labs(x = "Visual Offset (ms)", 
       title = "Accuracy by Modality and Visual Offset") +
  theme_minimal(base_size = 14)

# ACURACY CALC #
# 1️⃣ Average accuracy for EACH participant × modality × offset
accuracy_by_participant <- clean_df %>%
  filter(!is.na(correct),
         rt_corrected >= 250) %>%
  group_by(participant_number, modality, offset_corrected) %>%
  summarise(mean_accuracy = mean(correct), .groups = "drop")

anova_results <- accuracy_by_participant %>%
  anova_test(
    dv = mean_accuracy,
    wid = participant_number,
    within = c(modality, offset_corrected)
  )

anova_results

# 2️⃣ Paired t-tests: compare AVC to A and V at each offset
pairwise_results <- accuracy_by_participant %>%
  group_by(offset_corrected) %>%
  pairwise_t_test(
    mean_accuracy ~ modality,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

print(pairwise_results, n = Inf)

