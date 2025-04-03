library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggsignif)

# Set working directory
setwd("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Pilot_Data/")

# Read the INDIVIDUAL participant data
df <- read.csv("(I)AV_Stress_PilotData_P003.csv")

######################
### CLEAN THE DATA ###
######################

# Prepare the data
df_clean <- df %>%
  mutate(
    Correct = as.logical(Correct),         # Convert to TRUE/FALSE
    RT = as.numeric(RT),                   # Ensure RT is numeric
    Block = as.factor(Block),              # Treat Block as categorical
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI"))  # Trial type
  )

# Remove RTs under 200ms and over 0.8ms
df_clean <- df_clean %>%
  filter(RT >= 0.2, RT <= 0.8)

######################
### DATA SUMMARIES ###
######################

# Overall Summary
summary(df_clean)

# Summarize by Type
summary_by_type <- df_clean %>%
  group_by(Type) %>%
  summarise(
    mean_accuracy = mean(Correct, na.rm = TRUE),
    mean_RT = mean(RT, na.rm = TRUE),
    median_RT = median(RT, na.rm = TRUE),
    sd_RT = sd(RT, na.rm = TRUE),
    n_trials = n()
  )

print(summary_by_type)

# Accuracy and RT by Block
summary_by_block <- df_clean %>%
  group_by(Block) %>%
  summarise(
    mean_accuracy = mean(Correct, na.rm = TRUE),
    mean_rt = mean(RT, na.rm = TRUE),
    median_RT = median(RT, na.rm = TRUE),
    sd_rt = sd(RT, na.rm = TRUE),
    n_trials = n()
  )

print(summary_by_block)

#############################
### VISUALIZING SUMMARIES ###
#############################

### MOST BEAUTIFUL PLOT OF ALL TIME ###
ggplot(df_clean, aes(x = Type, y = RT, color = Type)) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_signif(
    comparisons = list(c("AVC", "A"), c("AVC", "V"), c("AVC", "AVI"), c("AVI", "A"), c("AVI", "V")),
    test = "wilcox.test",
    map_signif_level = TRUE,
    step_increase = 0.07,
    tip_length = 0.01
  ) +
  scale_color_manual(values = c("plum", "#7CAE00", "#F8766D", "#00BFC4")) +
  theme_minimal() +
  labs(title = "Reaction Time by Trial Type", y = "RT (s)", x = "Trial Type") +
  theme(plot.title = element_text(hjust = 0.5))

wilcox.test(RT ~ Type, data = df_clean %>% filter(Type %in% c("AVC", "A")))

# Reaction Time by Type
ggplot(summary_by_type, aes(x = Type, y = mean_RT)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), width = 0.2) +
  labs(title = "Mean Reaction Time by Trial Type", y = "RT (s)", x = "Trial Type") +
  theme_minimal()

# Accuracy by Type
ggplot(summary_by_type, aes(x = Type, y = mean_accuracy)) +
  geom_col(fill = "seagreen") +
  labs(title = "Accuracy by Trial Type", y = "Proportion Correct", x = "Trial Type") +
  ylim(0, 1) +
  theme_minimal()

# Reaction Time by Block
ggplot(summary_by_block, aes(x = factor(Block), y = mean_rt)) +
  geom_col(fill = "orange") +
  geom_errorbar(aes(ymin = mean_rt - sd_rt, ymax = mean_rt + sd_rt), width = 0.2) +
  labs(title = "Mean Reaction Time by Block", y = "RT (s)", x = "Block") +
  theme_minimal()

# Accuracy by Block
ggplot(summary_by_block, aes(x = factor(Block), y = mean_accuracy)) +
  geom_col(fill = "purple") +
  labs(title = "Accuracy by Block", y = "Proportion Correct", x = "Block") +
  ylim(0, 1) +
  theme_minimal()

####################################
### ONE WAY ANOVA ON RT x TRIAL TYPE
####################################

# Run one-way ANOVA
rt_anova <- aov(RT ~ Type, data = df)

# View results
summary(rt_anova)

# See which group significantly differs
TukeyHSD(rt_anova)

###########################################
### CHI SQUARE ON ACCURACY x TRIAL TYPE ###
###########################################

# Remove AVI trials, since they have NA for Correct (optional if you're doing RT only)
df_accuracy <- df_clean %>%
  filter(!is.na(Correct), Type != "AVI") %>%
  droplevels()  # THIS is what makes AVI go away

# Create a contingency table
acc_table <- table(df_accuracy$Type, df_accuracy$Correct)

# Run chi-square test
chisq.test(acc_table)

# Run pairwise proportion tests to compare accuracy between each pair of trial types
# This tests: A vs V, A vs AVC, V vs AVC
# It uses Bonferroni correction to adjust for multiple comparisons
pairwise_results <- pairwise.prop.test(
  x = acc_table[, "TRUE"],                    # Number of correct trials for each Type
  n = rowSums(acc_table),                     # Total number of trials for each Type
  p.adjust.method = "bonferroni"              # Correct for multiple comparisons
)

# View results (shows adjusted p-values for each comparison)
print(pairwise_results)

######################################################################################
######################################################################################
######################################################################################

######################################
### CORRELATION MATRIX WITH SURVEY ###
######################################

# ----------------------------
# 1. Prepare Block 4 data
# ----------------------------

block4_data <- df_clean %>%
  filter(Block == 4, Correct == TRUE) %>%
  mutate(
    ITI_group = ntile(ITI, 4),
    ITI_group = factor(ITI_group, labels = c("Shortest", "Short", "Long", "Longest"))
  )

block4_medians <- block4_data %>%
  group_by(Participant, Type, ITI_group) %>%
  summarise(MedianRT = median(RT, na.rm = TRUE), .groups = "drop") %>%
  filter(ITI_group %in% c("Shortest", "Longest"))

# ----------------------------
# 2. Prepare Block 1–3 data
# ----------------------------

preblock_data <- df_clean %>%
  filter(Block != 4, Correct == TRUE) %>%
  group_by(Participant, Type) %>%
  summarise(MedianRT = median(RT, na.rm = TRUE), .groups = "drop") %>%
  mutate(ITI_group = "Pre")

# ----------------------------
# 3. Combine all RT data
# ----------------------------

combined_rt_data <- bind_rows(preblock_data, block4_medians)

# ----------------------------
# 4. Prepare survey responses
# ----------------------------

survey_long <- survey_df %>%
  pivot_longer(cols = starts_with("Q"), names_to = "Question", values_to = "Score")

# ----------------------------
# 5. Join everything
# ----------------------------

plot_data <- inner_join(survey_long, combined_rt_data, by = "Participant")

# Optional: make survey score a factor (discrete) if you want
# plot_data <- plot_data %>% mutate(Score = factor(Score, levels = 1:5))

# ----------------------------
# 6. Plot it!
# ----------------------------

ggplot(plot_data, aes(x = Score, y = MedianRT, color = ITI_group)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method = "pearson",
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           size = 3, show.legend = FALSE) +
  facet_grid(Type ~ Question) +
  labs(title = "Survey Responses vs Median RTs (Pre vs Shortest vs Longest ITI)",
       x = "Survey Score",
       y = "Median RT (s)",
       color = "RT Group") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 14))

###############
### T-TESTS ###
###############

# Define the group pairs to compare
group_pairs <- list(
  c("Pre", "Shortest"),
  c("Pre", "Longest"),
  c("Shortest", "Longest")
)

# Run t-tests by Type and group comparison
library(purrr)

t_test_results <- map_dfr(group_pairs, function(pair) {
  group1 <- pair[1]
  group2 <- pair[2]
  
  combined_rt_data %>%
    filter(ITI_group %in% c(group1, group2)) %>%
    group_by(Type) %>%
    summarise(
      t_test = list(t.test(MedianRT ~ ITI_group)),
      comparison = paste(group1, "vs", group2),
      .groups = "drop"
    ) %>%
    mutate(
      t_stat = map_dbl(t_test, ~ .x$statistic),
      p_value = map_dbl(t_test, ~ .x$p.value),
      df = map_dbl(t_test, ~ .x$parameter),
      mean_group1 = map_dbl(t_test, ~ .x$estimate[[1]]),
      mean_group2 = map_dbl(t_test, ~ .x$estimate[[2]])
    ) %>%
    select(Type, comparison, t_stat, df, p_value, mean_group1, mean_group2)
})

print(t_test_results)
