##################################
### LOAD LIBRARIES & SETUP ######
##################################

library(tidyverse)
library(ggsignif)
library(rstatix)
library(dplyr)

# Set working directory to where your CSV files are stored
setwd("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Data/")

# Load participant data for INDIVUDUAL PARTICIPANT
# df <- read.csv("AV_Stress_Results_P005.csv")
#survey_df <- read.csv("AV_Stress_Survey_P001.csv")  # If you plan to use survey data later

# Load and COMBINE PARTICIPANTS 2–5 from AV_Stress_Data folder
file_list <- list.files(pattern = "AV_Stress_Results_P00[2-5]\\.csv", 
                        full.names = TRUE)
print(file_list)  # Optional: check the list

df <- file_list %>%
  map_dfr(~ read_csv(.x)) %>%
  filter(Block %in% c(1, 2, 3)) %>%
  filter(Correct == TRUE | is.na(Correct))

######################
### CLEAN THE DATA ###
######################

df_clean <- df %>%
  mutate(
    Correct = as.logical(Correct),         # Convert to TRUE/FALSE for accuracy
    RT = as.numeric(RT),                   # Ensure RT is numeric
    Block = as.factor(Block),              # Treat Block as categorical
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI"))  # Set order of trial types
  ) %>%
  filter(RT >= 0.2, RT <= 0.8)  # Exclude implausibly fast or slow RTs

################################
### BLOCKWISE COMPARISONS ######
################################

# Create grouping variable for fill/color by Block
df_clean <- df_clean %>%
  mutate(
    TypeGroup = Type,
    Block = factor(Block, levels = c("2", "3", "1")),  # Reorders blocks
    BlockLabel = recode(Block,
                        `1` = "210ms V-delay",
                        `2` = "40ms V-delay",
                        `3` = "140ms V-delay")
  )

# Assign gentle, aesthetic colors to each Block
block_colors <- c("210ms V-delay" = "plum", "40ms V-delay" = "#F8766D", "140ms V-delay" = "#00BFC4")  # Soft red and teal shades

# Make sure Trial Type is ordered the way you want (e.g., A, V, AVC, AVI)
df_clean$Type <- factor(df_clean$Type, levels = c("A", "V", "AVC", "AVI"))

# Plot with grouped x-axis and soft coloring
ggplot(df_clean, aes(x = Type, y = RT, color = BlockLabel)) +
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
  scale_color_manual(values = block_colors, name = "Block") +
  theme_minimal() +
  labs(
    title = "ALL Participants: RT by Trial Type and Block",
    x = "Trial Type",
    y = "RT (s)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12)
  )

# Summarize median RT by Type and Block
median_summary <- df_clean %>%
  group_by(Type, BlockLabel) %>%
  summarise(median_rt = median(RT, na.rm = TRUE), .groups = "drop")

# Bar plot of medians with y-axis starting at 0.3
ggplot(median_summary, aes(x = Type, y = median_rt, fill = BlockLabel)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  scale_fill_manual(values = block_colors, name = "Block") +
  theme_minimal() +
  labs(
    title = "ALL Participants: Median RT by Trial Type and Block",
    x = "Trial Type",
    y = "Median RT (s)"
  ) +
  coord_cartesian(ylim = c(0.3, NA)) +  # Zoom instead of removing data
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12)
  )

# Run pairwise t-tests within each block
ttest_results <- df_clean %>%
  group_by(BlockLabel) %>%
  pairwise_t_test(
    RT ~ Type,
    paired = FALSE,            # Assuming within-subject design
    p.adjust.method = "fdr"
  )

print(ttest_results)

##################
### BASIC MATH ###
##################

# Filter for Block 3 and A/V trials
block3_diff <- df %>%
  filter(Block == 3, Type %in% c("A", "V")) %>%
  group_by(Type) %>%
  summarise(median_rt = median(RT, na.rm = TRUE)) %>%
  pivot_wider(names_from = Type, values_from = median_rt) %>%
  mutate(rt_difference = V - A)

print(block3_diff)
