############################################
### LOAD LIBRARIES & SETUP ##############
############################################

# Load tidyverse if not already loaded
library(tidyverse)

# USE DF CREATED FROM FORMAT DATA

# Step 1: Recode response
soa_df <- soa_df %>%
  mutate(
    simultaneous = case_when(
      response == "S" ~ 1,
      response == "A" ~ 0,
      TRUE ~ NA_real_
    ),
    soa_factor = factor(soa_)  # Treat SOA as a categorical variable
  )

# Step 2: Summarize by SOA
soa_summary <- soa_df %>%
  group_by(participant_number, soa_factor) %>%
  summarise(
    proportion_simultaneous = mean(simultaneous, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

# Step 3: Plot with facets for each participant
ggplot(soa_summary, aes(x = soa_factor, y = proportion_simultaneous, group = 1)) +
  geom_line(linewidth = 1, color = "#9e91c3") +
  geom_point(size = 2, color = "#9e91c3") +
  scale_y_continuous(name = "Proportion Judged Simultaneous", limits = c(0, 1)) +
  xlab("Stimulus Onset Asynchrony (SOA)") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  facet_wrap(~ participant_number, ncol = 4) +
  theme_minimal(base_size = 14) +
  labs(title = "Perceived Simultaneity by SOA per Participant")

# Step 4: Plot with facets average across particicpants
soa_avg <- soa_summary %>%
  group_by(soa_factor) %>%
  summarise(
    mean_proportion = mean(proportion_simultaneous, na.rm = TRUE),
    se = sd(proportion_simultaneous, na.rm = TRUE) / sqrt(n())
  )

ggplot(soa_avg, aes(x = soa_factor, y = mean_proportion, group = 1)) +
  geom_line(linewidth = 1, color = "#9e91c3") +
  geom_point(size = 3, color = "#9e91c3") +
  geom_errorbar(aes(ymin = mean_proportion - se, ymax = mean_proportion + se),
                width = 0.2, color = "#9e91c3") +
  scale_y_continuous(name = "Mean Proportion Judged Simultaneous", limits = c(0, 1)) +
  xlab("Stimulus Onset Asynchrony (SOA)") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 14) +
  labs(title = "Average Perceived Simultaneity by SOA")
