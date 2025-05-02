### Saving DF as CSV to make it GUI friendly! ###

# Set working directory
setwd("/Users/harpermarshall/Desktop/Project 1/AV_Stress_Pilot_Data/")

# List all participant files (assuming they all start with "AV_Stress_PilotData_")
file_list <- list.files(pattern = "AV_Stress_PilotData_.*\\.csv")
print(file_list)  # Optional: check the list

# Function to read and tag each file with participant ID
# Read, clean, and combine
df <- file_list %>%
  lapply(function(file) {
    df <- read_csv(file)
    
    # Extract and add participant ID from filename (e.g., "AV_Stress_PilotData_P001.csv" → "P001")
    df$Participant <- gsub(".*_P(\\d+)\\.csv", "P\\1", file)
    
    return(df)
  }) %>%
  bind_rows()

# Load all survey CSVs (assuming one per participant)
survey_files <- list.files(pattern = "AV_Stress_SurveyPilot_.*\\.csv")

survey_df <- survey_files %>%
  lapply(function(file) {
    df <- read_csv(file)
    df$Participant <- gsub(".*_P(\\d+)\\.csv", "P\\1", file)
    return(df)
  }) %>%
  bind_rows()

######################
### CLEAN THE DATA ###
######################

# Clean and prepare the data
df_clean <- df %>%
  mutate(
    Correct = as.logical(Correct),         # Convert to TRUE/FALSE
    RT = as.numeric(RT),                   # Ensure RT is numeric
    Block = as.factor(Block),              # Treat Block as categorical
    Type = factor(Type, levels = c("A", "V", "AVC", "AVI"))  # Trial type
  )

# Remove RTs under 50ms and over 0.8ms
df_clean <- df_clean %>%
  filter(RT >= 0.05, RT <= 0.8)

########################################
### REMOVE DATA FOR SPECIFIC NEW CSV ###
########################################

df_clean <- df_clean %>%
  # Remove AVI trials
  filter(Type != "AVC", is.na(Correct) | Correct == TRUE) %>%
  
  # Recode Type
  mutate(
    Type = case_when(
      Type == "A" ~ 1,
      Type == "V" ~ 2,
      Type == "AVI" ~ 3,
      TRUE ~ NA_real_  # fallback just in case
    ),
    
    # Clean Participant ID
    Participant = as.numeric(gsub("\\D", "", Participant))
  )

# To save df as CSV file
library(readr)
write_csv(df_clean, "AV_Stress_PilotData_AVI.csv")

