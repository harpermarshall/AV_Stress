########################################
### Single Unisensory: Full Analysis ###
########################################

# ----------------------------
# LOAD LIBRARIES + SET UP
# ----------------------------
setwd("/Users/harpermarshall/Desktop/Project 1/Analysis/")
library(tidyverse)
library(rstatix)
library(ggsignif)
library(dplyr)
library(stringr)
library(patchwork)


# ----------------------------
# CLEANING SETTINGS
exclude_participants <- c("P011", "P014", "P018")
rt_abs_min <- 100
rt_abs_max <- 2000
drop_incorrect <- TRUE
drop_outliers <- FALSE
outlier_z <- 3

# ----------------------------
# CLEAN + CREATE DFS 
# ----------------------------
source("CleanData-SCT.R")


# ----------------------------
# ANALYSIS SETTINGS
# ----------------------------
p_adjust_method <- "holm"   # options: "bonferroni", "fdr", "holm", "BH", "BY", "none"


# ----------------------------
# ANALYSIS
# ----------------------------
source("Stats-Median-1UNISENSORY.R")


# ----------------------------
# PLOT SETTINGS
# ----------------------------
source("ThemesAesthetics.R")
COLORS <- multisensory3
THEME <- paper_light_tnr

offsets <- c(-50.01, -33.34, -16.67, 0, 16.67, 33.34, 50.01)
modalities_keep <- c("A", "V", "AVC")
rt_min <- 200
bins <- seq(rt_min, 1500, by = 20)
probs <- c(seq(0.01, 0.20, by = 0.01),
           seq(0.25, 0.95, by = 0.05)) #probs <- seq(0.05, 0.95, by = 0.05)
x_break_by <- 500


# ----------------------------
# PAPER FIGURES
# ----------------------------
source("LineGraph-SOA-SCT.R")
source("BoxPlot-1UNISENSORY.R")
source("BarGraph-1UNISENSORY.R")
source("RaceModel-1UNISENSORY.R")


# ----------------------------
# EXTRA PLOTS
# ----------------------------
#source("BarGraph-Ind-SCT.R")
#source("LineGraph-Ind-RTxOffset-SCT.R")
#source("LineGraph-Ind-RTxBlock-SCT.R")
source("BarGraph-Ind-RTxModality-SCT.R")
source("LineGraph-Ind-SOA-SCT.R")

# ----------------------------
# EXTRA ANALYSIS
# ----------------------------
#source("Stats-Mean-SCT.R")