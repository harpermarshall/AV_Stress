################################
###        Full Analysis     ###
################################

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
# ----------------------------
exclude_participants <- c("P011", "P014", "P018")     #or use this to isolate one participant -> c(sprintf("P%03d", 1:13),sprintf("P%03d", 15:21))
rt_abs_min <- 200
rt_abs_max <- 2000
drop_incorrect <- TRUE
drop_outliers <- FALSE
outlier_z     <- 3


# ----------------------------
# CLEAN + CREATE DFS 
# ----------------------------
source("CleanData-SCT.R")


# ----------------------------
# ANALYSIS SETTINGS
# ----------------------------
p_adjust_method <- "bonferroni"   # options: "bonferroni", "fdr", "holm", "BH", "BY", "none"


# ----------------------------
# ANALYSIS
# ----------------------------
source("Stats-Median-SCT.R")


# ----------------------------
# PLOT SETTINGS
# ----------------------------
source("ThemesAesthetics.R")
COLORS <- multisensory3
THEME <- paper_light_tnr

offsets <- c(-50.01, -33.34, -16.67, 0, 16.67, 33.34, 50.01)
modalities_keep <- c("A", "V", "AVC")
rt_max <- 800
rt_min <- 200
bins <- seq(rt_min, 800, by = 10)
x_break_by <- 200


# ----------------------------
# PAPER FIGURES
# ----------------------------
source("LineGraph-SOA-SCT.R")
source("CombinedBoxBar-SCT.R")
#source("BoxPlot-SCT.R")
source("BarGraph-SCT.R")
source("RaceModel-SCT.R")


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

