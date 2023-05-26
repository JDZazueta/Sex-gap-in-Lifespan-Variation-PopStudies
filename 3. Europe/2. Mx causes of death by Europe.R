# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Compute proportion of deaths to mx by Europe
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Compute mix (mortality rates by causes of death i)
#   3. Save the data
#   4. Combine datasets

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")
# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")


# ---------------------------------------------------------------------------- #
#     1. Open data set
# ---------------------------------------------------------------------------- #

get(load("Data/Data_combined_Europe_period.RData"))

# ---------------------------------------------------------------------------- #
#     2. Compute mix (mortality rates by causes of death i)
# ---------------------------------------------------------------------------- #


# Multiplicate proportions of death to the mx of Life table
Data_combined_Europe$mix_1 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_1
Data_combined_Europe$mix_2 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_2
Data_combined_Europe$mix_3 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_3
Data_combined_Europe$mix_4 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_4
Data_combined_Europe$mix_5 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_5
Data_combined_Europe$mix_6 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_6
Data_combined_Europe$mix_7 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_7
Data_combined_Europe$mix_8 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_8
Data_combined_Europe$mix_9 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_9
Data_combined_Europe$mix_10 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_10
Data_combined_Europe$mix_11 <- Data_combined_Europe$mx*Data_combined_Europe$Pr_11


Mx_CoD_Europe <- Data_combined_Europe

table(Mx_CoD_Europe$Region, Mx_CoD_Europe$Periods)
# ---------------------------------------------------------------------------- #
#     3. Save the data
# ---------------------------------------------------------------------------- #

save(Mx_CoD_Europe, file = "Data/Mx_CoD_Europe.RData")
