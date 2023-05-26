# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Compute proportion of deaths to mx
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

get(load("Data/Data_combined_countries_period.RData"))

# ---------------------------------------------------------------------------- #
#     2. Compute mix (mortality rates by causes of death i)
# ---------------------------------------------------------------------------- #


# Multiplicate proportions of death to the mx of Life table
Data_combined_countries$mix_1 <- Data_combined_countries$mx*Data_combined_countries$Pr_1
Data_combined_countries$mix_2 <- Data_combined_countries$mx*Data_combined_countries$Pr_2
Data_combined_countries$mix_3 <- Data_combined_countries$mx*Data_combined_countries$Pr_3
Data_combined_countries$mix_4 <- Data_combined_countries$mx*Data_combined_countries$Pr_4
Data_combined_countries$mix_5 <- Data_combined_countries$mx*Data_combined_countries$Pr_5
Data_combined_countries$mix_6 <- Data_combined_countries$mx*Data_combined_countries$Pr_6
Data_combined_countries$mix_7 <- Data_combined_countries$mx*Data_combined_countries$Pr_7
Data_combined_countries$mix_8 <- Data_combined_countries$mx*Data_combined_countries$Pr_8
Data_combined_countries$mix_9 <- Data_combined_countries$mx*Data_combined_countries$Pr_9
Data_combined_countries$mix_10 <- Data_combined_countries$mx*Data_combined_countries$Pr_10
Data_combined_countries$mix_11 <- Data_combined_countries$mx*Data_combined_countries$Pr_11


Mx_CoD_countries <- Data_combined_countries

table(Mx_CoD_countries$Country.name, Mx_CoD_countries$Periods)
# ---------------------------------------------------------------------------- #
#     3. Save the data
# ---------------------------------------------------------------------------- #

save(Mx_CoD_countries, file = "Data/Mx_CoD_countries.RData")



