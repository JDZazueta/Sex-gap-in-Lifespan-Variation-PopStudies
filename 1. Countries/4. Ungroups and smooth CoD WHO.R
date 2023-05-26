# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Smoothing causes of deaths
# Dataset: WHO Mortality database
#          Human Causes of Death (HCD)
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# Content:
#   0. Working directory, package, and functions
#   1. Read and preparing data
#   2. Smoothing
#      2.1 WHO data
#      2.2 HCD
#   3. Create and save data by countries, regions and Europe

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")
# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")

# ------------------------------------------------------------------- #
#     1. Read and preparing data
# ------------------------------------------------------------------- #

# -----------------------------
#   WHO all countries
# -----------------------------

get(load("Data/DT_COD_Melt_00.RData"))

# -----------------------------
#  Human Causes of Death
# -----------------------------

get(load("Data/HCDD_Countries_2000_2015_melt.RData"))


# ------------------------------------------------------------------- #
#     2. Smoothing
# ------------------------------------------------------------------- #

# ----------------
#  WHO Database
# ----------------

DT_COD.melt_00 <- data.table(DT_COD.melt_00)
DT_COD.melt_00$Age <- as.numeric(as.character(DT_COD.melt_00$Age))
unique(DT_COD.melt_00$Country.name)
unique(DT_COD.melt_00$Sex)
unique(DT_COD.melt_00$Cat)
unique(DT_COD.melt_00$ICD)

Smoothed_Data_WHO_countries <- DT_COD.melt_00[,list(Dx = pclm(x = Age,
                                       y = Dx,
                                       nlast = 31,
                                       offset = NULL)$fitted,
                                       Age = 0:110),
                          by = list(Country,Country.name, ICD,Year,Sex,Cat)]


# ----------------
#  HMCC Database
# ----------------


HCDD_Countries_2000_2015_melt <- data.table(HCDD_Countries_2000_2015_melt)
HCDD_Countries_2000_2015_melt$Age <- as.numeric(as.character(HCDD_Countries_2000_2015_melt$Age))
unique(HCDD_Countries_2000_2015_melt$Country.name)
unique(HCDD_Countries_2000_2015_melt$Sex)
unique(HCDD_Countries_2000_2015_melt$Cat)
unique(HCDD_Countries_2000_2015_melt$ICD)


Smoothed_Data_HCD_countries <- HCDD_Countries_2000_2015_melt[,list(Dx = pclm(x = Age,
                                                                y = Dx,
                                                              nlast = 31,
                                                              offset = NULL)$fitted,
                                                    Age = 0:110),
                                              by = list(Country,Country.name, ICD,Year,Sex,Cat)]



# ------------------------------------------------------------------- #
#     3. Save data by countries and data source
# ------------------------------------------------------------------- #


# Save by country
# WHO
save(Smoothed_Data_WHO_countries, file = "Data/Smoothed_Data_WHO_countries.RData")
# HCD
save(Smoothed_Data_HCD_countries, file = "Data/Smoothed_Data_HCD_countries.RData")

# ------------------------------------------------------------------- #
#     4. Combine FSR from HCD to WHO
# ------------------------------------------------------------------- #

get(load("Data/Smoothed_Data_WHO_countries.RData"))

# We remove the FSR countries from WHO databased
Smoothed_Data_WHO_countries_no_FSR <- Smoothed_Data_WHO_countries %>% 
  filter(Country.name=="Norway" | Country.name=="Sweden" |
           Country.name=="Denmark" | Country.name=="Finland" |
           Country.name=="Austria" | Country.name=="Belgium" |
           Country.name=="Switzerland" | Country.name=="Germany" |
           Country.name=="France" | Country.name=="United Kingdom" |  
           Country.name=="Ireland" | Country.name=="Netherlands"  |
           Country.name=="Greece" | Country.name=="Italy" |
           Country.name=="Portugal" | Country.name=="Spain" |
           Country.name=="Bulgaria" | Country.name=="Czech Republic" |
           Country.name=="Slovakia" | Country.name=="Slovenia" |
           Country.name=="Hungary" | Country.name=="Poland")

# Smoothed data from HCD
get(load("Data/Smoothed_Data_HCD_countries.RData"))

Smoothed_Data_HCD_countries_adj <- Smoothed_Data_HCD_countries %>% 
  filter(Sex==1 | Sex==2) %>% 
  mutate(sex_2 = case_when(Sex==1 ~ "m",
                           Sex==2 ~ "f")) %>% 
  dplyr::select(-c(Sex)) %>% 
  rename(Sex = sex_2)

# We aggregate the FSR from HCD to the WHO database without FSR countries
Smoothed_Data_countries <- rbind(Smoothed_Data_WHO_countries_no_FSR,
                                 Smoothed_Data_HCD_countries_adj)
# ------------------------------------------------------------------- #
#     5. Create & save data by countries, regions and Europe
# ------------------------------------------------------------------- #

# Countries
save(Smoothed_Data_countries, file = "Data/Smoothed_final_countries.RData")

# Regions
Smoothed_Data_WHO_regionsprep <- Smoothed_Data_WHO_countries %>% 
  mutate(Region = case_when(Country.name=="Norway" | Country.name=="Sweden" |
                            Country.name=="Denmark" | Country.name=="Finland" ~ 1,
                            Country.name=="Austria" | Country.name=="Belgium" |
                            Country.name=="Switzerland" | Country.name=="Germany" |
                            Country.name=="France" | Country.name=="United Kingdom" |  
                            Country.name=="Ireland" | Country.name=="Netherlands"  ~ 2,
                            Country.name=="Greece" | Country.name=="Italy" |
                            Country.name=="Portugal" | Country.name=="Spain" ~ 3,
                            Country.name=="Bulgaria" | Country.name=="Czech Republic" |
                            Country.name=="Slovakia" | Country.name=="Slovenia" |
                            Country.name=="Hungary" | Country.name=="Poland" ~ 4,
                            Country.name=="Belarus" | Country.name=="Estonia" |
                            Country.name=="Latvia" | Country.name=="Lithuania" |
                            Country.name=="Russia Federation" | Country.name=="Ukarine" ~ 5))

# Transform to data.table format
Smoothed_Data_WHO_regionsprep <- data.table(Smoothed_Data_WHO_regionsprep) 

# Aggregate smoothed deaths by regions
Smoothed_Data_WHO_regions <- Smoothed_Data_WHO_regionsprep[,list(Dx=sum(Dx)),
                                                           by=list(Region, ICD,Year,Sex,Age,Cat)]

# Save by regions
save(Smoothed_Data_WHO_regions, file = "Data/Smoothed_final_regions.RData")

# Europe
# Aggregate smoothed deaths by Europe
Smoothed_Data_WHO_Europe <- Smoothed_Data_WHO_regionsprep[,list(Dx=sum(Dx)),
                                                          by=list(ICD,Year,Sex,Age,Cat)]

# Save by Europe as a whole
save(Smoothed_Data_WHO_Europe, file = "Data/Smoothed_final_europe.RData")





