# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Combine smoothed deaths with WHO and HCD Europe
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Estimate the proportion of causes of death by age, periods, sex, country
#   3. Aggregate data from HMD by periods and compute life tables
#   4. Combine datasets

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")
# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")


# ---------------------------------------------------------------------------- #
#     1. Open data sets
# ---------------------------------------------------------------------------- #

# Smoothed death counts from WHO and HCD
get(load("Data/Smoothed_final_Europe.RData"))

# HMD data by regions
get(load("Data/HMD europe/Europe.RData"))

# ---------------------------------------------------------------------------- #
#     2. Estimate the proportion of causes of death by age groups and regions
# ---------------------------------------------------------------------------- #

# --- We need to estimate the proportions of Dx by periods of study
Smoothed_Data_WHO_Europe$Year <- as.numeric(as.character(Smoothed_Data_WHO_Europe$Year))

Smoothed_Data_WHO_Europe <- Smoothed_Data_WHO_Europe %>% 
  mutate(Periods = case_when(Year>=2000 & Year<=2004 ~ 2000,
                             Year>=2005 & Year<=2009 ~ 2005,
                             Year>=2010 & Year<=2015 ~ 2010),
         Region = 6) 

Smoothed_Data_WHO_Europe$Dx[is.na(Smoothed_Data_WHO_Europe$Dx)] <- 0


# Get total deaths by age, sex, category, year, regions.
SAC_Sum  <- Smoothed_Data_WHO_Europe[, list(Dx=sum(Dx)), by =  list(Region,Periods,Sex,Age,Cat)]


SAC_Sum_prop <- SAC_Sum[, Dx.p := Dx/sum(Dx), by = list(Region,Periods,Sex,Age)]

# Transfor the data of WHO where each column is  a proportion of Dx.prop
WHO_cast <- dcast(SAC_Sum_prop, Periods+Sex+Region+Age ~ Cat,value.var = 'Dx.p')
head(WHO_cast)

# Declare as factor in order to merge
WHO_cast$Periods <- as.numeric(as.character(WHO_cast$Periods))
WHO_cast$Region<-factor(WHO_cast$Region)
WHO_cast$Age<-factor(WHO_cast$Age)

# ------------------------------------------------------------------- #
#   3. Aggregate data from HMD by periods and compute life tables
# ------------------------------------------------------------------- #

# --- From the Human Mortality Database

# Create periods
HMD_Europe_preperiods <- Europe %>% 
  mutate(Periods = case_when(Year>=2000 & Year<=2004 ~ 2000,
                             Year>=2005 & Year<=2009 ~ 2005,
                             Year>=2010 & Year<=2015 ~ 2010),
         Region = 6) 

# We transform to numeric the varaible Age from HMD
HMD_Europe_preperiods$Age <- as.numeric(as.character(HMD_Europe_preperiods$Age))
HMD_Europe_preperiods$Age[is.na(HMD_Europe_preperiods$Age)] <- 110

# We create a new age variable to avoid problems
HMD_Europe_preperiods$Age2 <-as.numeric(as.character(HMD_Europe_preperiods$Age))
HMD_Europe_preperiods$Age2[is.na(HMD_Europe_preperiods$Age2)] <- 110

# We order the dataframe by period, country, sex and age
HMD_Europe_preperiods <- HMD_Europe_preperiods[
  order(HMD_Europe_preperiods$Periods,
        HMD_Europe_preperiods$Region,
        HMD_Europe_preperiods$Sex,
        HMD_Europe_preperiods$Age2),]


# Transform to data.table format
HMD_Europe_preperiods <- data.table(HMD_Europe_preperiods)

# Aggregate data by periods
HMD_Europe_period <- HMD_Europe_preperiods[, list(PSY=sum(Exposure),
                                                    Dx =sum(Deaths)), by = list(Periods, Region, Sex, Age2)]

# New varaible sex
HMD_Europe_period$Sex[HMD_Europe_period$Sex=="Male"]<- "m"
HMD_Europe_period$Sex[HMD_Europe_period$Sex=="Female"]<- "f"

# We compute the mortality rate
HMD_Europe_period$mx <- HMD_Europe_period$Dx/HMD_Europe_period$PSY
HMD_Europe_period$mx[HMD_Europe_period$mx>=1] <- 1
HMD_Europe_period$mx[is.na(HMD_Europe_period$mx)] <- 0


# We compute Life tables by period, country, and Sex
HMD_Europe_period_LT <- HMD_Europe_period[,cbind(SLT_1(nmx=mx,
                                                         sex = Sex,
                                                         age = Age2)),
                                            by=list(Periods,Region, Sex)]

# We order the life tables by period, country, sex and age
HMD_Europe_period_LT <- HMD_Europe_period_LT[
  order(HMD_Europe_period_LT$Periods,
        HMD_Europe_period_LT$Region,
        HMD_Europe_period_LT$Sex,
        HMD_Europe_period_LT$Age),]

# Re name country variables name
HMD_LT_data_Europe <- HMD_Europe_period_LT %>% 
  dplyr::select(Periods, Sex, Region, 
                Age, nmx, nqx, nax, lx, ndx, Lx, Tx, ex) %>% 
  rename(mx = nmx,
         qx = nqx,
         ax = nax,
         dx = ndx) 


# Declare as factor in order to merge
HMD_LT_data_Europe$Periods <-as.numeric(as.character(HMD_LT_data_Europe$Periods))
HMD_LT_data_Europe$Sex<-factor(HMD_LT_data_Europe$Sex)
HMD_LT_data_Europe$Region<-factor(HMD_LT_data_Europe$Region)
HMD_LT_data_Europe$Age<-factor(HMD_LT_data_Europe$Age)

table(HMD_LT_data_Europe$Region, HMD_LT_data_Europe$Period)


# ------------------------------------------------------------------- #
#   4. Combine datasets
# ------------------------------------------------------------------- #

table(HMD_LT_data_Europe$Region, HMD_LT_data_Europe$Periods)
table(WHO_cast$Region, WHO_cast$Periods)

table(HMD_LT_data_Europe$Age, HMD_LT_data_Europe$Region)


# ---- Now we merge both dataset

Data_combined_Europe <- merge(HMD_LT_data_Europe,WHO_cast, 
                               by = c("Periods","Sex","Region","Age"))


table(Data_combined_Europe$Region, Data_combined_Europe$Periods)

#Data_combined_countries <- inner_join(HMD_data,WHO_cast_2)
#Data_combined_countries <- left_join(HMD_data,WHO_cast, by = c("Year","Sex","Country.name","Age"))

# We change the name of the proportion to the related cause of death
colnames(Data_combined_Europe)[13] <-"Pr_1"
colnames(Data_combined_Europe)[14] <-"Pr_2"
colnames(Data_combined_Europe)[15] <-"Pr_3"
colnames(Data_combined_Europe)[16] <-"Pr_4"
colnames(Data_combined_Europe)[17] <-"Pr_5"
colnames(Data_combined_Europe)[18] <-"Pr_6"
colnames(Data_combined_Europe)[19] <-"Pr_7"
colnames(Data_combined_Europe)[20] <-"Pr_8"
colnames(Data_combined_Europe)[21] <-"Pr_9"
colnames(Data_combined_Europe)[22] <-"Pr_10"
colnames(Data_combined_Europe)[23] <-"Pr_11"


# Causa de muerte que representan cada una
#"Pr_1" <-"Pr.C_S"
#"Pr_2"  <-"Pr.C_Sex_specific"
#"Pr_3"  <-"Pr.Rest_cancer"
#"Pr_4"  <-"Pr.IHD_Stroke"
#"Pr_5"  <-"Pr.Other_CD"
#"Pr_6"  <-"Pr.Mental_Nervous"
#"Pr_7"  <-"Pr.Alcohol"
#"Pr_8"  <-"Pr.Infectious"
#"Pr_9"  <-"Pr.Non_Infectious"
#"Pr_10"  <-"Pr.External"
#"Pr_11"  <-"Pr.ONINR"

# This version of code is Classifcation of 11 causes of deaths
# 1) Cancer sensitive to smoking
# 2) Sex-specif cancer
# 3) Other Cancer
# 4) Ischemic heart diseases and Stroke
# 5) Other circulatory diseases
# 6) Mental Disorder + Nervous system
# 7) Alcohol attributable mortality
# 8) Infectious (respiratory diseases)
# 9) Non infectious respiratory disases
# 10) External causes of death
# 11) Rest of causes of death

Data_combined_Europe$Pr_1[is.na(Data_combined_Europe$Pr_1)] <- 0
Data_combined_Europe$Pr_2[is.na(Data_combined_Europe$Pr_2)] <- 0
Data_combined_Europe$Pr_3[is.na(Data_combined_Europe$Pr_3)] <- 0
Data_combined_Europe$Pr_4[is.na(Data_combined_Europe$Pr_4)] <- 0
Data_combined_Europe$Pr_5[is.na(Data_combined_Europe$Pr_5)] <- 0
Data_combined_Europe$Pr_6[is.na(Data_combined_Europe$Pr_6)] <- 0
Data_combined_Europe$Pr_7[is.na(Data_combined_Europe$Pr_7)] <- 0
Data_combined_Europe$Pr_8[is.na(Data_combined_Europe$Pr_8)] <- 0
Data_combined_Europe$Pr_9[is.na(Data_combined_Europe$Pr_9)] <- 0
Data_combined_Europe$Pr_10[is.na(Data_combined_Europe$Pr_10)] <- 0
Data_combined_Europe$Pr_11[is.na(Data_combined_Europe$Pr_11)] <- 0

# Save final data
save(Data_combined_Europe, file = "Data/Data_combined_Europe_period.RData")

# We can check that everything is fine, estimation the sum of proportion
Data_combined_Europe$Sum <- Data_combined_Europe$Pr_1 + Data_combined_Europe$Pr_2 + Data_combined_Europe$Pr_3 +
  Data_combined_Europe$Pr_4 + Data_combined_Europe$Pr_5 + Data_combined_Europe$Pr_6 +
  Data_combined_Europe$Pr_7 + Data_combined_Europe$Pr_8 + Data_combined_Europe$Pr_9 +  Data_combined_Europe$Pr_10 + 
  Data_combined_Europe$Pr_11

mean(Data_combined_Europe$Sum)
# if it is one is 1 everything is ok