# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Classification of causes of death, version 11 categories
# Dataset: Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# Content:
#   0. Working directory, package, and functions
#   1. Read and preparing data
#   2. Causes of death classification
#   3. Prepare data for smoothing

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")
# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")

# ---------------------------------------------------------------------------- #
#     1. Read and preparing data
# ---------------------------------------------------------------------------- #

# Belarus
Belarus <- read.csv("Data/HCDD/BLR_d_interm_idr.csv", header = T, sep = ",")

# Estonia
Estonia <- read.csv("Data/HCDD/EST_d_interm_idr.csv", header = T, sep = ",")

# Latvia
Latvia <- read.csv("Data/HCDD/LVA_d_interm_idr.csv", header = T, sep = ",")

# Lithuania
Lithuania <- read.csv("Data/HCDD/LTU_d_interm_idr.csv", header = T, sep = ",")

# Russia
Russia <- read.csv("Data/HCDD/RUS_d_interm_idr.csv", header = T, sep = ",")

# Ukraine
Ukraine <- read.csv("Data/HCDD/UKR_d_interm_idr.csv", header = T, sep = ",")


# Combine all countries
HCDD_Countries <- rbind(Belarus, Estonia, Latvia, Lithuania, Russia, Ukraine)

table(HCDD_Countries$year, HCDD_Countries$country)


# ---------------------------------------------------------------------------- #
#     2. Causes of death classification
# ---------------------------------------------------------------------------- #

# ---------------------------------------
#       ICD 10 classification 
# ---------------------------------------

# I took this comment from Jose Manuel Code from the Article on Denmark and modified

# This version of code is Classification of 11 causes of deaths
# 1) Smoking-related cancers
# 2) Sex-specif cancer
# 3) Other Cancer
# 4) Ischemic heart diseases and Stroke
# 5) Other circulatory diseases
# 6) Mental and nervous system
# 7) Alcohol-attributable mortality
# 8) Infectious (respiratory) diseases
# 9) Non-infectious respiratory diseases
# 10) External causes of death
# 11) Rest of causes of death

table(HCDD_Countries$year)

HCDD_Countries_2000_2015 <- HCDD_Countries %>% 
  filter(year>=2000 & year<=2015) %>% 
  filter(cause!=0) %>% 
  mutate(Cat = case_when(cause==10 | cause==11 |
                           cause==12 | cause==13 | 
                           cause==14 | cause==16 |
                           cause==18 | cause==19 | 
                           cause==27 | cause==28 ~ 1, # Smoking related cancer
                         cause==21 | cause==22 |
                           cause==23 | cause==24 | 
                           cause==25 ~ 2, # Sex specific cancer
                         cause==15 | cause==17 |
                           cause==20 | cause==26 | 
                           cause==29 | cause==30 |
                           cause==31 | cause==32 | 
                           cause==33 ~ 3, # Other type of cancers
                         cause==51 | cause==52 |
                           cause==58 | cause==59 | 
                           cause==60 | cause==61 ~ 4, # Rest of circulatory diseases
                         cause==48 | cause==49 |
                           cause==50 | cause==53 | 
                           cause==54 | cause==56 |
                           cause==57 | cause==62 | 
                           cause==63 ~ 5, # Other type of cancers
                         cause==39 | cause==41 |
                           cause==42 | cause==43 | 
                           cause==44 | cause==45 | 
                           cause==46 | cause==47 ~ 6, # Mental and Nervous system
                         cause==40 | cause==96 | 
                           cause==76  ~ 7, # Alcohol attributable mortality
                         cause==1 | cause==2 | cause==3 | cause==4 | 
                           cause==5 | cause==6 | cause==7 | cause==8 | 
                           cause==9 | cause==66 | 
                           cause==64 | cause==65  ~ 8, # Infectious respiratory disesas
                         cause==68 | cause==69 | 
                           cause==70 | cause==71 |  cause==72  ~ 9, # Non infectious respiratory diseases
                         cause==92 | cause==93 |
                           cause==94 | cause==95 | 
                           cause==96 | cause==97 |
                           cause==98 | cause==99 | 
                           cause==100 | cause==101 | 
                           cause==102 | cause==103  ~ 10, # External causes of death
                         cause==34 | cause==38 |
                           cause==37 | cause==35 | 
                           cause==36 | cause==47 |
                           cause==73 | cause==74 | cause==75 | cause==76 | 
                           cause==77 | cause==78 | cause==79 | cause==80 |
                           cause==81 | cause==82 | cause==83 | cause==84 | 
                           cause==85 | cause==86 | cause==87 | cause==88 |
                           cause==89 | cause==90 |
                           cause==91 ~ 11)) %>% # Rest of causes 
  mutate(Country.name = case_when(country=="BLR" ~ "Belarus",
                                  country=="EST" ~ "Estonia",
                                  country=="LVA" ~ "Latvia",
                                  country=="LTU" ~ "Lithuania",
                                  country=="RUS" ~ "Russia",
                                  country=="UKR" ~ "Ukraine"),
         ICD = 10)  


colnames(HCDD_Countries_2000_2015)[1] <- "Country"
colnames(HCDD_Countries_2000_2015)[2] <- "Year"
colnames(HCDD_Countries_2000_2015)[3] <- "Sex"

HCDD_Countries_2000_2015 <- data.table(HCDD_Countries_2000_2015)
# ----------------------------------------------------------------- #
#   3. Prepare data for smoothing
# ----------------------------------------------------------------- #

HCDD_Countries_2000_2015_1  <- HCDD_Countries_2000_2015[,c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat',
                                                           'd0', 'd1', 'd5', 'd10',  'd15','d20',  'd25', 'd30',
                                                           'd35','d40',  'd45', 'd50',  'd55','d60',  'd65', 'd70',
                                                           'd75','d80')]
colnames(HCDD_Countries_2000_2015_1)  <- c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat',as.character(c(0,1,seq(5,80,5))))
HCDD_Countries_2000_2015_2            <- HCDD_Countries_2000_2015_1[with(HCDD_Countries_2000_2015_1,order(Country,Country.name,Sex,Year,Cat))]


# Melt the ages
HCDD_Countries_2000_2015_melt  <- melt(HCDD_Countries_2000_2015_2, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'),
                                       variable.name = 'Age',value.name = 'Dx')
HCDD_Countries_2000_2015_melt  <- data.table(HCDD_Countries_2000_2015_melt)


# Declare as factor
HCDD_Countries_2000_2015_melt$Country <- as.factor(HCDD_Countries_2000_2015_melt$Country)
HCDD_Countries_2000_2015_melt$Country.name <- as.factor(HCDD_Countries_2000_2015_melt$Country.name)
HCDD_Countries_2000_2015_melt$ICD <- as.factor(HCDD_Countries_2000_2015_melt$ICD)
HCDD_Countries_2000_2015_melt$Year <- as.factor(HCDD_Countries_2000_2015_melt$Year)
HCDD_Countries_2000_2015_melt$Sex <- as.factor(HCDD_Countries_2000_2015_melt$Sex)
HCDD_Countries_2000_2015_melt$Age <- as.factor(as.character(HCDD_Countries_2000_2015_melt$Age))
HCDD_Countries_2000_2015_melt$Cat <- as.factor(HCDD_Countries_2000_2015_melt$Cat)

HCDD_Countries_2000_2015_melt$Dx <- as.numeric(as.character(HCDD_Countries_2000_2015_melt$Dx))

### Get total deaths by age, sex, category, year.
HCDD_Countries_2000_2015_melt  <- HCDD_Countries_2000_2015_melt[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]
HCDD_Countries_2000_2015_melt$Dx[is.na(HCDD_Countries_2000_2015_melt$Dx)] <- 0


### get proportions of causes of death by age 
HCDD_Countries_2000_2015_melt  <- HCDD_Countries_2000_2015_melt[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]



# ---------------------------
#  Process by single country
# ---------------------------

#reduce to variables needed (age < 85), until Deaths22 

#save(DT_COD.melt, file = "Data/DT_COD_Melt.RData")

# ---- Save only with 2000-2015
save(HCDD_Countries_2000_2015_melt, file = "Data/HCDD_Countries_2000_2015_melt.RData")


