# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Prepare data
# Dataset: Human Mortality Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, package, and functions
#   1. Read and prepare exposure data from HMD by regions
#      1.1 Exposure 1x1 format by country and regions
#      1.2 Combine regions
#      1.3 Combine Europe
#   2. Read and prepare deaths data from HMD by regions
#      2.1 Deaths 1x1 format by country and regions
#      2.2 Combine regions
#      2.3 Combine Europe
#   3. Combine exposure and death counts
#      3.1 Countries
#      3.2 Regions
#      3.3 Europe

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")

# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")


# Generate a country vector name to be compatible with HMD and WHO database,
# in the future

# Order acording to HMD Number
Country.name.vec <- c("Austria","Belarus", "Belgium","Bulgaria",
                      "Czech Republic", "Denmark","Estonia","Finland",
                      "France","Germany","Greece","Hungary",
                      "Ireland", "Italy","Latvia","Lithuania",
                      "Netherlands", "Norway", "Poland","Portugal",
                      "Russia Federation", "Slovakia", "Slovenia",  "Spain",
                      "Sweden","Switzerland", "Ukraine", "United Kingdom")


# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(4010,4018,4020,4030,
                      4045,4050,4055,4070,
                      4080,4085,4140,4150,
                      4170,4180,4186,4188,
                      4210,4220,4230,4240,
                      4272,4274,4276,4280,
                      4290,4300,4303,4308)



# ------------------------------------------------------------------- #
#     1. Read and prepare exposure data from HMD by regions
# ------------------------------------------------------------------- #

# ------------------------------------------------------------------- #
#    1.1 Exposure 1x1 format by country and regions
# ------------------------------------------------------------------- #

########################
# Nordic Euroepan countries
########################

# Denmark
Denmark_exposure <- read.csv("Data/HMD_Exposures_1x1/DNK.Exposures_1x1.txt",
                             skip = 2, sep = "")
Denmark_exposure$Country <- "DNK"
save(Denmark_exposure,file="Data/HMD countries/Denmark_exposure.RData")

# Sweden
Sweden_exposure <- read.csv("Data/HMD_Exposures_1x1/SWE.Exposures_1x1.txt",
                               skip = 2, sep = "")
Sweden_exposure$Country <- "SWE"
save(Sweden_exposure,file="Data/HMD countries/Sweden_exposure.RData")

# Norway
Norway_exposure <- read.csv("Data/HMD_Exposures_1x1/NOR.Exposures_1x1.txt",
                            skip = 2, sep = "")
Norway_exposure$Country <- "NOR"
save(Norway_exposure,file="Data/HMD countries/Norway_exposure.RData")

# Finland
Finland_exposure <- read.csv("Data/HMD_Exposures_1x1/FIN.Exposures_1x1.txt",
                            skip = 2, sep = "")
Finland_exposure$Country <- "FIN"
save(Finland_exposure,file="Data/HMD countries/Finland_exposure.RData")


# ----------------------------
# --- Prepare the region
# ----------------------------

Nordic_region_prep <- rbind(Denmark_exposure, Sweden_exposure,
                            Norway_exposure, Finland_exposure)

# Prepare region data
Nordic_region_prep <- data.table(Nordic_region_prep)
Nordic_region_prep <- Nordic_region_prep[Year>=2000 & Year<=2015]

Nordic_region_prep$Region <- 1

Nordic_region_country <- Nordic_region_prep[,-c("Total")]
Nordic_region_country_exp <- Nordic_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Pop")

Nordic_region_country_exp <- data.table(Nordic_region_country_exp)

Nordic_region_Exp <- Nordic_region_country_exp[, list(Population=sum(Pop)), by =  list(Year,Region,Sex,Age)]

save(Nordic_region_Exp,file="Data/HMD regions/Nordic_region_Exp.RData")



########################
# Western European Countries
########################

# Austria
Austria_exposure  <- read.csv("Data/HMD_Exposures_1x1/AUS.Exposures_1x1.txt",
                              skip = 2, sep = "")
Austria_exposure$Country <- "AUS"
save(Austria_exposure,file="Data/HMD countries/Austria_exposure.RData")

# Belgium
Belgium_exposure  <- read.csv("Data/HMD_Exposures_1x1/BEL.Exposures_1x1.txt",
                              skip = 2, sep = "")
Belgium_exposure$Country <- "BEL"
save(Belgium_exposure,file="Data/HMD countries/Belgium_exposure.RData")

# All Germany
Germany_exposure  <- read.csv("Data/HMD_Exposures_1x1/DEUTNP.Exposures_1x1.txt",
                              skip = 2, sep = "")
Germany_exposure$Country <- "DEUTNP"
Germany_exposure <- Germany_exposure %>% 
  filter(Year>=2000)
save(Germany_exposure,file="Data/HMD countries/Germany_exposure.RData")


# France
France_exposure <- read.csv("Data/HMD_Exposures_1x1/FRATNP.Exposures_1x1.txt",
                            skip = 2, sep = "")
France_exposure$Country <- "FRATNP"
save(France_exposure,file="Data/HMD countries/France_exposure.RData")

# Ireland
Ireland_exposure <- read.csv("Data/HMD_Exposures_1x1/IRL.Exposures_1x1.txt",
                             skip = 2, sep = "")
Ireland_exposure$Country <- "IRL"
save(Ireland_exposure,file="Data/HMD countries/Ireland_exposure.RData")

# Netherlands
Netherlands_exposure <- read.csv("Data/HMD_Exposures_1x1/NLD.Exposures_1x1.txt",
                                 skip = 2, sep = "")
Netherlands_exposure$Country <- "NLD"
save(Netherlands_exposure,file="Data/HMD countries/Netherlands_exposure.RData")

# Switzerland
Switzerland_exposure <- read.csv("Data/HMD_Exposures_1x1/CHE.Exposures_1x1.txt",
                                 skip = 2, sep = "")
Switzerland_exposure$Country <- "CHE"
save(Switzerland_exposure,file="Data/HMD countries/Switzerland_exposure.RData")

# United Kingdom
UK_exposure <- read.csv("Data/HMD_Exposures_1x1/GBR_NP.Exposures_1x1.txt",
                        skip = 2, sep = "")
UK_exposure$Country <- "UK"
save(UK_exposure,file="Data/HMD countries/UK_exposure.RData")

# ----------------------------
# --- Prepare the region
# ----------------------------

Western_region_prep <- rbind(Austria_exposure, Belgium_exposure,
                             Germany_exposure, France_exposure,
                             Ireland_exposure, Netherlands_exposure,
                             Switzerland_exposure, UK_exposure)

# Prepare region data
Western_region_prep <- data.table(Western_region_prep)
Western_region_prep <- Western_region_prep[Year>=2000 & Year<=2015]

# We create the number for the region
Western_region_prep$Region <- 2

Western_region_country <- Western_region_prep[,-c("Total")]
Western_region_country_exp <- Western_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Pop")

Western_region_country_exp <- data.table(Western_region_country_exp)
Western_region_country_exp$Pop <- as.numeric(Western_region_country_exp$Pop)
Western_region_Exp <- Western_region_country_exp[, list(Population = sum(Pop)),
                                             by =  list(Year,Age,Region,Sex)]

save(Western_region_Exp,file="Data/HMD regions/Western_region_Exp.RData")

########################
# Southern European countries
########################

# Spain
Spain_exposure <- read.csv("Data/HMD_Exposures_1x1/ESP.Exposures_1x1.txt",
                           skip = 2, sep = "")
Spain_exposure$Country <- "ESP"
save(Spain_exposure,file="Data/HMD countries/Spain_exposure.RData")

# Italy
Italy_exposure <- read.csv("Data/HMD_Exposures_1x1/ITA.Exposures_1x1.txt",
                           skip = 2, sep = "")
Italy_exposure$Country <- "ITA"
save(Italy_exposure,file="Data/HMD countries/Italy_exposure.RData")

# Portugal
Portugal_exposure <- read.csv("Data/HMD_Exposures_1x1/PRT.Exposures_1x1.txt",
                              skip = 2, sep = "")
Portugal_exposure$Country <- "PRT"
save(Portugal_exposure,file="Data/HMD countries/Portugal_exposure.RData")

# Greece
Greece_exposure <-  read.csv("Data/HMD_Exposures_1x1/GRC.Exposures_1x1.txt",
                         skip = 2, sep = "")
Greece_exposure$Country <- "GRC"
save(Greece_exposure,file="Data/HMD countries/Greece_exposure.RData")

# ----------------------------
# --- Prepare the region
# ----------------------------

Southern_region_prep <- rbind(Spain_exposure, Italy_exposure,
                              Portugal_exposure, Greece_exposure)

# Prepare region data
Southern_region_prep <- data.table(Southern_region_prep)
Southern_region_prep <- Southern_region_prep[Year>=2000 & Year<=2015]

Southern_region_prep$Region <- 3

Southern_region_country <- Southern_region_prep[,-c("Total")]
Southern_region_country_exp <- Southern_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Pop")

Southern_region_country_exp <- data.table(Southern_region_country_exp)

Southern_region_Exp <- Southern_region_country_exp[, list(Population=sum(Pop)), by =  list(Year,Region,Sex,Age)]

save(Southern_region_Exp,file="Data/HMD regions/Southern_region_Exp.RData")

########################
#  Central Eastern European countries
########################

# Bulgaria
Bulgaria_exposure <- read.csv("Data/HMD_Exposures_1x1/BGR.Exposures_1x1.txt",
                              skip = 2, sep = "")
Bulgaria_exposure$Country <- "BGR"
save(Bulgaria_exposure,file="Data/HMD countries/Bulgaria_exposure.RData")

# Czech Republic
CZRepublic_exposure <- read.csv("Data/HMD_Exposures_1x1/CZE.Exposures_1x1.txt",
                                skip = 2, sep = "")
CZRepublic_exposure$Country <- "CZE"
save(CZRepublic_exposure,file="Data/HMD countries/CZRepublic_exposure.RData")

# Hungary
Hungary_exposure <- read.csv("Data/HMD_Exposures_1x1/HUN.Exposures_1x1.txt",
                             skip = 2, sep = "")
Hungary_exposure$Country <- "HUN"
save(Hungary_exposure,file="Data/HMD countries/Hungary_exposure.RData")

# Poland
Poland_exposure <- read.csv("Data/HMD_Exposures_1x1/POL.Exposures_1x1.txt",
                            skip = 2, sep = "")
Poland_exposure$Country <- "POL"
save(Poland_exposure,file="Data/HMD countries/Poland_exposure.RData")

# Slovakia
Slovakia_exposure <- read.csv("Data/HMD_Exposures_1x1/SVK.Exposures_1x1.txt",
                              skip = 2, sep = "")
Slovakia_exposure$Country <- "SVK"
save(Slovakia_exposure,file="Data/HMD countries/Slovakia_exposure.RData")

# Slovenia
Slovania_exposure <- read.csv("Data/HMD_Exposures_1x1/SVN.Exposures_1x1.txt",
                              skip = 2, sep = "")
Slovania_exposure$Country <- "SVN"
save(Slovania_exposure,file="Data/HMD countries/Slovania_exposure.RData")


# ----------------------------
# --- Prepare the region
# ----------------------------

CEE_region_prep <- rbind(Bulgaria_exposure, CZRepublic_exposure,
                         Hungary_exposure, Poland_exposure,
                         Slovakia_exposure, Slovania_exposure)

# Prepare region data
CEE_region_prep <- data.table(CEE_region_prep)
CEE_region_prep <- CEE_region_prep[Year>=2000 & Year<=2015]

CEE_region_prep$Region <- 4

CEE_region_country <- CEE_region_prep[,-c("Total")]
CEE_region_country_exp <- CEE_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Pop")

CEE_region_country_exp <- data.table(CEE_region_country_exp)

CEE_region_Exp <- CEE_region_country_exp[, list(Population=sum(Pop)), by =  list(Year,Region,Sex,Age)]

save(CEE_region_Exp,file="Data/HMD regions/CEE_region_Exp.RData")


########################
#  Former Soviet Republics countries
########################

# Belaruse
Belaruse_exposure <- read.csv("Data/HMD_Exposures_1x1/BLR.Exposures_1x1.txt",
                              skip = 2, sep = "")
Belaruse_exposure$Country <- "BLR"
save(Belaruse_exposure,file="Data/HMD countries/Belaruse_exposure.RData")

# Estonia
Estonia_exposure <- read.csv("Data/HMD_Exposures_1x1/EST.Exposures_1x1.txt",
                             skip = 2, sep = "")
Estonia_exposure$Country <- "EST"
save(Estonia_exposure,file="Data/HMD countries/Estonia_exposure.RData")

# Latvia
Latvia_exposure <- read.csv("Data/HMD_Exposures_1x1/LVA.Exposures_1x1.txt",
                            skip = 2, sep = "")
Latvia_exposure$Country <- "LVA"
save(Latvia_exposure,file="Data/HMD countries/Latvia_exposure.RData")

# Lithuania
Lithuania_exposure <- read.csv("Data/HMD_Exposures_1x1/LTU.Exposures_1x1.txt",
                               skip = 2, sep = "")
Lithuania_exposure$Country <- "LTU"
save(Lithuania_exposure,file="Data/HMD countries/Lithuania_exposure.RData")

# Ukraine
Ukraine_exposure <- read.csv("Data/HMD_Exposures_1x1/UKR.Exposures_1x1.txt",
                             skip = 2, sep = "")
Ukraine_exposure$Country <- "UKR"
save(Ukraine_exposure,file="Data/HMD countries/Ukraine_exposure.RData")

# Russia
Russia_exposure <- read.csv("Data/HMD_Exposures_1x1/RUS.Exposures_1x1.txt",
                            skip = 2, sep = "")
Russia_exposure$Country <- "RUS"
save(Russia_exposure,file="Data/HMD countries/Russia_exposure.RData")

# ----------------------------
# --- Prepare the region
# ----------------------------

FSR_region_prep <- rbind(Belaruse_exposure, Estonia_exposure,
                         Latvia_exposure, Lithuania_exposure,
                         Ukraine_exposure, Russia_exposure)

# Prepare region data
FSR_region_prep <- data.table(FSR_region_prep)
FSR_region_prep <- FSR_region_prep[Year>=2000 & Year<=2015]

FSR_region_prep$Region <- 5

FSR_region_country <- FSR_region_prep[,-c("Total")]
FSR_region_country_exp <- FSR_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Pop")

FSR_region_country_exp <- data.table(FSR_region_country_exp)

FSR_region_Exp <- FSR_region_country_exp[, list(Population=sum(Pop)), by =  list(Year,Region,Sex,Age)]

save(FSR_region_Exp,file="Data/HMD regions/FSR_region_Exp.RData")


# ------------------------------------------------------------------- #
#    1.2 Combine regions
# ------------------------------------------------------------------- #

Exposure_regions <- rbind(Nordic_region_Exp, Western_region_Exp,
                          Southern_region_Exp, CEE_region_Exp, FSR_region_Exp)

table(Exposure_regions$Year, Exposure_regions$Region)

Exposure_regions <- data.table(Exposure_regions)

save(Exposure_regions,file="Data/HMD regions/Exposure_regions.RData")

# ------------------------------------------------------------------- #
#    1.3 Combine Europe
# ------------------------------------------------------------------- #


Exposure_europe <- Exposure_regions[, list(Population=sum(Population)), 
                                    by =  list(Year,Sex,Age)]

save(Exposure_europe,file="Data/HMD europe/Exposure_europe.RData")



# ------------------------------------------------------------------- #
#     2. Read and prepare deaths data from HMD by regions
# ------------------------------------------------------------------- #


# ------------------------------------------------------------------- #
#    2.1 Deaths 1x1 format by country and regions
# ------------------------------------------------------------------- #


########################
# Nordic Euroepan countries
########################

# Denmark
Denmark_Deaths <- read.csv("Data/HMD_Deaths_1x1/DNK.Deaths_1x1.txt",
                           skip = 2, sep = "")
Denmark_Deaths$Country <- "DNK"
save(Denmark_Deaths,file="Data/HMD countries/Denmark_Deaths.RData")

# Sweden
Sweden_Deaths <- read.csv("Data/HMD_Deaths_1x1/SWE.Deaths_1x1.txt",
                          skip = 2, sep = "")
Sweden_Deaths$Country <- "SWE"
save(Sweden_Deaths,file="Data/HMD countries/Sweden_Deaths.RData")

# Norway
Norway_Deaths <- read.csv("Data/HMD_Deaths_1x1/NOR.Deaths_1x1.txt",
                          skip = 2, sep = "")
Norway_Deaths$Country <- "NOR"
save(Norway_Deaths,file="Data/HMD countries/Norway_Deaths.RData")

# Finland
Finland_Deaths <- read.csv("Data/HMD_Deaths_1x1/FIN.Deaths_1x1.txt",
                           skip = 2, sep = "")
Finland_Deaths$Country <- "FIN"
save(Finland_Deaths,file="Data/HMD countries/Finland_Deaths.RData")


# ----------------------------
# --- Prepare the region
# ----------------------------

Nordic_region_prep <- rbind(Denmark_Deaths, Sweden_Deaths,
                            Norway_Deaths, Finland_Deaths)

# Prepare region data
Nordic_region_prep <- data.table(Nordic_region_prep)
Nordic_region_prep <- Nordic_region_prep[Year>=2000 & Year<=2015]

Nordic_region_prep$Region <- 1

Nordic_region_country <- Nordic_region_prep[,-c("Total")]
Nordic_region_country_Dx <- Nordic_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Dx")

Nordic_region_country_Dx <- data.table(Nordic_region_country_Dx)

Nordic_region_Dx <- Nordic_region_country_Dx[, list(Deaths=sum(Dx)), by =  list(Year,Region,Sex,Age)]

save(Nordic_region_Dx,file="Data/HMD regions/Nordic_region_Dx.RData")



########################
# Western European Countries
########################

# Austria
Austria_Deaths  <- read.csv("Data/HMD_Deaths_1x1/AUS.Deaths_1x1.txt",
                            skip = 2, sep = "")
Austria_Deaths$Country <- "AUS"
save(Austria_Deaths,file="Data/HMD countries/Austria_Deaths.RData")

# Belgium
Belgium_Deaths  <- read.csv("Data/HMD_Deaths_1x1/BEL.Deaths_1x1.txt",
                            skip = 2, sep = "")
Belgium_Deaths$Country <- "BEL"
save(Belgium_Deaths,file="Data/HMD countries/Belgium_Deaths.RData")

# All Germany
Germany_Deaths  <- read.csv("Data/HMD_Deaths_1x1/DEUTNP.Deaths_1x1.txt",
                            skip = 2, sep = "")
Germany_Deaths$Country <- "DEUTNP"
Germany_Deaths <- Germany_Deaths %>% 
  filter(Year>=2000)
save(Germany_Deaths,file="Data/HMD countries/Germany_Deaths.RData")


# France
France_Deaths <- read.csv("Data/HMD_Deaths_1x1/FRATNP.Deaths_1x1.txt",
                          skip = 2, sep = "")
France_Deaths$Country <- "FRATNP"
save(France_Deaths,file="Data/HMD countries/France_Deaths.RData")

# Ireland
Ireland_Deaths <- read.csv("Data/HMD_Deaths_1x1/IRL.Deaths_1x1.txt",
                           skip = 2, sep = "")
Ireland_Deaths$Country <- "IRL"
save(Ireland_Deaths,file="Data/HMD countries/Ireland_Deaths.RData")

# Netherlands
Netherlands_Deaths <- read.csv("Data/HMD_Deaths_1x1/NLD.Deaths_1x1.txt",
                               skip = 2, sep = "")
Netherlands_Deaths$Country <- "NLD"
save(Netherlands_Deaths,file="Data/HMD countries/Netherlands_Deaths.RData")

# Switzerland
Switzerland_Deaths <- read.csv("Data/HMD_Deaths_1x1/CHE.Deaths_1x1.txt",
                               skip = 2, sep = "")
Switzerland_Deaths$Country <- "CHE"
save(Switzerland_Deaths,file="Data/HMD countries/Switzerland_Deaths.RData")

# United Kingdom
UK_Deaths <- read.csv("Data/HMD_Deaths_1x1/GBR_NP.Deaths_1x1.txt",
                      skip = 2, sep = "")
UK_Deaths$Country <- "UK"
save(UK_Deaths,file="Data/HMD countries/UK_Deaths.RData")

# ----------------------------
# --- Prepare the region
# ----------------------------

Western_region_prep <- rbind(Austria_Deaths, Belgium_Deaths,
                             Germany_Deaths, France_Deaths,
                             Ireland_Deaths, Netherlands_Deaths,
                             Switzerland_Deaths, UK_Deaths)

# Prepare region data
Western_region_prep <- data.table(Western_region_prep)
Western_region_prep <- Western_region_prep[Year>=2000 & Year<=2015]

# We create the number for the region
Western_region_prep$Region <- 2

Western_region_country <- Western_region_prep[,-c("Total")]
Western_region_country_Dx <- Western_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Dx")

Western_region_country_Dx <- data.table(Western_region_country_Dx)
Western_region_country_Dx$Dx <- as.numeric(Western_region_country_Dx$Dx)
Western_region_Dx <- Western_region_country_Dx[, list(Deaths = sum(Dx)),
                                            by =  list(Year,Age,Region,Sex)]

save(Western_region_Dx,file="Data/HMD regions/Western_region_Dx.RData")

########################
# Southern European countries
########################

# Spain
Spain_Deaths <- read.csv("Data/HMD_Deaths_1x1/ESP.Deaths_1x1.txt",
                         skip = 2, sep = "")
Spain_Deaths$Country <- "ESP"
save(Spain_Deaths,file="Data/HMD countries/Spain_Deaths.RData")

# Italy
Italy_Deaths <- read.csv("Data/HMD_Deaths_1x1/ITA.Deaths_1x1.txt",
                         skip = 2, sep = "")
Italy_Deaths$Country <- "ITA"
save(Italy_Deaths,file="Data/HMD countries/Italy_Deaths.RData")

# Portugal
Portugal_Deaths <- read.csv("Data/HMD_Deaths_1x1/PRT.Deaths_1x1.txt",
                            skip = 2, sep = "")
Portugal_Deaths$Country <- "PRT"
save(Portugal_Deaths,file="Data/HMD countries/Portugal_Deaths.RData")

# Greece
Greece_Deaths <-  read.csv("Data/HMD_Deaths_1x1/GRC.Deaths_1x1.txt",
                           skip = 2, sep = "")
Greece_Deaths$Country <- "GRC"
save(Greece_Deaths,file="Data/HMD countries/Greece_Deaths.RData")

# ----------------------------
# --- Prepare the region
# ----------------------------

Southern_region_prep <- rbind(Spain_Deaths, Italy_Deaths,
                              Portugal_Deaths, Greece_Deaths)

# Prepare region data
Southern_region_prep <- data.table(Southern_region_prep)
Southern_region_prep <- Southern_region_prep[Year>=2000 & Year<=2015]

Southern_region_prep$Region <- 3

Southern_region_country <- Southern_region_prep[,-c("Total")]
Southern_region_country_Dx <- Southern_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Dx")

Southern_region_country_Dx <- data.table(Southern_region_country_Dx)

Southern_region_Dx <- Southern_region_country_Dx[, list(Deaths=sum(Dx)), by =  list(Year,Region,Sex,Age)]

save(Southern_region_Dx,file="Data/HMD regions/Southern_region_Dx.RData")

########################
#  Central Eastern European countries
########################

# Bulgaria
Bulgaria_Deaths <- read.csv("Data/HMD_Deaths_1x1/BGR.Deaths_1x1.txt",
                            skip = 2, sep = "")
Bulgaria_Deaths$Country <- "BGR"
save(Bulgaria_Deaths,file="Data/HMD countries/Bulgaria_Deaths.RData")

# Czech Republic
CZRepublic_Deaths <- read.csv("Data/HMD_Deaths_1x1/CZE.Deaths_1x1.txt",
                              skip = 2, sep = "")
CZRepublic_Deaths$Country <- "CZE"
save(CZRepublic_Deaths,file="Data/HMD countries/CZRepublic_Deaths.RData")

# Hungary
Hungary_Deaths <- read.csv("Data/HMD_Deaths_1x1/HUN.Deaths_1x1.txt",
                           skip = 2, sep = "")
Hungary_Deaths$Country <- "HUN"
save(Hungary_Deaths,file="Data/HMD countries/Hungary_Deaths.RData")

# Poland
Poland_Deaths <- read.csv("Data/HMD_Deaths_1x1/POL.Deaths_1x1.txt",
                          skip = 2, sep = "")
Poland_Deaths$Country <- "POL"
save(Poland_Deaths,file="Data/HMD countries/Poland_Deaths.RData")

# Slovakia
Slovakia_Deaths <- read.csv("Data/HMD_Deaths_1x1/SVK.Deaths_1x1.txt",
                            skip = 2, sep = "")
Slovakia_Deaths$Country <- "SVK"
save(Slovakia_Deaths,file="Data/HMD countries/Slovakia_Deaths.RData")

# Slovenia
Slovania_Deaths <- read.csv("Data/HMD_Deaths_1x1/SVN.Deaths_1x1.txt",
                            skip = 2, sep = "")
Slovania_Deaths$Country <- "SVN"
save(Slovania_Deaths,file="Data/HMD countries/Slovania_Deaths.RData")


# ----------------------------
# --- Prepare the region
# ----------------------------

CEE_region_prep <- rbind(Bulgaria_Deaths, CZRepublic_Deaths,
                         Hungary_Deaths, Poland_Deaths,
                         Slovakia_Deaths, Slovania_Deaths)

# Prepare region data
CEE_region_prep <- data.table(CEE_region_prep)
CEE_region_prep <- CEE_region_prep[Year>=2000 & Year<=2015]

CEE_region_prep$Region <- 4

CEE_region_country <- CEE_region_prep[,-c("Total")]
CEE_region_country_Dx <- CEE_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Dx")

CEE_region_country_Dx <- data.table(CEE_region_country_Dx)

CEE_region_Dx <- CEE_region_country_Dx[, list(Deaths=sum(Dx)), by =  list(Year,Region,Sex,Age)]

save(CEE_region_Dx,file="Data/HMD regions/CEE_region_Dx.RData")


########################
#  Former Soviet Republics countries
########################

# Belaruse
Belaruse_Deaths <- read.csv("Data/HMD_Deaths_1x1/BLR.Deaths_1x1.txt",
                            skip = 2, sep = "")
Belaruse_Deaths$Country <- "BLR"
save(Belaruse_Deaths,file="Data/HMD countries/Belaruse_Deaths.RData")

# Estonia
Estonia_Deaths <- read.csv("Data/HMD_Deaths_1x1/EST.Deaths_1x1.txt",
                           skip = 2, sep = "")
Estonia_Deaths$Country <- "EST"
save(Estonia_Deaths,file="Data/HMD countries/Estonia_Deaths.RData")

# Latvia
Latvia_Deaths <- read.csv("Data/HMD_Deaths_1x1/LVA.Deaths_1x1.txt",
                          skip = 2, sep = "")
Latvia_Deaths$Country <- "LVA"
save(Latvia_Deaths,file="Data/HMD countries/Latvia_Deaths.RData")

# Lithuania
Lithuania_Deaths <- read.csv("Data/HMD_Deaths_1x1/LTU.Deaths_1x1.txt",
                             skip = 2, sep = "")
Lithuania_Deaths$Country <- "LTU"
save(Lithuania_Deaths,file="Data/HMD countries/Lithuania_Deaths.RData")

# Ukraine
Ukraine_Deaths <- read.csv("Data/HMD_Deaths_1x1/UKR.Deaths_1x1.txt",
                           skip = 2, sep = "")
Ukraine_Deaths$Country <- "UKR"
save(Ukraine_Deaths,file="Data/HMD countries/Ukraine_Deaths.RData")

# Russia
Russia_Deaths <- read.csv("Data/HMD_Deaths_1x1/RUS.Deaths_1x1.txt",
                          skip = 2, sep = "")
Russia_Deaths$Country <- "RUS"
save(Russia_Deaths,file="Data/HMD countries/Russia_Deaths.RData")

# ----------------------------
# --- Prepare the region
# ----------------------------

FSR_region_prep <- rbind(Belaruse_Deaths, Estonia_Deaths,
                         Latvia_Deaths, Lithuania_Deaths,
                         Ukraine_Deaths, Russia_Deaths)

# Prepare region data
FSR_region_prep <- data.table(FSR_region_prep)
FSR_region_prep <- FSR_region_prep[Year>=2000 & Year<=2015]

FSR_region_prep$Region <- 5

FSR_region_country <- FSR_region_prep[,-c("Total")]
FSR_region_country_Dx <- FSR_region_country %>% 
  pivot_longer(-c(Year,Age,Country, Region), names_to = "Sex", values_to="Dx")

FSR_region_country_Dx <- data.table(FSR_region_country_Dx)

FSR_region_Dx <- FSR_region_country_Dx[, list(Deaths=sum(Dx)), by =  list(Year,Region,Sex,Age)]

save(FSR_region_Dx,file="Data/HMD regions/FSR_region_Dx.RData")


# ------------------------------------------------------------------- #
#    2.2 Combine regions
# ------------------------------------------------------------------- #

Deaths_regions <- rbind(Nordic_region_Dx, Western_region_Dx,
                        Southern_region_Dx, CEE_region_Dx, FSR_region_Dx)

table(Deaths_regions$Year, Deaths_regions$Region)

Deaths_regions <- data.table(Deaths_regions)

save(Deaths_regions,file="Data/HMD regions/Deaths_regions.RData")

# ------------------------------------------------------------------- #
#    2.3 Combine Europe
# ------------------------------------------------------------------- #


Deaths_europe <- Deaths_regions[, list(Deaths=sum(Deaths)), 
                                by =  list(Year,Sex,Age)]

save(Deaths_europe,file="Data/HMD europe/Deaths_europe.RData")


# ------------------------------------------------------------------- #
#     3. Combine exposure and death counts
# ------------------------------------------------------------------- #

##################
# 3.1 Countries
##################

# All country exposures
Countries_exp <- rbind(Nordic_region_country_exp,
                       Western_region_country_exp,
                       Southern_region_country_exp,
                       CEE_region_country_exp,
                       FSR_region_country_exp)
# All country deaths
Countries_Dx <- rbind(Nordic_region_country_Dx,
                      Western_region_country_Dx,
                      Southern_region_country_Dx,
                      CEE_region_country_Dx,
                      FSR_region_country_Dx)
#Combine
Countries <- merge(Countries_exp, Countries_Dx,
                   by=c("Year", "Age", "Sex", "Country", "Region"))
Countries <- data.table(Countries)
save(Countries,file="Data/HMD countries/Countries.RData")


##################
# 3.2. Regions
##################

Regions <- Countries[,list(Exposure=sum(Pop),
                           Deaths = sum(Dx)),
                           by=list(Year, Age, Sex, Region)]


save(Regions,file="Data/HMD regions/Regions.RData")



##################
# 3.3 Europe
##################

Europe <- Regions[,list(Exposure=sum(Exposure),
                        Deaths = sum(Deaths)),
                          by=list(Year, Age, Sex)]
save(Europe,file="Data/HMD europe/Europe.RData")







