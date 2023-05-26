# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Prepare data of causes of death
# Dataset: WHO Mortality database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")

# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")


# Generate a country vector name to be compatible with HMD and WHO database,
# in the future

# Order according to WHO Number
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




# ---------------------------------------------
#     ICD 9 WHO March 9 
# ---------------------------------------------

# ---- Now we ready the data for each ICD code and select the countries of interest of this study
# In the Table 1 in the appendix we show the countries with information in ICD9
# Get ICD-9
ICD9 <- data.table(read.table("Data/WHO Data 8-10/Morticd9.txt",header = T,sep = ',',stringsAsFactors = F))
ICD9 <- ICD9[(ICD9$Country==4010 | ICD9$Country==4018 | ICD9$Country==4030 |
                ICD9$Country==4140 | ICD9$Country==4170 | ICD9$Country==4180 | 
                ICD9$Country==4240 | ICD9$Country==4303 | ICD9$Country==4308 & ICD9$Year>=2000)]
ICD9$ICD <- 9
table(ICD9$Country)

table(ICD9$Year, ICD9$Country)

# ---------------------------------------------
#     ICD10 WHO March 2021 is divided in 5 files 
# ---------------------------------------------


# Get ICD-10_1
ICD10_1 <- data.table(read.table("Data/WHO Data 8-10/March 2021/Morticd10_part1.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_1 <- ICD10_1[(ICD10_1$Country==4010 |   ICD10_1$Country==4018 | ICD10_1$Country==4020 | ICD10_1$Country==4030 | 
                      ICD10_1$Country==4045 | ICD10_1$Country==4050 | ICD10_1$Country==4055 | ICD10_1$Country==4070 | 
                      ICD10_1$Country==4080 | ICD10_1$Country==4085 | ICD10_1$Country==4140 | ICD10_1$Country==4150 |
                      ICD10_1$Country==4170 | ICD10_1$Country==4180 | ICD10_1$Country==4186 | ICD10_1$Country==4188 |
                      ICD10_1$Country==4210 | ICD10_1$Country==4220 | ICD10_1$Country==4230 | ICD10_1$Country==4240 |
                      ICD10_1$Country==4272 | ICD10_1$Country==4274 | ICD10_1$Country==4276 | ICD10_1$Country==4280 |
                      ICD10_1$Country==4290 | ICD10_1$Country==4300 | ICD10_1$Country==4303 | ICD10_1$Country==4308) & ICD10_1$Year>=2000]
ICD10_1$ICD <- 10
table(ICD10_1$Country)

# Get ICD-10_2
ICD10_2 <- data.table(read.table("Data/WHO Data 8-10/March 2021/MortIcd10_part2.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_2 <- ICD10_2[(ICD10_2$Country==4010 |   ICD10_2$Country==4018 | ICD10_2$Country==4020 | ICD10_2$Country==4030 | 
                      ICD10_2$Country==4045 | ICD10_2$Country==4050 | ICD10_2$Country==4055 | ICD10_2$Country==4070 | 
                      ICD10_2$Country==4080 | ICD10_2$Country==4085 | ICD10_2$Country==4140 | ICD10_2$Country==4150 |
                      ICD10_2$Country==4170 | ICD10_2$Country==4180 | ICD10_2$Country==4186 | ICD10_2$Country==4188 |
                      ICD10_2$Country==4210 | ICD10_2$Country==4220 | ICD10_2$Country==4230 | ICD10_2$Country==4240 |
                      ICD10_2$Country==4272 | ICD10_2$Country==4274 | ICD10_2$Country==4276 | ICD10_2$Country==4280 |
                      ICD10_2$Country==4290 | ICD10_2$Country==4300 | ICD10_2$Country==4303 | ICD10_2$Country==4308) & ICD10_2$Year>=2000]
ICD10_2$ICD <- 10
table(ICD10_2$Country)


# Get ICD-10_3
ICD10_3 <- data.table(read.table("Data/WHO Data 8-10/March 2021/MortIcd10_part3.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_3 <- ICD10_3[(ICD10_3$Country==4010 |   ICD10_3$Country==4018 | ICD10_3$Country==4020 | ICD10_3$Country==4030 | 
                      ICD10_3$Country==4045 | ICD10_3$Country==4050 | ICD10_3$Country==4055 | ICD10_3$Country==4070 | 
                      ICD10_3$Country==4080 | ICD10_3$Country==4085 | ICD10_3$Country==4140 | ICD10_3$Country==4150 |
                      ICD10_3$Country==4170 | ICD10_3$Country==4180 | ICD10_3$Country==4186 | ICD10_3$Country==4188 |
                      ICD10_3$Country==4210 | ICD10_3$Country==4220 | ICD10_3$Country==4230 | ICD10_3$Country==4240 |
                      ICD10_3$Country==4272 | ICD10_3$Country==4274 | ICD10_3$Country==4276 | ICD10_3$Country==4280 |
                      ICD10_3$Country==4290 | ICD10_3$Country==4300 | ICD10_3$Country==4303 | ICD10_3$Country==4308) & ICD10_3$Year>=2000]
ICD10_3$ICD <- 10
table(ICD10_3$Country)


# Get ICD-10_4
ICD10_4 <- data.table(read.table("Data/WHO Data 8-10/March 2021/MortIcd10_part4.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_4 <- ICD10_4[(ICD10_4$Country==4010 |   ICD10_4$Country==4018 | ICD10_4$Country==4020 | ICD10_4$Country==4030 | 
                      ICD10_4$Country==4045 | ICD10_4$Country==4050 | ICD10_4$Country==4055 | ICD10_4$Country==4070 | 
                      ICD10_4$Country==4080 | ICD10_4$Country==4085 | ICD10_4$Country==4140 | ICD10_4$Country==4150 |
                      ICD10_4$Country==4170 | ICD10_4$Country==4180 | ICD10_4$Country==4186 | ICD10_4$Country==4188 |
                      ICD10_4$Country==4210 | ICD10_4$Country==4220 | ICD10_4$Country==4230 | ICD10_4$Country==4240 |
                      ICD10_4$Country==4272 | ICD10_4$Country==4274 | ICD10_4$Country==4276 | ICD10_4$Country==4280 |
                      ICD10_4$Country==4290 | ICD10_4$Country==4300 | ICD10_4$Country==4303 | ICD10_4$Country==4308) & ICD10_4$Year>=2000]
ICD10_4$ICD <- 10
table(ICD10_4$Country)



# Get ICD-10_5
ICD10_5 <- data.table(read.table("Data/WHO Data 8-10/March 2021/MortIcd10_part5.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_5 <- ICD10_5[(ICD10_5$Country==4010 |   ICD10_5$Country==4018 | ICD10_5$Country==4020 | ICD10_5$Country==4030 | 
                      ICD10_5$Country==4045 | ICD10_5$Country==4050 | ICD10_5$Country==4055 | ICD10_5$Country==4070 | 
                      ICD10_5$Country==4080 | ICD10_5$Country==4085 | ICD10_5$Country==4140 | ICD10_5$Country==4150 |
                      ICD10_5$Country==4170 | ICD10_5$Country==4180 | ICD10_5$Country==4186 | ICD10_5$Country==4188 |
                      ICD10_5$Country==4210 | ICD10_5$Country==4220 | ICD10_5$Country==4230 | ICD10_5$Country==4240 |
                      ICD10_5$Country==4272 | ICD10_5$Country==4274 | ICD10_5$Country==4276 | ICD10_5$Country==4280 |
                      ICD10_5$Country==4290 | ICD10_5$Country==4300 | ICD10_5$Country==4303 | ICD10_5$Country==4308) & ICD10_5$Year>=2000]
ICD10_5$ICD <- 10
table(ICD10_5$Country)

# -----------------
#   Combine all
# -----------------


# --- Now we combine the dataset
WHO_data <- rbind(ICD9, ICD10_1,ICD10_2,
                  ICD10_3,ICD10_4,ICD10_5)




table(WHO_data$Country)
WHO_data$Country.name <- as.factor(WHO_data$Country)
levels(WHO_data$Country.name) <- Country.name.vec


# Is necessary to eliminate cases with sex unspecified
# sex 1 is male, 2 is female, 9 is unspecified
WHO_data <- WHO_data[WHO_data$Sex != 9,]
WHO_data$Sex <- as.factor(WHO_data$Sex)
levels(WHO_data$Sex) <- c('m', 'f')

# We reaarange the data and eliminates the columns of Amind and Subdiv1
WHO_data <- WHO_data[,c(1,41,40,4:32)]

WHO_data <- WHO_data %>% 
  filter(Year>=2000 & Year<=2015)

table(WHO_data$Country.name, WHO_data$Country)

table(WHO_data$Year[WHO_data$ICD==10], WHO_data$Country.name[WHO_data$ICD==10])

save(WHO_data, file = "Data/WHO_Database_March_2021.RData")

