# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Classification of causes of death, version 11 categories
# Dataset: WHO Mortality database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# Content:
#   0. Working directory, package, and functions
#   1. Read and preparing data
#      1.1 ICD 9 classification 
#      1.2 ICD 10 classification 
#   2. Arrange data
#   3. Create subgroups for faster smoothing

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

get(load("Data/WHO_Database_March_2021.RData"))

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

# ---------------------------------------
#     1.1 ICD 9 classification 
# ---------------------------------------

ICD9.data    <- WHO_data[WHO_data$ICD==9,]

# -- We create the vector of Cat, with value 12, to filter on later
ICD9.data$Cat <- 12


######
# -- 11) Rest of causes of death
######
ICD9.data[ICD9.data$Cause %in% c(paste0('B0',1:7),paste0('B', 184:185),
                                 paste0('B',15:17), 'B180', paste0('B',182:183),
                                 'B189', paste0('B',19:20),paste0('B',33:46)),]$Cat <- 11

######
# -- 1) Smoking-related cancers
######
ICD9.data[ICD9.data$Cause %in% c(paste0("B0",90:94),"B08","B096",
                                 "B100","B101","180","188","189"),]$Cat <- 1


######
# -- 3) Other cancers
######
ICD9.data[ICD9.data$Cause %in% c(paste0('B',13:14), "B095","B099","B109","B11","B14", 
                                 "179", "181","182","183","184",
                                 "185","186","187"),]$Cat <- 3

######
# -- 4) Ischemic heart diseases and Stroke
######
ICD9.data[ICD9.data$Cause %in% c("B027", "B029"),]$Cat <- 4

######
# -- 5) Circulatory system
######
ICD9.data[ICD9.data$Cause %in% c(paste0("B",25:30)),]$Cat <- 5

######
# -- 6) Mental Disorder + Nervous System
######
ICD9.data[ICD9.data$Cause %in% c(paste0('B',21), paste0("B",22)),]$Cat <- 6

######
# -- 7)  Alcohol-attributable Mortality
######
ICD9.data[ICD9.data$Cause %in% c("B125","B347","B48"),]$Cat <- 7

######
# -- 8) Infectious (respiratory) diseases
######
ICD9.data[ICD9.data$Cause %in% c(paste0('B',01:17),paste0('B',310:312),
                                 paste0('B',320:35)),]$Cat <- 8

######
# -- 9) Non-infectious respiratory diseases
######
ICD9.data[ICD9.data$Cause %in% c(paste0('B',313:315),paste0('B',326:327),
                                 'B319','B329'),]$Cat <- 9

######
# -- 10) External cause of death
######
ICD9.data[ICD9.data$Cause %in% c(paste0('B',47:56)),]$Cat <- 10

######
# -- 2) Sex-specific cancers
######
ICD9.data[ICD9.data$Cause %in% c(paste0("B",120:123),
                                 paste0("B",124:129), 'B113'),]$Cat <- 2


# -----
# We select the causes that we want
# -----

unique(ICD9.data$Cat)
#unique(ICD9.data[ICD9.data$Cat==10,]$Cause)
ICD9.data <- ICD9.data[ICD9.data$Cat<=11,]

# We check to table the total cause of deaths, and their relative contribution

table(ICD9.data$Cat)
prop.table(table(ICD9.data$Cat))*100


# ---------------------------------------
#     1.2 ICD 10 classification 
# ---------------------------------------

ICD10.data      <- WHO_data[WHO_data$ICD==10,]

table(ICD10.data$Country.name,ICD10.data$List)

table(Belgium_Norway$Cause)

#get only the first 3 digits of the cause of death
sort(unique(ICD10.data$Cause))[2000:length(unique(ICD10.data$Cause))]
ICD10.data$Cause2 <- substr(ICD10.data$Cause,1,3)
ICD10.data$Cause3 <- substr(ICD10.data$Cause,4,4)
sort(unique(ICD10.data$Cause2))
sort(unique(ICD10.data$Cause3))

# Exclude the cause A000, is all causes in documentation
ICD10.data <- ICD10.data[ICD10.data$Cause != "AAA",]

# -- We create the vector of Cat, with value 13, to filter on later
ICD10.data$Cat <- 12

######
# -- 1) Cancer sensitive to smoking
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C',0:9),paste0('C',10:21),
                                    paste0('C',25),paste0('C',30:34),
                                    paste0('C',64:68)),]$Cat <- 1


######
# -- 2) Sex-specific cancer
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C',50:58),paste0('C',60:63)),]$Cat <- 2

######
# -- 3) Other Cancers
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C',22:24),'C26',paste0('C',37:39),
                                    paste0('C',40:41),paste0('C',43:49),
                                    paste0('C',69:97)),]$Cat <- 3


######
# -- 4) Ischemic heart diseases and Stroke
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('I',20:25),paste0('I',60:69)),]$Cat <- 4

######
# -- 5) Other Circulatory system
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('I0',0:9), paste0('I',10:19),
                                    paste0('I',26:59), paste0('I',70:99)),]$Cat <- 5

######
# -- 6) Mental Disorder + Nervous System
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('F0',1:9),paste0('F',10:99),
                                    paste0('G0',1:9), paste0('G',10:98)),]$Cat <- 6

######
# -- 7) Alcohol Attributable Mortality
######
ICD10.data[ICD10.data$Cause2 %in% c("F10", "K70","X45"),]$Cat <- 7

######
# -- 8) Infectious (infectious respiratoty diseases)
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('A0',0:9),paste0('A', 10:99),
                                    paste0('B0',0:9),paste0('B', 10:89),paste0('B', 99),
                                    paste0('J0',0:6), "J09", paste0('J',10:18),
                                    paste0('J',20:22), "J34", "J36", "J39",
                                    "J44", "J85", "J86"),]$Cat <- 8

######
# -- 9) Non infectious respiratory disases
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('J',30:33), "J35", "J37", "J38",
                                    paste0('J',40:43), paste0('J',45:84),
                                    paste0('J',87:99)),]$Cat <- 9

######
# -- 10) External causes of death
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('V',10:99),paste0('V0',1:9),
                                    paste0('S0',0:9),paste0('T0',0:9),paste0('S',10:99),paste0('T',10:89),
                                    paste0('W',10:99),paste0('W0',0:9),
                                    paste0('X',10:99),paste0('X0',0:9),
                                    paste0('Y',10:89),paste0('Y0',0:9)),]$Cat <- 10

######
# -- 12) Other no infetious no respirtaoty
######
ICD10.data[ICD10.data$Cause2 %in% c(paste0('A0',0:9),paste0('A', 10:99),
                                    paste0('B0', 0:9),paste0('B', 10:89),paste0('B', 99),
                                    paste0('D0',0:9),paste0('D',10:48),paste0('D',50:89),
                                    paste0('E0',0:7),paste0('E',15:16),paste0('E',20:35),paste0('E',40:46),
                                    paste0('E',50:68),paste0('E',70:90),
                                    paste0('H0',0:9),paste0('H',10:59),paste0('H',60:95),
                                    paste0('K0',0:9),paste0('K',10:93),
                                    paste0('L0',0:9),paste0('L',10:99),
                                    paste0('M0',0:9),paste0('M',10:99),
                                    paste0('N0',0:9),paste0('N',10:99),
                                    paste0('O0',0:9),paste0('O',10:99),
                                    paste0('P0',0:9),paste0('P',10:96),
                                    paste0('Q0',0:9),paste0('Q',10:99),
                                    paste0('R0',0:9),paste0('R',10:99)),]$Cat <- 11


#------------------------
# For Belgium_Norway_Sweden
# ------------------------

Belgium_Norway_Sweden <- ICD10.data %>% 
  filter(Country.name=="Sweeden" | Country.name=="Norway" |
           Country.name=="Belgium") %>% 
  mutate(Cat = case_when(Cause>=1020 & Cause<=1030 | 
                           Cause>=1032 & Cause<=1034 | 
                           Cause==1041 ~ 1,
                         Cause>=1036 & Cause<=1040  ~ 2,
                         Cause==1031 | Cause==1035 |
                         Cause>=1042 & Cause<=1046  ~ 3,
                         Cause==1067 | Cause==1069 ~ 4,
                         Cause>=1065 & Cause<=1066 | 
                         Cause==1068 |  Cause==1070 |
                           Cause==1071 ~ 5,
                         Cause>=1055 & Cause==1061 ~ 6,
                         Cause==1056 | Cause==1080 | 
                         Cause==1100 ~ 7,
                         Cause>=1002 & Cause<=1025 |
                         Cause>=1073 & Cause<=1075 ~ 8,
                         Cause>=1076 & Cause<=1077 ~ 9,
                         Cause>=1095 & Cause<=1102 ~ 10,
                         Cause>=1047 & Cause<=1049 | 
                           Cause>=1051 & Cause<=1054  | 
                           Cause>=1062 & Cause<=1063 | 
                           Cause>=1078 & Cause<=1094 ~ 11))

# ------------------
#   Combine
# ------------------

ICD10.data_final <- rbind(ICD10.data,Belgium_Norway_Sweden)
#ICD10.data_final <- rbind(ICD10.data)
# -----
# We select the causes that we want
# -----
table(ICD10.data_final$Cat, ICD10.data$Country.name)

#ICD10.data <- ICD10.data[ICD10.data$Cat==12,]
sort(unique(ICD10.data_final$Cause))

ICD10.data_final <- ICD10.data_final[ICD10.data_final$Cat<=11,]

ICD10.data_final <- ICD10.data_final[,-c('Cause2','Cause3')]

table(ICD10.data_final$Cat)
prop.table(table(ICD10.data_final$Cat))*100

# ------------------------------------------------------------------- #
#     2. Arrange data
# ------------------------------------------------------------------- #


# ---------------------------------------
#       Combine all databases on Cuases of deaths
# ---------------------------------------


WHO_COD_Data <- rbind(ICD9.data,ICD10.data_final)

gdata::keep(WHO_COD_Data, sure = T)

table(WHO_COD_Data$Cat)
prop.table(table(WHO_COD_Data$Cat))

##### Now play with ages
unique(WHO_COD_Data$Frmat)

# groups ages 1:4 for format 0
DT_COD.0       <- WHO_COD_Data[WHO_COD_Data$Frmat == 0,]
DT_COD.0$A_1_4 <- DT_COD.0$Deaths3 + DT_COD.0$Deaths4 + DT_COD.0$Deaths5 + DT_COD.0$Deaths6

# groups ages 1:4 for format 1
DT_COD.1       <- WHO_COD_Data[WHO_COD_Data$Frmat == 1,]
DT_COD.1$A_1_4 <- DT_COD.1$Deaths3 + DT_COD.1$Deaths4 + DT_COD.1$Deaths5 + DT_COD.1$Deaths6

# groups ages 1:4 for format 2
DT_COD.2       <- WHO_COD_Data[WHO_COD_Data$Frmat == 2,]
DT_COD.2$A_1_4 <- DT_COD.2$Deaths3

# rbind the 2 datasets
DT_COD <- rbind(DT_COD.0,DT_COD.1,DT_COD.2)

DT_COD1 <- DT_COD
DT_COD2 <- DT_COD

# ---------------------------
#  Proces by single country
# ---------------------------

#reduce to variables needed (age < 85), until Deaths22 
DT_COD1           <- DT_COD1[,c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat', 'Deaths1', 'Deaths2', 'A_1_4', paste0('Deaths',7:22))]
colnames(DT_COD1) <- c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat','Total',as.character(c(0,1,seq(5,80,5))))
DT_COD1           <- DT_COD1[with(DT_COD1,order(Country,Country.name,Sex,Year,Cat))]


DT_COD.melt      <- melt(DT_COD1, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')
DT_COD.melt      <- data.table(DT_COD.melt)




### Get total deaths by age, sex, category, year.
DT_COD.melt      <- DT_COD.melt[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

### get proportions of causes of death by age 
DT_COD.melt      <- DT_COD.melt[DT_COD.melt$Age !="Total",]

DT_COD.melt      <- DT_COD.melt[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]

table(DT_COD.melt$Country.name, DT_COD.melt$Cat)


# ---------------------------
#  Proces by single country
# ---------------------------

#reduce to variables needed (age < 85), until Deaths22 
DT_COD           <- DT_COD[,c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat', 'Deaths1', 'Deaths2', 'A_1_4', paste0('Deaths',7:22))]
colnames(DT_COD) <- c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat','Total',as.character(c(0,1,seq(5,80,5))))
DT_COD           <- DT_COD[with(DT_COD,order(Country,Country.name,Sex,Year,Cat))]


DT_COD.melt      <- melt(DT_COD, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')
DT_COD.melt      <- data.table(DT_COD.melt)


# -- Get total deaths by age, sex, category, year.
DT_COD.melt      <- DT_COD.melt[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

# -- get proportions of causes of death by age 
DT_COD.melt      <- DT_COD.melt[DT_COD.melt$Age !="Total",]

DT_COD.melt      <- DT_COD.melt[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]



save(DT_COD.melt, file = "Data/DT_COD_Melt.RData")

# ---- Save only with 2000 onwards

DT_COD.melt_00 <- DT_COD.melt
save(DT_COD.melt_00, file = "Data/DT_COD_Melt_00.RData")

table(DT_COD.melt_00$Year,
      DT_COD.melt_00$Country.name)
# ------------------------------------------------------------------- #
#     3. Create subgroups for faster smoothing
# ------------------------------------------------------------------- #

# -- Now we create small gorups with small cluster of countries in order to do more easy the smoothing
# -- for the computer.


# -- We will create subgroups of 4 countries

# Group 1: Austria, Belaru, Belgium, Bulgaria
Group_1 <- DT_COD.melt_00[DT_COD.melt_00$Country==4010 | DT_COD.melt_00$Country==4018 |
                            DT_COD.melt_00$Country==4020 | DT_COD.melt_00$Country==4030 & Year<=2015]
save(Group_1, file = "Data/Subgroups smoothing/Group_1.RData")

# Group 2: Czech Republic, Denmark, Estonia, Finland
Group_2 <- DT_COD.melt_00[DT_COD.melt_00$Country==4045 | DT_COD.melt_00$Country==4050 |
                            DT_COD.melt_00$Country==4055 | DT_COD.melt_00$Country==4070 & Year<=2015]
save(Group_2, file = "Data/Subgroups smoothing/Group_2.RData")

# Group 3: France, Germany, Greece, Hungary
Group_3 <- DT_COD.melt_00[DT_COD.melt_00$Country==4080 | DT_COD.melt_00$Country==4085 |
                            DT_COD.melt_00$Country==4140 | DT_COD.melt_00$Country==4150 & Year<=2015]
save(Group_3, file = "Data/Subgroups smoothing/Group_3.RData")

# Group 4: Ireland, Italy, Latvia, Lithuania
Group_4 <- DT_COD.melt_00[DT_COD.melt_00$Country==4170 | DT_COD.melt_00$Country==4180 |
                            DT_COD.melt_00$Country==4186 | DT_COD.melt_00$Country==4188 & Year<=2015]
save(Group_4, file = "Data/Subgroups smoothing/Group_4.RData")

# Group 5: Netherlands, Norway, Poland, Portugal
Group_5 <- DT_COD.melt_00[DT_COD.melt_00$Country==4210 | DT_COD.melt_00$Country==4220 |
                            DT_COD.melt_00$Country==4230 | DT_COD.melt_00$Country==4240 & Year<=2015]
save(Group_5, file = "Data/Subgroups smoothing/Group_5.RData")

# Group 6: Slovakia, Slovenia, Spain, Sweden
Group_6 <- DT_COD.melt_00[DT_COD.melt_00$Country==4274 | DT_COD.melt_00$Country==4276 |
                            DT_COD.melt_00$Country==4280 | DT_COD.melt_00$Country==4290 & Year<=2015]
save(Group_6, file = "Data/Subgroups smoothing/Group_6.RData")

# Group 7: Switzwrland, Ukraine, United Kingdom & Russia
Group_7 <- DT_COD.melt_00[DT_COD.melt_00$Country==4300 | DT_COD.melt_00$Country==4303 |
                            DT_COD.melt_00$Country==4308 & Year<=2015]
save(Group_7, file = "Data/Subgroups smoothing/Group_7.RData")


