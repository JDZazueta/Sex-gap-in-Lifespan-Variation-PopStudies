# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Supplementary Material Tables and Figures 4
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Sensitivity Analysis
#      2.1 E-dagger instead of E.dagger
#      2.2 Decomposition lifespan variation condition from age 15
#      2.3 Decomposition lifespan variation using Horiuchi

# ---------------------------------------------------------------------------- #
#     0. Working directory, package, and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
setwd("//ia/NIDI$/home/DanielZ/Desktop/pubpack Zazueta et al PopStudies/Sex gap in lifespan variation - FINAL")
# I call the packages that I use for this analysis
source("Functions code/Packages, labels and colors.R")
source("Functions code/Decomposition functions.R")

Labels.age            <- c('0','1-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                           '40-44','45-49','50-54','55-59','60-64','65-69',
                           "70-74","75-79","80-84","85-89","90-94","95-99","100+")
Labels.period         <- c("2000-2004", "2005-2009",
                           "2010-2015")


# ---------------------------------------------------------------------------- #
#     1. Open data set
# ---------------------------------------------------------------------------- #

# --------------
# Regions
# --------------
get(load("Data/Mx_CoD_regions.RData"))

Mx_CoD_regions$Age <- as.numeric(Mx_CoD_regions$Age)

Mx_CoD_regions$mix_1[is.na(Mx_CoD_regions$mix_1)] <- 0
Mx_CoD_regions$mix_2[is.na(Mx_CoD_regions$mix_2)] <- 0
Mx_CoD_regions$mix_3[is.na(Mx_CoD_regions$mix_3)] <- 0
Mx_CoD_regions$mix_4[is.na(Mx_CoD_regions$mix_4)] <- 0
Mx_CoD_regions$mix_5[is.na(Mx_CoD_regions$mix_5)] <- 0
Mx_CoD_regions$mix_6[is.na(Mx_CoD_regions$mix_6)] <- 0
Mx_CoD_regions$mix_7[is.na(Mx_CoD_regions$mix_7)] <- 0
Mx_CoD_regions$mix_8[is.na(Mx_CoD_regions$mix_8)] <- 0
Mx_CoD_regions$mix_9[is.na(Mx_CoD_regions$mix_9)] <- 0
Mx_CoD_regions$mix_10[is.na(Mx_CoD_regions$mix_10)] <- 0
Mx_CoD_regions$mix_11[is.na(Mx_CoD_regions$mix_11)] <- 0

# --------------
# Europe
# --------------
get(load("Data/Mx_CoD_Europe.RData"))

Mx_CoD_Europe$Age <- as.numeric(Mx_CoD_Europe$Age)

Mx_CoD_Europe$mix_1[is.na(Mx_CoD_Europe$mix_1)] <- 0
Mx_CoD_Europe$mix_2[is.na(Mx_CoD_Europe$mix_2)] <- 0
Mx_CoD_Europe$mix_3[is.na(Mx_CoD_Europe$mix_3)] <- 0
Mx_CoD_Europe$mix_4[is.na(Mx_CoD_Europe$mix_4)] <- 0
Mx_CoD_Europe$mix_5[is.na(Mx_CoD_Europe$mix_5)] <- 0
Mx_CoD_Europe$mix_6[is.na(Mx_CoD_Europe$mix_6)] <- 0
Mx_CoD_Europe$mix_7[is.na(Mx_CoD_Europe$mix_7)] <- 0
Mx_CoD_Europe$mix_8[is.na(Mx_CoD_Europe$mix_8)] <- 0
Mx_CoD_Europe$mix_9[is.na(Mx_CoD_Europe$mix_9)] <- 0
Mx_CoD_Europe$mix_10[is.na(Mx_CoD_Europe$mix_10)] <- 0
Mx_CoD_Europe$mix_11[is.na(Mx_CoD_Europe$mix_11)] <- 0

# ------------------------------------------------------------------- #
#   2. Sensitivity Analysis
# ------------------------------------------------------------------- #

# ------------------------------------------------------------------- #
#       2.1 E-dagger instead of E.dagger
# ------------------------------------------------------------------- #

#####
# Nordic countries
#####
# Period 7
Nordic_7 <- Mx_CoD_regions %>% 
  filter(Region==1 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Nordic_M_COD_7 <- as.matrix(Nordic_7[Nordic_7$Sex=="m",24:34])
Nordic_F_COD_7  <- as.matrix(Nordic_7[Nordic_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Nordic.E.dagger_7 <- stepwise_replacement(func = edaggerfrommxc, pars1 = c(Nordic_F_COD_7), pars2 = c(Nordic_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Nordic.E.dagger_7) <- dim(Nordic_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Nordic.E.dagger_7 <- data.frame(Results.Nordic.E.dagger_7)
colnames(Results.Nordic.E.dagger_7) <- cause_names
Results.Nordic.E.dagger_7$Age <- c(seq(0,100,1))
Results.Nordic.E.dagger_7 <- gather(data = Results.Nordic.E.dagger_7,key = Cause,value = Contribution,-Age)
Results.Nordic.E.dagger_7$Age2       <- (cut(Results.Nordic.E.dagger_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Nordic.E.dagger_7 <- data.table(Results.Nordic.E.dagger_7)
Results.Nordic.E.dagger_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Nordic.E.dagger <- data.table(Results.Nordic.E.dagger_7)
Results.Nordic.E.dagger$Period <- factor(Results.Nordic.E.dagger$Period, levels = c(5,6,7),
                                   labels =   c("2000-2004", "2005-2009",
                                                "2010-2015"))

# For plotting
Results.Nordic.E.dagger.plot <- Results.Nordic.E.dagger[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Nordic.E.dagger.plot$Region <- "Nordic"

# For datatable
Results.Nordic.E.dagger.tab  <- Results.Nordic.E.dagger.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Nordic.E.dagger.tab <- data.table(Results.Nordic.E.dagger.tab)
Results.Nordic.E.dagger.tab$Region <- "Nordic"

#####
# Western countries
#####

Western_7 <- Mx_CoD_regions %>% 
  filter(Region==2 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Western_M_COD_7 <- as.matrix(Western_7[Western_7$Sex=="m",24:34])
Western_F_COD_7  <- as.matrix(Western_7[Western_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Western.E.dagger_7 <- stepwise_replacement(func = edaggerfrommxc, pars1 = c(Western_F_COD_7), pars2 = c(Western_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Western.E.dagger_7) <- dim(Western_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Western.E.dagger_7 <- data.frame(Results.Western.E.dagger_7)
colnames(Results.Western.E.dagger_7) <- cause_names
Results.Western.E.dagger_7$Age <- c(seq(0,100,1))
Results.Western.E.dagger_7 <- gather(data = Results.Western.E.dagger_7,key = Cause,value = Contribution,-Age)
Results.Western.E.dagger_7$Age2       <- (cut(Results.Western.E.dagger_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Western.E.dagger_7 <- data.table(Results.Western.E.dagger_7)
Results.Western.E.dagger_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Western.E.dagger <- data.table(Results.Western.E.dagger_7)
Results.Western.E.dagger$Period <- factor(Results.Western.E.dagger$Period, levels = c(5,6,7),
                                    labels =   c("2000-2004", "2005-2009",
                                                 "2010-2015"))

# For plotting
Results.Western.E.dagger.plot <- Results.Western.E.dagger[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Western.E.dagger.plot$Region <- "Western"

# For datatable
Results.Western.E.dagger.tab  <- Results.Western.E.dagger.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Western.E.dagger.tab <- data.table(Results.Western.E.dagger.tab)
Results.Western.E.dagger.tab$Region <- "Western"

#####
# Southern countries
#####

# Period 7
Southern_7 <- Mx_CoD_regions %>% 
  filter(Region==3 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Southern_M_COD_7 <- as.matrix(Southern_7[Southern_7$Sex=="m",24:34])
Southern_F_COD_7  <- as.matrix(Southern_7[Southern_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Southern.E.dagger_7 <- stepwise_replacement(func = edaggerfrommxc, pars1 = c(Southern_F_COD_7), pars2 = c(Southern_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Southern.E.dagger_7) <- dim(Southern_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Southern.E.dagger_7 <- data.frame(Results.Southern.E.dagger_7)
colnames(Results.Southern.E.dagger_7) <- cause_names
Results.Southern.E.dagger_7$Age <- c(seq(0,100,1))
Results.Southern.E.dagger_7 <- gather(data = Results.Southern.E.dagger_7,key = Cause,value = Contribution,-Age)
Results.Southern.E.dagger_7$Age2       <- (cut(Results.Southern.E.dagger_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Southern.E.dagger_7 <- data.table(Results.Southern.E.dagger_7)
Results.Southern.E.dagger_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Southern.E.dagger <- data.table(Results.Southern.E.dagger_7)
Results.Southern.E.dagger$Period <- factor(Results.Southern.E.dagger$Period, levels = c(5,6,7),
                                     labels =   c("2000-2004", "2005-2009",
                                                  "2010-2015"))

# For plotting
Results.Southern.E.dagger.plot <- Results.Southern.E.dagger[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Southern.E.dagger.plot$Region <- "Southern"

# For datatable
Results.Southern.E.dagger.tab  <- Results.Southern.E.dagger.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Southern.E.dagger.tab <- data.table(Results.Southern.E.dagger.tab)
Results.Southern.E.dagger.tab$Region <- "Southern"


#####
# CEE countries
#####

# Period 7
CEE_7 <- Mx_CoD_regions %>% 
  filter(Region==4 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
CEE_M_COD_7 <- as.matrix(CEE_7[CEE_7$Sex=="m",24:34])
CEE_F_COD_7  <- as.matrix(CEE_7[CEE_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.CEE.E.dagger_7 <- stepwise_replacement(func = edaggerfrommxc, pars1 = c(CEE_F_COD_7), pars2 = c(CEE_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.CEE.E.dagger_7) <- dim(CEE_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.CEE.E.dagger_7 <- data.frame(Results.CEE.E.dagger_7)
colnames(Results.CEE.E.dagger_7) <- cause_names
Results.CEE.E.dagger_7$Age <- c(seq(0,100,1))
Results.CEE.E.dagger_7 <- gather(data = Results.CEE.E.dagger_7,key = Cause,value = Contribution,-Age)
Results.CEE.E.dagger_7$Age2       <- (cut(Results.CEE.E.dagger_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CEE.E.dagger_7 <- data.table(Results.CEE.E.dagger_7)
Results.CEE.E.dagger_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.CEE.E.dagger <- data.table(Results.CEE.E.dagger_7)
Results.CEE.E.dagger$Period <- factor(Results.CEE.E.dagger$Period, levels = c(5,6,7),
                                labels =   c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.CEE.E.dagger.plot <- Results.CEE.E.dagger[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.CEE.E.dagger.plot$Region <- "CEE"

# For datatable
Results.CEE.E.dagger.tab  <- Results.CEE.E.dagger.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.CEE.E.dagger.tab <- data.table(Results.CEE.E.dagger.tab)
Results.CEE.E.dagger.tab$Region <- "CEE"

#####
# FSR countries
#####
# Period 7
FSR_7 <- Mx_CoD_regions %>% 
  filter(Region==5 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
FSR_M_COD_7 <- as.matrix(FSR_7[FSR_7$Sex=="m",24:34])
FSR_F_COD_7  <- as.matrix(FSR_7[FSR_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.FSR.E.dagger_7 <- stepwise_replacement(func = edaggerfrommxc, pars1 = c(FSR_F_COD_7), pars2 = c(FSR_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.FSR.E.dagger_7) <- dim(FSR_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.FSR.E.dagger_7 <- data.frame(Results.FSR.E.dagger_7)
colnames(Results.FSR.E.dagger_7) <- cause_names
Results.FSR.E.dagger_7$Age <- c(seq(0,100,1))
Results.FSR.E.dagger_7 <- gather(data = Results.FSR.E.dagger_7,key = Cause,value = Contribution,-Age)
Results.FSR.E.dagger_7$Age2       <- (cut(Results.FSR.E.dagger_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FSR.E.dagger_7 <- data.table(Results.FSR.E.dagger_7)
Results.FSR.E.dagger_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.FSR.E.dagger <- data.table(Results.FSR.E.dagger_7)
Results.FSR.E.dagger$Period <- factor(Results.FSR.E.dagger$Period, levels = c(5,6,7),
                                labels =   c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.FSR.E.dagger.plot <- Results.FSR.E.dagger[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.FSR.E.dagger.plot$Region <- "FSR"

# For datatable
Results.FSR.E.dagger.tab  <- Results.FSR.E.dagger.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.FSR.E.dagger.tab <- data.table(Results.FSR.E.dagger.tab)
Results.FSR.E.dagger.tab$Region <- "FSR"


#####
# Europe
#####

# Period 7
Europe_7 <- Mx_CoD_Europe %>% 
  filter(Region==6 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Europe_M_COD_7 <- as.matrix(Europe_7[Europe_7$Sex=="m",24:34])
Europe_F_COD_7  <- as.matrix(Europe_7[Europe_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Europe.E.dagger_7 <- stepwise_replacement(func = edaggerfrommxc, pars1 = c(Europe_F_COD_7), pars2 = c(Europe_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Europe.E.dagger_7) <- dim(Europe_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Europe.E.dagger_7 <- data.frame(Results.Europe.E.dagger_7)
colnames(Results.Europe.E.dagger_7) <- cause_names
Results.Europe.E.dagger_7$Age <- c(seq(0,100,1))
Results.Europe.E.dagger_7 <- gather(data = Results.Europe.E.dagger_7,key = Cause,value = Contribution,-Age)
Results.Europe.E.dagger_7$Age2       <- (cut(Results.Europe.E.dagger_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Europe.E.dagger_7 <- data.table(Results.Europe.E.dagger_7)
Results.Europe.E.dagger_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Europe.E.dagger <- data.table(Results.Europe.E.dagger_7)
Results.Europe.E.dagger$Period <- factor(Results.Europe.E.dagger$Period, levels = c(5,6,7),
                                   labels =   c("2000-2004", "2005-2009",
                                                "2010-2015"))

# For plotting
Results.Europe.E.dagger.plot <- Results.Europe.E.dagger[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Europe.E.dagger.plot$Region <- "Europe"

# For datatable
Results.Europe.E.dagger.tab  <- Results.Europe.E.dagger.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Europe.E.dagger.tab <- data.table(Results.Europe.E.dagger.tab)
Results.Europe.E.dagger.tab$Region <- "Europe"

# -------------------------------------------------------
#         Combine regions
# -------------------------------------------------------

Data_decomp_E.dagger <- rbind(Results.Nordic.E.dagger.plot,
                              Results.Western.E.dagger.plot,
                              Results.Southern.E.dagger.plot,
                              Results.CEE.E.dagger.plot,
                              Results.FSR.E.dagger.plot,
                              Results.Europe.E.dagger.plot)

# -- Causes of death labeling
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="C. Sesitive Smoking"]  <- 1
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Sex-specif cancer"]  <- 2
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Other Cancers"]  <- 3
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="IHD and Stroke"]  <- 4
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Rest of Circulatory diseases"]  <- 5
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Mental and Nervous system"]  <- 6
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Alcohol attributable"]  <- 7
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="External"]  <- 8
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Infectious (respiratory diseases)"]  <- 9
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Non infectious respiratory disases"]  <- 10
Data_decomp_E.dagger$Cause2[Data_decomp_E.dagger$Cause=="Other No Infections No respiratory"]  <- 11


Data_Fig_E.dagger <- Data_decomp_E.dagger %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6)) %>% 
  filter(Period=="2010-2015") 

# --- Labeling

# Causes of death
Data_Fig_E.dagger$Cause2 <- factor(Data_Fig_E.dagger$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                          "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                          "Non infectious respiratory disases","Rest of causes"))


# Measure
Data_Fig_E.dagger$Measure_2 <- factor(Data_Fig_E.dagger$Measure_2,
                                  levels = c(1,2),
                                  labels = c("Life expectancy (e0)", "Lifespan variation (SD)"))

# Region
Data_Fig_E.dagger$Region_2 <- factor(Data_Fig_E.dagger$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))

Fig_1 <- ggplot(data=Data_Fig_E.dagger,
                aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  #scale_fill_brewer(palette =  "Paired") +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    y= "",
    x="")
Fig_1
ggsave(filename = "Figure 1 SM-4.png", path= "Supplementary Material/4. Sensitivity analysis/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")

# Table e-dagger
Table_S1 <- Data_Fig_E.dagger[,list(Edag=sum(Contribution2)),
                              by=list(Period, Region_2)]
write.csv(Table_S1, file = "Supplementary Material/4. Sensitivity analysis/Table 1 SM-4.csv")



# ------------------------------------------------------------------- #
#       2.3 Decomposition lifespan variation using Horiuchi
# ------------------------------------------------------------------- #


#####
# Nordic countries
#####

# - Decomposition
Results.Nordic.Horiuchi_7 <- horiuchi(func = sdfrommxc, 
                                      pars1 = c(Nordic_F_COD_7), 
                                      pars2 = c(Nordic_M_COD_7),
                                      N=100)

# ----- Fourth: Define dimention of matrix
dim(Results.Nordic.Horiuchi_7) <- dim(Nordic_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Nordic.Horiuchi_7 <- data.frame(Results.Nordic.Horiuchi_7)
colnames(Results.Nordic.Horiuchi_7) <- cause_names
Results.Nordic.Horiuchi_7$Age <- c(seq(0,100,1))
Results.Nordic.Horiuchi_7 <- gather(data = Results.Nordic.Horiuchi_7,key = Cause,value = Contribution,-Age)
Results.Nordic.Horiuchi_7$Age2       <- (cut(Results.Nordic.Horiuchi_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Nordic.Horiuchi_7 <- data.table(Results.Nordic.Horiuchi_7)
Results.Nordic.Horiuchi_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Nordic.Horiuchi <- data.table(Results.Nordic.Horiuchi_7)
Results.Nordic.Horiuchi$Period <- factor(Results.Nordic.Horiuchi$Period, levels = c(5,6,7),
                                         labels =   c("2000-2004", "2005-2009",
                                                      "2010-2015"))

# For plotting
Results.Nordic.Horiuchi.plot <- Results.Nordic.Horiuchi[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Nordic.Horiuchi.plot$Region <- "Nordic"

# For datatable
Results.Nordic.Horiuchi.tab  <- Results.Nordic.Horiuchi.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Nordic.Horiuchi.tab <- data.table(Results.Nordic.Horiuchi.tab)
Results.Nordic.Horiuchi.tab$Region <- "Nordic"

#####
# Western countries
#####

Western_7 <- Mx_CoD_regions %>% 
  filter(Region==2 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Western_M_COD_7 <- as.matrix(Western_7[Western_7$Sex=="m",24:34])
Western_F_COD_7  <- as.matrix(Western_7[Western_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Western.Horiuchi_7 <- horiuchi(func = sdfrommxc, 
                                       pars1 = c(Western_F_COD_7), 
                                       pars2 = c(Western_M_COD_7),
                                       N=100)

# ----- Fourth: Define dimention of matrix
dim(Results.Western.Horiuchi_7) <- dim(Western_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Western.Horiuchi_7 <- data.frame(Results.Western.Horiuchi_7)
colnames(Results.Western.Horiuchi_7) <- cause_names
Results.Western.Horiuchi_7$Age <- c(seq(0,100,1))
Results.Western.Horiuchi_7 <- gather(data = Results.Western.Horiuchi_7,key = Cause,value = Contribution,-Age)
Results.Western.Horiuchi_7$Age2       <- (cut(Results.Western.Horiuchi_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Western.Horiuchi_7 <- data.table(Results.Western.Horiuchi_7)
Results.Western.Horiuchi_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Western.Horiuchi <- data.table(Results.Western.Horiuchi_7)
Results.Western.Horiuchi$Period <- factor(Results.Western.Horiuchi$Period, levels = c(5,6,7),
                                          labels =   c("2000-2004", "2005-2009",
                                                       "2010-2015"))

# For plotting
Results.Western.Horiuchi.plot <- Results.Western.Horiuchi[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Western.Horiuchi.plot$Region <- "Western"

# For datatable
Results.Western.Horiuchi.tab  <- Results.Western.Horiuchi.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Western.Horiuchi.tab <- data.table(Results.Western.Horiuchi.tab)
Results.Western.Horiuchi.tab$Region <- "Western"

#####
# Southern countries
#####

# Period 7
Southern_7 <- Mx_CoD_regions %>% 
  filter(Region==3 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Southern_M_COD_7 <- as.matrix(Southern_7[Southern_7$Sex=="m",24:34])
Southern_F_COD_7  <- as.matrix(Southern_7[Southern_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Southern.Horiuchi_7 <- horiuchi(func = sdfrommxc, 
                                        pars1 = c(Southern_F_COD_7),
                                        pars2 = c(Southern_M_COD_7),
                                        N=100)

# ----- Fourth: Define dimention of matrix
dim(Results.Southern.Horiuchi_7) <- dim(Southern_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Southern.Horiuchi_7 <- data.frame(Results.Southern.Horiuchi_7)
colnames(Results.Southern.Horiuchi_7) <- cause_names
Results.Southern.Horiuchi_7$Age <- c(seq(0,100,1))
Results.Southern.Horiuchi_7 <- gather(data = Results.Southern.Horiuchi_7,key = Cause,value = Contribution,-Age)
Results.Southern.Horiuchi_7$Age2       <- (cut(Results.Southern.Horiuchi_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Southern.Horiuchi_7 <- data.table(Results.Southern.Horiuchi_7)
Results.Southern.Horiuchi_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Southern.Horiuchi <- data.table(Results.Southern.Horiuchi_7)
Results.Southern.Horiuchi$Period <- factor(Results.Southern.Horiuchi$Period, levels = c(5,6,7),
                                           labels =   c("2000-2004", "2005-2009",
                                                        "2010-2015"))

# For plotting
Results.Southern.Horiuchi.plot <- Results.Southern.Horiuchi[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Southern.Horiuchi.plot$Region <- "Southern"

# For datatable
Results.Southern.Horiuchi.tab  <- Results.Southern.Horiuchi.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Southern.Horiuchi.tab <- data.table(Results.Southern.Horiuchi.tab)
Results.Southern.Horiuchi.tab$Region <- "Southern"


#####
# CEE countries
#####

# Period 7
CEE_7 <- Mx_CoD_regions %>% 
  filter(Region==4 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
CEE_M_COD_7 <- as.matrix(CEE_7[CEE_7$Sex=="m",24:34])
CEE_F_COD_7  <- as.matrix(CEE_7[CEE_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.CEE.Horiuchi_7 <- horiuchi(func = sdfrommxc, 
                                   pars1 = c(CEE_F_COD_7),
                                   pars2 = c(CEE_M_COD_7),
                                   N=100)

# ----- Fourth: Define dimention of matrix
dim(Results.CEE.Horiuchi_7) <- dim(CEE_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.CEE.Horiuchi_7 <- data.frame(Results.CEE.Horiuchi_7)
colnames(Results.CEE.Horiuchi_7) <- cause_names
Results.CEE.Horiuchi_7$Age <- c(seq(0,100,1))
Results.CEE.Horiuchi_7 <- gather(data = Results.CEE.Horiuchi_7,key = Cause,value = Contribution,-Age)
Results.CEE.Horiuchi_7$Age2       <- (cut(Results.CEE.Horiuchi_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CEE.Horiuchi_7 <- data.table(Results.CEE.Horiuchi_7)
Results.CEE.Horiuchi_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.CEE.Horiuchi <- data.table(Results.CEE.Horiuchi_7)
Results.CEE.Horiuchi$Period <- factor(Results.CEE.Horiuchi$Period, levels = c(5,6,7),
                                      labels =   c("2000-2004", "2005-2009",
                                                   "2010-2015"))

# For plotting
Results.CEE.Horiuchi.plot <- Results.CEE.Horiuchi[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.CEE.Horiuchi.plot$Region <- "CEE"

# For datatable
Results.CEE.Horiuchi.tab  <- Results.CEE.Horiuchi.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.CEE.Horiuchi.tab <- data.table(Results.CEE.Horiuchi.tab)
Results.CEE.Horiuchi.tab$Region <- "CEE"

#####
# FSR countries
#####
# Period 7
FSR_7 <- Mx_CoD_regions %>% 
  filter(Region==5 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
FSR_M_COD_7 <- as.matrix(FSR_7[FSR_7$Sex=="m",24:34])
FSR_F_COD_7  <- as.matrix(FSR_7[FSR_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.FSR.Horiuchi_7 <- horiuchi(func = sdfrommxc, 
                                   pars1 = c(FSR_F_COD_7),
                                   pars2 = c(FSR_M_COD_7),
                                   N=100)

# ----- Fourth: Define dimention of matrix
dim(Results.FSR.Horiuchi_7) <- dim(FSR_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.FSR.Horiuchi_7 <- data.frame(Results.FSR.Horiuchi_7)
colnames(Results.FSR.Horiuchi_7) <- cause_names
Results.FSR.Horiuchi_7$Age <- c(seq(0,100,1))
Results.FSR.Horiuchi_7 <- gather(data = Results.FSR.Horiuchi_7,key = Cause,value = Contribution,-Age)
Results.FSR.Horiuchi_7$Age2       <- (cut(Results.FSR.Horiuchi_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FSR.Horiuchi_7 <- data.table(Results.FSR.Horiuchi_7)
Results.FSR.Horiuchi_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.FSR.Horiuchi <- data.table(Results.FSR.Horiuchi_7)
Results.FSR.Horiuchi$Period <- factor(Results.FSR.Horiuchi$Period, levels = c(5,6,7),
                                      labels =   c("2000-2004", "2005-2009",
                                                   "2010-2015"))

# For plotting
Results.FSR.Horiuchi.plot <- Results.FSR.Horiuchi[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.FSR.Horiuchi.plot$Region <- "FSR"

# For datatable
Results.FSR.Horiuchi.tab  <- Results.FSR.Horiuchi.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.FSR.Horiuchi.tab <- data.table(Results.FSR.Horiuchi.tab)
Results.FSR.Horiuchi.tab$Region <- "FSR"


#####
# Europe
#####

# Period 7
Europe_7 <- Mx_CoD_Europe %>% 
  filter(Region==6 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Europe_M_COD_7 <- as.matrix(Europe_7[Europe_7$Sex=="m",24:34])
Europe_F_COD_7  <- as.matrix(Europe_7[Europe_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Europe.Horiuchi_7 <- horiuchi(func = sdfrommxc, 
                                      pars1 = c(Europe_F_COD_7),
                                      pars2 = c(Europe_M_COD_7),
                                      N=100)

# ----- Fourth: Define dimention of matrix
dim(Results.Europe.Horiuchi_7) <- dim(Europe_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Europe.Horiuchi_7 <- data.frame(Results.Europe.Horiuchi_7)
colnames(Results.Europe.Horiuchi_7) <- cause_names
Results.Europe.Horiuchi_7$Age <- c(seq(0,100,1))
Results.Europe.Horiuchi_7 <- gather(data = Results.Europe.Horiuchi_7,key = Cause,value = Contribution,-Age)
Results.Europe.Horiuchi_7$Age2       <- (cut(Results.Europe.Horiuchi_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Europe.Horiuchi_7 <- data.table(Results.Europe.Horiuchi_7)
Results.Europe.Horiuchi_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Europe.Horiuchi <- data.table(Results.Europe.Horiuchi_7)
Results.Europe.Horiuchi$Period <- factor(Results.Europe.Horiuchi$Period, levels = c(5,6,7),
                                         labels =   c("2000-2004", "2005-2009",
                                                      "2010-2015"))

# For plotting
Results.Europe.Horiuchi.plot <- Results.Europe.Horiuchi[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Europe.Horiuchi.plot$Region <- "Europe"

# For datatable
Results.Europe.Horiuchi.tab  <- Results.Europe.Horiuchi.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Europe.Horiuchi.tab <- data.table(Results.Europe.Horiuchi.tab)
Results.Europe.Horiuchi.tab$Region <- "Europe"

# -------------------------------------------------------
#         Combine regions
# -------------------------------------------------------

Data_decomp_Horiuchi <- rbind(Results.Nordic.Horiuchi.plot,
                              Results.Western.Horiuchi.plot,
                              Results.Southern.Horiuchi.plot,
                              Results.CEE.Horiuchi.plot,
                              Results.FSR.Horiuchi.plot,
                              Results.Europe.Horiuchi.plot)

# -- Causes of death labeling
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="C. Sesitive Smoking"]  <- 1
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Sex-specif cancer"]  <- 2
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Other Cancers"]  <- 3
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="IHD and Stroke"]  <- 4
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Rest of Circulatory diseases"]  <- 5
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Mental and Nervous system"]  <- 6
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Alcohol attributable"]  <- 7
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="External"]  <- 8
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Infectious (respiratory diseases)"]  <- 9
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Non infectious respiratory disases"]  <- 10
Data_decomp_Horiuchi$Cause2[Data_decomp_Horiuchi$Cause=="Other No Infections No respiratory"]  <- 11


Data_Fig_Horiuchi <- Data_decomp_Horiuchi %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6)) %>% 
  filter(Period=="2010-2015") 

# --- Labeling

# Causes of death
Data_Fig_Horiuchi$Cause2 <- factor(Data_Fig_Horiuchi$Cause2, levels = c(1:11),
                                   labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                              "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                              "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                              "Non infectious respiratory disases","Rest of causes"))


# Measure
Data_Fig_Horiuchi$Measure_2 <- factor(Data_Fig_Horiuchi$Measure_2,
                                      levels = c(1,2),
                                      labels = c("Life expectancy (e0)", "Lifespan variation (SD)"))

# Region
Data_Fig_Horiuchi$Region_2 <- factor(Data_Fig_Horiuchi$Region_2,
                                     levels = c(1,2,3,4,5,6),
                                     labels = c("Europe",
                                                "Nordic",
                                                "Western",
                                                "Southern",
                                                "CEE",
                                                "FSR"))

Fig_3 <- ggplot(data=Data_Fig_Horiuchi,
                aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  #scale_fill_brewer(palette =  "Paired") +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    y= "",
    x="")
Fig_3
ggsave(filename = "Figure 3 SM-4.png", path= "Supplementary Material/4. Sensitivity analysis/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")



# ------------------------------------------------------------------- #
#       2.2 Decomposition lifespan variation condition from age 15
# ------------------------------------------------------------------- #

#####
# Nordic countries
#####
# Period 7
Nordic_7 <- Mx_CoD_regions %>% 
  filter(Region==1 &  Age>=16 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Nordic_M_COD_7 <- as.matrix(Nordic_7[Nordic_7$Sex=="m",24:34])
Nordic_F_COD_7  <- as.matrix(Nordic_7[Nordic_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Nordic.Conditioning15_7 <- stepwise_replacement(func = sdfrommxc_15,
                                                        pars1 = c(Nordic_F_COD_7), 
                                                        pars2 = c(Nordic_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Nordic.Conditioning15_7) <- dim(Nordic_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Nordic.Conditioning15_7 <- data.frame(Results.Nordic.Conditioning15_7)
colnames(Results.Nordic.Conditioning15_7) <- cause_names
Results.Nordic.Conditioning15_7$Age <- c(seq(15,100,1))
Results.Nordic.Conditioning15_7 <- gather(data = Results.Nordic.Conditioning15_7,key = Cause,value = Contribution,-Age)
Results.Nordic.Conditioning15_7$Age2       <- (cut(Results.Nordic.Conditioning15_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Nordic.Conditioning15_7 <- data.table(Results.Nordic.Conditioning15_7)
Results.Nordic.Conditioning15_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Nordic.Conditioning15 <- data.table(Results.Nordic.Conditioning15_7)
Results.Nordic.Conditioning15$Period <- factor(Results.Nordic.Conditioning15$Period, levels = c(5,6,7),
                                               labels =   c("2000-2004", "2005-2009",
                                                            "2010-2015"))

# For plotting
Results.Nordic.Conditioning15.plot <- Results.Nordic.Conditioning15[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Nordic.Conditioning15.plot$Region <- "Nordic"

# For datatable
Results.Nordic.Conditioning15.tab  <- Results.Nordic.Conditioning15.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Nordic.Conditioning15.tab <- data.table(Results.Nordic.Conditioning15.tab)
Results.Nordic.Conditioning15.tab$Region <- "Nordic"

#####
# Western countries
#####

Western_7 <- Mx_CoD_regions %>% 
  filter(Region==2 &  Age>=16 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Western_M_COD_7 <- as.matrix(Western_7[Western_7$Sex=="m",24:34])
Western_F_COD_7  <- as.matrix(Western_7[Western_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Western.Conditioning15_7 <- stepwise_replacement(func = sdfrommxc_15,
                                                         pars1 = c(Western_F_COD_7),
                                                         pars2 = c(Western_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Western.Conditioning15_7) <- dim(Western_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Western.Conditioning15_7 <- data.frame(Results.Western.Conditioning15_7)
colnames(Results.Western.Conditioning15_7) <- cause_names
Results.Western.Conditioning15_7$Age <- c(seq(15,100,1))
Results.Western.Conditioning15_7 <- gather(data = Results.Western.Conditioning15_7,key = Cause,value = Contribution,-Age)
Results.Western.Conditioning15_7$Age2       <- (cut(Results.Western.Conditioning15_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Western.Conditioning15_7 <- data.table(Results.Western.Conditioning15_7)
Results.Western.Conditioning15_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Western.Conditioning15 <- data.table(Results.Western.Conditioning15_7)
Results.Western.Conditioning15$Period <- factor(Results.Western.Conditioning15$Period, levels = c(5,6,7),
                                                labels =   c("2000-2004", "2005-2009",
                                                             "2010-2015"))

# For plotting
Results.Western.Conditioning15.plot <- Results.Western.Conditioning15[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Western.Conditioning15.plot$Region <- "Western"

# For datatable
Results.Western.Conditioning15.tab  <- Results.Western.Conditioning15.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Western.Conditioning15.tab <- data.table(Results.Western.Conditioning15.tab)
Results.Western.Conditioning15.tab$Region <- "Western"

#####
# Southern countries
#####

# Period 7
Southern_7 <- Mx_CoD_regions %>% 
  filter(Region==3 &  Age>=16 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Southern_M_COD_7 <- as.matrix(Southern_7[Southern_7$Sex=="m",24:34])
Southern_F_COD_7  <- as.matrix(Southern_7[Southern_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Southern.Conditioning15_7 <- stepwise_replacement(func = sdfrommxc_15, 
                                                          pars1 = c(Southern_F_COD_7), 
                                                          pars2 = c(Southern_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Southern.Conditioning15_7) <- dim(Southern_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Southern.Conditioning15_7 <- data.frame(Results.Southern.Conditioning15_7)
colnames(Results.Southern.Conditioning15_7) <- cause_names
Results.Southern.Conditioning15_7$Age <- c(seq(15,100,1))
Results.Southern.Conditioning15_7 <- gather(data = Results.Southern.Conditioning15_7,key = Cause,value = Contribution,-Age)
Results.Southern.Conditioning15_7$Age2       <- (cut(Results.Southern.Conditioning15_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Southern.Conditioning15_7 <- data.table(Results.Southern.Conditioning15_7)
Results.Southern.Conditioning15_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Southern.Conditioning15 <- data.table(Results.Southern.Conditioning15_7)
Results.Southern.Conditioning15$Period <- factor(Results.Southern.Conditioning15$Period, levels = c(5,6,7),
                                                 labels =   c("2000-2004", "2005-2009",
                                                              "2010-2015"))

# For plotting
Results.Southern.Conditioning15.plot <- Results.Southern.Conditioning15[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Southern.Conditioning15.plot$Region <- "Southern"

# For datatable
Results.Southern.Conditioning15.tab  <- Results.Southern.Conditioning15.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Southern.Conditioning15.tab <- data.table(Results.Southern.Conditioning15.tab)
Results.Southern.Conditioning15.tab$Region <- "Southern"


#####
# CEE countries
#####

# Period 7
CEE_7 <- Mx_CoD_regions %>% 
  filter(Region==4 &  Age>=16 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
CEE_M_COD_7 <- as.matrix(CEE_7[CEE_7$Sex=="m",24:34])
CEE_F_COD_7  <- as.matrix(CEE_7[CEE_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.CEE.Conditioning15_7 <- stepwise_replacement(func = sdfrommxc_15, 
                                                     pars1 = c(CEE_F_COD_7), 
                                                     pars2 = c(CEE_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.CEE.Conditioning15_7) <- dim(CEE_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.CEE.Conditioning15_7 <- data.frame(Results.CEE.Conditioning15_7)
colnames(Results.CEE.Conditioning15_7) <- cause_names
Results.CEE.Conditioning15_7$Age <- c(seq(15,100,1))
Results.CEE.Conditioning15_7 <- gather(data = Results.CEE.Conditioning15_7,key = Cause,value = Contribution,-Age)
Results.CEE.Conditioning15_7$Age2       <- (cut(Results.CEE.Conditioning15_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CEE.Conditioning15_7 <- data.table(Results.CEE.Conditioning15_7)
Results.CEE.Conditioning15_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.CEE.Conditioning15 <- data.table(Results.CEE.Conditioning15_7)
Results.CEE.Conditioning15$Period <- factor(Results.CEE.Conditioning15$Period, levels = c(5,6,7),
                                            labels =   c("2000-2004", "2005-2009",
                                                         "2010-2015"))

# For plotting
Results.CEE.Conditioning15.plot <- Results.CEE.Conditioning15[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.CEE.Conditioning15.plot$Region <- "CEE"

# For datatable
Results.CEE.Conditioning15.tab  <- Results.CEE.Conditioning15.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.CEE.Conditioning15.tab <- data.table(Results.CEE.Conditioning15.tab)
Results.CEE.Conditioning15.tab$Region <- "CEE"

#####
# FSR countries
#####
# Period 7
FSR_7 <- Mx_CoD_regions %>% 
  filter(Region==5 &  Age>=16 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
FSR_M_COD_7 <- as.matrix(FSR_7[FSR_7$Sex=="m",24:34])
FSR_F_COD_7  <- as.matrix(FSR_7[FSR_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.FSR.Conditioning15_7 <- stepwise_replacement(func = sdfrommxc_15, 
                                                     pars1 = c(FSR_F_COD_7), 
                                                     pars2 = c(FSR_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.FSR.Conditioning15_7) <- dim(FSR_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.FSR.Conditioning15_7 <- data.frame(Results.FSR.Conditioning15_7)
colnames(Results.FSR.Conditioning15_7) <- cause_names
Results.FSR.Conditioning15_7$Age <- c(seq(15,100,1))
Results.FSR.Conditioning15_7 <- gather(data = Results.FSR.Conditioning15_7,key = Cause,value = Contribution,-Age)
Results.FSR.Conditioning15_7$Age2       <- (cut(Results.FSR.Conditioning15_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FSR.Conditioning15_7 <- data.table(Results.FSR.Conditioning15_7)
Results.FSR.Conditioning15_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.FSR.Conditioning15 <- data.table(Results.FSR.Conditioning15_7)
Results.FSR.Conditioning15$Period <- factor(Results.FSR.Conditioning15$Period, levels = c(5,6,7),
                                            labels =   c("2000-2004", "2005-2009",
                                                         "2010-2015"))

# For plotting
Results.FSR.Conditioning15.plot <- Results.FSR.Conditioning15[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.FSR.Conditioning15.plot$Region <- "FSR"

# For datatable
Results.FSR.Conditioning15.tab  <- Results.FSR.Conditioning15.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.FSR.Conditioning15.tab <- data.table(Results.FSR.Conditioning15.tab)
Results.FSR.Conditioning15.tab$Region <- "FSR"


#####
# Europe
#####

# Period 7
Europe_7 <- Mx_CoD_Europe %>% 
  filter(Region==6 & Age>=16 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Europe_M_COD_7 <- as.matrix(Europe_7[Europe_7$Sex=="m",24:34])
Europe_F_COD_7  <- as.matrix(Europe_7[Europe_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Europe.Conditioning15_7 <- stepwise_replacement(func = sdfrommxc_15, 
                                                        pars1 = c(Europe_F_COD_7),
                                                        pars2 = c(Europe_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Europe.Conditioning15_7) <- dim(Europe_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Europe.Conditioning15_7 <- data.frame(Results.Europe.Conditioning15_7)
colnames(Results.Europe.Conditioning15_7) <- cause_names
Results.Europe.Conditioning15_7$Age <- c(seq(15,100,1))
Results.Europe.Conditioning15_7 <- gather(data = Results.Europe.Conditioning15_7,key = Cause,value = Contribution,-Age)
Results.Europe.Conditioning15_7$Age2       <- (cut(Results.Europe.Conditioning15_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Europe.Conditioning15_7 <- data.table(Results.Europe.Conditioning15_7)
Results.Europe.Conditioning15_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.Europe.Conditioning15 <- data.table(Results.Europe.Conditioning15_7)
Results.Europe.Conditioning15$Period <- factor(Results.Europe.Conditioning15$Period, levels = c(5,6,7),
                                               labels =   c("2000-2004", "2005-2009",
                                                            "2010-2015"))

# For plotting
Results.Europe.Conditioning15.plot <- Results.Europe.Conditioning15[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Europe.Conditioning15.plot$Region <- "Europe"

# For datatable
Results.Europe.Conditioning15.tab  <- Results.Europe.Conditioning15.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Europe.Conditioning15.tab <- data.table(Results.Europe.Conditioning15.tab)
Results.Europe.Conditioning15.tab$Region <- "Europe"

# -------------------------------------------------------
#         Combine regions
# -------------------------------------------------------

Data_decomp_Conditioning15 <- rbind(Results.Nordic.Conditioning15.plot,
                                    Results.Western.Conditioning15.plot,
                                    Results.Southern.Conditioning15.plot,
                                    Results.CEE.Conditioning15.plot,
                                    Results.FSR.Conditioning15.plot,
                                    Results.Europe.Conditioning15.plot)


# Table e-dagger
Table_S2 <- Data_decomp_Conditioning15[,list(SD=sum(Contribution2)),
                                       by=list(Period, Region)]
write.csv(Table_S2, file = "Supplementary Material/4. Sensitivity analysis/Table 2 SM-4.csv")



