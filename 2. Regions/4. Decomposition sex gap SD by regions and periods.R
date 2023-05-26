# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Decomosition sex gap in lifespan varaition by regions & periods
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Decomposition of SD by region and period
#   3. Save the data
#   4. Combine datasets

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

# ------------------------------------------------------------------- #
#   2. Decomposition by Region
# ------------------------------------------------------------------- #

#####
# Nordic countries
#####

# Period 5
Nordic_5 <- Mx_CoD_regions %>% 
  filter(Region==1 & Age<=101 & Periods==2000)
# Period 6
Nordic_6 <- Mx_CoD_regions %>% 
  filter(Region==1 & Age<=101 & Periods==2005)
# Period 7
Nordic_7 <- Mx_CoD_regions %>% 
  filter(Region==1 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 5
Nordic_M_COD_5 <- as.matrix(Nordic_5[Nordic_5$Sex=="m",24:34])
Nordic_F_COD_5  <- as.matrix(Nordic_5[Nordic_5$Sex=="f",24:34])
# Period 6
Nordic_M_COD_6 <- as.matrix(Nordic_6[Nordic_6$Sex=="m",24:34])
Nordic_F_COD_6  <- as.matrix(Nordic_6[Nordic_6$Sex=="f",24:34])
# Period 7
Nordic_M_COD_7 <- as.matrix(Nordic_7[Nordic_7$Sex=="m",24:34])
Nordic_F_COD_7  <- as.matrix(Nordic_7[Nordic_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Nordic.SD_5 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Nordic_F_COD_5), pars2 = c(Nordic_M_COD_5))
Results.Nordic.SD_6 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Nordic_F_COD_6), pars2 = c(Nordic_M_COD_6))
Results.Nordic.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Nordic_F_COD_7), pars2 = c(Nordic_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Nordic.SD_5) <- dim(Nordic_M_COD_5) 
dim(Results.Nordic.SD_6) <- dim(Nordic_M_COD_6) 
dim(Results.Nordic.SD_7) <- dim(Nordic_M_COD_7) 

# ----- Fifth: Arrange data


# Period 5
Results.Nordic.SD_5 <- data.frame(Results.Nordic.SD_5)
colnames(Results.Nordic.SD_5) <- cause_names
Results.Nordic.SD_5$Age <- c(seq(0,100,1))
Results.Nordic.SD_5 <- gather(data = Results.Nordic.SD_5, key = Cause, value = Contribution,-Age)
Results.Nordic.SD_5$Age2       <- (cut(Results.Nordic.SD_5$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Nordic.SD_5 <- data.table(Results.Nordic.SD_5)
Results.Nordic.SD_5$Period <- 5

# Period 6
Results.Nordic.SD_6 <- data.frame(Results.Nordic.SD_6)
colnames(Results.Nordic.SD_6) <- cause_names
Results.Nordic.SD_6$Age <- c(seq(0,100,1))
Results.Nordic.SD_6 <- gather(data = Results.Nordic.SD_6,key = Cause,value = Contribution,-Age)
Results.Nordic.SD_6$Age2       <- (cut(Results.Nordic.SD_6$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Nordic.SD_6 <- data.table(Results.Nordic.SD_6)
Results.Nordic.SD_6$Period <- 6

# Period 7
Results.Nordic.SD_7 <- data.frame(Results.Nordic.SD_7)
colnames(Results.Nordic.SD_7) <- cause_names
Results.Nordic.SD_7$Age <- c(seq(0,100,1))
Results.Nordic.SD_7 <- gather(data = Results.Nordic.SD_7,key = Cause,value = Contribution,-Age)
Results.Nordic.SD_7$Age2       <- (cut(Results.Nordic.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Nordic.SD_7 <- data.table(Results.Nordic.SD_7)
Results.Nordic.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.Nordic.SD <- rbind(Results.Nordic.SD_5,Results.Nordic.SD_6,
                           Results.Nordic.SD_7)


Results.Nordic.SD <- data.table(Results.Nordic.SD)
Results.Nordic.SD$Period <- factor(Results.Nordic.SD$Period, levels = c(5,6,7),
                                   labels =   c("2000-2004", "2005-2009",
                                                "2010-2015"))

# For plotting
Results.Nordic.SD.plot <- Results.Nordic.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Nordic.SD.plot$Region <- "Nordic"

# For datatable
Results.Nordic.SD.tab  <- Results.Nordic.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Nordic.SD.tab <- data.table(Results.Nordic.SD.tab)
Results.Nordic.SD.tab$Region <- "Nordic"

# -- Plot of decomposition result
Nordic_SG_Deco <- ggplot(data=Results.Nordic.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Nordic' ),
       y=bquote(~'Change in Sex gap in '~ e[0]),
       x="Age")
Nordic_SG_Deco


#####
# Western countries
#####

# Period 5
Western_5 <- Mx_CoD_regions %>% 
  filter(Region==2 & Age<=101 & Periods==2000)
# Period 6
Western_6 <- Mx_CoD_regions %>% 
  filter(Region==2 & Age<=101 & Periods==2005)
# Period 7
Western_7 <- Mx_CoD_regions %>% 
  filter(Region==2 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 5
Western_M_COD_5 <- as.matrix(Western_5[Western_5$Sex=="m",24:34])
Western_F_COD_5  <- as.matrix(Western_5[Western_5$Sex=="f",24:34])
# Period 6
Western_M_COD_6 <- as.matrix(Western_6[Western_6$Sex=="m",24:34])
Western_F_COD_6  <- as.matrix(Western_6[Western_6$Sex=="f",24:34])
# Period 7
Western_M_COD_7 <- as.matrix(Western_7[Western_7$Sex=="m",24:34])
Western_F_COD_7  <- as.matrix(Western_7[Western_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Western.SD_5 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Western_F_COD_5), pars2 = c(Western_M_COD_5))
Results.Western.SD_6 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Western_F_COD_6), pars2 = c(Western_M_COD_6))
Results.Western.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Western_F_COD_7), pars2 = c(Western_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Western.SD_5) <- dim(Western_M_COD_5) 
dim(Results.Western.SD_6) <- dim(Western_M_COD_6) 
dim(Results.Western.SD_7) <- dim(Western_M_COD_7) 

# ----- Fifth: Arrange data


# Period 5
Results.Western.SD_5 <- data.frame(Results.Western.SD_5)
colnames(Results.Western.SD_5) <- cause_names
Results.Western.SD_5$Age <- c(seq(0,100,1))
Results.Western.SD_5 <- gather(data = Results.Western.SD_5,key = Cause,value = Contribution,-Age)
Results.Western.SD_5$Age2       <- (cut(Results.Western.SD_5$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Western.SD_5 <- data.table(Results.Western.SD_5)
Results.Western.SD_5$Period <- 5

# Period 6
Results.Western.SD_6 <- data.frame(Results.Western.SD_6)
colnames(Results.Western.SD_6) <- cause_names
Results.Western.SD_6$Age <- c(seq(0,100,1))
Results.Western.SD_6 <- gather(data = Results.Western.SD_6,key = Cause,value = Contribution,-Age)
Results.Western.SD_6$Age2       <- (cut(Results.Western.SD_6$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Western.SD_6 <- data.table(Results.Western.SD_6)
Results.Western.SD_6$Period <- 6

# Period 7
Results.Western.SD_7 <- data.frame(Results.Western.SD_7)
colnames(Results.Western.SD_7) <- cause_names
Results.Western.SD_7$Age <- c(seq(0,100,1))
Results.Western.SD_7 <- gather(data = Results.Western.SD_7,key = Cause,value = Contribution,-Age)
Results.Western.SD_7$Age2       <- (cut(Results.Western.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Western.SD_7 <- data.table(Results.Western.SD_7)
Results.Western.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.Western.SD <- rbind(Results.Western.SD_5,Results.Western.SD_6,
                            Results.Western.SD_7)


Results.Western.SD <- data.table(Results.Western.SD)
Results.Western.SD$Period <- factor(Results.Western.SD$Period, levels = c(5,6,7),
                                    labels =   c("2000-2004", "2005-2009",
                                                 "2010-2015"))

# For plotting
Results.Western.SD.plot <- Results.Western.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Western.SD.plot$Region <- "Western"

# For datatable
Results.Western.SD.tab  <- Results.Western.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Western.SD.tab <- data.table(Results.Western.SD.tab)
Results.Western.SD.tab$Region <- "Western"


# -- Plot of decomposition result
Western_SG_Deco <- ggplot(data=Results.Western.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Western' ),
       y=bquote(~'Change in Sex gap in '~ e[0]),
       x="Age")
Western_SG_Deco


#####
# Southern countries
#####

# Period 5
Southern_5 <- Mx_CoD_regions %>% 
  filter(Region==3 & Age<=101 & Periods==2000)
# Period 6
Southern_6 <- Mx_CoD_regions %>% 
  filter(Region==3 & Age<=101 & Periods==2005)
# Period 7
Southern_7 <- Mx_CoD_regions %>% 
  filter(Region==3 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 5
Southern_M_COD_5 <- as.matrix(Southern_5[Southern_5$Sex=="m",24:34])
Southern_F_COD_5  <- as.matrix(Southern_5[Southern_5$Sex=="f",24:34])
# Period 6
Southern_M_COD_6 <- as.matrix(Southern_6[Southern_6$Sex=="m",24:34])
Southern_F_COD_6  <- as.matrix(Southern_6[Southern_6$Sex=="f",24:34])
# Period 7
Southern_M_COD_7 <- as.matrix(Southern_7[Southern_7$Sex=="m",24:34])
Southern_F_COD_7  <- as.matrix(Southern_7[Southern_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Southern.SD_5 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Southern_F_COD_5), pars2 = c(Southern_M_COD_5))
Results.Southern.SD_6 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Southern_F_COD_6), pars2 = c(Southern_M_COD_6))
Results.Southern.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Southern_F_COD_7), pars2 = c(Southern_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Southern.SD_5) <- dim(Southern_M_COD_5) 
dim(Results.Southern.SD_6) <- dim(Southern_M_COD_6) 
dim(Results.Southern.SD_7) <- dim(Southern_M_COD_7) 

# ----- Fifth: Arrange data

# Period 5
Results.Southern.SD_5 <- data.frame(Results.Southern.SD_5)
colnames(Results.Southern.SD_5) <- cause_names
Results.Southern.SD_5$Age <- c(seq(0,100,1))
Results.Southern.SD_5 <- gather(data = Results.Southern.SD_5,key = Cause,value = Contribution,-Age)
Results.Southern.SD_5$Age2       <- (cut(Results.Southern.SD_5$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Southern.SD_5 <- data.table(Results.Southern.SD_5)
Results.Southern.SD_5$Period <- 5

# Period 6
Results.Southern.SD_6 <- data.frame(Results.Southern.SD_6)
colnames(Results.Southern.SD_6) <- cause_names
Results.Southern.SD_6$Age <- c(seq(0,100,1))
Results.Southern.SD_6 <- gather(data = Results.Southern.SD_6,key = Cause,value = Contribution,-Age)
Results.Southern.SD_6$Age2       <- (cut(Results.Southern.SD_6$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Southern.SD_6 <- data.table(Results.Southern.SD_6)
Results.Southern.SD_6$Period <- 6

# Period 7
Results.Southern.SD_7 <- data.frame(Results.Southern.SD_7)
colnames(Results.Southern.SD_7) <- cause_names
Results.Southern.SD_7$Age <- c(seq(0,100,1))
Results.Southern.SD_7 <- gather(data = Results.Southern.SD_7,key = Cause,value = Contribution,-Age)
Results.Southern.SD_7$Age2       <- (cut(Results.Southern.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Southern.SD_7 <- data.table(Results.Southern.SD_7)
Results.Southern.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.Southern.SD <- rbind(Results.Southern.SD_5,Results.Southern.SD_6,
                             Results.Southern.SD_7)


Results.Southern.SD <- data.table(Results.Southern.SD)
Results.Southern.SD$Period <- factor(Results.Southern.SD$Period, levels = c(5,6,7),
                                     labels =   c("2000-2004", "2005-2009",
                                                  "2010-2015"))

# For plotting
Results.Southern.SD.plot <- Results.Southern.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Southern.SD.plot$Region <- "Southern"

# For datatable
Results.Southern.SD.tab  <- Results.Southern.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Southern.SD.tab <- data.table(Results.Southern.SD.tab)
Results.Southern.SD.tab$Region <- "Southern"


# -- Plot of decomposition result
Southern_SG_Deco <- ggplot(data=Results.Southern.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Southern' ),
       y=bquote(~'Change in Sex gap in '~ e[0]),
       x="Age")
Southern_SG_Deco



#####
# CEE countries
#####

# Period 5
CEE_5 <- Mx_CoD_regions %>% 
  filter(Region==4 & Age<=101 & Periods==2000)
# Period 6
CEE_6 <- Mx_CoD_regions %>% 
  filter(Region==4 & Age<=101 & Periods==2005)
# Period 7
CEE_7 <- Mx_CoD_regions %>% 
  filter(Region==4 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 5
CEE_M_COD_5 <- as.matrix(CEE_5[CEE_5$Sex=="m",24:34])
CEE_F_COD_5  <- as.matrix(CEE_5[CEE_5$Sex=="f",24:34])
# Period 6
CEE_M_COD_6 <- as.matrix(CEE_6[CEE_6$Sex=="m",24:34])
CEE_F_COD_6  <- as.matrix(CEE_6[CEE_6$Sex=="f",24:34])
# Period 7
CEE_M_COD_7 <- as.matrix(CEE_7[CEE_7$Sex=="m",24:34])
CEE_F_COD_7  <- as.matrix(CEE_7[CEE_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.CEE.SD_5 <- stepwise_replacement(func = sdfrommxc, pars1 = c(CEE_F_COD_5), pars2 = c(CEE_M_COD_5))
Results.CEE.SD_6 <- stepwise_replacement(func = sdfrommxc, pars1 = c(CEE_F_COD_6), pars2 = c(CEE_M_COD_6))
Results.CEE.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(CEE_F_COD_7), pars2 = c(CEE_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.CEE.SD_5) <- dim(CEE_M_COD_5) 
dim(Results.CEE.SD_6) <- dim(CEE_M_COD_6) 
dim(Results.CEE.SD_7) <- dim(CEE_M_COD_7) 

# ----- Fifth: Arrange data


# Period 5
Results.CEE.SD_5 <- data.frame(Results.CEE.SD_5)
colnames(Results.CEE.SD_5) <- cause_names
Results.CEE.SD_5$Age <- c(seq(0,100,1))
Results.CEE.SD_5 <- gather(data = Results.CEE.SD_5,key = Cause,value = Contribution,-Age)
Results.CEE.SD_5$Age2       <- (cut(Results.CEE.SD_5$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CEE.SD_5 <- data.table(Results.CEE.SD_5)
Results.CEE.SD_5$Period <- 5

# Period 6
Results.CEE.SD_6 <- data.frame(Results.CEE.SD_6)
colnames(Results.CEE.SD_6) <- cause_names
Results.CEE.SD_6$Age <- c(seq(0,100,1))
Results.CEE.SD_6 <- gather(data = Results.CEE.SD_6,key = Cause,value = Contribution,-Age)
Results.CEE.SD_6$Age2       <- (cut(Results.CEE.SD_6$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CEE.SD_6 <- data.table(Results.CEE.SD_6)
Results.CEE.SD_6$Period <- 6

# Period 7
Results.CEE.SD_7 <- data.frame(Results.CEE.SD_7)
colnames(Results.CEE.SD_7) <- cause_names
Results.CEE.SD_7$Age <- c(seq(0,100,1))
Results.CEE.SD_7 <- gather(data = Results.CEE.SD_7,key = Cause,value = Contribution,-Age)
Results.CEE.SD_7$Age2       <- (cut(Results.CEE.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CEE.SD_7 <- data.table(Results.CEE.SD_7)
Results.CEE.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.CEE.SD <- rbind(Results.CEE.SD_5,Results.CEE.SD_6,
                        Results.CEE.SD_7)


Results.CEE.SD <- data.table(Results.CEE.SD)
Results.CEE.SD$Period <- factor(Results.CEE.SD$Period, levels = c(5,6,7),
                                labels =   c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.CEE.SD.plot <- Results.CEE.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.CEE.SD.plot$Region <- "CEE"

# For datatable
Results.CEE.SD.tab  <- Results.CEE.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.CEE.SD.tab <- data.table(Results.CEE.SD.tab)
Results.CEE.SD.tab$Region <- "CEE"


# -- Plot of decomposition result
CEE_SG_Deco <- ggplot(data=Results.CEE.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, CEE' ),
       y=bquote(~'Change in Sex gap in '~ e[0]),
       x="Age")
CEE_SG_Deco



#####
# FSR countries
#####

# Period 5
FSR_5 <- Mx_CoD_regions %>% 
  filter(Region==5 & Age<=101 & Periods==2000)
# Period 6
FSR_6 <- Mx_CoD_regions %>% 
  filter(Region==5 & Age<=101 & Periods==2005)
# Period 7
FSR_7 <- Mx_CoD_regions %>% 
  filter(Region==5 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 5
FSR_M_COD_5 <- as.matrix(FSR_5[FSR_5$Sex=="m",24:34])
FSR_F_COD_5  <- as.matrix(FSR_5[FSR_5$Sex=="f",24:34])
# Period 6
FSR_M_COD_6 <- as.matrix(FSR_6[FSR_6$Sex=="m",24:34])
FSR_F_COD_6  <- as.matrix(FSR_6[FSR_6$Sex=="f",24:34])
# Period 7
FSR_M_COD_7 <- as.matrix(FSR_7[FSR_7$Sex=="m",24:34])
FSR_F_COD_7  <- as.matrix(FSR_7[FSR_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.FSR.SD_5 <- stepwise_replacement(func = sdfrommxc, pars1 = c(FSR_F_COD_5), pars2 = c(FSR_M_COD_5))
Results.FSR.SD_6 <- stepwise_replacement(func = sdfrommxc, pars1 = c(FSR_F_COD_6), pars2 = c(FSR_M_COD_6))
Results.FSR.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(FSR_F_COD_7), pars2 = c(FSR_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.FSR.SD_5) <- dim(FSR_M_COD_5) 
dim(Results.FSR.SD_6) <- dim(FSR_M_COD_6) 
dim(Results.FSR.SD_7) <- dim(FSR_M_COD_7) 

# ----- Fifth: Arrange data


# Period 5
Results.FSR.SD_5 <- data.frame(Results.FSR.SD_5)
colnames(Results.FSR.SD_5) <- cause_names
Results.FSR.SD_5$Age <- c(seq(0,100,1))
Results.FSR.SD_5 <- gather(data = Results.FSR.SD_5,key = Cause,value = Contribution,-Age)
Results.FSR.SD_5$Age2       <- (cut(Results.FSR.SD_5$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FSR.SD_5 <- data.table(Results.FSR.SD_5)
Results.FSR.SD_5$Period <- 5

# Period 6
Results.FSR.SD_6 <- data.frame(Results.FSR.SD_6)
colnames(Results.FSR.SD_6) <- cause_names
Results.FSR.SD_6$Age <- c(seq(0,100,1))
Results.FSR.SD_6 <- gather(data = Results.FSR.SD_6,key = Cause,value = Contribution,-Age)
Results.FSR.SD_6$Age2       <- (cut(Results.FSR.SD_6$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FSR.SD_6 <- data.table(Results.FSR.SD_6)
Results.FSR.SD_6$Period <- 6

# Period 7
Results.FSR.SD_7 <- data.frame(Results.FSR.SD_7)
colnames(Results.FSR.SD_7) <- cause_names
Results.FSR.SD_7$Age <- c(seq(0,100,1))
Results.FSR.SD_7 <- gather(data = Results.FSR.SD_7,key = Cause,value = Contribution,-Age)
Results.FSR.SD_7$Age2       <- (cut(Results.FSR.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FSR.SD_7 <- data.table(Results.FSR.SD_7)
Results.FSR.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.FSR.SD <- rbind(Results.FSR.SD_5,Results.FSR.SD_6,
                        Results.FSR.SD_7)


Results.FSR.SD <- data.table(Results.FSR.SD)
Results.FSR.SD$Period <- factor(Results.FSR.SD$Period, levels = c(5,6,7),
                                labels =   c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.FSR.SD.plot <- Results.FSR.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.FSR.SD.plot$Region <- "FSR"

# For datatable
Results.FSR.SD.tab  <- Results.FSR.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.FSR.SD.tab <- data.table(Results.FSR.SD.tab)
Results.FSR.SD.tab$Region <- "FSR"


# -- Plot of decomposition result
FSR_SG_Deco <- ggplot(data=Results.FSR.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, FSR' ),
       y=bquote(~'Change in Sex gap in '~ e[0]),
       x="Age")
FSR_SG_Deco


# -------------------------------------------------------
#         Combine regions
# -------------------------------------------------------


All_regions_decomp_SD <- rbind(Results.Nordic.SD.plot,
                               Results.Western.SD.plot,
                               Results.Southern.SD.plot,
                               Results.CEE.SD.plot,
                               Results.FSR.SD.plot)

save(All_regions_decomp_SD, file = "Data/Decomposition_SD_regions.RData")





