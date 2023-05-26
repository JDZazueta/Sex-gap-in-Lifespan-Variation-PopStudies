# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Decomosition sex gap in life expectancy by regions & periods
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Decomposition of e0 Europe by period
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
#   2. Decomposition e0 Europe
# ------------------------------------------------------------------- #


#####
# Europe
#####

# Period 5
Europe_5 <- Mx_CoD_Europe %>% 
  filter(Region==6 & Age<=101 & Periods==2000)
# Period 6
Europe_6 <- Mx_CoD_Europe %>% 
  filter(Region==6 & Age<=101 & Periods==2005)
# Period 7
Europe_7 <- Mx_CoD_Europe %>% 
  filter(Region==6 & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 5
Europe_M_COD_5 <- as.matrix(Europe_5[Europe_5$Sex=="m",24:34])
Europe_F_COD_5  <- as.matrix(Europe_5[Europe_5$Sex=="f",24:34])
# Period 6
Europe_M_COD_6 <- as.matrix(Europe_6[Europe_6$Sex=="m",24:34])
Europe_F_COD_6  <- as.matrix(Europe_6[Europe_6$Sex=="f",24:34])
# Period 7
Europe_M_COD_7 <- as.matrix(Europe_7[Europe_7$Sex=="m",24:34])
Europe_F_COD_7  <- as.matrix(Europe_7[Europe_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Europe.e0_5 <- stepwise_replacement(func = e0frommxc, pars1 = c(Europe_M_COD_5), pars2 = c(Europe_F_COD_5))
Results.Europe.e0_6 <- stepwise_replacement(func = e0frommxc, pars1 = c(Europe_M_COD_6), pars2 = c(Europe_F_COD_6))
Results.Europe.e0_7 <- stepwise_replacement(func = e0frommxc, pars1 = c(Europe_M_COD_7), pars2 = c(Europe_F_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Europe.e0_5) <- dim(Europe_M_COD_5) 
dim(Results.Europe.e0_6) <- dim(Europe_M_COD_6) 
dim(Results.Europe.e0_7) <- dim(Europe_M_COD_7) 

# ----- Fifth: Arrange data


# Period 5
Results.Europe.e0_5 <- data.frame(Results.Europe.e0_5)
colnames(Results.Europe.e0_5) <- cause_names
Results.Europe.e0_5$Age <- c(seq(0,100,1))
Results.Europe.e0_5 <- gather(data = Results.Europe.e0_5,key = Cause,value = Contribution,-Age)
Results.Europe.e0_5$Age2       <- (cut(Results.Europe.e0_5$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Europe.e0_5 <- data.table(Results.Europe.e0_5)
Results.Europe.e0_5$Period <- 5

# Period 6
Results.Europe.e0_6 <- data.frame(Results.Europe.e0_6)
colnames(Results.Europe.e0_6) <- cause_names
Results.Europe.e0_6$Age <- c(seq(0,100,1))
Results.Europe.e0_6 <- gather(data = Results.Europe.e0_6,key = Cause,value = Contribution,-Age)
Results.Europe.e0_6$Age2       <- (cut(Results.Europe.e0_6$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Europe.e0_6 <- data.table(Results.Europe.e0_6)
Results.Europe.e0_6$Period <- 6

# Period 7
Results.Europe.e0_7 <- data.frame(Results.Europe.e0_7)
colnames(Results.Europe.e0_7) <- cause_names
Results.Europe.e0_7$Age <- c(seq(0,100,1))
Results.Europe.e0_7 <- gather(data = Results.Europe.e0_7,key = Cause,value = Contribution,-Age)
Results.Europe.e0_7$Age2       <- (cut(Results.Europe.e0_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.Europe.e0_7 <- data.table(Results.Europe.e0_7)
Results.Europe.e0_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.Europe.e0 <- rbind(Results.Europe.e0_5,Results.Europe.e0_6,
                        Results.Europe.e0_7)


Results.Europe.e0 <- data.table(Results.Europe.e0)
Results.Europe.e0$Period <- factor(Results.Europe.e0$Period, levels = c(5,6,7),
                                labels =   c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.Europe.e0.plot <- Results.Europe.e0[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Europe.e0.plot$Region <- "Europe"

# For datatable
Results.Europe.e0.tab  <- Results.Europe.e0.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Europe.e0.tab <- data.table(Results.Europe.e0.tab)
Results.Europe.e0.tab$Region <- "Europe"


# -- Plot of decomposition result
Europe_SG_Deco <- ggplot(data=Results.Europe.e0.plot, aes(x=Age2, y=Contribution2, fill=Cause)) +
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
  labs(title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
       y=bquote(~'Change in Sex gap in '~ e[0]),
       x="Age")
Europe_SG_Deco


# -------------------------------------------------------
#         Save data
# -------------------------------------------------------
Europe_decomp_e0 <- Results.Europe.e0.plot
save(Europe_decomp_e0, file = "Data/Decomposition_e0_Europe.RData")
