# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Age and causes of death decomposition to the sex gap in 
#          life expectancy
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# Content:
#   0. Working directory, package, and functions
#   1. Open data sets and labesl and 
#   2. Decomposition of SD by country and period
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

# ------------------------------------------------------------------- #
#   1.  Open data sets
# ------------------------------------------------------------------- #

get(load("Data/Mx_CoD_countries.RData"))

Labels.age            <- c('0','1-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                           '40-44','45-49','50-54','55-59','60-64','65-69',
                           "70-74","75-79","80-84","85-89","90-94","95-99","100+")
Labels.period         <- c("2000-2004", "2005-2009",
                           "2010-2015")

Mx_CoD_countries_Periods <- Mx_CoD_countries

Mx_CoD_countries_Periods$Age <- as.numeric(Mx_CoD_countries_Periods$Age)

Mx_CoD_countries_Periods <- Mx_CoD_countries_Periods %>% 
  dplyr::select(-c(24))


# ------------------------------------------------------------------- #
#   2. Decomposition by Country.name
# ------------------------------------------------------------------- #

#####
# Austria
#####

# ----- First define the period
# Period 1
Austria_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Austria" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Aus_M_COD_7 <- as.matrix(Austria_7[Austria_7$Sex=="m",24:34])
Aus_F_COD_7  <- as.matrix(Austria_7[Austria_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Aus.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Aus_F_COD_7), pars2 = c(Aus_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Aus.SD_7) <- dim(Aus_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Aus.SD_7 <- data.frame(Results.Aus.SD_7)
colnames(Results.Aus.SD_7) <- cause_names
Results.Aus.SD_7$Age <- c(seq(0,100,1))
Results.Aus.SD_7 <- gather(data = Results.Aus.SD_7,key = Cause,value = Contribution,-Age)
Results.Aus.SD_7$Age2       <- (cut(Results.Aus.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.Aus.SD_7 <- data.table(Results.Aus.SD_7)
Results.Aus.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions


Results.Aus.SD <- data.table(Results.Aus.SD_7)
Results.Aus.SD$Period <- factor(Results.Aus.SD$Period, levels = c(5,6,7),
                                labels =  c("2000-2004",
                                            "2005-2009",
                                            "2010-2015"))

# For plotting
Results.Aus.SD.plot <- Results.Aus.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Aus.SD.plot$Country.name <- "Austria"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Other Cancers"]  <- 3
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="External"]  <- 8
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.Aus.SD.plot$Cause2[Results.Aus.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.Aus.SD.plot$Cause2 <- factor(Results.Aus.SD.plot$Cause2, levels = c(1:11),
                                     labels =  c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                 "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                 "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                 "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.Aus.SD.tab  <- Results.Aus.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Aus.SD.tab <- data.table(Results.Aus.SD.tab)
Results.Aus.SD.tab$Country.name <- "Austria"

# -- Plot of decomposition result
Aus_SG_Deco <- ggplot(data=Results.Aus.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Austria' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
Aus_SG_Deco


#####
# Belarus
#####

# ----- First define the period

# Period 1
Belarus_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Belarus" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Bela_M_COD_7 <- as.matrix(Belarus_7[Belarus_7$Sex=="m",24:34])
Bela_F_COD_7  <- as.matrix(Belarus_7[Belarus_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Bela.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Bela_F_COD_7), pars2 = c(Bela_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Bela.SD_7) <- dim(Bela_M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.Bela.SD_7 <- data.frame(Results.Bela.SD_7)
colnames(Results.Bela.SD_7) <- cause_names
Results.Bela.SD_7$Age <- c(seq(0,100,1))
Results.Bela.SD_7 <- gather(data = Results.Bela.SD_7,key = Cause,value = Contribution,-Age)
Results.Bela.SD_7$Age2       <- (cut(Results.Bela.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.Bela.SD_7 <- data.table(Results.Bela.SD_7)
Results.Bela.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.Bela.SD <- Results.Bela.SD_7
Results.Bela.SD <- data.table(Results.Bela.SD)
Results.Bela.SD$Period <- factor(Results.Bela.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.Bela.SD.plot <- Results.Bela.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Bela.SD.plot$Country.name <- "Belarus"

# -- Create the categories of cBelaes of death for plotting, to have the order of column that I want
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Other Cancers"]  <- 3
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="External"]  <- 8
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.Bela.SD.plot$Cause2[Results.Bela.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.Bela.SD.plot$Cause2 <- factor(Results.Bela.SD.plot$Cause2, levels = c(1:11),
                                      labels =  c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                  "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                  "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                  "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.Bela.SD.tab  <- Results.Bela.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause2, Period)]
Results.Bela.SD.tab <- data.table(Results.Bela.SD.tab)
Results.Bela.SD.tab$Country.name <- "Belarus"

# -- Plot of decomposition result
Bela_SG_Deco <- ggplot(data=Results.Bela.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Belaruse' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
Bela_SG_Deco

#####
# Belgium
#####


# Period 1
Belgium_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Belgium" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Bel_M_COD_7 <- as.matrix(Belgium_7[Belgium_7$Sex=="m",24:34])
Bel_F_COD_7  <- as.matrix(Belgium_7[Belgium_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition

Results.Bel.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Bel_F_COD_7), pars2 = c(Bel_M_COD_7))

dim(Results.Bel.SD_7) <- dim(Bel_M_COD_7) 


# Period 7
Results.Bel.SD_7 <- data.frame(Results.Bel.SD_7)
colnames(Results.Bel.SD_7) <- cause_names
Results.Bel.SD_7$Age <- c(seq(0,100,1))
Results.Bel.SD_7 <- gather(data = Results.Bel.SD_7,key = Cause,value = Contribution,-Age)
Results.Bel.SD_7$Age2       <- (cut(Results.Aus.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.Bel.SD_7 <- data.table(Results.Bel.SD_7)
Results.Bel.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.Bel.SD <- rbind(Results.Bel.SD_5,Results.Bel.SD_6,
                        Results.Bel.SD_7)
Results.Bel.SD <- data.table(Results.Bel.SD_7)
Results.Bel.SD$Period <- factor(Results.Bel.SD$Period, levels = c(5,6,7),
                                labels =   c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.Bel.SD.plot <- Results.Bel.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Bel.SD.plot$Country.name <- "Belgium"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Other Cancers"]  <- 3
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="External"]  <- 8
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.Bel.SD.plot$Cause2[Results.Bel.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.Bel.SD.plot$Cause2 <- factor(Results.Bel.SD.plot$Cause2, levels = c(1:11),
                                     labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.Bel.SD.tab  <- Results.Bel.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.Bel.SD.tab <- data.table(Results.Bel.SD.tab)
Results.Bel.SD.tab$Country.name <- "Belgium"

# -- Plot of decomposition result
Bel_SG_Deco <- ggplot(data=Results.Bel.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Belgium' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
Bel_SG_Deco

#####
# Bulgaria
#####

# ----- First define the period
# Period 1
Bulgaria <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Bulgaria" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
Bul_M_COD_7 <- as.matrix(Bulgaria[Bulgaria$Sex=="m",24:34])
Bul_F_COD_7  <- as.matrix(Bulgaria[Bulgaria$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.Bul.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(Bul_F_COD_7), pars2 = c(Bul_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.Bul.SD_7) <- dim(Bul_M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.Bul.SD_7 <- data.frame(Results.Bul.SD_7)
colnames(Results.Bul.SD_7) <- cause_names
Results.Bul.SD_7$Age <- c(seq(0,100,1))
Results.Bul.SD_7 <- gather(data = Results.Bul.SD_7,key = Cause,value = Contribution,-Age)
Results.Bul.SD_7$Age2       <- (cut(Results.Bul.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.Bul.SD_7 <- data.table(Results.Bul.SD_7)
Results.Bul.SD_7$Period <- 7


Results.Bul.SD <- data.table(Results.Bul.SD_7)
Results.Bul.SD$Period <- factor(Results.Bul.SD$Period, levels = c(7),
                                labels =  c("2010-2015"))

# For plotting
Results.Bul.SD.plot <- Results.Bul.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.Bul.SD.plot$Country.name <- "Bulgaria"

# -- Create the categories of cBules of death for plotting, to have the order of column that I want
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Other Cancers"]  <- 3
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="External"]  <- 8
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.Bul.SD.plot$Cause2[Results.Bul.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.Bul.SD.plot$Cause2 <- factor(Results.Bul.SD.plot$Cause2, levels = c(1:11),
                                     labels =  c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                 "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                 "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                 "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.Bul.SD.tab  <- Results.Bul.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause2, Period)]
Results.Bul.SD.tab <- data.table(Results.Bul.SD.tab)
Results.Bul.SD.tab$Country.name <- "Bulgaria"

# -- Plot of decomposition result
Bul_SG_Deco <- ggplot(data=Results.Bul.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Bulgaria' ),
       fill = "Cause",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")



#####
# Czech_Republic
#####

# ----- First define the period
# Period 1
CZE_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Czech Republic" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
CZE__M_COD_7 <- as.matrix(CZE_7[CZE_7$Sex=="m",24:34])
CZE__F_COD_7  <- as.matrix(CZE_7[CZE_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.CZE_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(CZE__F_COD_7), pars2 = c(CZE__M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.CZE_.SD_7) <- dim(CZE__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.CZE_.SD_7 <- data.frame(Results.CZE_.SD_7)
colnames(Results.CZE_.SD_7) <- cause_names
Results.CZE_.SD_7$Age <- c(seq(0,100,1))
Results.CZE_.SD_7 <- gather(data = Results.CZE_.SD_7,key = Cause,value = Contribution,-Age)
Results.CZE_.SD_7$Age2       <- (cut(Results.CZE_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.CZE_.SD_7 <- data.table(Results.CZE_.SD_7)
Results.CZE_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.CZE_.SD <- Results.CZE_.SD_7
Results.CZE_.SD <- data.table(Results.CZE_.SD)
Results.CZE_.SD$Period <- factor(Results.CZE_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.CZE_.SD.plot <- Results.CZE_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.CZE_.SD.plot$Country.name <- "Czech Republic"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="External"]  <- 8
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.CZE_.SD.plot$Cause2[Results.CZE_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.CZE_.SD.plot$Cause2 <- factor(Results.CZE_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.CZE_.SD.tab  <- Results.CZE_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.CZE_.SD.tab <- data.table(Results.CZE_.SD.tab)
Results.CZE_.SD.tab$Country.name <- "Czech Republic"

# -- Plot of decomposition result
CZE_SG_Deco <- ggplot(data=Results.CZE_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Czech Republic'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Denmark
#####

# ----- First define the period
# Period 1
DNK_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Denmark" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
DNK__M_COD_7 <- as.matrix(DNK_7[DNK_7$Sex=="m",24:34])
DNK__F_COD_7  <- as.matrix(DNK_7[DNK_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.DNK_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(DNK__F_COD_7), pars2 = c(DNK__M_COD_7))

# ----- Fourth: Define dimention of matrix

dim(Results.DNK_.SD_7) <- dim(DNK__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.DNK_.SD_7 <- data.frame(Results.DNK_.SD_7)
colnames(Results.DNK_.SD_7) <- cause_names
Results.DNK_.SD_7$Age <- c(seq(0,100,1))
Results.DNK_.SD_7 <- gather(data = Results.DNK_.SD_7,key = Cause,value = Contribution,-Age)
Results.DNK_.SD_7$Age2       <- (cut(Results.DNK_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.DNK_.SD_7 <- data.table(Results.DNK_.SD_7)
Results.DNK_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.DNK_.SD <- Results.DNK_.SD_7
Results.DNK_.SD <- data.table(Results.DNK_.SD)
Results.DNK_.SD$Period <- factor(Results.DNK_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.DNK_.SD.plot <- Results.DNK_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.DNK_.SD.plot$Country.name <- "Denmark"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="External"]  <- 8
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.DNK_.SD.plot$Cause2[Results.DNK_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.DNK_.SD.plot$Cause2 <- factor(Results.DNK_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.DNK_.SD.tab  <- Results.DNK_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.DNK_.SD.tab <- data.table(Results.DNK_.SD.tab)
Results.DNK_.SD.tab$Country.name <- "Denmark"

# -- Plot of decomposition result
DNK_SG_Deco <- ggplot(data=Results.DNK_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Denmark'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Estonia
#####

# ----- First define the period
# Period 1
EST_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Estonia" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
EST__M_COD_7 <- as.matrix(EST_7[EST_7$Sex=="m",24:34])
EST__F_COD_7  <- as.matrix(EST_7[EST_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.EST_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(EST__F_COD_7), pars2 = c(EST__M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.EST_.SD_7) <- dim(EST__M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.EST_.SD_7 <- data.frame(Results.EST_.SD_7)
colnames(Results.EST_.SD_7) <- cause_names
Results.EST_.SD_7$Age <- c(seq(0,100,1))
Results.EST_.SD_7 <- gather(data = Results.EST_.SD_7,key = Cause,value = Contribution,-Age)
Results.EST_.SD_7$Age2       <- (cut(Results.EST_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.EST_.SD_7 <- data.table(Results.EST_.SD_7)
Results.EST_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.EST_.SD <- Results.EST_.SD_7
Results.EST_.SD <- data.table(Results.EST_.SD)
Results.EST_.SD$Period <- factor(Results.EST_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.EST_.SD.plot <- Results.EST_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.EST_.SD.plot$Country.name <- "Estonia"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="External"]  <- 8
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.EST_.SD.plot$Cause2[Results.EST_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.EST_.SD.plot$Cause2 <- factor(Results.EST_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.EST_.SD.tab  <- Results.EST_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.EST_.SD.tab <- data.table(Results.EST_.SD.tab)
Results.EST_.SD.tab$Country.name <- "Estonia"

# -- Plot of decomposition result
EST_SG_Deco <- ggplot(data=Results.EST_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Estonia'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")



#####
# Finland
#####

# ----- First define the period
# Period 1
FIN_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Finland" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
FIN__M_COD_7 <- as.matrix(FIN_7[FIN_7$Sex=="m",24:34])
FIN__F_COD_7  <- as.matrix(FIN_7[FIN_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.FIN_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(FIN__F_COD_7), pars2 = c(FIN__M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.FIN_.SD_7) <- dim(FIN__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.FIN_.SD_7 <- data.frame(Results.FIN_.SD_7)
colnames(Results.FIN_.SD_7) <- cause_names
Results.FIN_.SD_7$Age <- c(seq(0,100,1))
Results.FIN_.SD_7 <- gather(data = Results.FIN_.SD_7,key = Cause,value = Contribution,-Age)
Results.FIN_.SD_7$Age2       <- (cut(Results.FIN_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FIN_.SD_7 <- data.table(Results.FIN_.SD_7)
Results.FIN_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.FIN_.SD <- Results.FIN_.SD_7
Results.FIN_.SD <- data.table(Results.FIN_.SD)
Results.FIN_.SD$Period <- factor(Results.FIN_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.FIN_.SD.plot <- Results.FIN_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.FIN_.SD.plot$Country.name <- "Finland"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="External"]  <- 8
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.FIN_.SD.plot$Cause2[Results.FIN_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.FIN_.SD.plot$Cause2 <- factor(Results.FIN_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.FIN_.SD.tab  <- Results.FIN_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.FIN_.SD.tab <- data.table(Results.FIN_.SD.tab)
Results.FIN_.SD.tab$Country.name <- "Finland"

# -- Plot of decomposition result
FIN_SG_Deco <- ggplot(data=Results.FIN_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Finland'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")



#####
# France
#####

# ----- First define the period
# Period 1
FRA_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="France" & Age<=101 & Periods==2010)

# ----- Second deFRAe the period for matrix
# Period 7
FRA__M_COD_7 <- as.matrix(FRA_7[FRA_7$Sex=="m",24:34])
FRA__F_COD_7  <- as.matrix(FRA_7[FRA_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.FRA_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(FRA__F_COD_7), pars2 = c(FRA__M_COD_7))

# ----- Fourth: DeFRAe dimention of matrix
dim(Results.FRA_.SD_7) <- dim(FRA__M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.FRA_.SD_7 <- data.frame(Results.FRA_.SD_7)
colnames(Results.FRA_.SD_7) <- cause_names
Results.FRA_.SD_7$Age <- c(seq(0,100,1))
Results.FRA_.SD_7 <- gather(data = Results.FRA_.SD_7,key = Cause,value = Contribution,-Age)
Results.FRA_.SD_7$Age2       <- (cut(Results.FRA_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.FRA_.SD_7 <- data.table(Results.FRA_.SD_7)
Results.FRA_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.FRA_.SD <- Results.FRA_.SD_7
Results.FRA_.SD <- data.table(Results.FRA_.SD)
Results.FRA_.SD$Period <- factor(Results.FRA_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.FRA_.SD.plot <- Results.FRA_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.FRA_.SD.plot$Country.name <- "France"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="External"]  <- 8
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.FRA_.SD.plot$Cause2[Results.FRA_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.FRA_.SD.plot$Cause2 <- factor(Results.FRA_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.FRA_.SD.tab  <- Results.FRA_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.FRA_.SD.tab <- data.table(Results.FRA_.SD.tab)
Results.FRA_.SD.tab$Country.name <- "France"

# -- Plot of decomposition result
FRA_SG_Deco <- ggplot(data=Results.FRA_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, France'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Germany
#####

# ----- First define the period
# Period 1
GER_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Germany" & Age<=101 & Periods==2010)

# ----- Second deGERe the period for matrix
# Period 7
GER__M_COD_7 <- as.matrix(GER_7[GER_7$Sex=="m",24:34])
GER__F_COD_7  <- as.matrix(GER_7[GER_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.GER_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(GER__F_COD_7), pars2 = c(GER__M_COD_7))

# ----- Fourth: DeGERe dimention of matrix
dim(Results.GER_.SD_7) <- dim(GER__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.GER_.SD_7 <- data.frame(Results.GER_.SD_7)
colnames(Results.GER_.SD_7) <- cause_names
Results.GER_.SD_7$Age <- c(seq(0,100,1))
Results.GER_.SD_7 <- gather(data = Results.GER_.SD_7,key = Cause,value = Contribution,-Age)
Results.GER_.SD_7$Age2       <- (cut(Results.GER_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.GER_.SD_7 <- data.table(Results.GER_.SD_7)
Results.GER_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.GER_.SD <- Results.GER_.SD_7
Results.GER_.SD <- data.table(Results.GER_.SD)
Results.GER_.SD$Period <- factor(Results.GER_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.GER_.SD.plot <- Results.GER_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.GER_.SD.plot$Country.name <- "Germany"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="External"]  <- 8
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.GER_.SD.plot$Cause2[Results.GER_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.GER_.SD.plot$Cause2 <- factor(Results.GER_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.GER_.SD.tab  <- Results.GER_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.GER_.SD.tab <- data.table(Results.GER_.SD.tab)
Results.GER_.SD.tab$Country.name <- "Germany"

# -- Plot of decomposition result
GER_SG_Deco <- ggplot(data=Results.GER_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Germany'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Greece
#####

# ----- First define the period
# Period 1
Greece <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Greece" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix

# Period 7
GRE_M_COD_7 <- as.matrix(Greece[Greece$Sex=="m",24:34])
GRE_F_COD_7  <- as.matrix(Greece[Greece$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.GRE.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(GRE_F_COD_7), pars2 = c(GRE_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.GRE.SD_7) <- dim(GRE_M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.GRE.SD_7 <- data.frame(Results.GRE.SD_7)
colnames(Results.GRE.SD_7) <- cause_names
Results.GRE.SD_7$Age <- c(seq(0,100,1))
Results.GRE.SD_7 <- gather(data = Results.GRE.SD_7,key = Cause,value = Contribution,-Age)
Results.GRE.SD_7$Age2       <- (cut(Results.GRE.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.GRE.SD_7 <- data.table(Results.GRE.SD_7)
Results.GRE.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions


Results.GRE.SD <- data.table(Results.GRE.SD_7)
Results.GRE.SD$Period <- factor(Results.GRE.SD$Period, levels = c(7),
                                labels =  c("2010-2015"))

# For plotting
Results.GRE.SD.plot <- Results.GRE.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.GRE.SD.plot$Country.name <- "Greece"

# -- Create the categories of cGREes of death for plotting, to have the order of column that I want
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Other Cancers"]  <- 3
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="External"]  <- 8
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.GRE.SD.plot$Cause2[Results.GRE.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.GRE.SD.plot$Cause2 <- factor(Results.GRE.SD.plot$Cause2, levels = c(1:11),
                                     labels =  c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                 "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                 "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                 "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.GRE.SD.tab  <- Results.GRE.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause2, Period)]
Results.GRE.SD.tab <- data.table(Results.GRE.SD.tab)
Results.GRE.SD.tab$Country.name <- "Greece"

# -- Plot of decomposition result
GRE_SG_Deco <- ggplot(data=Results.GRE.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Greece' ),
       fill = "Cause",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")



#####
# Hungary
#####

# ----- First define the period
# Period 1
HUN_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Hungary" & Age<=101 & Periods==2010)

# ----- Second deHUNe the period for matrix
# Period 7
HUN__M_COD_7 <- as.matrix(HUN_7[HUN_7$Sex=="m",24:34])
HUN__F_COD_7  <- as.matrix(HUN_7[HUN_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.HUN_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(HUN__F_COD_7), pars2 = c(HUN__M_COD_7))

# ----- Fourth: DeHUNe dimention of matrix
dim(Results.HUN_.SD_7) <- dim(HUN__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.HUN_.SD_7 <- data.table(Results.HUN_.SD_7)
colnames(Results.HUN_.SD_7) <- cause_names
Results.HUN_.SD_7$Age <- c(seq(0,100,1))
Results.HUN_.SD_7 <- gather(data = Results.HUN_.SD_7,key = Cause,value = Contribution,-Age)
Results.HUN_.SD_7$Age2       <- (cut(Results.HUN_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.HUN_.SD_7 <- data.table(Results.HUN_.SD_7)
Results.HUN_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.HUN_.SD <- Results.HUN_.SD_7
Results.HUN_.SD <- data.table(Results.HUN_.SD)
Results.HUN_.SD$Period <- factor(Results.HUN_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.HUN_.SD.plot <- Results.HUN_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.HUN_.SD.plot$Country.name <- "Hungary"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Sex-specific cancer"]  <- 2
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="External"]  <- 8
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.HUN_.SD.plot$Cause2[Results.HUN_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.HUN_.SD.plot$Cause2 <- factor(Results.HUN_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.HUN_.SD.tab  <- Results.HUN_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.HUN_.SD.tab <- data.table(Results.HUN_.SD.tab)
Results.HUN_.SD.tab$Country.name <- "Hungary"

# -- Plot of decomposition result
HUN_SG_Deco <- ggplot(data=Results.HUN_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Hungary'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
HUN_SG_Deco


#####
# Ireland
#####

# Period 1
Ireland <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Ireland" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
IRE_M_COD_7 <- as.matrix(Ireland[Ireland$Sex=="m",24:34])
IRE_F_COD_7  <- as.matrix(Ireland[Ireland$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.IRE.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(IRE_F_COD_7), pars2 = c(IRE_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.IRE.SD_7) <- dim(IRE_M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.IRE.SD_7 <- data.frame(Results.IRE.SD_7)
colnames(Results.IRE.SD_7) <- cause_names
Results.IRE.SD_7$Age <- c(seq(0,100,1))
Results.IRE.SD_7 <- gather(data = Results.IRE.SD_7,key = Cause,value = Contribution,-Age)
Results.IRE.SD_7$Age2       <- (cut(Results.IRE.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.IRE.SD_7 <- data.table(Results.IRE.SD_7)
Results.IRE.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.IRE.SD <- data.table(Results.IRE.SD_7)
Results.IRE.SD$Period <- factor(Results.IRE.SD$Period, levels = c(7),
                                labels =  c("2010-2015"))

# For plotting
Results.IRE.SD.plot <- Results.IRE.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.IRE.SD.plot$Country.name <- "Ireland"

# -- Create the categories of cIREes of death for plotting, to have the order of column that I want
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Other Cancers"]  <- 3
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="External"]  <- 8
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.IRE.SD.plot$Cause2[Results.IRE.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.IRE.SD.plot$Cause2 <- factor(Results.IRE.SD.plot$Cause2, levels = c(1:11),
                                     labels =  c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                 "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                 "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                 "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.IRE.SD.tab  <- Results.IRE.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause2, Period)]
Results.IRE.SD.tab <- data.table(Results.IRE.SD.tab)
Results.IRE.SD.tab$Country.name <- "Ireland"

# -- Plot of decomposition result
IRE_SG_Deco <- ggplot(data=Results.IRE.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Ireland' ),
       fill = "Cause",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Italy
#####

# ----- First define the period
# Period 1
ITA_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Italy" & Age<=101 & Periods==2010)

# ----- Second deITAe the period for matrix
# Period 7
ITA__M_COD_7 <- as.matrix(ITA_7[ITA_7$Sex=="m",24:34])
ITA__F_COD_7  <- as.matrix(ITA_7[ITA_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.ITA_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(ITA__F_COD_7), pars2 = c(ITA__M_COD_7))

# ----- Fourth: DeITAe dimention of matrix
dim(Results.ITA_.SD_7) <- dim(ITA__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.ITA_.SD_7 <- data.table(Results.ITA_.SD_7)
colnames(Results.ITA_.SD_7) <- cause_names
Results.ITA_.SD_7$Age <- c(seq(0,100,1))
Results.ITA_.SD_7 <- gather(data = Results.ITA_.SD_7,key = Cause,value = Contribution,-Age)
Results.ITA_.SD_7$Age2       <- (cut(Results.ITA_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.ITA_.SD_7 <- data.table(Results.ITA_.SD_7)
Results.ITA_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.ITA_.SD <- data.table(Results.ITA_.SD_7)
Results.ITA_.SD$Period <- factor(Results.ITA_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.ITA_.SD.plot <- Results.ITA_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.ITA_.SD.plot$Country.name <- "Italy"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="External"]  <- 8
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.ITA_.SD.plot$Cause2[Results.ITA_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.ITA_.SD.plot$Cause2 <- factor(Results.ITA_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.ITA_.SD.tab  <- Results.ITA_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.ITA_.SD.tab <- data.table(Results.ITA_.SD.tab)
Results.ITA_.SD.tab$Country.name <- "Italy"

# -- Plot of decomposition result
ITA_SG_Deco <- ggplot(data=Results.ITA_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Italy'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Latvia
#####

# ----- First define the period
# Period 1
LAT_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Latvia" & Age<=101 & Periods==2010)

# ----- Second deLATe the period for matrix
# Period 7
LAT__M_COD_7 <- as.matrix(LAT_7[LAT_7$Sex=="m",24:34])
LAT__F_COD_7  <- as.matrix(LAT_7[LAT_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.LAT_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(LAT__F_COD_7), pars2 = c(LAT__M_COD_7))

# ----- Fourth: DeLATe dimention of matrix
dim(Results.LAT_.SD_7) <- dim(LAT__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.LAT_.SD_7 <- data.table(Results.LAT_.SD_7)
colnames(Results.LAT_.SD_7) <- cause_names
Results.LAT_.SD_7$Age <- c(seq(0,100,1))
Results.LAT_.SD_7 <- gather(data = Results.LAT_.SD_7,key = Cause,value = Contribution,-Age)
Results.LAT_.SD_7$Age2       <- (cut(Results.LAT_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.LAT_.SD_7 <- data.table(Results.LAT_.SD_7)
Results.LAT_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.LAT_.SD <- data.table(Results.LAT_.SD_7)
Results.LAT_.SD$Period <- factor(Results.LAT_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.LAT_.SD.plot <- Results.LAT_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.LAT_.SD.plot$Country.name <- "Latvia"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="External"]  <- 8
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.LAT_.SD.plot$Cause2[Results.LAT_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.LAT_.SD.plot$Cause2 <- factor(Results.LAT_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.LAT_.SD.tab  <- Results.LAT_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.LAT_.SD.tab <- data.table(Results.LAT_.SD.tab)
Results.LAT_.SD.tab$Country.name <- "Latvia"

# -- Plot of decomposition result
LAT_SG_Deco <- ggplot(data=Results.LAT_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Latvia'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Lithuania
#####

# ----- First define the period
# Period 1
LIT_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Lithuania" & Age<=101 & Periods==2010)

# ----- Second deLITe the period for matrix
# Period 7
LIT__M_COD_7 <- as.matrix(LIT_7[LIT_7$Sex=="m",24:34])
LIT__F_COD_7  <- as.matrix(LIT_7[LIT_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.LIT_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(LIT__F_COD_7), pars2 = c(LIT__M_COD_7))

# ----- Fourth: DeLITe dimention of matrix

dim(Results.LIT_.SD_7) <- dim(LIT__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.LIT_.SD_7 <- data.table(Results.LIT_.SD_7)
colnames(Results.LIT_.SD_7) <- cause_names
Results.LIT_.SD_7$Age <- c(seq(0,100,1))
Results.LIT_.SD_7 <- gather(data = Results.LIT_.SD_7,key = Cause,value = Contribution,-Age)
Results.LIT_.SD_7$Age2       <- (cut(Results.LIT_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.LIT_.SD_7 <- data.table(Results.LIT_.SD_7)
Results.LIT_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.LIT_.SD <- data.table(Results.LIT_.SD_7)
Results.LIT_.SD$Period <- factor(Results.LIT_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.LIT_.SD.plot <- Results.LIT_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.LIT_.SD.plot$Country.name <- "Lithuania"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="External"]  <- 8
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.LIT_.SD.plot$Cause2[Results.LIT_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.LIT_.SD.plot$Cause2 <- factor(Results.LIT_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.LIT_.SD.tab  <- Results.LIT_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.LIT_.SD.tab <- data.table(Results.LIT_.SD.tab)
Results.LIT_.SD.tab$Country.name <- "Lithuania"

# -- Plot of decomposition result
LIT_SG_Deco <- ggplot(data=Results.LIT_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Lithuania'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")

#####
# Netherlands
#####

# ----- First define the period
# Period 7
NDL_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Netherlands" & Age<=101 & Periods==2010)

# ----- Second deNDLe the period for matrix
# Period 7
NDL__M_COD_7 <- as.matrix(NDL_7[NDL_7$Sex=="m",24:34])
NDL__F_COD_7  <- as.matrix(NDL_7[NDL_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.NDL_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(NDL__F_COD_7), pars2 = c(NDL__M_COD_7))

# ----- Fourth: DeNDLe dimention of matrix
dim(Results.NDL_.SD_7) <- dim(NDL__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.NDL_.SD_7 <- data.table(Results.NDL_.SD_7)
colnames(Results.NDL_.SD_7) <- cause_names
Results.NDL_.SD_7$Age <- c(seq(0,100,1))
Results.NDL_.SD_7 <- gather(data = Results.NDL_.SD_7,key = Cause,value = Contribution,-Age)
Results.NDL_.SD_7$Age2       <- (cut(Results.NDL_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.NDL_.SD_7 <- data.table(Results.NDL_.SD_7)
Results.NDL_.SD_7$Period <- 7

# --- Sixth: Combine all the decomposition

Results.NDL_.SD <- data.table(Results.NDL_.SD_7)
Results.NDL_.SD$Period <- factor(Results.NDL_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.NDL_.SD.plot <- Results.NDL_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.NDL_.SD.plot$Country.name <- "Netherlands"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="External"]  <- 8
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.NDL_.SD.plot$Cause2[Results.NDL_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.NDL_.SD.plot$Cause2 <- factor(Results.NDL_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.NDL_.SD.tab  <- Results.NDL_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.NDL_.SD.tab <- data.table(Results.NDL_.SD.tab)
Results.NDL_.SD.tab$Country.name <- "Netherlands"

# -- Plot of decomposition result
NDL_SG_Deco <- ggplot(data=Results.NDL_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Netherlands'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")

#####
# Norway
#####

# ----- First define the period
# Period 7
NOR_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Norway" & Age<=101 & Periods==2010)

# ----- Second deNORe the period for matrix
# Period 7
NOR__M_COD_7 <- as.matrix(NOR_7[NOR_7$Sex=="m",24:34])
NOR__F_COD_7  <- as.matrix(NOR_7[NOR_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.NOR_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(NOR__F_COD_7), pars2 = c(NOR__M_COD_7))

# ----- Fourth: DeNORe dimention of matrix
dim(Results.NOR_.SD_7) <- dim(NOR__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.NOR_.SD_7 <- data.table(Results.NOR_.SD_7)
colnames(Results.NOR_.SD_7) <- cause_names
Results.NOR_.SD_7$Age <- c(seq(0,100,1))
Results.NOR_.SD_7 <- gather(data = Results.NOR_.SD_7,key = Cause,value = Contribution,-Age)
Results.NOR_.SD_7$Age2       <- (cut(Results.NOR_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.NOR_.SD_7 <- data.table(Results.NOR_.SD_7)
Results.NOR_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.NOR_.SD <- data.table(Results.NOR_.SD_7)
Results.NOR_.SD$Period <- factor(Results.NOR_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.NOR_.SD.plot <- Results.NOR_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.NOR_.SD.plot$Country.name <- "Norway"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="External"]  <- 8
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.NOR_.SD.plot$Cause2[Results.NOR_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.NOR_.SD.plot$Cause2 <- factor(Results.NOR_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.NOR_.SD.tab  <- Results.NOR_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.NOR_.SD.tab <- data.table(Results.NOR_.SD.tab)
Results.NOR_.SD.tab$Country.name <- "Norway"

# -- Plot of decomposition result
NOR_SG_Deco <- ggplot(data=Results.NOR_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Norway'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Poland
#####

# ----- First define the period
# Period 7
POL_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Poland" & Age<=101 & Periods==2010)

# ----- Second dePOLe the period for matrix
# Period 7
POL__M_COD_7 <- as.matrix(POL_7[POL_7$Sex=="m",24:34])
POL__F_COD_7  <- as.matrix(POL_7[POL_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.POL_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(POL__F_COD_7), pars2 = c(POL__M_COD_7))

# ----- Fourth: DePOLe dimention of matrix
dim(Results.POL_.SD_7) <- dim(POL__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.POL_.SD_7 <- data.table(Results.POL_.SD_7)
colnames(Results.POL_.SD_7) <- cause_names
Results.POL_.SD_7$Age <- c(seq(0,100,1))
Results.POL_.SD_7 <- gather(data = Results.POL_.SD_7,key = Cause,value = Contribution,-Age)
Results.POL_.SD_7$Age2       <- (cut(Results.POL_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.POL_.SD_7 <- data.table(Results.POL_.SD_7)
Results.POL_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.POL_.SD <- data.table(Results.POL_.SD_7)
Results.POL_.SD$Period <- factor(Results.POL_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.POL_.SD.plot <- Results.POL_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.POL_.SD.plot$Country.name <- "Poland"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="External"]  <- 8
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.POL_.SD.plot$Cause2[Results.POL_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.POL_.SD.plot$Cause2 <- factor(Results.POL_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.POL_.SD.tab  <- Results.POL_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.POL_.SD.tab <- data.table(Results.POL_.SD.tab)
Results.POL_.SD.tab$Country.name <- "Poland"

# -- Plot of decomposition result
POL_SG_Deco <- ggplot(data=Results.POL_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Poland'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Portugal
#####

# ----- First define the period
# Period 1
Portugal <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Portugal" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
POR_M_COD_7 <- as.matrix(Portugal[Portugal$Sex=="m",24:34])
POR_F_COD_7  <- as.matrix(Portugal[Portugal$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.POR.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(POR_F_COD_7), pars2 = c(POR_M_COD_7))

# ----- Fourth: Define dimention of matrix
dim(Results.POR.SD_7) <- dim(POR_M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.POR.SD_7 <- data.frame(Results.POR.SD_7)
colnames(Results.POR.SD_7) <- cause_names
Results.POR.SD_7$Age <- c(seq(0,100,1))
Results.POR.SD_7 <- gather(data = Results.POR.SD_7,key = Cause,value = Contribution,-Age)
Results.POR.SD_7$Age2       <- (cut(Results.POR.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels = Labels.age))
Results.POR.SD_7 <- data.table(Results.POR.SD_7)
Results.POR.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.POR.SD <- data.table(Results.POR.SD_7)
Results.POR.SD$Period <- factor(Results.POR.SD$Period, levels = c(7),
                                labels =  c("2010-2015"))

# For plotting
Results.POR.SD.plot <- Results.POR.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.POR.SD.plot$Country.name <- "Portugal"

# -- Create the categories of cPORes of death for plotting, to have the order of column that I want
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Other Cancers"]  <- 3
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="External"]  <- 8
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.POR.SD.plot$Cause2[Results.POR.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.POR.SD.plot$Cause2 <- factor(Results.POR.SD.plot$Cause2, levels = c(1:11),
                                     labels =  c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                 "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                 "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                 "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.POR.SD.tab  <- Results.POR.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause2, Period)]
Results.POR.SD.tab <- data.table(Results.POR.SD.tab)
Results.POR.SD.tab$Country.name <- "Portugal"

# -- Plot of decomposition result
POR_SG_Deco <- ggplot(data=Results.POR.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Portugal' ),
       fill = "Cause",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Russia
#####

# ----- First define the period
# Period 7
RUS_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Russia" & Age<=101 & Periods==2010)

# ----- Second define the period for matrix
# Period 7
RUS__M_COD_7 <- as.matrix(RUS_7[RUS_7$Sex=="m",24:34])
RUS__F_COD_7  <- as.matrix(RUS_7[RUS_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.RUS_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(RUS__F_COD_7), pars2 = c(RUS__M_COD_7))

# ----- Fourth: DeRUSe dimention of matrix
dim(Results.RUS_.SD_7) <- dim(RUS__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.RUS_.SD_7 <- data.table(Results.RUS_.SD_7)
colnames(Results.RUS_.SD_7) <- cause_names
Results.RUS_.SD_7$Age <- c(seq(0,100,1))
Results.RUS_.SD_7 <- gather(data = Results.RUS_.SD_7,key = Cause,value = Contribution,-Age)
Results.RUS_.SD_7$Age2       <- (cut(Results.RUS_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.RUS_.SD_7 <- data.table(Results.RUS_.SD_7)
Results.RUS_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.RUS_.SD <- data.table(Results.RUS_.SD_7)
Results.RUS_.SD$Period <- factor(Results.RUS_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.RUS_.SD.plot <- Results.RUS_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.RUS_.SD.plot$Country.name <- "Russia"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="External"]  <- 8
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.RUS_.SD.plot$Cause2[Results.RUS_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.RUS_.SD.plot$Cause2 <- factor(Results.RUS_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.RUS_.SD.tab  <- Results.RUS_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.RUS_.SD.tab <- data.table(Results.RUS_.SD.tab)
Results.RUS_.SD.tab$Country.name <- "Russia Federation"

# -- Plot of decomposition result
RUS_SG_Deco <- ggplot(data=Results.RUS_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Russia'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Slovakia
#####

# ----- First define the period
# Period 7
SLV_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Slovakia" & Age<=101 & Periods==2010)

# ----- Second deSLVe the period for matrix
# Period 7
SLV__M_COD_7 <- as.matrix(SLV_7[SLV_7$Sex=="m",24:34])
SLV__F_COD_7  <- as.matrix(SLV_7[SLV_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.SLV_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(SLV__F_COD_7), pars2 = c(SLV__M_COD_7))

# ----- Fourth: DeSLVe dimention of matrix
dim(Results.SLV_.SD_7) <- dim(SLV__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.SLV_.SD_7 <- data.table(Results.SLV_.SD_7)
colnames(Results.SLV_.SD_7) <- cause_names
Results.SLV_.SD_7$Age <- c(seq(0,100,1))
Results.SLV_.SD_7 <- gather(data = Results.SLV_.SD_7,key = Cause,value = Contribution,-Age)
Results.SLV_.SD_7$Age2       <- (cut(Results.SLV_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.SLV_.SD_7 <- data.table(Results.SLV_.SD_7)
Results.SLV_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.SLV_.SD <- data.table(Results.SLV_.SD_7)
Results.SLV_.SD$Period <- factor(Results.SLV_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.SLV_.SD.plot <- Results.SLV_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.SLV_.SD.plot$Country.name <- "Slovakia"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="External"]  <- 8
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.SLV_.SD.plot$Cause2[Results.SLV_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.SLV_.SD.plot$Cause2 <- factor(Results.SLV_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.SLV_.SD.tab  <- Results.SLV_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.SLV_.SD.tab <- data.table(Results.SLV_.SD.tab)
Results.SLV_.SD.tab$Country.name <- "Slovakia"

# -- Plot of decomposition result
SLV_SG_Deco <- ggplot(data=Results.SLV_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Slovakia'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")

#####
# Slovenia
#####

# ----- First define the period
# Period 7
SLN_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Slovenia" & Age<=101 & Periods==2010)

# ----- Second deSLNe the period for matrix
# Period 7
SLN__M_COD_7 <- as.matrix(SLN_7[SLN_7$Sex=="m",24:34])
SLN__F_COD_7  <- as.matrix(SLN_7[SLN_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.SLN_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(SLN__F_COD_7), pars2 = c(SLN__M_COD_7))

# ----- Fourth: DeSLNe dimention of matrix
dim(Results.SLN_.SD_7) <- dim(SLN__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.SLN_.SD_7 <- data.table(Results.SLN_.SD_7)
colnames(Results.SLN_.SD_7) <- cause_names
Results.SLN_.SD_7$Age <- c(seq(0,100,1))
Results.SLN_.SD_7 <- gather(data = Results.SLN_.SD_7,key = Cause,value = Contribution,-Age)
Results.SLN_.SD_7$Age2       <- (cut(Results.SLN_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.SLN_.SD_7 <- data.table(Results.SLN_.SD_7)
Results.SLN_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.SLN_.SD <- data.table(Results.SLN_.SD_7)
Results.SLN_.SD$Period <- factor(Results.SLN_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.SLN_.SD.plot <- Results.SLN_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.SLN_.SD.plot$Country.name <- "Slovenia"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="External"]  <- 8
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.SLN_.SD.plot$Cause2[Results.SLN_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.SLN_.SD.plot$Cause2 <- factor(Results.SLN_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.SLN_.SD.tab  <- Results.SLN_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.SLN_.SD.tab <- data.table(Results.SLN_.SD.tab)
Results.SLN_.SD.tab$Country.name <- "Slovenia"

# -- Plot of decomposition result
SLN_SG_Deco <- ggplot(data=Results.SLN_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Slovenia'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# Spain
#####

# ----- First define the period
# Period 7
ESP_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Spain" & Age<=101 & Periods==2010)

# ----- Second deESPe the period for matrix
# Period 7
ESP__M_COD_7 <- as.matrix(ESP_7[ESP_7$Sex=="m",24:34])
ESP__F_COD_7  <- as.matrix(ESP_7[ESP_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.ESP_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(ESP__F_COD_7), pars2 = c(ESP__M_COD_7))

# ----- Fourth: DeESPe dimention of matrix
dim(Results.ESP_.SD_7) <- dim(ESP__M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.ESP_.SD_7 <- data.table(Results.ESP_.SD_7)
colnames(Results.ESP_.SD_7) <- cause_names
Results.ESP_.SD_7$Age <- c(seq(0,100,1))
Results.ESP_.SD_7 <- gather(data = Results.ESP_.SD_7,key = Cause,value = Contribution,-Age)
Results.ESP_.SD_7$Age2       <- (cut(Results.ESP_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.ESP_.SD_7 <- data.table(Results.ESP_.SD_7)
Results.ESP_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.ESP_.SD <- data.table(Results.ESP_.SD_7)
Results.ESP_.SD$Period <- factor(Results.ESP_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.ESP_.SD.plot <- Results.ESP_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.ESP_.SD.plot$Country.name <- "Spain"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="External"]  <- 8
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.ESP_.SD.plot$Cause2[Results.ESP_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.ESP_.SD.plot$Cause2 <- factor(Results.ESP_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.ESP_.SD.tab  <- Results.ESP_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.ESP_.SD.tab <- data.table(Results.ESP_.SD.tab)
Results.ESP_.SD.tab$Country.name <- "Spain"

# -- Plot of decomposition result
ESP_SG_Deco <- ggplot(data=Results.ESP_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Spain'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")

#####
# Sweden
#####

# ----- First define the period
# Period 7
SWE_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Sweden" & Age<=101 & Periods==2010)

# ----- Second deSWEe the period for matrix
# Period 7
SWE__M_COD_7 <- as.matrix(SWE_7[SWE_7$Sex=="m",24:34])
SWE__F_COD_7  <- as.matrix(SWE_7[SWE_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.SWE_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(SWE__F_COD_7), pars2 = c(SWE__M_COD_7))

# ----- Fourth: DeSWEe dimention of matrix
dim(Results.SWE_.SD_7) <- dim(SWE__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.SWE_.SD_7 <- data.table(Results.SWE_.SD_7)
colnames(Results.SWE_.SD_7) <- cause_names
Results.SWE_.SD_7$Age <- c(seq(0,100,1))
Results.SWE_.SD_7 <- gather(data = Results.SWE_.SD_7,key = Cause,value = Contribution,-Age)
Results.SWE_.SD_7$Age2       <- (cut(Results.SWE_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.SWE_.SD_7 <- data.table(Results.SWE_.SD_7)
Results.SWE_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.SWE_.SD <- data.table(Results.SWE_.SD_7)
Results.SWE_.SD$Period <- factor(Results.SWE_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.SWE_.SD.plot <- Results.SWE_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.SWE_.SD.plot$Country.name <- "Sweden"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="External"]  <- 8
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.SWE_.SD.plot$Cause2[Results.SWE_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.SWE_.SD.plot$Cause2 <- factor(Results.SWE_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.SWE_.SD.tab  <- Results.SWE_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.SWE_.SD.tab <- data.table(Results.SWE_.SD.tab)
Results.SWE_.SD.tab$Country.name <- "Sweden"

# -- Plot of decomposition result
SWE_SG_Deco <- ggplot(data=Results.SWE_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Sweden'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
SWE_SG_Deco

#####
# Switzerland
#####

# ----- First define the period
# Period 7
SWI_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Switzerland" & Age<=101 & Periods==2010)

# ----- Second deSWIe the period for matrix
# Period 7
SWI__M_COD_7 <- as.matrix(SWI_7[SWI_7$Sex=="m",24:34])
SWI__F_COD_7  <- as.matrix(SWI_7[SWI_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.SWI_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(SWI__F_COD_7), pars2 = c(SWI__M_COD_7))

# ----- Fourth: DeSWIe dimention of matrix
dim(Results.SWI_.SD_7) <- dim(SWI__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.SWI_.SD_7 <- data.table(Results.SWI_.SD_7)
colnames(Results.SWI_.SD_7) <- cause_names
Results.SWI_.SD_7$Age <- c(seq(0,100,1))
Results.SWI_.SD_7 <- gather(data = Results.SWI_.SD_7,key = Cause,value = Contribution,-Age)
Results.SWI_.SD_7$Age2       <- (cut(Results.SWI_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.SWI_.SD_7 <- data.table(Results.SWI_.SD_7)
Results.SWI_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.SWI_.SD <- data.table(Results.SWI_.SD_7)
Results.SWI_.SD$Period <- factor(Results.SWI_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.SWI_.SD.plot <- Results.SWI_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.SWI_.SD.plot$Country.name <- "Switzerland"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="External"]  <- 8
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.SWI_.SD.plot$Cause2[Results.SWI_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.SWI_.SD.plot$Cause2 <- factor(Results.SWI_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.SWI_.SD.tab  <- Results.SWI_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.SWI_.SD.tab <- data.table(Results.SWI_.SD.tab)
Results.SWI_.SD.tab$Country.name <- "Switzerland"

# -- Plot of decomposition result
SWI_SG_Deco <- ggplot(data=Results.SWI_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Switzerland'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")

#####
# Ukraine
#####

# ----- First define the period
# Period 7
UKR_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="Ukraine" & Age<=101 & Periods==2010)

# ----- Second deUKRe the period for matrix
# Period 7
UKR__M_COD_7 <- as.matrix(UKR_7[UKR_7$Sex=="m",24:34])
UKR__F_COD_7  <- as.matrix(UKR_7[UKR_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.UKR_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(UKR__F_COD_7), pars2 = c(UKR__M_COD_7))

# ----- Fourth: DeUKRe dimention of matrix
dim(Results.UKR_.SD_7) <- dim(UKR__M_COD_7) 

# ----- Fifth: Arrange data

# Period 7
Results.UKR_.SD_7 <- data.table(Results.UKR_.SD_7)
colnames(Results.UKR_.SD_7) <- cause_names
Results.UKR_.SD_7$Age <- c(seq(0,100,1))
Results.UKR_.SD_7 <- gather(data = Results.UKR_.SD_7,key = Cause,value = Contribution,-Age)
Results.UKR_.SD_7$Age2       <- (cut(Results.UKR_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.UKR_.SD_7 <- data.table(Results.UKR_.SD_7)
Results.UKR_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions

Results.UKR_.SD <- data.table(Results.UKR_.SD_7)
Results.UKR_.SD$Period <- factor(Results.UKR_.SD$Period, levels = c(5,6,7),
                                 labels =  c("2000-2004", "2005-2009",
                                             "2010-2015"))

# For plotting
Results.UKR_.SD.plot <- Results.UKR_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.UKR_.SD.plot$Country.name <- "Ukraine"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="External"]  <- 8
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.UKR_.SD.plot$Cause2[Results.UKR_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.UKR_.SD.plot$Cause2 <- factor(Results.UKR_.SD.plot$Cause2, levels = c(1:11),
                                      labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                   "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                   "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                   "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.UKR_.SD.tab  <- Results.UKR_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.UKR_.SD.tab <- data.table(Results.UKR_.SD.tab)
Results.UKR_.SD.tab$Country.name <- "Ukraine"

# -- Plot of decomposition result
UKR_SG_Deco <- ggplot(data=Results.UKR_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, Ukraine'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


#####
# United Kingdome
#####

# ----- First define the period
# Period 7
UK_7 <- Mx_CoD_countries_Periods %>% 
  filter(Country.name=="United Kingdom" & Age<=101 & Periods==2010)


# ----- Second deUKe the period for matrix
# Period 7
UK__M_COD_7 <- as.matrix(UK_7[UK_7$Sex=="m",24:34])
UK__F_COD_7  <- as.matrix(UK_7[UK_7$Sex=="f",24:34])

# ----- Third Decomposition by periods
# - Decomposition
Results.UK_.SD_7 <- stepwise_replacement(func = sdfrommxc, pars1 = c(UK__F_COD_7), pars2 = c(UK__M_COD_7))

# ----- Fourth: DeUKe dimention of matrix
dim(Results.UK_.SD_7) <- dim(UK__M_COD_7) 

# ----- Fifth: Arrange data
# Period 7
Results.UK_.SD_7 <- data.table(Results.UK_.SD_7)
colnames(Results.UK_.SD_7) <- cause_names
Results.UK_.SD_7$Age <- c(seq(0,100,1))
Results.UK_.SD_7 <- gather(data = Results.UK_.SD_7,key = Cause,value = Contribution,-Age)
Results.UK_.SD_7$Age2       <- (cut(Results.UK_.SD_7$Age+1, breaks=c(0,1,seq(5,100,5),Inf),labels=Labels.age))
Results.UK_.SD_7 <- data.table(Results.UK_.SD_7)
Results.UK_.SD_7$Period <- 7

# --- Sixth: Combine all the decompositions
Results.UK_.SD <- data.table(Results.UK_.SD_7)
Results.UK_.SD$Period <- factor(Results.UK_.SD$Period, levels = c(5,6,7),
                                labels =  c("2000-2004", "2005-2009",
                                            "2010-2015"))

# For plotting
Results.UK_.SD.plot <- Results.UK_.SD[,list(Contribution2 = sum(Contribution)), by = list(Cause,Age2, Period)]
Results.UK_.SD.plot$Country.name <- "United Kingdom"

# -- Create the categories of causes of death for plotting, to have the order of column that I want
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="C. Sesitive Smoking"]  <- 1
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Sex-specif cancer"]  <- 2
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Other Cancers"]  <- 3
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="IHD and Stroke"]  <- 4
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Rest of Circulatory diseases"]  <- 5
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Mental and Nervous system"]  <- 6
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Alcohol attributable"]  <- 7
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="External"]  <- 8
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Infectious (respiratory diseases)"]  <- 9
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Non infectious respiratory disases"]  <- 10
Results.UK_.SD.plot$Cause2[Results.UK_.SD.plot$Cause=="Other No Infections No respiratory"]  <- 11


Results.UK_.SD.plot$Cause2 <- factor(Results.UK_.SD.plot$Cause2, levels = c(1:11),
                                     labels =   c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                                  "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                                  "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                                  "Non-infectious respiratory disases","Rest of causes"))

# For datatable
Results.UK_.SD.tab  <- Results.UK_.SD.plot[,list(Contribution = sum(Contribution2)), by = list(Cause, Period)]
Results.UK_.SD.tab <- data.table(Results.UK_.SD.tab)
Results.UK_.SD.tab$Country.name <- "United Kingdom"

# -- Plot of decomposition result
UK_SG_Deco <- ggplot(data=Results.UK_.SD.plot, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Period, ncol=3)+
  coord_flip()+
  theme_classic() +
  scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2000-2015, United Kingdom'),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")


######################
# Combine data and save it
######################



#Results.Aus.SD.plot
Change_SD_years <- rbind(Results.Aus.SD.plot,  Results.Bel.SD.plot, Results.Bela.SD.plot, Results.Bul.SD.plot, 
                         Results.CZE_.SD.plot, Results.DNK_.SD.plot, Results.EST_.SD.plot, Results.FIN_.SD.plot, Results.HUN_.SD.plot,
                         Results.FRA_.SD.plot, Results.GER_.SD.plot, Results.GRE.SD.plot, Results.ITA_.SD.plot, Results.IRE.SD.plot,
                         Results.NDL_.SD.plot, Results.NOR_.SD.plot, Results.RUS_.SD.plot, Results.LAT_.SD.plot, Results.LIT_.SD.plot,
                         Results.POL_.SD.plot, Results.POR.SD.plot, Results.SLV_.SD.plot, Results.SLN_.SD.plot, Results.ESP_.SD.plot,
                         Results.SWE_.SD.plot, Results.SWI_.SD.plot, Results.UKR_.SD.plot, Results.UK_.SD.plot)

# To check that we are not missing any country
table(Change_SD_years$Country.name)


# Save the data with all countries and decomposition results
save(Change_SD_years, file = "Data/SD_Decomposition_stepwise_replacement.RData")
