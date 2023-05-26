# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Supplementary Material Tables and Figures 2
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Tables and Figures
#      2.1 Table 1
#      2.2 Figure 1
#      2.3 Figure 2
#      2.4 Figure 3
#      2.5 Figure 4
#      2.6 Figure 5
#      2.7 Figure 6

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

# -- e0
get(load("Data/e0_Decomposition_stepwise_replacement.RData"))

# -- SD
get(load("Data/SD_Decomposition_stepwise_replacement.RData"))


# --------------------------------------------------------------------------- #
#   Table 2 and 4 - e0 
# --------------------------------------------------------------------------- #

# ----------------------
#     Table 2
# ----------------------

# We create the age groups according to table in SM2 
Change_e0_years$Age_groups[Change_e0_years$Age2=="0" | Change_e0_years$Age2=="1-4"  |
                             Change_e0_years$Age2=="5-9"] <- 1
Change_e0_years$Age_groups[Change_e0_years$Age2=="10-14" | Change_e0_years$Age2=="15-19"] <-2 
Change_e0_years$Age_groups[Change_e0_years$Age2=="20-24"| Change_e0_years$Age2=="25-29"] <- 3
Change_e0_years$Age_groups[Change_e0_years$Age2=="30-34" | Change_e0_years$Age2=="35-39"] <- 4
Change_e0_years$Age_groups[Change_e0_years$Age2=="40-44"| Change_e0_years$Age2=="45-49"] <- 5
Change_e0_years$Age_groups[Change_e0_years$Age2=="50-54" | Change_e0_years$Age2=="55-59"] <- 6
Change_e0_years$Age_groups[Change_e0_years$Age2=="60-64" | Change_e0_years$Age2=="65-69"] <- 7
Change_e0_years$Age_groups[Change_e0_years$Age2=="70-74" | Change_e0_years$Age2=="75-79"] <- 8 
Change_e0_years$Age_groups[Change_e0_years$Age2=="80-84"| Change_e0_years$Age2=="85-89"] <- 9
Change_e0_years$Age_groups[Change_e0_years$Age2=="90-94" | Change_e0_years$Age2=="95-99" 
                           | Change_e0_years$Age2=="100+"] <- 10


Change_e0_years$Age_groups <- factor(Change_e0_years$Age_groups, 
                                     levels = c(1,2,3,4,5,6,7,8,9,10),
                                     labels = c("0-9","10-19","20-29","30-39",
                                                "40-49","50-59","60-69","70-79","80-89","90+"))


# Filter last period
Data_table_2 <- Change_e0_years %>% 
  filter(Period=="2010-2015") %>% 
  dplyr::select(Age_groups, Contribution2, Country.name, Cause2, Period)

Data_table_2 <- data.table(Data_table_2)

Data_table_2_age <- Data_table_2[,list(Contribution = sum(Contribution2)),
                                 by = list(Country.name,Age_groups, Period)]

# Transform to table SM2 Format

Table_2 <- Data_table_2_age %>% 
  group_by(Country.name, Period) %>% 
  mutate(Relative_contribution = Contribution/sum(Contribution)) %>% 
  dplyr::select(Age_groups, Country.name, Period, Relative_contribution) %>% 
  pivot_wider(names_from = Age_groups, values_from = Relative_contribution)

write.csv(Table_2, file = "Supplementary Material/3. Individual Countries/Table_2.csv")


# ----------------------
#     Table 4
# ----------------------

Data_table_4 <- Change_e0_years %>% 
  filter(Period=="2010-2015") %>% 
  dplyr::select(Age2, Contribution2, Country.name, Cause2, Period)

Data_table_4 <- data.table(Data_table_4)

Data_table_4_COD <- Data_table_4[,list(Contribution = sum(Contribution2)),
                                 by = list(Country.name,Cause2, Period)]

# Transform to table SM2 Format

Table_4 <- Data_table_4_COD %>% 
  group_by(Country.name, Period) %>% 
  mutate(Relative_contribution = Contribution/sum(Contribution)) %>% 
  dplyr::select(Cause2, Country.name, Period, Relative_contribution) %>% 
  pivot_wider(names_from = Cause2, values_from = Relative_contribution)

write.csv(Table_4, file = "Supplementary Material/3. Individual Countries/Table_4.csv")


# --------------------------------------------------------------------------- #
#   Table 3 and 5 - SD
# --------------------------------------------------------------------------- #

# ----------------------
#     Table 3
# ----------------------

Change_SD_years$Age_groups[Change_SD_years$Age2=="0" | Change_SD_years$Age2=="1-4"  |
                             Change_SD_years$Age2=="5-9"] <- 1
Change_SD_years$Age_groups[Change_SD_years$Age2=="10-14" | Change_SD_years$Age2=="15-19"] <-2 
Change_SD_years$Age_groups[Change_SD_years$Age2=="20-24"| Change_SD_years$Age2=="25-29"] <- 3
Change_SD_years$Age_groups[Change_SD_years$Age2=="30-34" | Change_SD_years$Age2=="35-39"] <- 4
Change_SD_years$Age_groups[Change_SD_years$Age2=="40-44"| Change_SD_years$Age2=="45-49"] <- 5
Change_SD_years$Age_groups[Change_SD_years$Age2=="50-54" | Change_SD_years$Age2=="55-59"] <- 6
Change_SD_years$Age_groups[Change_SD_years$Age2=="60-64" | Change_SD_years$Age2=="65-69"] <- 7
Change_SD_years$Age_groups[Change_SD_years$Age2=="70-74" | Change_SD_years$Age2=="75-79"] <- 8 
Change_SD_years$Age_groups[Change_SD_years$Age2=="80-84"| Change_SD_years$Age2=="85-89"] <- 9
Change_SD_years$Age_groups[Change_SD_years$Age2=="90-94" | Change_SD_years$Age2=="95-99" 
                           | Change_SD_years$Age2=="100+"] <- 10


Change_SD_years$Age_groups <- factor(Change_SD_years$Age_groups, 
                                     levels = c(1,2,3,4,5,6,7,8,9,10),
                                     labels = c("0-9","10-19","20-29","30-39",
                                                "40-49","50-59","60-69","70-79","80-89","90+"))


Data_table_3 <- Change_SD_years %>% 
  filter(Period=="2010-2015") %>% 
  dplyr::select(Age_groups, Contribution2, Country.name, Cause2, Period)


Data_table_3 <- data.table(Data_table_3)

Data_table_3_age <- Data_table_3[,list(Contribution = sum(Contribution2)),
                                 by = list(Country.name,Age_groups, Period)]

# Transform to table SM2 Format
Table_3 <- Data_table_3_age %>% 
  group_by(Country.name, Period) %>% 
  mutate(Relative_contribution = Contribution/sum(Contribution)) %>% 
  dplyr::select(Age_groups, Country.name, Period, Relative_contribution) %>% 
  pivot_wider(names_from = Age_groups, values_from = Relative_contribution)


write.csv(Table_3, file = "Supplementary Material/3. Individual Countries/Table_3.csv")

# ----------------------
#     Table 5
# ----------------------

Data_table_5 <- Change_SD_years %>% 
  filter(Period=="2010-2015") %>% 
  dplyr::select(Age2, Contribution2, Country.name, Cause2, Period)


Data_table_5 <- data.table(Data_table_5)

Data_table_5_CoD <- Data_table_5[,list(Contribution = sum(Contribution2)),
                                 by = list(Country.name,Cause2, Period)]

# Transform to table SM2 Format
Table_5 <- Data_table_5_CoD %>% 
  group_by(Country.name, Period) %>% 
  mutate(Relative_contribution = Contribution/sum(Contribution)) %>% 
  dplyr::select(Cause2, Country.name, Period, Relative_contribution) %>% 
  pivot_wider(names_from = Cause2, values_from = Relative_contribution)

write.csv(Table_5, file = "Supplementary Material/3. Individual Countries/Table_5.csv")

# --------------------------------------------------------------------------- #
#   Figures e0 1-5
# --------------------------------------------------------------------------- #


Period_10_15 <- Change_e0_years %>% 
  filter(Period=="2010-2015") %>% 
  mutate(Region = case_when(Country.name=="Denmark" | Country.name=="Norway" |
                              Country.name=="Sweden" | Country.name=="Finland" ~ 1,
                            Country.name=="Austria" | Country.name=="Belgium" | Country.name=="France" |
                              Country.name=="Germany" | Country.name=="Netherlands" | Country.name=="Ireland" |
                              Country.name=="Switzerland" | Country.name=="United Kingdom" ~ 2,
                            Country.name=="Greece" | Country.name=="Portugal" | 
                              Country.name=="Italy" | Country.name=="Spain" ~ 3,
                            Country.name=="Czech Republic" | Country.name=="Hungary" | Country.name=="Bulgaria" |
                              Country.name=="Poland" | Country.name=="Slovakia" |
                              Country.name=="Slovenia" ~ 4,
                            Country.name=="Estonia" | Country.name=="Latvia" | Country.name=="Ukraine" |
                              Country.name=="Belarus" | Country.name=="Russia" | Country.name=="Lithuania" ~ 5))


# ------------------------------------ #
#   Nordic
# ------------------------------------ #

Nordic <- Period_10_15 %>% 
  filter(Region==1)

Nordic_SG_Deco <- ggplot(data=Nordic, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=2)+
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
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2010-2015, Nordic countries' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
Nordic_SG_Deco
ggsave(filename = "Fig 1.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   Western
# ------------------------------------ #

Western <- Period_10_15 %>% 
  filter(Region==2)

Western_SG_Deco <- ggplot(data=Western, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=3)+
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
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2010-2015, Western countries' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
Western_SG_Deco
ggsave(filename = "Fig 2.png",path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   Southern
# ------------------------------------ #

Southern <- Period_10_15 %>% 
  filter(Region==3)

Southern_SG_Deco <- ggplot(data=Southern, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=2)+
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
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2010-2015, Southern countries' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
Southern_SG_Deco
ggsave(filename = "Fig 3.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   CEE
# ------------------------------------ #

CEE <- Period_10_15 %>% 
  filter(Region==4)

CEE_SG_Deco <- ggplot(data=CEE, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=3)+
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
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2010-2015, CEE countries' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
CEE_SG_Deco
ggsave(filename = "Fig 4.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   FSR
# ------------------------------------ #

FSR <- Period_10_15 %>% 
  filter(Region==5)

FSR_SG_Deco <- ggplot(data=FSR, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=3)+
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
  labs(title = bquote(~'Contribution to the Sex gap in '~ e[0] ~'2010-2015, FSR countries' ),
       fill = "Causes",
       y=bquote(~'Contribution to the Sex gap in '~ e[0]),
       x="Age")
FSR_SG_Deco
ggsave(filename = "Fig 5.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")


# --------------------------------------------------------------------------- #
#   Figures SD 6-10
# --------------------------------------------------------------------------- #

Period_10_15_SD <- Change_SD_years %>% 
  filter(Period=="2010-2015") %>% 
  mutate(Region = case_when(Country.name=="Denmark" | Country.name=="Norway" |
                              Country.name=="Sweden" | Country.name=="Finland" ~ 1,
                            Country.name=="Austria" | Country.name=="Belgium" | Country.name=="France" |
                              Country.name=="Germany" | Country.name=="Netherlands" | Country.name=="Ireland" |
                              Country.name=="Switzerland" | Country.name=="United Kingdom" ~ 2,
                            Country.name=="Greece" | Country.name=="Portugal" | 
                              Country.name=="Italy" | Country.name=="Spain" ~ 3,
                            Country.name=="Czech Republic" | Country.name=="Hungary" | Country.name=="Bulgaria" |
                              Country.name=="Poland" | Country.name=="Slovakia" |
                              Country.name=="Slovenia" ~ 4,
                            Country.name=="Estonia" | Country.name=="Latvia" | Country.name=="Ukraine" |
                              Country.name=="Belarus" | Country.name=="Russia" | Country.name=="Lithuania" ~ 5))


# ------------------------------------ #
#   Nordic
# ------------------------------------ #

Nordic_SD <- Period_10_15_SD %>% 
  filter(Region==1)

Nordic_SG_Deco_SD <- ggplot(data=Nordic_SD, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=2)+
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
  labs(title = 'Contribution to the Sex gap in SD 2010-2015, Nordic countries',
       fill = "Causes",
       y='Contribution to the Sex gap in SD',
       x="Age")
Nordic_SG_Deco_SD
ggsave(filename = "Fig 6.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   Western
# ------------------------------------ #

Western_SD <- Period_10_15_SD %>% 
  filter(Region==2)

Western_SG_Deco_SD <- ggplot(data=Western_SD, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=3)+
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
  labs(title = 'Contribution to the Sex gap in SD 2010-2015, Western countries',
       fill = "Causes",
       y='Contribution to the Sex gap in SD',
       x="Age")
Western_SG_Deco_SD
ggsave(filename = "Fig 7.png",path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   Southern
# ------------------------------------ #

Southern_SD <- Period_10_15_SD %>% 
  filter(Region==3)

Southern_SG_Deco_SD <- ggplot(data=Southern_SD, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=2)+
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
  labs(title = 'Contribution to the Sex gap in SD 2010-2015, Southern countries',
       fill = "Causes",
       y='Contribution to the Sex gap in SD',
       x="Age")
Southern_SG_Deco_SD
ggsave(filename = "Fig 8.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   CEE
# ------------------------------------ #

CEE_SD <- Period_10_15_SD %>% 
  filter(Region==4)

CEE_SG_Deco_SD <- ggplot(data=CEE_SD, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=3)+
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
  labs(title = 'Contribution to the Sex gap in SD 2010-2015, CEE countries',
       fill = "Causes",
       y='Contribution to the Sex gap in SD',
       x="Age")
CEE_SG_Deco_SD
ggsave(filename = "Fig 9.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")

# ------------------------------------ #
#   FSR
# ------------------------------------ #

FSR_SD <- Period_10_15_SD %>% 
  filter(Region==5)

FSR_SG_Deco_SD <- ggplot(data=FSR_SD, aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Country.name, ncol=3)+
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
  labs(title = 'Contribution to the Sex gap in SD 2010-2015, FSR countries',
       fill = "Causes",
       y='Contribution to the Sex gap in SD',
       x="Age")
FSR_SG_Deco_SD
ggsave(filename = "Fig 10.png", path= "Supplementary Material/3. Individual Countries/",
       dpi = 320, width = 10.5, height = 8,
       bg = "transparent")



