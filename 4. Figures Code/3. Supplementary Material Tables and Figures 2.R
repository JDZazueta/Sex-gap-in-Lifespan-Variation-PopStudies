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


# -- Europe
get(load("Data/Decomposition_SD_Europe.RData"))
Europe_decomp_SD$Measure <- "SD"

get(load("Data/Decomposition_e0_Europe.RData"))
Europe_decomp_e0$Measure <- "e0"

# -- Regions
get(load("Data/Decomposition_SD_regions.RData"))
All_regions_decomp_SD$Measure <- "SD"

get(load("Data/Decomposition_e0_regions.RData"))
All_regions_decomp_e0$Measure <- "e0"

Data_figures_tables <- rbind(All_regions_decomp_e0, 
                             All_regions_decomp_SD,
                             Europe_decomp_e0, 
                             Europe_decomp_SD)



# We create 10 year age groups
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="0"] <- 0  
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="1-4"  | Data_figures_tables$Age2=="5-9"] <- 1
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="10-14" | Data_figures_tables$Age2=="15-19"] <-2 
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="20-24"| Data_figures_tables$Age2=="25-29"] <- 3
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="30-34" | Data_figures_tables$Age2=="35-39"] <- 4
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="40-44"| Data_figures_tables$Age2=="45-49"] <- 5
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="50-54" | Data_figures_tables$Age2=="55-59"] <- 6
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="60-64" | Data_figures_tables$Age2=="65-69"] <- 7
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="70-74" | Data_figures_tables$Age2=="75-79"] <- 8 
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="80-84"| Data_figures_tables$Age2=="85-89"] <- 9
Data_figures_tables$Age_groups[Data_figures_tables$Age2=="90-94" | Data_figures_tables$Age2=="95-99" 
                               | Data_figures_tables$Age2=="100+"] <- 10


# We create Causes of death groups
Data_figures_tables$Cause2[Data_figures_tables$Cause=="C. Sesitive Smoking"]  <- 1
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Sex-specif cancer"]  <- 2
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Other Cancers"]  <- 3
Data_figures_tables$Cause2[Data_figures_tables$Cause=="IHD and Stroke"]  <- 4
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Rest of Circulatory diseases"]  <- 5
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Mental and Nervous system"]  <- 6
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Alcohol attributable"]  <- 7
Data_figures_tables$Cause2[Data_figures_tables$Cause=="External"]  <- 8
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Infectious (respiratory diseases)"]  <- 9
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Non infectious respiratory disases"]  <- 10
Data_figures_tables$Cause2[Data_figures_tables$Cause=="Other No Infections No respiratory"]  <- 11



# ---------------------------------------------------------------------------- #
#    2. Tables and Figures
# ---------------------------------------------------------------------------- #

# ------------------------
#  Table 1
# ------------------------

Data_figures_tables <- data.table(Data_figures_tables)

Data_table_1 <- Data_figures_tables[,list(Gap = sum(Contribution2)),
                                    by=list(Period, Region, Measure)]

Table_SM_1 <- Data_table_1 

write.csv(Table_SM_1,
          file = "Supplementary Material/2. European Regions all periods/Table 1 SM2.csv")

# ------------------------
#  Figure 1 
# ------------------------

Data_figure_1 <- Data_figures_tables[,list(Contribution = sum(Contribution2)),
                                     by=list(Period, Region, Measure, Age_groups)]



Data_figure_1 <- Data_figure_1 %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6),
         Measure_2 = case_when(Measure=="e0" ~ 1,
                               Measure=="SD" ~ 2)) 

# --- Labeling

# Age groups
Data_figure_1$Age_groups <- factor(Data_figure_1$Age_groups, 
                                   levels = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0", "1-9","10-19","20-29","30-39",
                                              "40-49","50-59","60-69","70-79","80-89","90+"))
# Measure
Data_figure_1$Measure_2 <- factor(Data_figure_1$Measure_2,
                                  levels = c(1,2),
                                  labels = c("Life expectancy (e0)", "Lifespan variation (SD)"))

# Region
Data_figure_1$Region_2 <- factor(Data_figure_1$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))



Fig_1 <- ggplot(Data_figure_1, mapping = aes(x=Contribution,
                                             y=Age_groups,
                                             fill=Period))+
  geom_bar(stat = "identity", position = "dodge", alpha =.7) +
  #geom_text(aes(x=Contribution, y=Age_groups,
  #              label = round(Contribution,2)),
  #          position=position_dodge(width = 1),
  #          hjust = -.2, size = 4) + 
  facet_grid(Measure_2~Region_2)+
  theme_bw() +
  #coord_flip() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits =  c(-1,3)) +
  scale_fill_manual(values = color_year) +
  theme(text = element_text(size = 20),
        legend.position="bottom", legend.box = "vertical",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "",
       fill = "Sex gap",
       y="",
       x= "Absolute contribution (years) to the sex gap")
Fig_1
ggsave(filename = "Figure 1 SM2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")


# ------------------------
#  Figure 2
# ------------------------


Data_figure_2 <- Data_figures_tables[,list(Contribution = sum(Contribution2)),
                                     by=list(Period, Region, Measure, Cause2)]



Data_figure_2 <- Data_figure_2 %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6),
         Measure_2 = case_when(Measure=="e0" ~ 1,
                               Measure=="SD" ~ 2)) 

# --- Labeling

# Causes of death
Data_figure_2$Cause2 <- factor(Data_figure_2$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                          "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                          "Non infectious respiratory disases","Rest of causes"))


# Measure
Data_figure_2$Measure_2 <- factor(Data_figure_2$Measure_2,
                                  levels = c(1,2),
                                  labels = c("Life expectancy (e0)", "Lifespan variation (SD)"))

# Region
Data_figure_2$Region_2 <- factor(Data_figure_2$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))



Fig_2 <- ggplot(Data_figure_2, mapping = aes(x=Contribution,
                                             y=Cause2,
                                             fill=Period))+
  geom_bar(stat = "identity", position = "dodge", alpha =.7) +
  facet_grid(Measure_2~Region_2)+
  theme_bw() +
  #coord_flip() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits =  c(-1,5)) +
  scale_fill_manual(values = color_year) +
  theme(text = element_text(size = 20),
        legend.position="bottom", legend.box = "vertical",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "",
       fill = "Sex gap",
       y="",
       x= "Absolute contribution (years) to the sex gap")
Fig_2
ggsave(filename = "Figure 2 SM2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")


# ------------------------
#  Figure 3
# ------------------------

Data_figure_3 <- Data_figures_tables %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6),
         Measure_2 = case_when(Measure=="e0" ~ 1,
                               Measure=="SD" ~ 2)) %>% 
  filter(Measure_2==1)

# --- Labeling

# Causes of death
Data_figure_3$Cause2 <- factor(Data_figure_3$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                          "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                          "Non infectious respiratory disases","Rest of causes"))


# Measure
Data_figure_3$Measure_2 <- factor(Data_figure_3$Measure_2,
                                  levels = c(1,2),
                                  labels = c("Life expectancy (e0)", "Lifespan variation (SD)"))

# Region
Data_figure_3$Region_2 <- factor(Data_figure_3$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))
# ---------
# Non-Eastern European countries
# ---------
Data_figure_3a <- Data_figure_3 %>% 
  filter(Region_2=="Nordic" | Region_2=="Western" | Region_2=="Southern")

Fig_3a <- ggplot(data=Data_figure_3a,
                aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Region_2~Period)+
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
Fig_3a
ggsave(filename = "Figure 3a SM2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")

# ---------
# Eastern European countries
# ---------

Data_figure_3b <- Data_figure_3 %>% 
  filter(Region_2=="Europe" | Region_2=="CEE" | Region_2=="FSR")

Fig_3b <- ggplot(data=Data_figure_3b,
                 aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Region_2~Period)+
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
Fig_3b
ggsave(filename = "Figure 3b SM2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")


# ------------------------
#  Figure 4
# ------------------------

Data_figure_4 <- Data_figures_tables %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6),
         Measure_2 = case_when(Measure=="e0" ~ 1,
                               Measure=="SD" ~ 2)) %>% 
  filter(Measure_2==2)

# --- Labeling

# Causes of death
Data_figure_4$Cause2 <- factor(Data_figure_4$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                          "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                          "Non infectious respiratory disases","Rest of causes"))


# Measure
Data_figure_4$Measure_2 <- factor(Data_figure_4$Measure_2,
                                  levels = c(1,2),
                                  labels = c("Life expectancy (e0)", "Lifespan variation (SD)"))

# Region
Data_figure_4$Region_2 <- factor(Data_figure_4$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))


# ---------
# Non-Eastern European countries
# ---------
Data_figure_4a <- Data_figure_4 %>% 
  filter(Region_2=="Nordic" | Region_2=="Western" | Region_2=="Southern")

Fig_4a <- ggplot(data=Data_figure_4a,
                 aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Region_2~Period)+
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
Fig_4a
ggsave(filename = "Figure 4a SM2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")

# ---------
# Eastern European countries
# ---------

Data_figure_4b <- Data_figure_4 %>% 
  filter(Region_2=="Europe" | Region_2=="CEE" | Region_2=="FSR")

Fig_4b <- ggplot(data=Data_figure_4b,
                 aes(x=Age2, y=Contribution2, fill=Cause2)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(Region_2~Period)+
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
Fig_4b
ggsave(filename = "Figure 4b SM2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")


# ------------------------
#  Figure 5
# ------------------------

Data_figure_5 <- Data_figures_tables[,list(Contribution = sum(Contribution2)),
                                     by=list(Period, Region, Measure, Age_groups)]


Data_figure_5 <- Data_figure_5 %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6),
         Measure_2 = case_when(Measure=="e0" ~ 1,
                               Measure=="SD" ~ 2)) %>% 
  dplyr::select(-c(Region, Measure)) %>% 
  group_by(Measure_2,Region_2, Period) %>% 
  mutate(Total = sum(Contribution)) %>% 
  ungroup() %>% 
  mutate(Relative = Contribution/Total*100)


# --- Labeling

# Age groups
Data_figure_5$Age_groups <- factor(Data_figure_5$Age_groups, 
                                   levels = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0", "1-9","10-19","20-29","30-39",
                                              "40-49","50-59","60-69","70-79","80-89","90+"))
# Measure
Data_figure_5$Measure_2 <- factor(Data_figure_5$Measure_2,
                                  levels = c(1,2),
                                  labels = c("SGLE", "SGLV"))

# Region
Data_figure_5$Region_2 <- factor(Data_figure_5$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))


Data_figure_5_wider <- Data_figure_5 %>% 
  dplyr::select(-c(Total,Contribution)) %>% 
  pivot_wider(names_from = Measure_2, values_from = Relative)


Fig_5 <- ggplot(Data_figure_5_wider, mapping = aes(x=SGLE,
                                                   y=SGLV))+
  geom_point(aes(col=Age_groups, shape=Period), size=5) +
  facet_wrap(~Region_2,  ncol=3, scales = "free") +
  theme_tufte() +
  scale_color_manual(values = Age_color_fig5) +
  scale_y_continuous(limits = c(-80,80), breaks = c(-80,-60,-40,-20,0,20,40,60,80)) +
  scale_x_continuous(limits = c(-80,80), breaks = c(-80,-60,-40,-20,0,20,40,60,80)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 20),
        legend.position="bottom", legend.box = "vertical",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18),
        axis.text.x = element_text(angle = 90)) +
  guides(color = guide_legend(order=1, ncol = 6),
         shape = guide_legend(order=2)) +
  labs(title = "",
       fill = "Sex gap",
       x=bquote(~'Contribution to the sex gap in '~ e0 ~ ""),
       y=bquote(~'Relative contribution (%) to the sex gap in '~ SD ~ ""))
Fig_5
ggsave(filename = "Figure 5 SM-2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")


# ------------------------
#  Figure 6
# ------------------------

Data_figure_6 <- Data_figures_tables[,list(Contribution = sum(Contribution2)),
                                     by=list(Period, Region, Measure, Cause2)]




Data_figure_6 <- Data_figure_6 %>% 
  mutate(Region_2 = case_when(Region=="Europe" ~ 1,
                              Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6),
         Measure_2 = case_when(Measure=="e0" ~ 1,
                               Measure=="SD" ~ 2)) %>% 
 dplyr::select(-c(Region, Measure)) %>% 
  group_by(Measure_2,Region_2, Period) %>% 
  mutate(Total = sum(Contribution)) %>% 
  ungroup() %>% 
  mutate(Relative = Contribution/Total*100)


# --- Labeling

# Causes of death
Data_figure_6$Cause2 <- factor(Data_figure_6$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and Stroke", "Rest of circulatory diseases", "Mental and nervous system",
                                          "Alcohol-attributable causes", "External causes", "Infectious (respiratory) diseases",
                                          "Non infectious respiratory disases","Rest of causes"))

# Measure
Data_figure_6$Measure_2 <- factor(Data_figure_6$Measure_2,
                                  levels = c(1,2),
                                  labels = c("SGLE", "SGLV"))

# Region
Data_figure_6$Region_2 <- factor(Data_figure_6$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))


Data_figure_6_wider <- Data_figure_6 %>% 
  dplyr::select(-c(Total,Contribution)) %>% 
  pivot_wider(names_from = Measure_2, values_from = Relative)


Fig_6 <- ggplot(Data_figure_6_wider, mapping = aes(x=SGLE,
                                                   y=SGLV))+
  geom_point(aes(col=Cause2, shape=Period), size=5) +
  facet_wrap(~Region_2,  ncol=3, scales = "free") +
  theme_tufte() +
  scale_color_manual(values = CoD_Decomp.new) +
  scale_y_continuous(limits = c(-100,100), breaks = c(-100,-50,0,50,100)) +
  scale_x_continuous(limits = c(-100,100), breaks = c(-100,-50,0,50,100)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 20),
        legend.position="bottom", legend.box = "vertical",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18),
        axis.text.x = element_text(angle = 90)) +
  guides(color = guide_legend(order=1, ncol = 6),
         shape = guide_legend(order=2)) +
  labs(title = "",
       fill = "Sex gap",
       x=bquote(~'Contribution to the sex gap in '~ e0 ~ ""),
       y=bquote(~'Relative contribution (%) to the sex gap in '~ SD ~ ""))
Fig_6
ggsave(filename = "Figure 6 SM-2.png", path= "Supplementary Material/2. European Regions all periods/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")