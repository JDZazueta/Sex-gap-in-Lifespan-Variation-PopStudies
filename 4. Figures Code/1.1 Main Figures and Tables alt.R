# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Main figures and tables
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

Table_1 <- Data_table_1 %>% 
  filter(Period=="2010-2015")

write.csv(Table_1, file = "Main Figures and Tables/Table 1.csv")

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
                               Measure=="SD" ~ 2)) %>% 
  filter(Period=="2010-2015")

# --- Labeling

# Age groups
Data_figure_1$Age_groups <- factor(Data_figure_1$Age_groups, 
                                   levels = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0", "1-9","10-19","20-29","30-39",
                                              "40-49","50-59","60-69","70-79","80-89","90+"))
# Measure
Data_figure_1$Measure_2 <- factor(Data_figure_1$Measure_2,
                                  levels = c(1,2),
                                  labels = c("Life expectancy e0",
                                             "Lifespan variation (SD)"))

# Region
Data_figure_1$Region_2 <- factor(Data_figure_1$Region_2,
                                 levels = c(1,2,3,4,5,6),
                                 labels = c("Europe",
                                            "Nordic",
                                            "Western",
                                            "Southern",
                                            "CEE",
                                            "FSR"))

Data_figure_1$Contribution <- round(Data_figure_1$Contribution,2)

Fig_1 <- ggplot(Data_figure_1, mapping = aes(x=Contribution,
                                             y=Age_groups,
                                             fill=Measure_2))+
  geom_bar(stat = "identity", position = "dodge", alpha =.7) +
  geom_text(aes(x=Contribution, y=Age_groups,
                label = sprintf("%0.2f", round(Contribution, digits = 2))),
            position=position_dodge(width = 1),
            hjust = -.2, size = 4) + 
  facet_grid(Measure_2~Region_2)+
  theme_bw() +
  #coord_flip() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits =  c(-1,3.5)) +
  scale_fill_manual(values = c("black", "grey80")) +
  theme(text = element_text(size = 20),
        legend.position="bottom", legend.box = "vertical",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "",
       fill = "Sex gap",
       y="Age group",
       x= "Absolute contribution (years) to the sex gap")
Fig_1
ggsave(filename = "Figure 1.eps", path= "Main Figures and Tables/",
       dpi = 500, width = 12.5, height = 10.5,
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
                               Measure=="SD" ~ 2)) %>% 
  filter(Period=="2010-2015")

# --- Labeling

# Causes of death
Data_figure_2$Cause2 <- factor(Data_figure_2$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and stroke", "Rest of circulatory diseases", "Mental and nervous system",
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
                                             fill=Measure_2))+
  geom_bar(stat = "identity", position = "dodge", alpha =.7) +
  geom_text(aes(x=Contribution, y=Cause2,
                label = sprintf("%0.2f", round(Contribution, digits = 2))),
            position=position_dodge(width = 1),
            hjust = -.2, size = 4) + 
  facet_grid(Measure_2~Region_2)+
  theme_bw() +
  #coord_flip() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits =  c(-1,5)) +
  scale_fill_manual(values = c("black", "grey80")) +
  theme(text = element_text(size = 19),
        legend.position="bottom", legend.box = "vertical",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 17),
        legend.text = element_text(color = "Black", size = 17),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "",
       fill = "Sex gap",
       y="",
       x= "Absolute contribution (years) to the sex gap")
Fig_2
ggsave(filename = "Figure 2.eps", path= "Main Figures and Tables/",
       dpi = 500, width = 12.5, height = 10.5,
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
  filter(Period=="2010-2015") %>% 
  filter(Measure_2==1)

# --- Labeling

# Causes of death
Data_figure_3$Cause2 <- factor(Data_figure_3$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and stroke", "Rest of circulatory diseases", "Mental and nervous system",
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



Fig_3 <- ggplot(data=Data_figure_3,
                aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  #scale_fill_brewer(palette = "RdGy") +
  scale_fill_brewer(palette = "Greys") +
  scale_pattern_fill2_grey() +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    #pattern = "Causes",
    #color = "Causes",
    y=bquote(~'Absolute contribution (years) to the sex gap '~ e0 ~ ""),
    x="Age groups")
#Fig_3
#ggsave(filename = "Figure 3.eps", path= "Main Figures and Tables/",
#       dpi = 320, width = 12.5, height = 10.5,
#       bg = "transparent")



# -------------
#  Europe
# -------------

Data_fig_3_Europe <- Data_figure_3 %>% 
  filter(Region_2=="Europe")


fig_3_Europe <- ggplot(data=Data_fig_3_Europe,
                       aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  15.5, linetype="dashed") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
fig_3_Europe

# -------------
#  Nordic
# -------------

Data_fig_3_Nordic <- Data_figure_3 %>% 
  filter(Region_2=="Nordic")


fig_3_Nordic <- ggplot(data=Data_fig_3_Nordic,
                       aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  #scale_pattern_fill2_grey() +
  geom_vline(xintercept =  16.5, linetype="dashed") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
fig_3_Nordic


# -------------
#  Western
# -------------

Data_fig_3_Western <- Data_figure_3 %>% 
  filter(Region_2=="Western")


fig_3_Western <- ggplot(data=Data_fig_3_Western,
                        aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  15.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
fig_3_Western

# -------------
#  Southern
# -------------

Data_fig_3_Southern <- Data_figure_3 %>% 
  filter(Region_2=="Southern")


fig_3_Southern <- ggplot(data=Data_fig_3_Southern,
                         aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  16.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
fig_3_Southern

# -------------
#  CEE
# -------------

Data_fig_3_CEE <- Data_figure_3 %>% 
  filter(Region_2=="CEE")


fig_3_CEE <- ggplot(data=Data_fig_3_CEE,
                    aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  14.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y=bquote(~'Absolute contribution (years) to the sex gap '~ e0 ~ ""),
    y = "",
    x="")
fig_3_CEE

# -------------
#  FSR
# -------------

Data_fig_3_FSR <- Data_figure_3 %>% 
  filter(Region_2=="FSR")


fig_3_FSR <- ggplot(data=Data_fig_3_FSR,
                    aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  13.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    #color = "Causes",
    y= "",
    x="")
fig_3_FSR


# We combine the different figures
fig_3 <- ggarrange(fig_3_Europe, fig_3_Nordic, fig_3_Western,
                   fig_3_Southern, fig_3_CEE, fig_3_FSR, 
                   #label.x = "Absolute contribuiton",
                   #label.y = 0,
                   common.legend = T, legend="bottom", ncol = 3, nrow = 2)


# Annotate the figure by adding a common labels
annotate_figure(fig_3,
                #bottom = text_grob(bquote(~'Absolute contribution (years) to the sex gap '~ SD ~ ""),
                #                   color = "Black",
                #                   hjust = 0.5, x = 0.5, size = 10),
                left = text_grob("Age group", size=16, 
                                 color = "black", rot = 90))

ggsave(filename = "Figure 3.eps", path= "Main Figures and Tables/",
       dpi = 500, width = 12.5, height = 10.5,
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
  filter(Period=="2010-2015") %>% 
  filter(Measure_2==2)

# --- Labeling

# Causes of death
Data_figure_4$Cause2 <- factor(Data_figure_4$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and stroke", "Rest of circulatory diseases", "Mental and nervous system",
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

# For figure 4 since we define each individual threshold age, we need to do
# individual figures and later combined them

# -------------
#  Europe
# -------------

Data_fig_4_Europe <- Data_figure_4 %>% 
  filter(Region_2=="Europe")


Fig_4_Europe <- ggplot(data=Data_fig_4_Europe,
                       aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  15.5, linetype="dashed") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
Fig_4_Europe

# -------------
#  Nordic
# -------------

Data_fig_4_Nordic <- Data_figure_4 %>% 
  filter(Region_2=="Nordic")


Fig_4_Nordic <- ggplot(data=Data_fig_4_Nordic,
                       aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  #scale_pattern_fill2_grey() +
  geom_vline(xintercept =  16.5, linetype="dashed") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
Fig_4_Nordic


# -------------
#  Western
# -------------

Data_fig_4_Western <- Data_figure_4 %>% 
  filter(Region_2=="Western")


Fig_4_Western <- ggplot(data=Data_fig_4_Western,
                        aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  15.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
Fig_4_Western

# -------------
#  Southern
# -------------

Data_fig_4_Southern <- Data_figure_4 %>% 
  filter(Region_2=="Southern")


Fig_4_Southern <- ggplot(data=Data_fig_4_Southern,
                         aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  16.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= "",
    x="")
Fig_4_Southern

# -------------
#  CEE
# -------------

Data_fig_4_CEE <- Data_figure_4 %>% 
  filter(Region_2=="CEE")


Fig_4_CEE <- ggplot(data=Data_fig_4_CEE,
                    aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  14.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 12), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    y= bquote(~'Absolute contribution (years) to the sex gap '~ SD ~ ""),
    y = "",
    x="")
Fig_4_CEE

# -------------
#  FSR
# -------------

Data_fig_4_FSR <- Data_figure_4 %>% 
  filter(Region_2=="FSR")


Fig_4_FSR <- ggplot(data=Data_fig_4_FSR,
                    aes(x=Age2, y=Contribution2, fill=Cause2)) +
  #geom_bar(stat = "identity", position = "stack") +
  geom_bar_pattern(
    aes(pattern_fill = Cause2),
    stat = "identity", position = "stack",
    colour          = 'black', 
    pattern         = 'stripe') +
  facet_wrap(.~Region_2, ncol=3)+
  coord_flip()+
  #theme_dark() +
  #theme_tufte() +
  theme_classic() +
  scale_fill_brewer(palette = "Greys") +
  #scale_fill_brewer(palette = "RdGy") +
  scale_pattern_fill2_grey() +
  geom_vline(xintercept =  13.5, linetype="dashed") +
  #scale_fill_brewer(palette =  "Paired") +
  #scale_fill_manual(values = CoD_Decomp.new) +
  theme(text = element_text(size = 15), 
        #legend.position=c(.6, 0.15),
        legend.position="bottom",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(color = "Black", size = 14),
        legend.text = element_text(color = "Black", size = 14)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = bquote(~'Change in Sex gap in '~ e[0] ~'2000-2015, Europe' ),
    fill = "Causes",
    pattern_fill = "Causes",
    #color = "Causes",
    y= "",
    x="")
Fig_4_FSR


# We combine the different figures
Fig_4 <- ggarrange(Fig_4_Europe, Fig_4_Nordic, Fig_4_Western,
                   Fig_4_Southern, Fig_4_CEE, Fig_4_FSR, 
                   #label.x = "Absolute contribuiton",
                   #label.y = 0,
                   common.legend = T, legend="bottom", ncol = 3, nrow = 2)


# Annotate the figure by adding a common labels
annotate_figure(Fig_4,
                #bottom = text_grob(bquote(~'Absolute contribution (years) to the sex gap '~ SD ~ ""),
                #                   color = "Black",
                #                   hjust = 0.5, x = 0.5, size = 10),
                left = text_grob("Age group", size=16, 
                                 color = "black", rot = 90))

ggsave(filename = "Figure 4.eps", path= "Main Figures and Tables/",
       dpi = 500, width = 12.5, height = 10.5,
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
  filter(Period=="2010-2015") %>% 
  dplyr::select(-c(Region, Measure)) %>% 
  group_by(Measure_2,Region_2) %>% 
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
                                                   y=SGLV,
                                                   shape=Age_groups,
                                                   group=Age_groups))+
  geom_point(size=5) +
  facet_wrap(~Region_2,  ncol=3, scales = "free") +
  theme_tufte() +
  #scale_color_manual(values = CoD_Decomp.new) +
  scale_shape_manual(values=c(0:4, 15:20)) +
  scale_y_continuous(limits = c(-60,60), breaks = c(-50,-40,-30,-20,-10,0,10, 20, 30,40,50)) +
  scale_x_continuous(limits = c(-60,60), breaks = c(-50,-40,-30,-20,-10,0,10, 20, 30,40,50)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 20),
        legend.position="bottom", legend.box = "vertical",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 18),
        legend.text = element_text(color = "Black", size = 18),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "",
       shape = "Age group",
       x=bquote(~'Relative contribution (%) to the sex gap in'~ e0 ~ ""),
       y=bquote(~'Relative contribution (%) to the sex gap in '~ SD ~ ""))
Fig_5
ggsave(filename = "Figure 5.eps", path= "Main Figures and Tables/",
       dpi = 500, width = 12.5, height = 10.5,
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
  filter(Period=="2010-2015") %>% 
  dplyr::select(-c(Region, Measure)) %>% 
  group_by(Measure_2,Region_2) %>% 
  mutate(Total = sum(Contribution)) %>% 
  ungroup() %>% 
  mutate(Relative = Contribution/Total*100)


# --- Labeling

# Causes of death
Data_figure_6$Cause2 <- factor(Data_figure_6$Cause2, levels = c(1:11),
                               labels = c("Smoking-related cancers", "Sex-specific cancers","Other cancers",
                                          "IHD and stroke", "Rest of circulatory diseases", "Mental and nervous system",
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
                                                   y=SGLV,
                                                   shape=Cause2,
                                                   group=Cause2))+
  geom_point(size=5) +
  facet_wrap(~Region_2,  ncol=3, scales = "free") +
  theme_tufte() +
  #scale_color_manual(values = CoD_Decomp.new) +
  scale_shape_manual(values=c(0:4, 15:20)) +
  scale_y_continuous(limits = c(-100,100), breaks = c(-100,-50,0,50,100)) +
  scale_x_continuous(limits = c(-100,100), breaks = c(-100,-50,0,50,100)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(text = element_text(size = 18),
        legend.position="bottom", legend.box = "vertical",
        legend.background = element_rect(fill="transparent", 
                                         size=1, linetype="solid", color = "white"),
        #strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        strip.background = element_rect(fill = "grey40", color = "grey20", size = 1),
        strip.text = element_text(colour = "white"),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(color = "Black", size = 16),
        legend.text = element_text(color = "Black", size = 16),
        axis.text.x = element_text(angle = 90)) +
  guides(shape=guide_legend(ncol=3)) +
  labs(title = "",
       shape = "Causes",
       x=bquote(~'Relative contribution (%) to the sex gap in'~ e0 ~ ""),
       y=bquote(~'Relative contribution (%) to the sex gap in '~ SD ~ ""))
Fig_6
ggsave(filename = "Figure 6.eps", path= "Main Figures and Tables/",
       dpi = 500, width = 12.5, height = 10.5,
       bg = "transparent")


