# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Supplementary Material Tables and Figures 5
# Dataset: Human Mortality Database
#          World Health Organization
#          Human Causes of death Database
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, package, and functions
#   1. Open data sets
#   2. Create figures and tables

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
#     1. Open dataset
# ---------------------------------------------------------------------------- #


# -- European regions
get(load("Data/Data_combined_regions_period.RData"))

# -- Europe
get(load("Data/Data_combined_Europe_period.RData"))


# -- Combine datasets

Data_figure_S5 <- rbind(Data_combined_Europe,
                        Data_combined_regions)

# ---------------------------------------------------------------------------- #
#     2. Create figures
# ---------------------------------------------------------------------------- #

# --------------
#  Figure 1 Supplementary Material 5
# --------------

Data_figure_S5 <- Data_figure_S5 %>% 
  mutate(Sex_2 = case_when(Sex=="m" ~ 1,
                           Sex=="f" ~ 2)) %>% 
  filter(Periods==2010)

Data_figure_S5$Sex_2 <- factor(Data_figure_S5$Sex_2,
                               levels = c(1,2),
                               labels = c("Males", "Females"))

Data_figure_S5$Periods <- factor(Data_figure_S5$Periods,
                               levels = c(2000,2005,2010),
                               labels = c("2000-2004",
                                          "2005-2009",
                                          "2010-2015"))

Data_figure_S5$Region <- factor(Data_figure_S5$Region,
                               levels = c(1,2,3,4,5,6),
                               labels = c("Nordic", "Western", "Southern",
                                          "CEE", "FSR", "Europe"))

Data_figure_S5 <- Data_figure_S5 %>% 
  mutate(Region_2 = case_when(Region=="Nordic" ~ 2,
                              Region=="Western" ~ 3,
                              Region=="Southern" ~ 4,
                              Region=="CEE" ~ 5,
                              Region=="FSR" ~ 6,
                              Region=="Europe" ~ 1))

Data_figure_S5$Region_2 <- factor(Data_figure_S5$Region_2,
                                levels = c(1,2,3,4,5,6),
                                labels = c( "Europe", "Nordic", "Western",
                                            "Southern","CEE", "FSR"))

Figure_1_SM5 <- ggplot(Data_figure_S5, mapping = aes(x=Age,
                                                     y=dx, 
                                                     color=Sex_2,
                                                     group=Sex_2)) +
  geom_line(size=1) +
  facet_wrap(.~Region_2, ncol = 3) +
  theme_tufte() +
  scale_color_manual(values = c("black", "grey")) +
  scale_x_discrete(breaks = c(0,1,5,seq(10,100,10))) +
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
    color = "Sex",
    caption = "Source data: Human Mortality Database",
    y= "",
    x="")
Figure_1_SM5
ggsave(filename = "Figure 1 SM-5.png", path= "Supplementary Material/5. Age and deaths and share of deaths/",
       dpi = 320, width = 12.5, height = 10.5,
       bg = "transparent")


# --------------
#  Table 1a & 1b Supplementary Material 5
# --------------

# Smoothed death counts from WHO and HCD
get(load("Data/Smoothed_final_Europe.RData"))


# --- We need to estimate the proportions of Dx by periods of study
Smoothed_Data_WHO_Europe$Year <- as.numeric(as.character(Smoothed_Data_WHO_Europe$Year))

Smoothed_Data_WHO_Europe <- Smoothed_Data_WHO_Europe %>% 
  mutate(Periods = case_when(Year>=2000 & Year<=2004 ~ 2000,
                             Year>=2005 & Year<=2009 ~ 2005,
                             Year>=2010 & Year<=2015 ~ 2010),
         Region = 6) 

Smoothed_Data_WHO_Europe$Dx[is.na(Smoothed_Data_WHO_Europe$Dx)] <- 0


# Get total deaths by age, sex, category, year, regions.
SAC_Sum  <- Smoothed_Data_WHO_Europe[, list(Dx=sum(Dx)), by =  list(Region,Periods,Sex,Age,Cat)]

SAC_Sum_prop <- SAC_Sum[, Dx.p := Dx/sum(Dx), by = list(Region,Periods,Sex,Age)]


Table_1a_1b <- SAC_Sum_prop[,list(Share_dx=sum(Dx.p)),
                        by =  list(Region,Periods,Sex,Cat)]

Table_1a_1b <- Table_1a_1b %>% 
  filter(Periods==2010)
write.csv(Table_1a_1b, 
          file = "Supplementary Material/5. Age and deaths and share of deaths/Table_1a_1b_Europe")








# ----------------
#  Figure Share of deaths
# ----------------

Europe <- Europe %>% 
  group_by(Periods, Sex, Age) %>% 
  mutate(Total = sum(Dx)) %>% 
  ungroup() %>% 
  mutate(SEX = case_when(Sex=="f" ~ 2,
                         Sex=="m" ~ 1),
         Share_dx = Dx/Total) 


Europe$Cat <- factor(Europe$Cat, levels = c(1:11),
                     labels =   cause_names)

Europe$SEX <- factor(Europe$SEX, levels =c(1,2),
                     labels = c("Males", "Females"))




Fig_Europe <- ggplot(Europe, mapping = aes(x=Age, y=Share_dx*100,
                                           fill=Cat)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_area() +
  facet_grid(.~SEX) +
  theme_classic() +
  scale_fill_manual(values = Share_deaths) +
  scale_x_continuous(breaks = c(seq(0,110,10)))+
  theme(text = element_text(size = 12), legend.position="bottom",
        strip.background = element_rect(color="black", fill="grey80", size=0.5, linetype="solid"),
        legend.key.size = unit(.6, "cm"),
        legend.key.width = unit(.6,"cm"),
        legend.title = element_text(color = "Black", size = 12),
        legend.text = element_text(color = "Black", size = 12)) +
  guides(fill=guide_legend(ncol=3)) +
  labs(#title = "Proportion of deaths by age groups and sex. Europe, 2010-2015",
    fill = "Causes",
    y="Proportion of deaths (%)",
    x="Age")
Fig_Europe


