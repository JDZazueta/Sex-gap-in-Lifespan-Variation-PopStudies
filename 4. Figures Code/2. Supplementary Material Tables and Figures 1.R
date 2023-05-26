# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Supplementary Material Tables and Figures 1
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
#     1. Open data sets
# ---------------------------------------------------------------------------- #

# -------------------------- Original data


# -----------------------------
#   WHO all countries
# -----------------------------

get(load("Data/DT_COD_Melt_00.RData"))


Nordic_wide <- DT_COD.melt_00 %>% 
  filter(Country.name=="Norway" | Country.name=="Sweden" |
           Country.name=="Denmark" | Country.name=="Finland") %>% 
  mutate(`0` = case_when(Age==0 ~ Dx),
         `1` = case_when(Age==1 ~ Dx),
         `2` = case_when(Age==1 ~ Dx),
         `3` = case_when(Age==1 ~ Dx),
         `4` = case_when(Age==1 ~ Dx),
         `5` = case_when(Age==5 ~ Dx),
         `6` = case_when(Age==5 ~ Dx),
         `7` = case_when(Age==5 ~ Dx),
         `8` = case_when(Age==5 ~ Dx),
         `9` = case_when(Age==5 ~ Dx),
         `10` = case_when(Age==10 ~ Dx),
         `11` = case_when(Age==10 ~ Dx),
         `12` = case_when(Age==10 ~ Dx),
         `13` = case_when(Age==10 ~ Dx),
         `14` = case_when(Age==10 ~ Dx),
         `15` = case_when(Age==15 ~ Dx),
         `16` = case_when(Age==15 ~ Dx),
         `17` = case_when(Age==15 ~ Dx),
         `18` = case_when(Age==15 ~ Dx),
         `19` = case_when(Age==15 ~ Dx),
         `20` = case_when(Age==20 ~ Dx),
         `21` = case_when(Age==20 ~ Dx),
         `22` = case_when(Age==20 ~ Dx),
         `23` = case_when(Age==20 ~ Dx),
         `24` = case_when(Age==20 ~ Dx),
         `25` = case_when(Age==25 ~ Dx),
         `26` = case_when(Age==25 ~ Dx),
         `27` = case_when(Age==25 ~ Dx),
         `28` = case_when(Age==25 ~ Dx),
         `29` = case_when(Age==25 ~ Dx),
         `30` = case_when(Age==30 ~ Dx),
         `31` = case_when(Age==30 ~ Dx),
         `32` = case_when(Age==30 ~ Dx),
         `33` = case_when(Age==30 ~ Dx),
         `34` = case_when(Age==30 ~ Dx),
         `35` = case_when(Age==35 ~ Dx),
         `36` = case_when(Age==35 ~ Dx),
         `37` = case_when(Age==35 ~ Dx),
         `38` = case_when(Age==35 ~ Dx),
         `39` = case_when(Age==35 ~ Dx),
         `40` = case_when(Age==40 ~ Dx),
         `41` = case_when(Age==40 ~ Dx),
         `42` = case_when(Age==40 ~ Dx),
         `43` = case_when(Age==40 ~ Dx),
         `44` = case_when(Age==40 ~ Dx),
         `45` = case_when(Age==45 ~ Dx),
         `46` = case_when(Age==45 ~ Dx),
         `47` = case_when(Age==45 ~ Dx),
         `48` = case_when(Age==45 ~ Dx),
         `49` = case_when(Age==45 ~ Dx),
         `50` = case_when(Age==50 ~ Dx),
         `51` = case_when(Age==50 ~ Dx),
         `52` = case_when(Age==50 ~ Dx),
         `53` = case_when(Age==50 ~ Dx),
         `54` = case_when(Age==50 ~ Dx),
         `55` = case_when(Age==55 ~ Dx),
         `56` = case_when(Age==55 ~ Dx),
         `57` = case_when(Age==55 ~ Dx),
         `58` = case_when(Age==55 ~ Dx),
         `59` = case_when(Age==55 ~ Dx),
         `60` = case_when(Age==60 ~ Dx),
         `61` = case_when(Age==60 ~ Dx),
         `62` = case_when(Age==60 ~ Dx),
         `63` = case_when(Age==60 ~ Dx),
         `64` = case_when(Age==60 ~ Dx),
         `65` = case_when(Age==65 ~ Dx),
         `66` = case_when(Age==65 ~ Dx),
         `67` = case_when(Age==65 ~ Dx),
         `68` = case_when(Age==65 ~ Dx),
         `69` = case_when(Age==65 ~ Dx),
         `70` = case_when(Age==70 ~ Dx),
         `71` = case_when(Age==70 ~ Dx),
         `72` = case_when(Age==70 ~ Dx),
         `73` = case_when(Age==70 ~ Dx),
         `74` = case_when(Age==70 ~ Dx),
         `75` = case_when(Age==75 ~ Dx),
         `76` = case_when(Age==75 ~ Dx),
         `77` = case_when(Age==75 ~ Dx),
         `78` = case_when(Age==75 ~ Dx),
         `79` = case_when(Age==75 ~ Dx),
         `80` = case_when(Age==80 ~ Dx),
         `81` = case_when(Age==80 ~ Dx),
         `82` = case_when(Age==80 ~ Dx),
         `83` = case_when(Age==80 ~ Dx),
         `84` = case_when(Age==80 ~ Dx),
         `85` = 0,
         `86` = 0,
         `87` = 0,
         `88` = 0,
         `89` = 0,
         `90` = 0,
         `91` = 0,
         `92` = 0,
         `93` = 0,
         `94` = 0,
         `95` = 0,
         `96` = 0,
         `97` = 0,
         `98` = 0,
         `99` = 0,
         `100` = 0,
         `101` = 0,
         `102` = 0,
         `103` = 0,
         `104` = 0,
         `105` = 0,
         `106` = 0,
         `107` = 0,
         `108` = 0,
         `109` = 0,
         `110` = 0,
         Region = 1) %>% 
  dplyr::select(-c(Age, Country.name, Country)) %>% 
  filter(Cat==1 | Cat==2 | Cat==3 | Cat==4 |
           Cat==5 | Cat==6 | Cat==7 | Cat==8 |
           Cat==9 | Cat==10 | Cat==11) %>% 
  pivot_longer(!c(Region, ICD, Year, Sex, Cat, Dx, Dx.p),
               names_to = "Age", values_to="Deaths")

Nordic_wide$Deaths[is.na(Nordic_wide$Deaths)] <- 0

Nordic_wide <- data.table(Nordic_wide)

Nordic <- Nordic_wide[,list(Deaths=sum(Deaths)),
                      by=list(Region, ICD, Year, Age, Sex, Cat)]


Nordic$Age2 <- as.numeric(as.character(Nordic$Age))

Nordic <- Nordic %>% 
  dplyr::select(-Age) %>% 
  rename(Age = Age2)


# -----------------------------
#  Human Causes of Death
# -----------------------------

get(load("Data/HCDD_Countries_2000_2015_melt.RData"))

HCDD_Countries_2000_2015_long <- HCDD_Countries_2000_2015_melt %>% 
  mutate(`0` = case_when(Age==0 ~ Dx),
         `1` = case_when(Age==1 ~ Dx),
         `2` = case_when(Age==1 ~ Dx),
         `3` = case_when(Age==1 ~ Dx),
         `4` = case_when(Age==1 ~ Dx),
         `5` = case_when(Age==5 ~ Dx),
         `6` = case_when(Age==5 ~ Dx),
         `7` = case_when(Age==5 ~ Dx),
         `8` = case_when(Age==5 ~ Dx),
         `9` = case_when(Age==5 ~ Dx),
         `10` = case_when(Age==10 ~ Dx),
         `11` = case_when(Age==10 ~ Dx),
         `12` = case_when(Age==10 ~ Dx),
         `13` = case_when(Age==10 ~ Dx),
         `14` = case_when(Age==10 ~ Dx),
         `15` = case_when(Age==15 ~ Dx),
         `16` = case_when(Age==15 ~ Dx),
         `17` = case_when(Age==15 ~ Dx),
         `18` = case_when(Age==15 ~ Dx),
         `19` = case_when(Age==15 ~ Dx),
         `20` = case_when(Age==20 ~ Dx),
         `21` = case_when(Age==20 ~ Dx),
         `22` = case_when(Age==20 ~ Dx),
         `23` = case_when(Age==20 ~ Dx),
         `24` = case_when(Age==20 ~ Dx),
         `25` = case_when(Age==25 ~ Dx),
         `26` = case_when(Age==25 ~ Dx),
         `27` = case_when(Age==25 ~ Dx),
         `28` = case_when(Age==25 ~ Dx),
         `29` = case_when(Age==25 ~ Dx),
         `30` = case_when(Age==30 ~ Dx),
         `31` = case_when(Age==30 ~ Dx),
         `32` = case_when(Age==30 ~ Dx),
         `33` = case_when(Age==30 ~ Dx),
         `34` = case_when(Age==30 ~ Dx),
         `35` = case_when(Age==35 ~ Dx),
         `36` = case_when(Age==35 ~ Dx),
         `37` = case_when(Age==35 ~ Dx),
         `38` = case_when(Age==35 ~ Dx),
         `39` = case_when(Age==35 ~ Dx),
         `40` = case_when(Age==40 ~ Dx),
         `41` = case_when(Age==40 ~ Dx),
         `42` = case_when(Age==40 ~ Dx),
         `43` = case_when(Age==40 ~ Dx),
         `44` = case_when(Age==40 ~ Dx),
         `45` = case_when(Age==45 ~ Dx),
         `46` = case_when(Age==45 ~ Dx),
         `47` = case_when(Age==45 ~ Dx),
         `48` = case_when(Age==45 ~ Dx),
         `49` = case_when(Age==45 ~ Dx),
         `50` = case_when(Age==50 ~ Dx),
         `51` = case_when(Age==50 ~ Dx),
         `52` = case_when(Age==50 ~ Dx),
         `53` = case_when(Age==50 ~ Dx),
         `54` = case_when(Age==50 ~ Dx),
         `55` = case_when(Age==55 ~ Dx),
         `56` = case_when(Age==55 ~ Dx),
         `57` = case_when(Age==55 ~ Dx),
         `58` = case_when(Age==55 ~ Dx),
         `59` = case_when(Age==55 ~ Dx),
         `60` = case_when(Age==60 ~ Dx),
         `61` = case_when(Age==60 ~ Dx),
         `62` = case_when(Age==60 ~ Dx),
         `63` = case_when(Age==60 ~ Dx),
         `64` = case_when(Age==60 ~ Dx),
         `65` = case_when(Age==65 ~ Dx),
         `66` = case_when(Age==65 ~ Dx),
         `67` = case_when(Age==65 ~ Dx),
         `68` = case_when(Age==65 ~ Dx),
         `69` = case_when(Age==65 ~ Dx),
         `70` = case_when(Age==70 ~ Dx),
         `71` = case_when(Age==70 ~ Dx),
         `72` = case_when(Age==70 ~ Dx),
         `73` = case_when(Age==70 ~ Dx),
         `74` = case_when(Age==70 ~ Dx),
         `75` = case_when(Age==75 ~ Dx),
         `76` = case_when(Age==75 ~ Dx),
         `77` = case_when(Age==75 ~ Dx),
         `78` = case_when(Age==75 ~ Dx),
         `79` = case_when(Age==75 ~ Dx),
         `80` = case_when(Age==80 ~ Dx),
         `81` = case_when(Age==80 ~ Dx),
         `82` = case_when(Age==80 ~ Dx),
         `83` = case_when(Age==80 ~ Dx),
         `84` = case_when(Age==80 ~ Dx),
         `85` = 0,
         `86` = 0,
         `87` = 0,
         `88` = 0,
         `89` = 0,
         `90` = 0,
         `91` = 0,
         `92` = 0,
         `93` = 0,
         `94` = 0,
         `95` = 0,
         `96` = 0,
         `97` = 0,
         `98` = 0,
         `99` = 0,
         `100` = 0,
         `101` = 0,
         `102` = 0,
         `103` = 0,
         `104` = 0,
         `105` = 0,
         `106` = 0,
         `107` = 0,
         `108` = 0,
         `109` = 0,
         `110` = 0,
         Region = 5) %>% 
  dplyr::select(-c(Age, Country.name, Country)) %>% 
  filter(Cat==1 | Cat==2 | Cat==3 | Cat==4 |
           Cat==5 | Cat==6 | Cat==7 | Cat==8 |
           Cat==9 | Cat==10 | Cat==11) %>% 
  pivot_longer(!c(Region, ICD, Year, Sex, Cat, Dx, Dx.p),
               names_to = "Age", values_to="Deaths") %>% 
  filter(Sex==1 | Sex==2) %>% 
  mutate(Sex_2 = case_when(Sex==1 ~ 1,
                           Sex==2 ~ 2)) %>% 
  dplyr::select(-c(Sex)) %>% 
  rename(Sex = Sex_2)

HCDD_Countries_2000_2015_long$Deaths[is.na(HCDD_Countries_2000_2015_long$Deaths)] <- 0

HCDD_Countries_2000_2015_long <- data.table(HCDD_Countries_2000_2015_long)

FSR <- HCDD_Countries_2000_2015_long[,list(Deaths=sum(Deaths)),
                                     by=list(Region, ICD, Year, Age, Sex, Cat)]

FSR$Sex <- factor(FSR$Sex,
                  levels = c(1,2),
                  labels = c("m", "f"))

FSR$Age2 <- as.numeric(as.character(FSR$Age))

FSR <- FSR %>% 
  dplyr::select(-Age) %>% 
  rename(Age = Age2)

# -----------------------------------
# Smoothed data
# ------------------------------------

get(load("Data/Smoothed_final_regions.RData"))

# Nordic
Nordic_smoothed <- Smoothed_Data_WHO_regions %>% 
  filter(Region==1)

# FSR
FSR_smoothed <- Smoothed_Data_WHO_regions %>% 
  filter(Region==5)



# ---------------------------------------------------------------------------- #
#     2. Figures Nordic region
# ---------------------------------------------------------------------------- #

# -------------------------
#  Smoking-related cancers
# -------------------------

Nordic_C1 <- Nordic %>% 
  filter(Cat==1)

Nordic_smoothed_C1 <- Nordic_smoothed %>% 
  filter(Cat==1)

# Original
Cause_1_Original <- ggplot(Nordic_C1,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_1_smoothed<- ggplot(Nordic_smoothed_C1,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_1_comparison <- ggarrange(Cause_1_Original,Cause_1_smoothed, 
                                labels = c("Nordic 2010: Original Smoking-related cancers",
                                           "Nordic 2010: Smoothed Smoking-related cancers"),
                                ncol=1, nrow = 2)
Cause_1_comparison
ggsave(filename = "Nordic C1.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Sex-specific cancers
# -------------------------

Nordic_C2 <- Nordic %>% 
  filter(Cat==2)

Nordic_smoothed_C2 <- Nordic_smoothed %>% 
  filter(Cat==2)


# Original
Cause_2_Original <- ggplot(Nordic_C2,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_2_smoothed<- ggplot(Nordic_smoothed_C2,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_2_comparison <- ggarrange(Cause_2_Original,Cause_2_smoothed, 
                                labels = c("Nordic 2010: Original Sex-specific cancers",
                                           "Nordic 2010: Smoothed Sex-specific cancers"),
                                ncol=1, nrow = 2)
Cause_2_comparison
ggsave(filename = "Nordic C2.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")



# -------------------------
#  Other cancers
# -------------------------

Nordic_C3 <- Nordic %>% 
  filter(Cat==3)

Nordic_smoothed_C3 <- Nordic_smoothed %>% 
  filter(Cat==3)


# Original
Cause_3_Original <- ggplot(Nordic_C3,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_3_smoothed<- ggplot(Nordic_smoothed_C3,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_3_comparison <- ggarrange(Cause_3_Original,Cause_3_smoothed, 
                                labels = c("Nordic 2010: Original Other cancers",
                                           "Nordic 2010: Smoothed Other cancers"),
                                ncol=1, nrow = 2)
Cause_3_comparison
ggsave(filename = "Nordic C3.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")

# -------------------------
#  IHD and Stroke
# -------------------------

Nordic_C4 <- Nordic %>% 
  filter(Cat==4)

Nordic_smoothed_C4 <- Nordic_smoothed %>% 
  filter(Cat==4)


# Original
Cause_4_Original <- ggplot(Nordic_C4,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_4_smoothed<- ggplot(Nordic_smoothed_C4,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_4_comparison <- ggarrange(Cause_4_Original,Cause_4_smoothed, 
                                labels = c("Nordic 2010: Original IHD and Stroke",
                                           "Nordic 2010: Smoothed IHD and Stroke"),
                                ncol=1, nrow = 2)
Cause_4_comparison
ggsave(filename = "Nordic C4.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")

# -------------------------
#  Rest of circulatory diseases
# -------------------------

Nordic_C5 <- Nordic %>% 
  filter(Cat==5)

Nordic_smoothed_C5 <- Nordic_smoothed %>% 
  filter(Cat==5)


# Original
Cause_5_Original <- ggplot(Nordic_C5,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_5_smoothed<- ggplot(Nordic_smoothed_C5,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_5_comparison <- ggarrange(Cause_5_Original,Cause_5_smoothed, 
                                labels = c("Nordic 2010: Original Rest of circulatory diseases",
                                           "Nordic 2010: Smoothed Rest of circulatory diseases"),
                                ncol=1, nrow = 2)
Cause_5_comparison
ggsave(filename = "Nordic C5.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")

# -------------------------
#  Mental and nervous system
# -------------------------

Nordic_C6 <- Nordic %>% 
  filter(Cat==6)

Nordic_smoothed_C6 <- Nordic_smoothed %>% 
  filter(Cat==6)


# Original
Cause_6_Original <- ggplot(Nordic_C6,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_6_smoothed<- ggplot(Nordic_smoothed_C6,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_6_comparison <- ggarrange(Cause_6_Original,Cause_6_smoothed, 
                                labels = c("Nordic 2010: Original Mental and nervous system",
                                           "Nordic 2010: Smoothed Mental and nervous system"),
                                ncol=1, nrow = 2)
Cause_6_comparison
ggsave(filename = "Nordic C6.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Alcohol-attributable causes
# -------------------------

Nordic_C7 <- Nordic %>% 
  filter(Cat==7)

Nordic_smoothed_C7 <- Nordic_smoothed %>% 
  filter(Cat==7)


# Original
Cause_7_Original <- ggplot(Nordic_C7,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_7_smoothed<- ggplot(Nordic_smoothed_C7,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_7_comparison <- ggarrange(Cause_7_Original,Cause_7_smoothed, 
                                labels = c("Nordic 2010: Original Alcohol-attributable causes",
                                           "Nordic 2010: Smoothed Alcohol-attributable causes"),
                                ncol=1, nrow = 2)
Cause_7_comparison
ggsave(filename = "Nordic C7.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  External causes
# -------------------------

Nordic_C10 <- Nordic %>% 
  filter(Cat==10)

Nordic_smoothed_C10 <- Nordic_smoothed %>% 
  filter(Cat==10)


# Original
Cause_10_Original <- ggplot(Nordic_C10,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_10_smoothed<- ggplot(Nordic_smoothed_C10,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_10_comparison <- ggarrange(Cause_10_Original,Cause_10_smoothed, 
                                labels = c("Nordic 2010: Original External causes",
                                           "Nordic 2010: Smoothed External causes"),
                                ncol=1, nrow = 2)
Cause_10_comparison
ggsave(filename = "Nordic C10.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Infectious (respiratory) diseases
# -------------------------

Nordic_C8 <- Nordic %>% 
  filter(Cat==8)

Nordic_smoothed_C8 <- Nordic_smoothed %>% 
  filter(Cat==8)


# Original
Cause_8_Original <- ggplot(Nordic_C8,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_8_smoothed<- ggplot(Nordic_smoothed_C8,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_8_comparison <- ggarrange(Cause_8_Original,Cause_8_smoothed, 
                                labels = c("Nordic 2010: Original Infectious (respiratory) diseases",
                                           "Nordic 2010: Smoothed Infectious (respiratory) diseases"),
                                ncol=1, nrow = 2)
Cause_8_comparison
ggsave(filename = "Nordic C8.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")



# -------------------------
#  Non infectious respiratory disases
# -------------------------

Nordic_C9 <- Nordic %>% 
  filter(Cat==9)

Nordic_smoothed_C9 <- Nordic_smoothed %>% 
  filter(Cat==9)


# Original
Cause_9_Original <- ggplot(Nordic_C9,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_9_smoothed<- ggplot(Nordic_smoothed_C9,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_9_comparison <- ggarrange(Cause_9_Original,Cause_9_smoothed, 
                                labels = c("Nordic 2010: Original Non infectious respiratory disases",
                                           "Nordic 2010: Smoothed Non infectious respiratory disases"),
                                ncol=1, nrow = 2)
Cause_9_comparison
ggsave(filename = "Nordic C9.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Rest of causes
# -------------------------

Nordic_C11 <- Nordic %>% 
  filter(Cat==11)

Nordic_smoothed_C11 <- Nordic_smoothed %>% 
  filter(Cat==11)


# Original
Cause_11_Original <- ggplot(Nordic_C11,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_11_smoothed<- ggplot(Nordic_smoothed_C11,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_11_comparison <- ggarrange(Cause_11_Original,Cause_11_smoothed, 
                                 labels = c("Nordic 2010: Original Rest of causes",
                                            "Nordic 2010: Smoothed Rest of causes"),
                                 ncol=1, nrow = 2)
Cause_11_comparison
ggsave(filename = "Nordic C11.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")



# ---------------------------------------------------------------------------- #
#     3. Figures FSR region
# ---------------------------------------------------------------------------- #

# -------------------------
#  Smoking-related cancers
# -------------------------

FSR_C1 <- FSR %>% 
  filter(Cat==1)

FSR_smoothed_C1 <- FSR_smoothed %>% 
  filter(Cat==1)

# Original
Cause_1_Original <- ggplot(FSR_C1,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_1_smoothed<- ggplot(FSR_smoothed_C1,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_1_comparison <- ggarrange(Cause_1_Original,Cause_1_smoothed, 
                                labels = c("FSR 2010: Original Smoking-related cancers",
                                           "FSR 2010: Smoothed Smoking-related cancers"),
                                ncol=1, nrow = 2)
Cause_1_comparison
ggsave(filename = "FSR C1.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Sex-specific cancers
# -------------------------

FSR_C2 <- FSR %>% 
  filter(Cat==2)

FSR_smoothed_C2 <- FSR_smoothed %>% 
  filter(Cat==2)


# Original
Cause_2_Original <- ggplot(FSR_C2,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_2_smoothed<- ggplot(FSR_smoothed_C2,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_2_comparison <- ggarrange(Cause_2_Original,Cause_2_smoothed, 
                                labels = c("FSR 2010: Original Sex-specific cancers",
                                           "FSR 2010: Smoothed Sex-specific cancers"),
                                ncol=1, nrow = 2)
Cause_2_comparison
ggsave(filename = "FSR C2.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")



# -------------------------
#  Other cancers
# -------------------------

FSR_C3 <- FSR %>% 
  filter(Cat==3)

FSR_smoothed_C3 <- FSR_smoothed %>% 
  filter(Cat==3)


# Original
Cause_3_Original <- ggplot(FSR_C3,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_3_smoothed<- ggplot(FSR_smoothed_C3,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_3_comparison <- ggarrange(Cause_3_Original,Cause_3_smoothed, 
                                labels = c("FSR 2010: Original Other cancers",
                                           "FSR 2010: Smoothed Other cancers"),
                                ncol=1, nrow = 2)
Cause_3_comparison
ggsave(filename = "FSR C3.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")

# -------------------------
#  IHD and Stroke
# -------------------------

FSR_C4 <- FSR %>% 
  filter(Cat==4)

FSR_smoothed_C4 <- FSR_smoothed %>% 
  filter(Cat==4)


# Original
Cause_4_Original <- ggplot(FSR_C4,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_4_smoothed<- ggplot(FSR_smoothed_C4,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_4_comparison <- ggarrange(Cause_4_Original,Cause_4_smoothed, 
                                labels = c("FSR 2010: Original IHD and Stroke",
                                           "FSR 2010: Smoothed IHD and Stroke"),
                                ncol=1, nrow = 2)
Cause_4_comparison
ggsave(filename = "FSR C4.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")

# -------------------------
#  Rest of circulatory diseases
# -------------------------

FSR_C5 <- FSR %>% 
  filter(Cat==5)

FSR_smoothed_C5 <- FSR_smoothed %>% 
  filter(Cat==5)


# Original
Cause_5_Original <- ggplot(FSR_C5,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_5_smoothed<- ggplot(FSR_smoothed_C5,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_5_comparison <- ggarrange(Cause_5_Original,Cause_5_smoothed, 
                                labels = c("FSR 2010: Original Rest of circulatory diseases",
                                           "FSR 2010: Smoothed Rest of circulatory diseases"),
                                ncol=1, nrow = 2)
Cause_5_comparison
ggsave(filename = "FSR C5.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")

# -------------------------
#  Mental and nervous system
# -------------------------

FSR_C6 <- FSR %>% 
  filter(Cat==6)

FSR_smoothed_C6 <- FSR_smoothed %>% 
  filter(Cat==6)


# Original
Cause_6_Original <- ggplot(FSR_C6,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_6_smoothed<- ggplot(FSR_smoothed_C6,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_6_comparison <- ggarrange(Cause_6_Original,Cause_6_smoothed, 
                                labels = c("FSR 2010: Original Mental and nervous system",
                                           "FSR 2010: Smoothed Mental and nervous system"),
                                ncol=1, nrow = 2)
Cause_6_comparison
ggsave(filename = "FSR C6.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Alcohol-attributable causes
# -------------------------

FSR_C7 <- FSR %>% 
  filter(Cat==7)

FSR_smoothed_C7 <- FSR_smoothed %>% 
  filter(Cat==7)


# Original
Cause_7_Original <- ggplot(FSR_C7,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_7_smoothed<- ggplot(FSR_smoothed_C7,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_7_comparison <- ggarrange(Cause_7_Original,Cause_7_smoothed, 
                                labels = c("FSR 2010: Original Alcohol-attributable causes",
                                           "FSR 2010: Smoothed Alcohol-attributable causes"),
                                ncol=1, nrow = 2)
Cause_7_comparison
ggsave(filename = "FSR C7.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  External causes
# -------------------------

FSR_C10 <- FSR %>% 
  filter(Cat==10)

FSR_smoothed_C10 <- FSR_smoothed %>% 
  filter(Cat==10)


# Original
Cause_10_Original <- ggplot(FSR_C10,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_10_smoothed<- ggplot(FSR_smoothed_C10,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_10_comparison <- ggarrange(Cause_10_Original,Cause_10_smoothed, 
                                 labels = c("FSR 2010: Original External causes",
                                            "FSR 2010: Smoothed External causes"),
                                 ncol=1, nrow = 2)
Cause_10_comparison
ggsave(filename = "FSR C10.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Infectious (respiratory) diseases
# -------------------------

FSR_C8 <- FSR %>% 
  filter(Cat==8)

FSR_smoothed_C8 <- FSR_smoothed %>% 
  filter(Cat==8)


# Original
Cause_8_Original <- ggplot(FSR_C8,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_8_smoothed<- ggplot(FSR_smoothed_C8,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_8_comparison <- ggarrange(Cause_8_Original,Cause_8_smoothed, 
                                labels = c("FSR 2010: Original Infectious (respiratory) diseases",
                                           "FSR 2010: Smoothed Infectious (respiratory) diseases"),
                                ncol=1, nrow = 2)
Cause_8_comparison
ggsave(filename = "FSR C8.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")



# -------------------------
#  Non infectious respiratory disases
# -------------------------

FSR_C9 <- FSR %>% 
  filter(Cat==9)

FSR_smoothed_C9 <- FSR_smoothed %>% 
  filter(Cat==9)


# Original
Cause_9_Original <- ggplot(FSR_C9,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_9_smoothed<- ggplot(FSR_smoothed_C9,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_9_comparison <- ggarrange(Cause_9_Original,Cause_9_smoothed, 
                                labels = c("FSR 2010: Original Non infectious respiratory disases",
                                           "FSR 2010: Smoothed Non infectious respiratory disases"),
                                ncol=1, nrow = 2)
Cause_9_comparison
ggsave(filename = "FSR C9.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


# -------------------------
#  Rest of causes
# -------------------------

FSR_C11 <- FSR %>% 
  filter(Cat==11)

FSR_smoothed_C11 <- FSR_smoothed %>% 
  filter(Cat==11)


# Original
Cause_11_Original <- ggplot(FSR_C11,mapping=aes(Age,Deaths, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Smoothed
Cause_11_smoothed<- ggplot(FSR_smoothed_C11,mapping=aes(Age,Dx, color=Sex, fill=Sex)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,85,90,100,110))

# Combine
Cause_11_comparison <- ggarrange(Cause_11_Original,Cause_11_smoothed, 
                                 labels = c("FSR 2010: Original Rest of causes",
                                            "FSR 2010: Smoothed Rest of causes"),
                                 ncol=1, nrow = 2)
Cause_11_comparison
ggsave(filename = "FSR C11.png", path= "Supplementary Material/1. Smoothed vs Original/",
       dpi = 320, width = 10.5, height = 4.5,
       bg = "transparent")


