# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Packages, and labes and color for figures
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#   Packages
# ---------------------------------------------------------------------------- #

library(data.table)
library(HMDHFDplus)
library(reshape2)
library(tidyverse)
library(ggthemes)
library(gghighlight)
library(RColorBrewer)
library(wesanderson)
library(ggpubr)
library(ggsci)
library(hrbrthemes)
library(ragg)
library(ungroup)
library(gdata)
library(ggpattern)
library(magick)

# ---------------------------------------------------------------------------- #
#   Labels for causes of deaths
# ---------------------------------------------------------------------------- #

cause_names<-c("1"="C. Sesitive Smoking",
               "2"="Sex-specif cancer",
               "3"="Other Cancers",
               "4"="IHD and Stroke",
               "5"="Rest of Circulatory diseases",
               "6"= "Mental and Nervous system",
               "7"="Alcohol attributable",
               "8"="Infectious (respiratory diseases)",
               "9"="Non infectious respiratory disases",
               "10" = "External",
               "11" = "Other No Infections No respiratory")


# ---------------------------------------------------------------------------- #
#  Pallet of color for figures
# ---------------------------------------------------------------------------- #

Colores <- c("powder blue", "cadet blue", "steel blue", 
             "dark slate blue")
Colores_2 <- c("#FFDB6D", "#C4961A", "#F4EDCA",  "#D16103")

Colores_3 <- c("#CA0020", "#FFDB6D", "darkorange","#7570B3", "#92C5DE", "#0571B0")

CoD_colors <- c("indianred3",  "#1B4F72", "#3498DB", "#D1E5F0",
                "#238B45", "#74C476","#807DBA", "#FDAE6B")


CoD_Decomp.old <- c("#FDBF6F",
                    "#1F78B4",
                    "#FF7F00",
                    "#33A02C",
                    "#FB9A99",
                    "#FFFF99",
                    "#E31A1C",
                    "#6A3D9A",
                    "#B2DF8A",
                    "#CAB2D6",
                    "#A6CEE3")

CoD_Decomp.new <- c("#053061",
                    "#2166AC",
                    "#4393C3",
                    "#67000D",
                    "#A50F15",
                    "#CB181D",
                    "#F16913",
                    "#FDAE6B",
                    "#238B45",
                    "#A1D99B",
                    "#807DBA")

Share_deaths <- c("#053061",
                  "#2166AC",
                  "#4393C3",
                  "#67000D",
                  "#A50F15",
                  "#CB181D",
                  "#F16913",
                  "#238B45",
                  "#A1D99B",
                  "#FDAE6B",
                  "#807DBA")

Reg_col <-c("#053061",
            "#2166AC",
            "#4393C3",
            "#67000D",
            "#A50F15",
            "Black")

lifesan_expectancy <- c("#084594","#9ECAE1")

color_year <- c("#053061","#4393C3","#A50F15")

Palete_Age_Cause <- c("#F7F7F7",
                      "#D9D9D9",
                      "#BDBDBD",
                      "#969696",
                      "#737373",
                      "#525252",
                      "#252525",
                      "#053061",
                      "#2166AC",
                      "#4393C3",
                      "#67000D",
                      "#A50F15",
                      "#CB181D",
                      "#F16913",
                      "#FDAE6B",
                      "#238B45",
                      "#A1D99B",
                      "#807DBA")

Age_colors <- c("#F0F0F0",
                "#F0F0F0",
                "#D9D9D9",
                "#D9D9D9",
                "#BDBDBD",
                "#969696",
                "#737373",
                "#525252",
                "#252525",
                "#000000",
                "#778899")

Age_color_fig5 <- c("#800026",  "#BD0026", "#E31A1C", "#FC4E2A",
                    "#FD8D3C", "#FEB24C","#FED976", 
                    "#4292C6","#2171B5","#08519C","#08306B")


CoD_Decomp.blackwhite  <-c("Smoking-related cancers" = "azure4", 
                           "Sex-specific cancers" = "azure3",
                           "Other cancers" = "azure2",
                           "IHD and stroke" = "Grey20", 
                           "Rest of circulatory diseases" = "Grey30",
                           "Mental and nervous system" = "Grey40",
                           "Alcohol-attributable causes" = "Grey50", 
                           "External causes" = "Grey60", 
                           "Infectious (respiratory) diseases" = "honeydew",
                           "Non infectious respiratory disases" = "honeydew2",
                           "Rest of causes" = "honeydew4") 

Pattern_cause <- c("Smoking-related cancers" = "none", 
                   "Sex-specific cancers" = "stripe",
                   "Other cancers" = "wave",
                   "IHD and stroke" = "crosshatch", 
                   "Rest of circulatory diseases" = "none", 
                   "Mental and nervous system"  = "wave",
                   "Alcohol-attributable causes" = "crosshatch",
                   "External causes" = "none",
                   "Infectious (respiratory) diseases" = "stripe",
                   "Non infectious respiratory disases" = "wave",
                   "Rest of causes" = "crosshatch")


# Life table functions
# For the last
SLT_1 <- function (nmx, age, sex){
  n <- 1
  x <- age
  #n <- c(diff(x), NA)
  nax <- rep(0, length(x))
  m <- length(x)
  for(i in 1:length(nax)){
    nax[i] <- 0.5*n
    nax[m] <- 0 # Last nax with 0
  }# formulta to compute last nax 
  
  nqx <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx <- c(nqx[-(length(nqx))], 1)
  for (i in 1:length(nqx)) {
    if (nqx[i] > 1) 
      nqx[i] <- 1
  }
  npx <- 1 - nqx
  l0 = 100000 #standard
  lx <- round(cumprod(c(l0, npx)))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLx <- n * lxpn + ndx * nax
  nLx[m] <- ndx[m]/nmx[m] # We compute the last nLx
  
  if (nmx[m]>=0) {
    nLx[m] <- 0
  } else {
    nLx[m] <- ndx[m]/nmx[m]
  }
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # Adjusting the ax value for the last age groups
  nax[m] <- ex[m] # we replace the nax in the last age group with the ex
  
  lt <- data.frame(Age = age, 
                   sex,
                   n,
                   nmx = round(nmx, 4),
                   nqx = round(nqx, 4),
                   nax = round(nax, 4),
                   npx = round(npx,4),
                   lx, ndx,
                   Lx = round(nLx,3),
                   Tx = round(Tx,3),
                   ex = round(ex,3))
  
  return(lt)
}



