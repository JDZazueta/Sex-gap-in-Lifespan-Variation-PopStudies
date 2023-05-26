# ---------------------------------------------------------------------------- #
# Paper:   Contribution of age groups and causes of death to the sex gap in 
#          lifespan variation in Europe.
# Title:   Life expectancy, standard deviation and e-dagger functions for
#          decomposition analysis
# Author:  J.Daniel Zazueta-Borboa
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#   Package
# ---------------------------------------------------------------------------- #

library(DemoDecomp)

# ---------------------------------------------------------------------------- #
#   Labels for causes of deaths and age groups
# ---------------------------------------------------------------------------- #

# This version of code is Classifcation of 11 causes of deaths
# 1) Cancer sensitive to smoking
# 2) Sex-specif cancer
# 3) Other Cancer
# 4) Ischemic heart diseases and Stroke
# 5) Other circulatory diseases
# 6) Mental Disorder + Nervous system
# 7) Alcohol attributable mortality
# 8) Infectious (respiratory diseases)
# 9) Non infectious respiratory disases
# 10) External causes of death
# 11) Rest of causes of death


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


Labels.age   <- c('0','1-4', '5-9', '10-14', '15-19', '20-24','25-29',
                  '30-34','35-39','40-44','45-49','50-54','55-59',
                  '60-64','65-69', "70-74","75-79","80-84","85-89",
                  "90-94","95-99","100-104","105-109",'110+')



# ---------------------------------------------------------------------------- #
#   Decomposition function
# ---------------------------------------------------------------------------- #


Decomp <-function (func, rates1, rates2, N, ...) {
  y1 <- func(rates1, ...)
  y2 <- func(rates2, ...)
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z <- rep(0, n)
      z[i] <- delta[i]/2
      cc[i, j] <- func((x[, j] + z), ...) - func((x[, j] - 
                                                    z), ...)
    }
  }
  return(rowSums(cc))
}


# ---------------------------------------------------------------------------- #
#   Life expectancy at birth functions for decomposition
# ---------------------------------------------------------------------------- #


# Life expectancy funciton
e0.frommx <- function(nmx =  mx, sex=1, age = c(seq(0, 100, 1)), nax = NULL){
  n   <- c(diff(age), 999)
  
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[1]
  
  return(e0)
}

e0frommxc <- function(mxcvec){ 
  dim(mxcvec) <- c(101,length(mxcvec)/101)
  mx <- rowSums(mxcvec)
  e0.frommx(mx)
}


# ---------------------------------------------------------------------------- #
#   Standard deviation with respect life expectancy at birth functions 
#   for decomposition
# ---------------------------------------------------------------------------- #

sd.frommx <- function(nmx =  mx, sex=1, age = c(seq(0, 100, 1)), nax = NULL){
  n   <- c(diff(age), 999)
  
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  nax[length(nax)] <- ex[length(ex)]
  vx <- sum(ndx*(age+nax-ex[1L])^2)
  sd <- sqrt(vx)
  return(sd)
}

sdfrommxc <- function(mxcvec){
  dim(mxcvec) <- c(101,length(mxcvec)/101)
  mx          <- rowSums(mxcvec)
  sd.frommx(mx,sex)
}



# ---------------------------------------------------------------------------- #
#   E-dagger function (Years of life lost)
# ---------------------------------------------------------------------------- #

edagger.frommx <- function(nmx =  mx, sex=1, age = c(seq(0, 100, 1)), nax = NULL){
  n   <- c(diff(age), 999)
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == 1) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == 2) {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  v        <- (nax*c(ex[-1L],0) + (1-nax)*ex)
  v[length(ex)] <- ex[length(ex)]
  v <- ndx*v
  e.dagger <- rev(cumsum(rev(v)))/lx
  e.dagger[1]
}


edaggerfrommxc <- function(mxcvec){
  dim(mxcvec) <- c(101,length(mxcvec)/101)
  mx          <- rowSums(mxcvec)
  edagger.frommx(mx,sex)
}


###########################################
# Conditioning after age 15
###########################################


sd.frommx_15 <- function(nmx =  mx, sex=1, age = c(seq(15, 100, 1)), nax = NULL){
  n   <- c(diff(age), 999)
  
  
  nax <- 0.5 * n
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  nax[length(nax)] <- ex[length(ex)]
  vx <- sum(ndx*(age+nax-ex[1L])^2)
  sd <- sqrt(vx)
  return(sd)
}

sdfrommxc_15 <- function(mxcvec){
  dim(mxcvec) <- c(86,length(mxcvec)/86)
  mx          <- rowSums(mxcvec)
  sd.frommx_15(mx,sex)
}



