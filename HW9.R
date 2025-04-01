################################################################################
# Homework 9
# Caroline Devine
################################################################################
library(tidyverse)
library(nleqslv)
################################################################################
# Question 1
################################################################################
# Load Data
dat.precip <- read_csv(file = "agacis.csv")
# Clean Data
dat.precip.long <- dat.precip |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation))
# Summarize Data
ggplot(data=dat.precip.long) +
  geom_histogram(aes(x=Precipitation, y=after_stat(density)),
                 breaks=seq(0, 15, 1),
                 color="grey")+
  geom_hline(yintercept = 0)+
  theme_bw() +
  xlab("Precipitation (Inches)")
ylab("Density")

library(e1071)
dat.precip.long |>
  summarize(
    mean = mean(Precipitation, na.rm=T),
    sd = sd(Precipitation, na.rm=T),
    min = min(Precipitation, na.rm=T),
    max = max(Precipitation, na.rm=T),
    skew = skewness(Precipitation, na.rm=T),
    kurt = kurtosis(Precipitation, na.rm=T)
  )

#####################################
# Part A: Maximum Likelihood (Gamma)
#####################################
llgamma <- function(par, data, neg=F){
  alpha <- exp(par[1]) # go from (-inf,inf) to (0,inf)
  beta <- exp(par[2]) # go from (-inf,inf) to (0,inf)
  
  ll <- sum(log(dgamma(x=data, shape=alpha, scale=beta)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs.gamma <- optim(fn = llgamma,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

hat.alpha <- MLEs.gamma$par[1]
hat.beta <- MLEs.gamma$par[2]

#####################################
# Part B: Maximum Likelihood (Log-Normal)
#####################################
lllognormal <- function(par, data, neg=F){
  mu <- par[1] 
  sigma <- par[2] 
  
  ll <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs.lnorm <- optim(fn = lllognormal,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

hat.mu <- MLEs.lnorm$par[1]
hat.sigma <- MLEs.lnorm$par[2]

# Part C - Likelihood Ratio

# Part D 