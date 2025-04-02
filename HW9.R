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
  alpha <- exp(par[1]) # need alpha greater than 0 bc alpha E R+
  beta <- exp(par[2]) # need beta greater than 0 bc beta E R+
  
  ll <- sum(log(dgamma(x=data, shape=alpha, scale=beta)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs.gamma <- optim(fn = llgamma,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

hat.alpha <- exp(MLEs.gamma$par[1])
hat.beta <- exp(MLEs.gamma$par[2])

#####################################
# Part B: Maximum Likelihood (Log-Normal)
#####################################
lllognormal <- function(par, data, neg=F){
  mu <- par[1] 
  sigma <- exp(par[2]) # need sigma greater than 0 bc sigma E R+
  
  ll <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs.lnorm <- optim(fn = lllognormal,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

hat.mu <- MLEs.lnorm$par[1]
hat.sigma <- exp(MLEs.lnorm$par[2])

#######################################
# Part C: Compute the Likelihood Ratio
# Weibull vs Gamma
#######################################
weibull.loglikelihood <- -2166.496
gamma.loglikelihood <- -MLEs.gamma$value 

Q <- exp(weibull.loglikelihood-gamma.loglikelihood)
# Q = 2.161379e-07 

#######################################
# Part D: Compute the Likelihood Ratio
# Weibull vs Log-Normal
#######################################
weibull.loglikelihood <- -2166.496
lnorm.loglikelihood <- -MLEs.lnorm$value # needs to be negative to 
                                         # get the log-likelihood

Q2 <- exp(weibull.loglikelihood-lnorm.loglikelihood)
# Q = 2.370639e+16

#######################################
# Part E: Compute the Likelihood Ratio
# Gamma vs Log-Normal
#######################################
gamma.loglikelihood <- -MLEs.gamma$value 
lnorm.loglikelihood <- -MLEs.lnorm$value 

Q3 <- exp(gamma.loglikelihood - lnorm.loglikelihood)
# Q = 1.096818e+23 

table <- tibble(
  distribution = c("Weibull", "Gamma", "Log-Normal"),
  `parameter 1` = c(2.1871,
                    round(hat.alpha, 4),
                    round(hat.mu, 4)),
  `parameter 2` = c(3.9683,
                    round(hat.beta, 4),
                    round(hat.sigma, 4)),
  `log-likelihood` = c( -2166.496,
                        -MLEs.gamma$value,
                        -MLEs.lnorm$value)
)
view(table)

table2 <- tibble(
  distribution = c("Weibull vs Gamma", 
                   "Weibull vs Log-Normal", 
                   "Gamma vs Log-Normal"),
  `Likelihood Ratio` = c(Q,
                         Q2,
                         Q3)
)
view(table2)
