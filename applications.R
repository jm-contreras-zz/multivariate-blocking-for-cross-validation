## Examine Canonical Data 
## Ryan T. Moore
## First: 25 July 2016
## Last: 25 July 2016

library(ISLR)
library(dplyr)

######### Auto data, 'Intro to Stat Learning' #########

data(Auto)
dim(Auto)

## How predictive are 1-10 polynomials of mpg on horsepower, full data set?
## Initialize:
k <- 10
fit_stats <- list()  #data.frame(mse = NA, adj_r_sq = NA)
## Estimate polynomial regression: 
for(i in 1:k){
  lm_summary <- summary(lm(mpg ~ poly(horsepower, i), data = Auto))
  fit_stats[[i]] <- c(lm_summary$adj.r.sq, mean(lm_summary$residuals^2))
}

## Transform to data frame:
fit_stats <- do.call("rbind", fit_stats) %>% data.frame()
names(fit_stats) <- c("adj_r_sq", "mse")

## Plot fit stats:
xl <- "Polynomial Degree"
plot(fit_stats$adj_r_sq, type = "b", xlab = xl, ylab = "Adj R-Sq")
plot(fit_stats$mse, type = "b", xlab = xl, ylab = "MSE")

## Clean up:
rm(i, k, xl, lm_summary)
