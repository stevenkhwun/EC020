##################################################
# R Applications for Block 10
##################################################

# Clear the workspace
rm(list=ls())

# Installing and loading relevant packages
#install.packages("wooldridge") # there is no need to install the package again if you've already installed it once before
library(wooldridge) # needed to load data from wooldridge package
#install.packages("car") # there is no need to install the package again if you've already installed it once before
library(car) # linearHypothesis command
#install.packages("zoo") # there is no need to install the package again if you've already installed it once before
library(zoo) #  loading irregular time series
#install.packages("dynlm") # there is no need to install the package again if you've already installed it once before
library(dynlm) #  dynlm function
#install.packages("lmtest") # there is no need to install the package again if you've already installed it once before
library(lmtest)
#install.packages("sandwich") # there is no need to install the package again if you've already installed it once before
library(sandwich)
#install.packages("tseries") # there is no need to install the package again if you've already installed it once before
library(tseries)

### Accounting for Trends ### 
# load and ts data
data("hseinv")
hseinv_ts <- zoo(hseinv, order.by = hseinv$year) # ts data

# naive regression 
house_reg <- dynlm(log(invpc) ~ log(price), data=hseinv_ts)
summary(house_reg)

# plotting data
plot(log(hseinv_ts$price)) # plot price
plot(log(hseinv_ts$invpc)) # plot investment

# include trend in regression 
house_reg2 <- dynlm(log(invpc) ~ log(price) + trend(hseinv_ts), data=hseinv_ts)
summary(house_reg2)

### Accounting for Seasonality ### 
# load and ts data
data("barium") # loading the barium data
barium_ts <- ts(barium, start = c(1978, 2), frequency = 12) # ts full data
imports<- ts(barium$chnimp, start=c(1978,2), frequency=12) # ts only one variable
plot(imports) # plot imports

# seasonal reg
barium_reg <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 
                    + affile6 + afdec6 + season(barium_ts), data=barium_ts)
summary(barium_reg)

# F-test on seasonality dummies
linearHypothesis(barium_reg, c("season(barium_ts)Feb=0", "season(barium_ts)Mar=0", 
                               "season(barium_ts)Apr=0", "season(barium_ts)May=0", 
                               "season(barium_ts)Jun=0", "season(barium_ts)Jul=0", 
                               "season(barium_ts)Aug=0", "season(barium_ts)Sep=0",
                               "season(barium_ts)Oct=0", "season(barium_ts)Nov=0",
                               "season(barium_ts)Dec=0"))


### Random Walks and First Differences ###

# simulate differenced random walk
set.seed(348546) # set seed
plot(c(0,50), c(2,2), type="l", lwd=2, ylim=c(-1, 5)) # initialize graph
for (r in 1:30){ # start loop to simulate 30 random walks
  eps <- rnorm(50) # simulate epsilons from std normal distr (get 50 epsilons)
  y <- ts(cumsum(2 + eps)) # y, the time series, is just the sum of all the epsilons
  Dy <- diff(y) # take difference
  lines(Dy, col=gray(.6)) # add this random walk into the plot above as a gray line
}

# load fertility data
data("fertil3")
# time set dataset
fertil3_ts <- zoo(fertil3, order.by = fertil3$year) # ts data
# run two regressions
fertil_reg1 <- dynlm(d(gfr) ~ d(pe), data=fertil3_ts)
fertil_reg2 <- dynlm(d(gfr) ~ d(pe) + L(d(pe)) + L(d(pe), 2), data=fertil3_ts)
summary(fertil_reg1)
summary(fertil_reg2)

### Unit Roots ###

adf.test(y, k=1) # ADF test
adf.test(Dy, k=1) # ADF test
