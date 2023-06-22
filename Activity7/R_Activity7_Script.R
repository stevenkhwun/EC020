##################################################
# R Applications for Block 9
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

### Equispaced Time Series Data ### 
# Loading Data
data("barium") # loading the barium data

# TS import variable
imports<- ts(barium$chnimp, start=c(1978,2), frequency=12)

# plot imports 
plot(imports)

### Irregular Time Series Data ### 
# loading data 
data("intdef")

# TS the whole dataset
intdef_ts <- zoo(intdef, order.by=intdef$year)

# plot interest rate over time
plot(intdef_ts$i3)
plot(intdef$i3, type="l")
plot(intdef$year, intdef$i3, type="l")

### FDL Models ### 

# load fertility data
data("fertil3")
# time set dataset
fertil3_ts <- ts(fertil3, start=1913) # ts version
fertil3_ts1 <- ts(fertil3, start=1913, frequency=1) # ts version w/ frequency
fertil3_ts2 <- zoo(fertil3, order.by = fertil3$year) # zoo version 

# run regression 
fert_reg <- dynlm(gfr ~ pe + L(pe) + L(pe, 2) + ww2 + pill, data=fertil3_ts)
summary(fert_reg)

# testing if LRP is zero
linearHypothesis(fert_reg, "pe + L(pe) + L(pe, 2) = 0")

### AR(p) Models ### 

# load and ts data
data("nyse") # loading the nyse data
nyse_ts <- zoo(nyse, order.by = nyse$t)

# ar models
reg_ar1 <- dynlm(return ~ L(return), data = nyse_ts) # AR(1) Model
reg_ar2 <- dynlm(return ~ L(return) + L(return, 2), data = nyse_ts) # AR(2) Model
reg_ar3 <- dynlm(return ~ L(return) + L(return, 2) + 
                   L(return, 3), data = nyse_ts) # AR(3) Model
summary(reg_ar1)
summary(reg_ar2)
summary(reg_ar3)

### Testing for Serial Correlation ###

# load data
data("phillips")
# ts data 
phillips_ts <- zoo(phillips, order.by = phillips$year)

# run regression 
phil_reg <- dynlm(inf ~ unem, data=phillips_ts)
summary(phil_reg)
# test for autocorrelation manually
phil_res <- resid(phil_reg) # get residuals
resid_test <- dynlm(phil_res ~ L(phil_res)) # reg residuals on lagged residuals
summary(resid_test) # display results
# BG and DW test
bgtest(phil_reg) # BG test
dwtest(phil_reg) # DW test
bgtest(phil_reg, order=2) # higher order BG test
bgtest(phil_reg, order=2, type="F") # BG test w/ Ftest

### HAC Standard Errors ###

# summary from reg above
summary(phil_reg)
coeftest(phil_reg)

# report HAC SEs
coeftest(phil_reg, vcovHAC)


