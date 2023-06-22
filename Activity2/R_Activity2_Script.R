##################################################
# R Applications for Block 2
##################################################

# Clear the workspace
rm(list=ls())

# Installing and loading relevant packages
#install.packages("foreign") # there is no need to install the package again if you've already installed it once before
library(foreign) # the function require() also works

# set working directory
#setwd("[ENTER YOUR PATH]")

## Loading Data ##
data <-read.dta("CARD.dta") # loading the CARD data

# exploring the data
View(data) # look at data
head(data) # only display first few rows
summary(data) # summarize data

# keep only relevant variables
data_for_analysis <- subset(data, select = -c(nearc2,nearc4, age, fatheduc, motheduc, 
                                              weight, momdad14, sinmom14, step14, 
                                              reg661, reg662, reg663, reg664, 
                                              reg665, reg666, reg667, reg668, 
                                              reg669, south66, smsa, south, 
                                              smsa66,enroll, KWW, married, 
                                              libcrd14, lwage,expersq))

### Bivariate Regressions ###

# create log wage outcome
data_for_analysis$lwage <- log(data_for_analysis$wage) # create log wages

# Bivariate OLS 
bivariate_OLS <- lm(lwage ~ educ, data=data_for_analysis) # run OLS regression
summary(bivariate_OLS) # summary of results

# get R squared
summary(bivariate_OLS)$r.squared # get R squared
summary(bivariate_OLS)$adj.r.squared # get adjusted R squared

### Multivariate Regressions ###

# Adding one control
multivar_OLS <- lm(lwage ~ educ + exper, data=data_for_analysis) # run OLS regression
summary(multivar_OLS) # summary of results

# OVB
aux_reg <- lm(exper ~ educ, data=data_for_analysis) # run OLS regression
summary(aux_reg)

# Adding two controls
multivariate_OLS <- lm(lwage ~ educ + IQ + exper, data=data_for_analysis,
                       na.action=na.exclude) # run OLS regression
summary(multivariate_OLS) # summary of results

# get R squared
summary(multivariate_OLS)$r.squared # get R squared
summary(multivariate_OLS)$adj.r.squared # get adjusted R squared

# FWL Theorem
step1 <- lm(educ ~ IQ + exper, data=data_for_analysis,na.action=na.exclude) # first step OLS
data_for_analysis$step1_residuals <- residuals(step1) # get residuals for step 1
step2 <- lm(lwage ~ step1_residuals, data=data_for_analysis,na.action=na.exclude) # second step OLS
summary(step2)

### Multicollinearity ###

# run regression
multivariate_OLS2 <- lm(lwage ~ educ + IQ + exper + black, data=data_for_analysis,
                       na.action=na.exclude) # run OLS regression
summary(multivariate_OLS2) # summary of results

# generate new variable
data_for_analysis$nblack = 1 - data_for_analysis$black

# run regression again 
multivariate_OLS3 <- lm(lwage ~ educ + IQ + exper + black + nblack, data=data_for_analysis,
                       na.action=na.exclude) # run OLS regression
summary(multivariate_OLS3) # summary of results

# without intercept
multivariate_OLS4 <- lm(lwage ~ educ + IQ + exper + black + nblack -1, data=data_for_analysis,
                        na.action=na.exclude) # run OLS regression
summary(multivariate_OLS4) # summary of results

### Standard Errors ###

# results from multivariate regression
summary(multivariate_OLS) # summary of results


