##################################################
# R Applications for Block 6
##################################################

# Clear the workspace
rm(list=ls())

# Installing and loading relevant packages
library(wooldridge) 
library(car) 
library(sandwich)
library(lmtest)
library(AER)

## Loading Data ##
data("card") # load data

## USING ONE INSTRUMENT ##

## First Stage
card_FS1 <- lm(educ ~ nearc4, data=card) # first stage reg
coeftest(card_FS1, vcov = vcovHC(card_FS1, type = "HC1")) # heterosk. SEs

## IV Regression
card_IV1 <- ivreg(lwage ~ educ | nearc4, data=card) # IV reg w/ one instrument
coeftest(card_IV1, vcov = vcovHC(card_IV1, type = "HC1")) # heterosk. SEs

## ADDING CONTROLS ##

# IV estimation
card_IV1C <- ivreg(lwage ~ educ + exper + expersq + black + 
                   smsa + south + smsa66 + reg662 + reg663 + reg664 + 
                   reg665 + reg666 + reg667 + reg668 + reg669 | 
                   nearc4 + exper + expersq + black + 
                   smsa + south + smsa66 + reg662 + reg663 + reg664 + 
                   reg665 + reg666 + reg667 + reg668 + reg669, data=card)
coeftest(card_IV1C, vcov = vcovHC(card_IV1C, type = "HC1")) # heterosk. SEs

## USING TWO INSTRUMENTS ##

## First Stage
card_FS2 <- lm(educ ~ nearc4 + nearc2 + exper + expersq + black + 
                smsa + south + smsa66 + reg662 + reg663 + reg664 + 
                reg665 + reg666 + reg667 + reg668 + reg669, data=card)
coeftest(card_FS2, vcov = vcovHC(card_FS2, type = "HC1")) # heterosk. SEs

# F test w/ heterosk.SEs
linearHypothesis(card_FS2, c("nearc4", "nearc2"), 
                 c(0, 0), vcov=vcovHC(card_FS2, type="HC1")) # F-test

# 2SLS Regression
# IV estimation
card_2SLS <- ivreg(lwage ~ educ + exper + expersq + black + 
                   smsa + south + smsa66 + reg662 + reg663 + reg664 + 
                   reg665 + reg666 + reg667 + reg668 + reg669 | 
                   nearc4 + nearc2 + exper + expersq + black + 
                   smsa + south + smsa66 + reg662 + reg663 + reg664 + 
                   reg665 + reg666 + reg667 + reg668 + reg669, data=card)
coeftest(card_2SLS, vcov = vcovHC(card_2SLS, type = "HC1")) # heterosk. SEs

## TESTING FOR ENDOGENEITY ##

# First Step: First Stage plus Residuals
card_FS1 <- lm(educ ~ nearc4, data=card) # first stage reg
card$v <- card_FS1$residuals # save residuals

# Second Step: Test for endogeneity
end_test <- lm(lwage ~ educ + v, data=card)
coeftest(end_test, vcov = vcovHC(end_test, type = "HC1")) # heterosk. SEs
