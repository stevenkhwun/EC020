##################################################
# R Applications for Block 5
##################################################

# Clear the workspace
rm(list=ls())

# Installing and loading relevant packages
library(wooldridge) 
library(car) 
library(sandwich)
library(lmtest)
library(AER)
library(ggplot2)

## Loading Data ##
data("wage2") # load data

## OLS with Heteroskedasticity Robust Standard Errors
wage2_ols1 <- lm(lwage ~ educ + exper, data=wage2) # run regression
summary(wage2_ols1) # homosk. SEs
coeftest(wage2_ols1, vcov = vcovHC(wage2_ols1, type = "HC1")) # heterosk. SEs
wage2_ols2<- lm(lwage ~ educ + exper + age + married + black + 
                  south, data=wage2) # run regression
linearHypothesis(wage2_ols2, c("exper", "age", "married", "black", "south"), 
                 c(0, 0, 0, 0, 0), vcov=hccm(wage2_ols2, type="hc1")) # F-test
linearHypothesis(wage2_ols2, c("exper", "age", "married", "black", "south"), 
                 c(0, 0, 0, 0, 0), vcov=vcovHC(wage2_ols2, type="HC1")) # F-test

## Visual Inspections
wage2$ols1resid <- resid(wage2_ols1) # get residuals
wage2$ols1residsq <- wage2$ols1resid^2 # square the residuals
wage2$fittedlwage <- fitted.values(wage2_ols1) # get fitted values 

# using plot command
plot(wage2$fittedlwage, wage2$ols1resid, xlab="Fitted Log Wage", ylab="Residuals")
plot(wage2$educ, wage2$ols1resid, xlab="Education", ylab="Residuals")

# using ggplot command
ggplot(wage2, aes(x=fittedlwage, y=ols1resid))
ggplot(wage2, aes(x=fittedlwage, y=ols1resid)) + geom_point()
g <- ggplot(wage2, aes(x=fittedlwage, y=ols1resid)) + geom_point()  
plot(g)
g + ggtitle("Residuals vs. Fitted Wage", subtitle="Exploring Heteroskedasticity") + 
  xlab("Fitted Log Wage") + ylab("Residuals")

# additional exericses using ggplot
# color the scatter plot according to different levels of education
p1 <- ggplot(wage2, aes(x=fittedlwage, y=ols1resid)) + 
  geom_point(aes(col=educ), size=1.5) + 
  ggtitle("Residuals vs. Fitted Wage", subtitle="Exploring Heteroskedasticity") + 
  xlab("Fitted Log Wage") + ylab("Residuals") # add xlim and ylim to change scales
plot(p1)
# scatter of log wages and education including best fit
p2 <- ggplot(wage2, aes(x=educ, y=lwage)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  ggtitle("Log Wages and Education") + 
  xlab("Education") + ylab("Log Wages")
plot(p2)
# bar plot of education
p3 <- ggplot(wage2, aes(x=educ)) + 
  geom_bar() +
  ggtitle("Histogram of Education Variable") + 
  xlab("Education") + ylab("Frequency")
plot(p3)

## Breusch-Pagan Test

# step 2: 
BP_step2 <- lm(ols1residsq~educ + exper, data=wage2)
summary(BP_step2)

## White Test

# define squares and interactions for white test
wage2$educsq<- wage2$educ^2 # education squared
wage2$expersq <- wage2$exper^2 # experience squared
wage2$educexper <- wage2$educ*wage2$exper # education * experience

# define fittes values and squares for the special white test
wage2$olsfitted <- fitted.values(wage2_ols1)
wage2$olsfittedsq <- wage2$olsfitted^2

# Usual White test
White_1 <- lm(ols1residsq~educ+exper+educsq + expersq+educexper, data=wage2)
summary(White_1)

# Special White test
White_2 <- lm(ols1residsq~olsfitted + olsfittedsq, data=wage2)
summary(White_2)

