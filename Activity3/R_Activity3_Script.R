##################################################
# R Applications for Block 3
##################################################

# Clear the workspace
rm(list=ls())

# Installing and loading relevant packages
#install.packages("wooldridge") # there is no need to install the package again if you've already installed it once before
library(wooldridge) # needed to load data from wooldridge package
#install.packages("car") # there is no need to install the package again if you've already installed it once before
library(car) # linearHypothesis command

# set working directory
#setwd("[ENTER YOUR PATH]") 

## Loading Data ##
data("gpa1") # loading the GPA data

# exploring the data
View(gpa1) # look at data
head(gpa1) # only display first few rows
summary(gpa1) # summarize data

# Multivariate OLS
multivariate_OLS <- lm(colGPA ~ hsGPA + ACT + skipped, data=gpa1) # run OLS regression
summary(multivariate_OLS) # summary of results

### Testing a Single Hypothesis ###

# results from multivariate regression
summary(multivariate_OLS) # summary of results

# critical value of t-dist
p = .05
df = 141-4
qt(p/2, df, lower.tail=FALSE) # critical value for two-sdied test
qt(p, df, lower.tail=FALSE) # critival value for one-sdied test 

# p-value from F-test
linearHypothesis(multivariate_OLS, c("skipped = 0"))

# confidence intervals
confint(multivariate_OLS, 'skipped', level=0.95)

# linear combination of parameters
linearHypothesis(multivariate_OLS, c("hsGPA + ACT = 0"))
linearHypothesis(multivariate_OLS, c("hsGPA =- ACT "))

### Testing Multiple Hypotheses ###

# F-test 
linearHypothesis(multivariate_OLS, c("hsGPA = 0", "ACT = 0", "skipped=0"))

# OLS summary
summary(multivariate_OLS) # summary of results
