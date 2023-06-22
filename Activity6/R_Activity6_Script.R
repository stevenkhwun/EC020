##################################################
# R Applications for Block 8
##################################################

# Clear the workspace
rm(list=ls())

#loading packages you have installed earlier
library(dplyr)
library(wooldridge)
library(ggplot2)
library(stats)

# Installing and loading new relevant packages

install.packages("glm2", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
install.packages("sandwich", dependencies = TRUE)
install.packages("mfx", dependencies = TRUE)

library(glm2)
help(glm2)

library(lmtest)
library(sandwich)
library(mfx)

# set working directory
setwd("/you/your workspace/your folder/") 

## Loading Data ##
data('recid')
data <-recid

# exploring the data
help(wooldrige)
View(data) # look at data
summary(data) # summarize data

# Who gets released from prison? Release is coded as a binary variable 'super'. What is the mean of this variable? 
summary(data$super)

# Run a Linear Probability Model to explain who gets released. Are prisoners that take part in a work program likelier to get released? Run the model and interpret the coefficients. Use robust SE
model1 <-lm(super~ workprg+married+priors+alcohol+age, data)
coeftest(model1, vcov = vcovHC(model1))
data<-cbind(data, fitted = fitted(model1))


# Now run the same model as a probit. How would you interpret the coefficient on priors? Save the fitted values and plot them against priors.
probit1 <- glm2(super~ workprg+married+priors+alcohol+age, family = binomial(link = "probit"), 
               data = data)

## Model summary and save the fitted values
summary(probit1) 
data<-cbind(data, fitted_probit = fitted(probit1))

## Average partial effects (APE) and Partial effects at the average (PEA)

# for APE
mod_APE <- probitmfx(super~ workprg+married+priors+alcohol+age, data = data, atmean = FALSE, robust = TRUE)
mod_APE

# for PEA
mod_PEA <- probitmfx(super~ workprg+married+priors+alcohol+age, data = data, robust = TRUE) 
mod_PEA

# What is the effect of attending a work program for a specific individual who is married, without priors, doesn't drink and is 40 years old. 
# We find the predicted probabilities at the predictors, with and without having attended the work program. 
person1 = data.frame(married=1, priors=0, alcohol=0, age=40, workprg=0)
person1_work = data.frame(married=1, priors=0, alcohol=0, age=40, workprg=1)

predict(probit1, person1, type="response")
predict(probit1, person1_work, type="response")
margins_workprg_person1=predict(probit1, person1_work, type="response")-predict(probit1, person1, type="response")
margins_workprg_person1 # This person is 11.8 percentage points likelier to get released if they take part in a work program.


# Test the joint significance of alcohol and age. Probit1 model has become the unrestricted model and we will estimate the restricted model as Probit2. 
probit2 <- glm2(super~ workprg+married+priors, family = binomial(link = "probit"), 
               data = data) 

lrtest (probit1, probit2)


## Plot the fitted values of the LPM and Probit against the observations of the super 

#scatterplot LPM
data %>%
  ggplot(aes(fitted, super)) +
  geom_point()+  geom_smooth(aes()) 
ggsave("ScatterLPM.png", width=9, height=6)

#scatterplot Probit
data %>%
  ggplot(aes(fitted_probit, super)) +
  geom_point()+  geom_smooth(aes()) 
ggsave("ScatterProbit.png", width=9, height=6)

  


