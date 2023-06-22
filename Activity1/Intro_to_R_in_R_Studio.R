##################################################
# INTRODUCTION TO R IN RSTUDIO 
##################################################

## Getting Started ###
# Clear the workspace
rm(list=ls())

# Installing and loading packages
install.packages("foreign")
library(foreign) # the function require() also works

# set working directory
setwd("[ENTER YOUR PATH]")

# help
?library

## Basic Operations ###
# Assignment:  the <- operator is how R does assignment
example_scalar <-  5  
example_vector <-  c(6,7,8,9,10)
example_matrix <- matrix(rep(0,9), nrow=3, ncol=3) # rep(0,9) = replicate 0 9 times
example_dataframe <- data.frame(example_vector, 2*example_vector, -2+5*example_vector)
example_list <- list(example_dataframe, example_vector, "your favourite professor", 5)
example_list

  # dataframes and lists are more flexible data types, and can hold multiple different column types
  # lists are the most flexible but use the most memory to carry around
  # as soon as possible you should use the "unlist()" function to kill the list

# Subsetting and Extraction
subsetted_example_vector <- example_vector[2:4]
extracted_from_example_vector <- example_vector[2]
example_vector_with_bit_deleted <- example_vector[-4]
extracted_from_example_list <- example_list[[2]]
extracted_and_subsetted_from_example_list <- example_list[[2]][1]
subvector_of_example_matrix <- example_matrix[,1]

# Simple transformations
demeaned_example_vector <- example_vector - mean(example_vector, na.rm=TRUE) # na.rm=TRUE means remove missing values
example_vector_elementwise_squared <- example_vector^2

## Loading Data ##
ex_data1 <-read.csv("example1.csv") # loading example csv file
ex_data2 <-read.xlsx("example2.xls") # loading example excel file (also for xlsx extensions)
ex_data3 <-read.dta("example3.dta") # loading example stata file

# loading actual data
data <-read.csv("lalondeexp.csv") # loading real data

# exploring the data
View(data) # look at data
head(data) # only display first few rows
summary(data) # summarize data

### Running an OLS Regression ###

OLS <- lm(re78 ~ treat, data=data) # run OLS regression
summary(OLS) # summary of results
coefficients <- coef(OLS) # get coefficients 
residuals <- residuals(OLS) # get residuals
fitted_values <- fitted(OLS) # get fitted values

# plots residuals against treatment
plot(residuals, data$treat, 
     ylab="Treatment", xlab="Residuals", 
     main="Residuals vs. Treatment") 

# are residuals mean zero?
mean(residuals)

# plotting residuals against fitted values
plot(residuals, fitted_values, 
     ylab="Fitted Values", xlab="Residuals", 
     main="Residuals vs. Fitted Values") 
