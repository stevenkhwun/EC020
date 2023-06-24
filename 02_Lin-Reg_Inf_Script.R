library(wooldridge)
data("gpa1")
multivariate_OLS <- lm(colGPA ~ hsGPA + ACT + skipped, data=gpa1)
summary(multivariate_OLS)
p = .05
df = 141 - 4
qt(p/2, df, lower.tail = FALSE)
library(car)
linearHypothesis(multivariate_OLS, c("skipped = 0"))