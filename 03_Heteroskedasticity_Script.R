library(wooldridge)
data("wage2")
wage2_ols1 <- lm(lwage ~ educ + + exper, data=wage2)
