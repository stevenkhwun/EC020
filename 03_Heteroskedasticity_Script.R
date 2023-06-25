library(wooldridge)
data("wage2")
wage2_ols1 <- lm(lwage ~ educ + + exper, data=wage2)
wage2$ols1resid <- resid(wage2_ols1)            # get residuals
wage2$fitted1wage <- fitted.values(wage2_ols1)  # get fitted values
wage2$ols1residsq <- wage2$ols1resid^2          # square the residuals