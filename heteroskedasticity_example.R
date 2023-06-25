# Example 8.4 on p. 270

library(wooldridge)
data("hprice1")
hp_ols <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
summary(hp_ols)

hprice1$hp_olsresid <- resid(hp_ols)            # get residuals
hprice1$fitted1price <- fitted.values(hp_ols)  # get fitted values
plot(hprice1$fitted1price, hprice1$hp_olsresid, xlab="Fitted Price", ylab="Residuals")

hprice1$hp_olsresidsq <- hprice1$hp_olsresid^2
BP_step2 <- lm(hp_olsresidsq ~ lotsize + sqrft + bdrms, data=hprice1)
summary(BP_step2)