### Solution to exercise 9.2 of the textbook

require(dobson)
require(ggplot2)
library(nnet)
library(MASS)

### preparation of data
housing <- dobson::housing
housing$type <- as.factor(housing$type)
housing$contact <- as.factor(housing$contact)

### model of interest
res.housing=multinom(satisfaction ~ type + contact, weights=frequency, data=housing)
summary(res.housing)

### full model
res.housing.full =multinom(satisfaction ~ type * contact, weights=frequency, data=housing)
summary(res.housing.full)

### deviance test
delta_deviance = deviance(res.housing) - deviance(res.housing.full)
# delta_df = 4 because df.residual(res.housing.full)=12 , df.residual(res.housing) = 8
# p_value > 0.05 so dont reject H0
p_value = 1-pchisq(delta_deviance, df= 4)

res.housing.polr = polr(factor(satisfaction) ~ type + contact, weights=frequency, data=housing)
res.housing.polr.full = polr(factor(satisfaction) ~ type * contact, weights=frequency, data=housing)

### deviance test
delta_deviance = deviance(res.housing.polr) - deviance(res.housing.polr.full)
# p_value > 0.05 so dont reject H0
p_value = 1-pchisq(delta_deviance, df= 4)

### fitted value and residual
fitted(res.housing)
fitted(res.housing.polr)
res.housing$residuals
