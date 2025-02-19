#load libraries 
require(dobson)
require(ggplot2)
library(MASS)

beetle <- dobson::beetle
head(beetle)
#data preparation
beetle$n_y=beetle$n-beetle$y
beetle$mat=cbind(beetle$y,beetle$n_y)
beetle$p = beetle$y / beetle$n

# logistic regression
res_logit=glm(beetle$mat~beetle$x, family=binomial(link="logit"))
# fitted.values
fit_p_logit = c(fitted.values(res_logit))
fit_y_logit = beetle$n*fit_p_logit

# probit model
res_probit=glm(beetle$mat ~ beetle$x, family=binomial(link="probit"))
# fitted.values
fit_p_probit = c(fitted.values(res_probit))
fit_y_probit = beetle$n*fit_p_probit

# extreme value model
res_cloglog=glm(beetle$mat ~ beetle$x, family=binomial(link="cloglog"))
# fitted.values
fit_p_cloglog = c(fitted.values(res_cloglog))
fit_y_cloglog = beetle$n*fit_p_cloglog

#plots
plot(beetle$x, beetle$p, pch=19,col="black",
     main ="Dose response models", xlab ="dose", ylab = "fitted probability")
lines(beetle$x, fit_p_logit, col="blue", lwd=3)
lines(beetle$x, fit_p_probit, col="red", lwd=3)
lines(beetle$x, fit_p_cloglog, col="green2", lwd=3)

#lethal dose (LD-50, LD-5)
# complementary log-log function. The model is similar to the logistic and
# probit models for probability values near 0.5 but differs for probability near 0 or 1.
exp(dose.p(res_logit,p=c(0.5, 0.05)))
exp(dose.p(res_probit,p=c(0.5, 0.05)))
exp(dose.p(res_cloglog,p=c(0.5, 0.05)))