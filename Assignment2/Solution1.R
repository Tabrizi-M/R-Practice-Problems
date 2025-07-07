
require(dobson)
require(ggplot2)

#preparation of data
leukemia <- dobson::leukemia
log_wbc = leukemia$wbc
time = leukemia$time
log_time = log(leukemia$time)

#plot
plot(y=leukemia$time, x=leukemia$wbc, pch=19, col= "black",
     main ="Survival time for leukemia", ylab ="time to death (weeks)", xlab = "log(white blood cell count)")

#models
lm1 <- lm(log_time ~ log_wbc)
summary(lm1)

glm1 <- glm(time ~ log_wbc, family="poisson")
summary(glm1)

#table
table1 <- rbind( x=log_wbc, y= glm1$y, fitted_values= glm1$fitted.values, 
                 residuals = resid(glm1), standard_residuals = glm1$residuals)
write.csv(x= table1, file = "./York Statistics/Math 6622 - GLM/A2/table.csv" )

#plots
plot(y=leukemia$time, x=leukemia$wbc, pch=19, col= "black",
     main ="Survival time for leukemia (red=lm, blue=glm)", xlab ="time to death (weeks)", ylab = "log(white blood cell count)")
lines(leukemia$wbc, glm1$fitted, col="blue",lwd=3)
lines(leukemia$wbc, exp(lm1$fitted.values), col="red",lwd=3)

#residual plot
plot(glm1$residuals , pch=19, ylab = "residuals", main = "residual plot")
abline(h = 0)

plot( glm1$fitted , resid(glm1), pch=19)
