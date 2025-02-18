### Simulation study for LM(normal) vs GLM(link=Poisson) ###

set.seed(3759) ## for reproducibility
reps <- 1000 ## number of simulated data sets
par.est.pois <- matrix(NA, nrow=reps, ncol = 4)
par.est.norm <- matrix(NA, nrow=reps, ncol = 4)
b0 <- .2
b1 <- .5
n<- 1000
X<- runif(n, 0, 1)

for(i in 1:reps)
{
  Y <-rpois(n, exp(b0 + b1*X))
  ## poisson regression fit and estimates
  glm1 <- glm(Y ~ X, family = 'poisson')
  vcv <- vcov(glm1)
  par.est.pois[i,1] <- glm1$coef[1]
  par.est.pois[i,2] <- glm1$coef[2]
  par.est.pois[i,3] <- sqrt(diag(vcv)[1])
  par.est.pois[i,4] <- sqrt(diag(vcv)[2])
  ## normal regression fit and estimates
  lm1 <- lm(Y ~ X)
  vcv <- vcov(lm1)
  par.est.norm[i,1] <- lm1$coef[1]
  par.est.norm[i,2] <- lm1$coef[2]
  par.est.norm[i,3] <- sqrt(diag(vcv)[1])
  par.est.norm[i,4] <- sqrt(diag(vcv)[2])
}

#means of coefficient estimates
print(mean(par.est.pois[ , 1]))
print(mean(par.est.pois[ , 2]))
print(mean(par.est.norm[ , 1]))
print(mean(par.est.norm[ , 2]))

#SE of coefficient estimates
print(sqrt( var(par.est.pois[ , 1]) / reps) )
print(sqrt( var(par.est.pois[ , 2]) / reps) )
print(sqrt( var(par.est.norm[ , 1]) / reps) )
print(sqrt( var(par.est.norm[ , 2]) / reps) )

##plots
par(mfrow=c(2,2))
hist(par.est.pois[ , 1], xlab="Estimate B0", ylab="", main="Poisson Model")
abline(v=b0, col='blue', lwd=3)
hist(par.est.norm[ , 1], xlim = c(0,2), xlab="Estimate B0", ylab="", main="Normal Model")
abline(v=b0, col='blue', lwd=3)
hist(par.est.pois[ , 2], xlab="Estimate B1", ylab="", main="Poisson Model")
abline(v=b1, col='blue', lwd=3)
hist(par.est.norm[ , 2], xlab="Estimate B1", ylab="", main="Normal Model")
abline(v=b1, col='blue', lwd=3)
par(mfrow=c(2,1))
plot(par.est.pois[ , 2], par.est.norm[ , 2],xlab="Poisson",ylab="Normal",main="Estimate B1")
abline(h=b1, col='blue', lwd=3)
abline(v=b1, col='blue', lwd=3)
plot(par.est.pois[ , 1], par.est.norm[ , 1], ylim = c(0,2), xlab="Poisson",ylab="Normal",main="Estimate B0")
abline(h=b0, col='blue', lwd=3)
abline(v=b0, col='blue', lwd=3)