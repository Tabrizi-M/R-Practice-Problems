### this solution needs polishing

### probit regression via EM
### Y for complete data, X for the binary response and U for the predictor variable. .

em.probreg <- function(U, X, theta0) {
  maxit <- 1000
  tol <- 1e-10
  i <- 0
  M <- solve(t(U) %*% U) %*% t(U)
  theta <- theta0
  m <- U %*% theta
  repeat {
  i <- i + 1
  v <- (2 * X - 1) * dnorm(m) / pnorm((2 * X - 1) * m)
  up <- M %*% v
  theta <- theta + up
  if(max(abs(up)) < tol || i >= maxit) break
  m <- U %*% theta
  }
  return(list(iterations=i, estimate=as.numeric(theta)))
}

A <- 0
B <- 1
x <- rnorm(50, 0, 4)
y <- rbinom(50, 1, pnorm(A + B * x))
nloglik <- function(theta) {
  a <- theta[1]
  b <- theta[2]
  p <- pnorm(a + b * x)
  o <- sum(dbinom(y, size=1, prob=p, log=TRUE))
  return(-o)
}

theta0 <- c(1,3)
o.optim <- optim(theta0, nloglik, method="BFGS"); print(o.optim)
glm1 <- glm(y ~ x, family=binomial(link="probit")); print(list(glm1$coef, glm1$iter))
o.em <- em.probreg(as.matrix(cbind(rep(1, 50), x)), y, theta0); print(o.em)
plot(x, y, xlab="X", ylab="Y")
f <- function(x) pnorm(o.em$estimate[1] + o.em$estimate[2] * x)
curve(f, add=TRUE, col=2)
