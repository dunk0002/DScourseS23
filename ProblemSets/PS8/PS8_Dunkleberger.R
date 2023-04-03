#!/usr/bin/env Rscript

install.packages('nloptr')
install.packages('modelsummary')
library(nloptr)
library(modelsummary)

#Setup
set.seed(100)
X <- matrix(NA, nrow = 100000, ncol = 10)
X[, 1] <- 1
X[, 2:10] <- rnorm(900000, mean = 0, sd = 1)
eps <- rnorm(100000, mean = 0, sd = 0.25)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- (X%*%beta) + eps
beta_ols <- (solve(crossprod(X)) %*% crossprod(X,Y))
print(beta_ols)
print(beta)

#Gradient Descent
stepsize <- 0.0000003
iter <- 50000
gradient <- function(beta, y, X) {
  return( as.vector(-2*t(X)%*%(Y-X%*%beta)))
}
x <- floor(runif(length(beta))*10)
allTheta <- vector("numeric", iter)
for (i in 1:iter) {
  x <- x - stepsize*gradient(x, Y, X)
  allTheta[i] <- x
  print(x)
}

#nloptr for betaOLS LBFGS 
beta_ols_LBFGS_fun <- function(beta, Y, X) {
  Y_pred <- X %*% beta
  sum((Y - Y_pred)^2)
}
gradientLBFGS <- function(beta, Y, X) {
  -2 * t(X) %*% (Y - X %*% beta)
}
beta0 <- runif(dim(X)[2])
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
resultLBFGS <- nloptr( x0=beta0,eval_f=beta_ols_LBFGS_fun,eval_grad_f=gradientLBFGS,opts=options,Y=Y,X=X)
print(resultLBFGS)
#Nedler Mead
NMoptions <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)
resultsNM <- nloptr( x0=beta0,eval_f=beta_ols_LBFGS_fun,eval_grad_f=gradientLBFGS,opts=NMoptions,Y=Y,X=X)
print(resultsNM)

#nloptr for betaMLE LBFGS
theta <- rbind(beta, 1)
objfunMLE  <- function(theta,y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradientMLE <- function (theta,y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod (y-X%*%beta)/(sig^3)
  return ( grad )
}
theta0 <- runif(dim(X)[2]+1)
theta0 <- append(as.vector(summary(lm(Y~X-1))$coefficients[,1]),runif(1))
optionsMLE <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
resultMLE <- nloptr(x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=optionsMLE,y=Y,X=X)
print(resultMLE)
betahat  <- result$solution[1:(length(resultMLE$solution)-1)]
sigmahat <- result$solution[length(resultMLE$solution)]
print(summary(lm(Y~X-1)))

#easy way and output
easy <- lm(Y ~ X-1)
modelsummary(easy, output = "easy_summary.tex")

