# Part of Chapter 10, J. Fox and S. Weisberg, An R Companion to Applied Regression, 3ed edition

zipmodBugged <- function(X, y, Z=X, intercept.X=TRUE, 
                         intercept.Z=TRUE, ...) { # bugged!
  if (intercept.X) {
    X <- cbind(1, X)
    colnames(X)[1] <- "intercept"
  }
  if (intercept.Z) {
    Z <- cbind(1, Z)
    colnames(Z)[1] <- "intercept"
  }
  n.x <- ncol(X)
  negLogL <- function(beta.gamma) {
    beta <- beta.gamma[1:n.x]
    gamma <- beta.gamma[-(1:n.x)]
    pi <- 1/(1 + exp(- Z %*% gamma))
    mu <- exp(X %*% beta)
    L1 <- sum(log(pi + (1 - pi)*dpois(y, mu)))[y == 0]
    L2 <- sum((log((1 - pi)*dpois(y, mu))))[y > 0]
    -(L1 + L2)
  }
  initial.beta <- coef(glm(y ~ X - 1, family=poisson))
  initial.gamma <- coef(glm(y == 0 ~ Z - 1, family=binomial))
  result <- optim(c(initial.beta, initial.gamma), negLogL,
                  hessian=TRUE, method="BFGS", ...)
  beta.gamma <- result$par
  vcov <- solve(result$hessian)
  par.names <- c(paste0("beta.", colnames(X)), paste0("gamma.",
                                                      colnames(Z)))
  names(beta.gamma) <- par.names
  rownames(vcov) <- colnames(vcov) <- par.names
  list(coefficients=beta.gamma,  vcov=vcov,
       deviance=2*result$value, converged=result$convergence == 0)
}