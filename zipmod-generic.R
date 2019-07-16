# Part of Chapter 10, J. Fox and S. Weisberg, An R Companion to Applied Regression, 3ed edition

zipmod <- function(X, ...){
  UseMethod("zipmod")
}

zipmod.default <- function(X, y, Z=X, intercept.X=TRUE, 
                           intercept.Z=TRUE, ...) {
  if (!is.matrix(X) || !is.numeric(X)) 
    stop("X must be a numeric matrix")
  if (!is.matrix(Z) || !is.numeric(Z)) 
    stop("Z must be a numeric matrix")
  if (!is.vector(y) || !is.numeric(y) || !all(y >= 0) 
      || !all(y == round(y)))
    stop("y must be a vector of counts")
  if (nrow(X) != length(y)) 
    stop("number of rows in X must be the same as length of y")
  if (nrow(Z) != length(y)) 
    stop("number of rows in Z must be the same as length of y")
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
    L1 <- sum(log(pi + (1 - pi)*dpois(y, mu))[y == 0])
    L2 <- sum((log((1 - pi)*dpois(y, mu)))[y > 0])
    -(L1 + L2)
  }
  initial.beta <- coef(glm(y ~ X - 1, family=poisson))
  initial.gamma <- coef(glm(y == 0 ~ Z - 1, family=binomial))
  result <- optim(c(initial.beta, initial.gamma), negLogL,
                  hessian=TRUE, method="BFGS", ...)
  beta.gamma <- result$par
  vcov <- solve(result$hessian)
  par.names <- c(paste0("beta.", colnames(X)), 
                 paste0("gamma.", colnames(Z))) 
  names(beta.gamma) <- par.names
  rownames(vcov) <- colnames(vcov) <- par.names
  result <- list(coefficients=beta.gamma,  vcov=vcov, 
                 npar=c(beta=ncol(X), gamma=ncol(Z)),
                 deviance=2*result$value, converged=result$convergence == 0)
  class(result) <- "zipmod"
  result
}

print.zipmod <- function(x, ...) {
  coef <- coef(x)
  npar <- x$npar
  beta <- coef[1:npar["beta"]]
  gamma <- coef[-(1:npar["beta"])]
  names.beta <- names(beta)
  names.gamma <- names(gamma)
  names(beta) <- sub("^beta\\.", "", names.beta)
  names(gamma) <- sub("^gamma\\.", "", names.gamma)
  cat("beta coeffients:\n")
  print(beta)
  cat("\ngamma coeffients:\n")
  print(gamma)
  if (!x$converged) warning("estimates did not converge")
  invisible(x)
}
vcov.zipmod <- function(object, separate=FALSE, ...){
  if (!separate) return(object$vcov)
  else{
    vcov <- object$vcov
    npar <- object$npar
    index <- 1:npar["beta"]
    vcov.beta <- vcov[index, index]
    vcov.gamma <- vcov[-index, -index]
    names.beta <- rownames(beta)
    names.gamma <- rownames(gamma)
    rownames(vcov.beta) <- colnames(vcov.beta) <-
      sub("^beta\\.", "", names.beta)
    rownames(vcov.gamma) <- colnames(vcov.gamma) <-
      sub("^gamma\\.", "", names.gamma)
    return(list(beta=vcov.beta, gamma=vcov.gamma))
  }
}
summary.zipmod <- function(object, ...) {
  coef <- coef(object)
  npar <- object$npar
  beta <- coef[1:npar["beta"]]
  gamma <- coef[-(1:npar["beta"])]
  names.beta <- names(beta)
  names.gamma <- names(gamma)
  names(beta) <- sub("^beta\\.", "", names.beta)
  names(gamma) <- sub("^gamma\\.", "", names.gamma)
  vcov <- vcov(object, separate=TRUE)
  se.beta <- sqrt(diag(vcov[["beta"]]))
  z.beta <- beta/se.beta
  table.beta <- cbind(beta, se.beta, z.beta, 
                      2*(pnorm(abs(z.beta), lower.tail=FALSE)))
  colnames(table.beta) <- c("Estimate", "Std.Err", 
                            "z value", "Pr(>|z|)")
  rownames(table.beta) <- names(beta)
  se.gamma <- sqrt(diag(vcov[["gamma"]]))
  z.gamma <- gamma/se.gamma
  table.gamma <- cbind(gamma, se.gamma, z.gamma, 
                       2*(pnorm(abs(z.gamma), lower.tail=FALSE)))
  colnames(table.gamma) <- c("Estimate", "Std.Err", 
                             "z value", "Pr(>|z|)")
  rownames(table.gamma) <- names(gamma)
  result <- list(coef.beta=table.beta, 
                 coef.gamma=table.gamma, deviance=object$deviance, 
                 converged=object$converged)
  class(result) <- "summary.zipmod"
  result
}
print.summary.zipmod <- function(x, ...) {
  cat("beta coefficients:\n")
  printCoefmat(x$coef.beta, signif.legend=FALSE, ...)
  cat("\ngamma coefficients:\n")
  printCoefmat(x$coef.gamma, ...)
  cat("\nDeviance =", x$deviance,"\n")
  if (!x$converged) warning("estimates did not converge")
  invisible(x)
}

zipmod.formula <- function(formula, zformula, data, subset, 
                           na.action, model = TRUE, contrasts = NULL, ...) {
  combineFormulas <- function(formula1, formula2){
    rhs <- as.character(formula2)[[2]]
    formula2 <- as.formula(paste("~ . +", rhs))
    update(formula1, formula2)
  }
  if (missing(zformula)) zformula <- formula[c(1, 3)]
  call <- match.call()  # returns the function call
  mf <- match.call(expand.dots = FALSE)  # the function call w/o ...
  args <- match(c("formula", "data", "subset", "na.action"),
                names(mf), 0)  # which arguments are present?
  mf <- mf[c(1, args)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf$formula <- combineFormulas(formula, zformula)
  mf <- eval.parent(mf)  # create a model frame
  terms <- attr(mf, "terms")  # terms object for the model
  y <- model.response(mf)  # response variable
  X <- model.matrix(formula, mf, contrasts)
  Z <- model.matrix(zformula, mf, contrasts)
  mod <- zipmod(X, y, Z, intercept.X=FALSE, intercept.Z=FALSE,
                ...)
  mod$na.action <- attr(mf, "na.action")
  mod$contrasts <- attr(X, "contrasts")
  if (model)  {
    mod$model <- mf
    mod$X <- X
    mod$Z <- Z
    mod$y <- y
  }
  mod
}

fitted.zipmod <- function(object, ...){
  beta.gamma <- coef(object)
  npar <- object$npar
  beta <- beta.gamma[1:npar["beta"]]
  gamma <- beta.gamma[-(1:npar["beta"])]
  pi <- as.vector(1/(1 + exp(-(object$Z %*% gamma))))
  as.vector((1 - pi)*exp(object$X %*% beta))
}