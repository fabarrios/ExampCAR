##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 10                 ##
##-----------------------------------------------------##

(x <- 1:10)

(x.lag <- c(NA, x))

(x.lag <- x.lag[-11])

(x.lag2 <- c(rep(NA, 2), x[1:8]))

Lag <- function(x, lag=1){ 
  c(rep(NA, lag), x[1:(length(x) - lag)])
}
Lag(x)
Lag(x, lag=2)
Lag(letters, 2)

set.seed(123) # for reproducibility
(fac <- factor(sample(c("a", "b", "c"), 20, replace=TRUE)))
Lag(fac, 2) # incorrect!
Lag(1:10, -1) # fails!

Lag <- function(x, lag=1){
    # lag a variable, padding with NAs
    # x: variable to lag
    # lag: number of observations to lag (negative = lead)
    if (!(is.vector(x) || is.factor(x))) 
        stop ("x must be a vector or a factor")
    if (!(length(lag) == 1 && floor(lag) == lag)) 
        stop ("lag must be an integer")
    length.x <- length(x)
    if (abs(lag) > length.x) stop("|lag| > length of x")
    result <- if (lag == 0) x
      else if (lag > 0) c(rep(NA, lag), x[1:(length.x - lag)])
      else {
        lag <- abs(lag)
        c(x[(lag + 1):length.x], rep(NA, lag))
      }
    if (is.factor(x)) result <- factor(levels(x)[result])
    result
}

Lag(1:10)      # works as before
Lag(1:10, 2)   # ditto
Lag(1:10, -2)  # lead 2 periods, works
Lag(1:10, 0)   # works
Lag(letters)   # works as before
Lag(fac, 2)    # lag a factor, works
Lag(1:10, 1.5) # error!
Lag(1:10, 11)  # error!

library("carData")  # for the Duncan data set
model <- lm(prestige ~ income + education, data=Duncan)
hat <- hatvalues(model)  # horizontal axis variable
rstud <- rstudent(model) # vertical axis variable
cook <- sqrt(cooks.distance(model)) # size of plotted point
plot(hat, rstud, cex=10*cook,
     xlab="Hatvalues", ylab="Studentized Residuals")
abline(h=c(-2, 0, 2), lty="dashed")
abline(v=c(2, 3)*length(coef(model))/length(rstud), 
    lty="dashed")
noteworthy <- which(abs(rstud) > 2 | 
    hat > 2*length(coef(model))/length(rstud))
text(hat[noteworthy], rstud[noteworthy], 
    names(rstud)[noteworthy])

inflPlot <- function(model) {
  hat <- hatvalues(model)
  rstud <- rstudent(model)
  cook <- sqrt(cooks.distance(model))
  plot(hat, rstud, cex=10*cook,
       xlab="Hatvalues", ylab="Studentized Residuals")
  abline(h=c(-2, 0, 2), lty="dashed")
  abline(v=c(2, 3)*length(coef(model))/length(rstud), 
      lty="dashed")
  noteworthy <- which(abs(rstud) > 2 | 
      hat > 2*length(coef(model))/length(rstud))
  text(hat[noteworthy], rstud[noteworthy], 
      names(rstud)[noteworthy])
}

inflPlot <- function(model, scale.factor=10, ...) {
    # Influence Bubble Plot
    # model:        linear or generalized linear model
    # scale factor: for circles representing Cook's Ds
    # ...:          arguments to pass to plot()
    hat <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cooks.distance(model))
    plot(hat, rstud, cex=scale.factor*cook,
        xlab="Hatvalues", ylab="Studentized Residuals", ...)
    abline(h=c(-2, 0, 2), lty="dashed")
    abline(v=c(2, 3)*length(coef(model))/length(rstud), 
        lty="dashed")
    noteworthy <- which(abs(rstud) > 2 | 
        hat > 2*length(coef(model))/length(rstud))
    labels <- names(rstud)[noteworthy]
    text(hat[noteworthy], rstud[noteworthy], labels)
    if (length(labels) == 0) invisible(NULL) else labels
}

inflPlot(model)


(A <- matrix(c(1, 2, -4, 3, 5, 0), nrow=2, ncol=3))
(B <- matrix(1:6, 2, 3))
(C <- matrix(c(2, -2, 0,  1, -1, 1,  4 ,4, -4), 3, 3, byrow=TRUE))

A + B  # addition
A - B  # subtraction
A + C  # error: A and C not of the same order!
2*A    # scalar times a matrix
-A     # negation

dim(A)
dim(C)
A %*% C  # matrix product
dim(B)
A %*% B  # fails!

(a <- rep(1, 3))
(b <- c(1, 5, 3))
C %*% a  # matrix times (column) vector
a %*% C  # (row) vector times matrix
a %*% b  # inner product of two vectors

outer(a, b)  # outer product
a %o% b      # equivalent

outer(1:3, 1:3, ">=")

t(B)  # transpose

solve(C)        # matrix inverse
solve(C) %*% C  # check

library("MASS")
fractions(solve(C))

(x <- solve(C, b))  # solution of Cx = b

C %*% x 
b  # check

X <- cbind(1, as.matrix(Duncan[ , 2:3]))  # model matrix with constant
colnames(X)[1] <- "intercept"
y <- Duncan[ , "prestige"]  # the response vector
head(X)  # first 6 rows
head(y)

head(Duncan[ , "prestige", drop=FALSE])

solve(t(X) %*% X) %*% t(X) %*% y

coef(lm(prestige ~ income + education, data=Duncan))

(R <- with(Duncan, cor(cbind(income, education)))) # correlations
eigen(R)  # eigenvalues and eigenvectors

det(R)

diag(R)           # extract diagonal
diag(R) <- NA     # set diagonal
R
(D <- diag(1:3))  # make diagonal matrix
(I3 <- diag(3))   # order-3 identity matrix

abs1 <- function(x) {
    if(x < 0) -x else x
}
abs1(-5)
abs1(5)

abs2 <- function(x) {ifelse(x < 0, -x, x)}
abs2(-3:3)

sign1 <- function(x) {
    if (x < 0) -1
        else if (x > 0) 1
            else 0
}
sign1(-5)

sign2 <- function(x){
    ifelse (x < 0, -1,
        ifelse(x > 0, 1, 0))
}
sign2(c(-5, 0, 10))

convert2meters <- function(x, 
    units=c("inches", "feet", "yards", "miles")) {
    units <- match.arg(units)
    switch(units,
        inches = x * 0.0254,
        feet = x * 0.3048,
        yards = x * 0.9144,
        miles = x * 1609.344
    )
}

convert2meters(10)  # uses first value of units ("inches") as default
convert2meters(10, units="inches")  # equivalent to the default
convert2meters(3, "feet")
convert2meters(100, "y")  # abbreviation of "yards"
convert2meters(5, "miles")

fact1 <- function(n){
    if (n <= 1) return(1)
    f <- 1  # initialize
    for (i in 1:n) {
        f <- f*i  # accumulate product
    }
    f  # return result
}
fact1(5)

fact1(5.2)  # wrong!

fact2 <- function(n) {
    if ((!is.numeric(n)) || (n != floor(n))
        || (n < 0) || (length(n) > 1))
        stop("n must be a non-negative integer")
    if (n <= 1) return(1)
    f <- 1  # initialize
    for (i in 1:n) {
        f <- f*i  # accumulate product
    }
    f  # return result
}
fact2(5)
fact2(5.2)

fact3 <- function(n){
    if ((!is.numeric(n)) || (n != floor(n))
        || (n < 0) || (length(n) > 1))
        stop("n must be a non-negative integer")
        i <- f <- 1  # initialize
        while (i <= n) {
            f <- f*i    # accumulate product
            i <- i + 1  # increment counter
        }
    f  # return result
}
fact3(5)

fact4 <- function(n) {
    if ((!is.numeric(n)) || (n != floor(n))
        || (n < 0) || (length(n) > 1))
        stop("n must be a non-negative integer")
    i <- f <- 1  # initialize
    repeat {
        f <- f*i          # accumulate product
        i <- i + 1        # increment counter
        if (i > n) break  # termination test
    }
    f  # return result
}
fact4(5)

fact5 <- function(n){
    if (n <= 1) 1          # termination condition
    else n * fact5(n - 1)  # recursive call
}
fact5(5)

trace(fact5)
fact5(5)
untrace(fact5)

head(DavisThin, 10)  # first 10 rows
dim(DavisThin)

DavisThin$thin.drive <- apply(DavisThin, 1, sum)
head(DavisThin$thin.drive, 10)

apply(DavisThin, 2, mean)  # variable (column) means

colMeans(DavisThin)  # equivalent

DavisThin$thin.drive <- NULL  # remove thin.drive
DavisThin[1, 2] <- DavisThin[2, 4] <- DavisThin[10, 3] <- NA
head(DavisThin, 10)

head(apply(DavisThin, 1, sum), 10)

head(apply(DavisThin, 1, function(x) 7 * mean(x, na.rm=TRUE)), 10)

DavisThin[1, 2:5] <- NA  # create more missing data
head(DavisThin, 10)
makeScale <- function(items) {
    if (sum(is.na(items)) >= 4) NA
    else 7 * mean(items, na.rm=TRUE)
}
head(apply(DavisThin, 1, makeScale), 10)

thin.list <- as.list(DavisThin)
str(thin.list)  # structure of the result

lapply(thin.list, mean, na.rm=TRUE)  # equivalent


sapply(thin.list, mean, na.rm=TRUE)  # simplified

(result <- integrate(dnorm, lower=-1.96, upper=1.96))

names(result)

(low <- c(-Inf, -3:3))
(high <- c(-3:3, Inf))
(P <- mapply(function(lo, hi) integrate(dnorm, lo, hi)$value,
    lo=low, hi=high))
sum(P)

pnorm(high) - pnorm(low)

Integrate <- Vectorize(
    function(fn, lower, upper) integrate(fn, lower, upper)$value,
        vectorize.args=c("lower", "upper")
)
Integrate(dnorm, lower=low, upper=high)

library("car")
summary(Guyer)

with(Guyer, tapply(cooperation,
    list(Condition=condition, Sex=sex), mean))

Tapply(cooperation ~ condition + sex, mean, data=Guyer)

time1 <- function(n) {  # inefficient!
    a <- NULL
    for (i in 1:n) a <- c(a, i^2)
    a
}
system.time(time1(1e5))

time2 <- function(n) {  # better
    a <- numeric(n)
    for (i in 1:n) a[i] <- i^2
    a
}

system.time(time2(1e7))

time3 <- function(n) {  # best
    a <- (1:n)^2
    a
}
system.time(time3(1e7))

time4 <- function(n) {  # (slightly) inefficient!
a <- numeric(n)
for (i in 1:n) a[i] <- 2*pi*sin(i)
a
}
system.time(time4(1e6))
time5 <- function(n) {  # better
a <- numeric(n)
for (i in 1:n) a[i] <- sin(i)
2*pi*a  # multiplication moved outside of the loop
}
system.time(time5(1e6))

time6 <- function(n) {  # best (fully vectorized)
    2*pi*(1:n)
}
system.time(time6(1e6))

set.seed(12345)  # for reproducibility
system.time({
    matrices <- vector(mode="list", length=10000)  # preallocate!
    for (i in 1:10000) matrices[[i]] <- matrix(rnorm(10000), 100, 100)
})

system.time({
    S1 <- matrix(0, 100, 100)         # initialize
    for (M in matrices) S1 <- S1 + M  # accumulate sum
})

system.time(S2 <- apply(array(unlist(matrices),
    dim=c(100, 100, 10000)), 1:2, sum)
)

all.equal(S1, S2)

system.time(S3 <- rowSums(array(unlist(matrices),
    dim=c(100, 100, 10000)), dims=2)
)
all.equal(S1, S3)

zipmod <- function(X, y, Z=X, intercept.X=TRUE, 
    intercept.Z=TRUE, ...) {
    # ZIP model
    # X: model matrix for Poisson model
    # y: response vector
    # Z: model model for logit model
    # intercept.X: add column of 1s for intercept to X
    # intercept.Z: add column of 1s for intercept to Z
    # ...: arguments to be passed to optim()
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
    par.names <- c(paste0("beta.", colnames(X)), paste0("gamma.",
        colnames(Z)))
    names(beta.gamma) <- par.names
    rownames(vcov) <- colnames(vcov) <- par.names
    list(coefficients=beta.gamma,  vcov=vcov,
         deviance=2*result$value, converged=result$convergence == 0)
}

X <- model.matrix(~ log2(assets) + nation + sector, 
    data=Ornstein)[, -1]  # removing the constant column 1
Z <- model.matrix(~ log2(assets) + nation, data=Ornstein)[, -1]
head(Z)  # X is similar, but with additional columns for sector

ornstein.zip <- zipmod(X, Ornstein$interlocks, Z)
ornstein.zip$coefficients
sqrt(diag(ornstein.zip$vcov)) # standard errors
ornstein.zip$converged

beta <- ornstein.zip$coefficients[1:14]
gamma <- ornstein.zip$coefficients[-(1:14)]
x.beta.fixed <- sum(beta[c(1, 3:14)]*c(1, colMeans(X[, 2:13])))
x.gamma.fixed <- sum(gamma[c(1, 3:5)]*c(1, colMeans(Z[, 2:4])))
assets <- with(Ornstein, seq(min(assets), max(assets), length=100))
pi <- 1/(1 + exp(-(log2(assets)*gamma[2] + x.gamma.fixed)))
e.interlocks <- (1 - pi)*exp(log2(assets)*beta[2] + x.beta.fixed)
plot(assets, e.interlocks, xlab="assets ($millions)",
     ylab="estimated expected interlocks", type="l", lwd=2)

set.seed(12345)

head(Salaries)
nrow(Salaries)

ftable(x1 <- xtabs(~ discipline + rank + sex, data=Salaries))

round(100*ftable(prop.table(x1, margin=c(1, 2))), 1) 
    # % Male and Female

library("lattice")
xyplot(salary ~ yrs.since.phd | discipline:rank, groups=sex,
    data=Salaries, type=c("g", "p", "r"), 
    key=simpleKey(text=c("Female", "Male"), points=TRUE, lines=TRUE))

bwplot(salary ~ discipline:sex | rank, data=Salaries,
    scales=list(rot=90), layout=c(3, 1))

fselector <- Salaries$sex == "Female" # TRUE for females
salmod <- lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
    subset=!fselector) # regression for males
S(salmod) # S() is from the car package
femalePreds <- predict(salmod, newdata=Salaries[fselector, ])
(meanDiff <- mean(Salaries$salary[fselector] - femalePreds))

set.seed(8141976)

fnumber <- sum(fselector) # number of females
n <- length(fselector) # number of observations
B <- 999 # number of replications
simDiff <- numeric(B) # initialize vector with B entries
for (j in 1:B){
    # random sample of nominated 'females':
    sel <- sample(1:n, fnumber)
    # refit regression model to simulated 'males':
    m2 <- update(salmod, subset=-sel)
    # simulated salary difference in means:
    simDiff[j] <- mean(Salaries$salary[sel]
        - predict(m2, newdata=Salaries[sel, ]))
}

(frac <- round(sum(meanDiff > simDiff)/(1 + B), 3))
hist(simDiff,
    main=paste(
        "Histogram of Simulated Mean Differences\np-value =",
        frac),
    xlab="Dollars")
abline(v=meanDiff, lty="dashed", lwd=2) # observed mean difference

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

summary
print

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

X <- model.matrix(~ log2(assets) + nation + sector, 
    data=Ornstein)[, -1]  # removing the constant column 1
Z <- model.matrix(~ log2(assets) + nation, data=Ornstein)[, -1]
ornstein.zip.2 <- zipmod(X, Ornstein$interlocks, Z)
class(ornstein.zip.2)
str(ornstein.zip.2, strict.width="cut") 
    # to avoid printing the entire object

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

ornstein.zip.2  # invokes the print() method
summary(ornstein.zip.2)  # invokes the summary() method

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

library("MASS") # for eqscplot()
(ornstein.zip.3 <- zipmod(interlocks ~ log(assets) + nation + sector, 
    ~ log(assets) + nation, data=Ornstein))
eqscplot(Ornstein$interlocks, fitted(ornstein.zip.3))
abline(0, 1, lwd=2)  # line y-hat = y

