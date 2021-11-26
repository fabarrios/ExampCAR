##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 5                  ##
##-----------------------------------------------------##

library("car")
summary(Transact)

S(trans.1 <- lm(time ~ t1 + t2, Transact), brief=TRUE)

coef(trans.1)

(V <- vcov(trans.1))

sqrt(diag(V))

round(cov2cor(V), 3)

round(with(Transact, cor(t1, t2)), 3)

hccm(trans.1)

S(trans.1, vcov.=hccm)

args(Boot)

set.seed(23435) # for reproducibility
betahat.boot <- Boot(trans.1)

set.seed(12345)

brief(betahat.boot$t)

hist(betahat.boot, parm=c("t1", "t2"))

summary(betahat.boot)

S(trans.1, vcov.=vcov(betahat.boot))

davis.mod <- lm(weight ~ repwt, data=Davis, subset=-12)
brief(davis.mod)

deltaMethod(davis.mod, "(Intercept)/(1 - repwt)")

deltaMethod(trans.1, "t1/t2")

boots <- cbind(betahat.boot$t, 
    "t1/t2"= betahat.boot$t[, "t1"]/betahat.boot$t[, "t2"])
nrow(boots)
head(boots)
sd(boots[, "t1/t2"]) # boostrap SE

deltaMethod(trans.1, "t1/t2", vcov.=hccm)

deltaMethod(trans.1, "t1 + t2")

Confint(trans.1, vcov.=hccm)
confint(trans.1)

Confint(trans.1, vcov.=vcov(betahat.boot))

duncan.mod <- lm(prestige ~ income + education, data=Duncan)
(ci <- Confint(duncan.mod))

confidenceEllipse(duncan.mod, levels=c(0.85, 0.95), main="(a)")
abline(v=ci[2, 2:3], lty=2, lwd=2) # marginal CI for education
abline(h=ci[3, 2:3], lty=2, lwd=2) # marginal CI for income

with(Duncan, 
     dataEllipse(income, education, levels=c(0.5, 0.75, 0.9, 0.95), 
        id=list(method="mahal", n=3, labels=rownames(Duncan)),
        main="(b)")
     )

tval <- (coef(davis.mod)[2] - 1)/sqrt(vcov(davis.mod)[2, 2])
pval <- 2*pt(abs(tval), df.residual(davis.mod), lower.tail=FALSE)
c(t=tval, p=pval)

prestige.mod.1 <- lm(prestige ~ education + log2(income) + type,
    data=na.omit(Prestige)) # full model
prestige.mod.0 <- update(prestige.mod.1, . ~ 1) # intercept only
anova(prestige.mod.0, prestige.mod.1) # compare models

prestige.mod.0inc <- update(prestige.mod.1, . ~ . - log2(income))
anova(prestige.mod.0inc, prestige.mod.1) # compare models

anova(prestige.mod.1)

Anova(prestige.mod.1)

prestige.mod.3 <- update(prestige.mod.1,
    . ~ . + log2(income):type + education:type)
Anova(prestige.mod.3)

Anova(trans.1, vcov.=hccm)

save.par <- par(mfrow=c(1, 2), cex.axis=1, cex=1.25, cex.lab=1, cex.main=1)

plot(c(0,1), c(0,1), type="n", axes=FALSE, xlab="", ylab="", 
     main="(a) No Interaction")
lines(c(0,0), c(0,1))
lines(c(0,1), c(0,0))
axis(1, at=c(0.2, 0.5, 0.8), pos=0, labels=c(expression(A[1]), expression(A[2]), expression(A[3]))) 
lines(c(0.2, 0.5, 0.8), c(0.65, 0.85, 0.75), pch=1, cex=2, type="b", lwd=2)
lines(c(0.2, 0.5, 0.8), c(0.15, 0.35, 0.25), pch=16, cex=2, type="b", lwd=2)
lines(c(0.2, 0.5, 0.8), c(0.4, 0.6, 0.5), pch=16, cex=2, type="b", col="darkgray", lty=2, lwd=2)
text(x=c(-0.075, 0.9, 0.9,  0.2, 0.5, 0.8,  0.2, 0.5, 0.8, c(0.2 - 0.10, 0.5, 0.8 + 0.10)), 
     y=c(1, 0.75, 0.25, c(0.65, 0.85, 0.75) + 0.10, c(0.15, 0.35, 0.25) - 0.10, c(0.4, 0.6 - 0.10, 0.5)), 
     c(expression(y),
       expression(B[1]), expression(B[2]),
       expression(mu[11]), expression(mu[21]), expression(mu[31]), 
       expression(mu[12]), expression(mu[22]), expression(mu[32]),
       expression(mu["1" %.% ""]), expression(mu["2" %.% ""]), expression(mu["3" %.% ""])), xpd=TRUE, cex=1.25)

plot(c(0,1), c(0,1), type="n", axes=FALSE, xlab="", ylab="", 
     main="(b) Interaction")
lines(c(0,0), c(0,1))
lines(c(0,1), c(0,0))
axis(1, at=c(0.2, 0.5, 0.8), pos=0, labels=c(expression(A[1]), expression(A[2]), expression(A[3]))) 
lines(c(0.2, 0.5, 0.8), c(0.5, 0.85, 0.6), pch=1, cex=2, type="b", lwd=2)
lines(c(0.2, 0.5, 0.8), c(0.3, 0.15, 0.25), pch=16, cex=2, type="b", lwd=2)
lines(c(0.2, 0.5, 0.8), c(0.4, 0.5, 0.425), pch=16, cex=2, type="b", col="darkgray", lty=2, lwd=2)
text(x=c(-0.075, 0.9, 0.9,  0.2, 0.5, 0.8,  0.2, 0.5, 0.8,  c(0.2 - 0.10, 0.5, 0.8 + 0.10)), 
     y=c(1, 0.6, 0.25, c(0.5, 0.85, 0.6) + 0.10, c(0.3, 0.15, 0.25) - 0.10, c(0.4, 0.5 - 0.10, 0.425)),
     c(expression(y),
       expression(B[1]), expression(B[2]),
       expression(mu[11]), expression(mu[21]), expression(mu[31]), 
       expression(mu[12]), expression(mu[22]), expression(mu[32]),
       expression(mu["1" %.% ""]), expression(mu["2" %.% ""]), expression(mu["3" %.% ""])), xpd=TRUE, cex=1.25)
par(save.par)

GSS16 <- na.omit(subset(GSSvocab, year=="2016", 
    select=c("vocab", "ageGroup", "nativeBorn")))
mod.vocab <- lm(vocab ~ nativeBorn*ageGroup, data=GSS16)
Anova(mod.vocab)

contrasts(GSS16$ageGroup) <-
      contrasts(GSS16$nativeBorn) <- "contr.sum"
contrasts(GSS16$ageGroup)
contrasts(GSS16$nativeBorn)
mod.vocab.3 <- update(mod.vocab)
Anova(mod.vocab.3, type="III")

brief(trans.1)
linearHypothesis(trans.1, c(0, 1, -1))

linearHypothesis(trans.1, c(0, 1, -1), vcov=hccm)

diag(2)  # order-2 identity matrix
linearHypothesis(davis.mod, diag(2), c(0, 1))
