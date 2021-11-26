##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 6                  ##
##-----------------------------------------------------##

Probit <- binomial(link=probit)
Logit <- binomial(link=logit)
Cloglog <- binomial(link=cloglog)
range <- seq(-10,10,length=1000)
plot(range,Logit$linkinv(range),type="l", xlim=c(-5,5), lty=1,
       xlab=expression(eta(x)), ylab=expression(mu(x)))
lines(sqrt(pi^2/3)*range, Probit$linkinv(range), lty=2)
lines(range,Cloglog$linkinv(range), lty=4, lwd=2)
legend("topleft",c("logit", "probit", "cloglog"), lty=c(1,2,4),
       lwd=c(1,1,2), inset=0.02)

library("car")
summary(Mroz)

mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
   family=binomial, data=Mroz)

S(mroz.mod)

brief(Cowles)
sum(Cowles$volunteer == "yes") # number yes

cowles.mod <- glm(volunteer ~ sex + neuroticism*extraversion,
    data=Cowles, family=binomial)
brief(cowles.mod, pvalues=TRUE)

library("effects")
plot(predictorEffects(mroz.mod))

plot(predictorEffects(cowles.mod, ~ neuroticism + extraversion,
    xlevels=list(neuroticism=seq(0, 24, by=8), 
        extraversion=seq(0, 24, by=8))), 
    lines=list(multiline=TRUE))

mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")

brief(cowles.mod.0 <- update(cowles.mod, 
    . ~ . - neuroticism:extraversion))
anova(cowles.mod.0, cowles.mod, test="Chisq")

Anova(mroz.mod)

Anova(cowles.mod)

linearHypothesis(mroz.mod, c("k5", "k618"))

linearHypothesis(mroz.mod, "k5 = k618")

opts <- options(digits=5)

head(predict(mroz.mod)) # first few values

head(predict(mroz.mod, type="response"))

options(opts)

predict.data <- data.frame(sex=c("female", "male"), 
    neuroticism=rep(12, 2), extraversion=rep(12, 2))
predict.data$p.volunteer <- predict(cowles.mod, 
    newdata=predict.data, type="response")
predict.data

Campbell <- data.frame(
    closeness = factor(rep(c("one.sided", "close"), c(3, 3)),
        levels=c("one.sided", "close")),
    preference = factor(rep(c("weak", "medium", "strong"), 2),
        levels=c("weak", "medium", "strong")),
    voted = c(91, 121, 64, 214, 284, 201),
    did.not.vote = c(39, 49, 24, 87, 76, 25)
)
Campbell

campbell.mod <- glm(cbind(voted, did.not.vote) ~
    closeness*preference, family=binomial, data=Campbell)

predict(campbell.mod)
residuals(campbell.mod)

plot(predictorEffects(campbell.mod, ~ preference),  
     main="", confint=list(style="bars"), lines=list(multiline=TRUE),
     xlab="Intensity of Preference", ylab="Probability of Voting",
     lattice=list(key.args = list(x =0.1, y = 0.95, corner=c(0, 1))))

library(emmeans)
emmeans(campbell.mod, pairwise ~ closeness | preference)$contrasts

campbell.mod.2 <- update(campbell.mod,
    . ~ . - closeness:preference) # no interactions
anova(campbell.mod.2, campbell.mod, test="Chisq")
Anova(campbell.mod)

deviance(campbell.mod.2)
df.residual(campbell.mod.2)
pchisq(deviance(campbell.mod.2), 2, lower.tail=FALSE)

set.seed(123456)

(Campbell.temp <- reshape(Campbell[, 1:4], 
    varying=list(response=3:4), direction="long", 
    timevar="turnout", v.names="count")) 

Campbell.long <- NULL
for (i in 1:nrow(Campbell.temp)){
    rows <- Campbell.temp[rep(i, Campbell.temp$count[i]), 1:3]
    Campbell.long <- rbind(Campbell.long, rows)
}

Campbell.long$turnout <- ifelse(Campbell.long$turnout == 2, 0, 1)

ftable(xtabs(~ closeness + preference + turnout, data=Campbell.long))

campbell.mod.long <- glm(turnout ~ closeness*preference,
    family=binomial, data=Campbell.long)
compareCoefs(campbell.mod, campbell.mod.long)
Anova(campbell.mod.long)

round(deviance(campbell.mod), 10) # binomial logit model
df.residual(campbell.mod)
deviance(campbell.mod.long)      # binary logit model
df.residual(campbell.mod.long)

brief(Ornstein)

tab <- xtabs(~interlocks, data=Ornstein)

x <- as.numeric(names(tab)) # distinct values of interlocks
plot(x, tab, type="h", xlab="Number of Interlocks", ylab="Frequency")
points(x, tab, pch=16)

mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
   family=poisson, data=Ornstein)
S(mod.ornstein)
Anova(mod.ornstein)

plot(predictorEffects(mod.ornstein, xlevels=list(assets=100)), 
    axes=list(x=list(rotate=40)))

brief(AMSsurvey[, 1:4])  # a few rows, first 4 columns

(tab.sex.citizen <- xtabs(count ~ sex + citizen, data=AMSsurvey))

save.digits <- options(digits=7)

chisq.test(tab.sex.citizen, correct=FALSE) # suppress Yates correction

options(save.digits)
remove(save.digits)

100*prop.table(tab.sex.citizen, margin=2)

(AMS2 <- as.data.frame(tab.sex.citizen))

phd.mod.indep <- glm(Freq ~ sex + citizen, family=poisson, data=AMS2)
  # estimated expected cell counts assuming independence:
predict(phd.mod.indep, type="response")
deviance(phd.mod.indep) # L.R. test statistic for independence
df.residual(phd.mod.indep)

pchisq(deviance(phd.mod.indep), df=df.residual(phd.mod.indep), 
    lower.tail=FALSE)

sum(residuals(phd.mod.indep, type="pearson")^2)

phd.mod.all <- glm(count ~ type*sex*citizen, # saturated model
    family=poisson, data=AMSsurvey)
Anova(phd.mod.all)

phd.mod.1 <- update(phd.mod.all,
    . ~ . - sex:citizen - type:sex:citizen)
deviance(phd.mod.1)
df.residual(phd.mod.1)

pchisq(deviance(phd.mod.1), df.residual(phd.mod.1), lower.tail=FALSE)

plot(predictorEffects(phd.mod.1, ~ sex + citizen), 
     lines=list(multiline=TRUE),
     lattice=list(key.args=list(columns=2)),
     confint=list(style="bars"))

Campbell.temp$turnout <- factor(Campbell.temp$turnout, 
    labels=c("voted", "did.not.vote"))
Campbell.temp

mod.loglin <- glm(count ~ closeness*preference*turnout,
   family=poisson, data=Campbell.temp)
Anova(mod.loglin)

summary(Womenlf)
nrow(Womenlf)

library(nnet)
Womenlf$partic <- factor(Womenlf$partic,
    levels=c("not.work", "parttime", "fulltime"))
mod.multinom <- multinom(partic ~ hincome*children + region,
    data=Womenlf)

Anova(mod.multinom)

mod.multinom.1 <- update(mod.multinom, . ~ . - region - hincome:children)

S(mod.multinom.1)

plot(predictorEffects(mod.multinom.1))

plot(Effect(c("hincome", "children"), mod.multinom.1))

plot(Effect(c("hincome", "children"), mod.multinom.1, xlevels=50),
     axes=list(y=list(style="stacked")),
     lines=list(col=gray(c(0.2, 0.5, 0.8))))

library(MASS) 
mod.polr <- polr(partic ~ hincome + children, data=Womenlf)
S(mod.polr)

plot(Effect(c("hincome", "children"), mod.polr))

plot(Effect(c("hincome", "children"), mod.polr), 
    axes=list(y=list(style="stacked")),
    lines=list(col=gray(c(.2, .5, .8))))

plot(Effect(c("hincome", "children"), mod.polr, latent=TRUE))

AIC(mod.multinom.1)
AIC(mod.polr)

poTest(mod.polr)

x <- seq(0.1, 4, by=.02)
d.5 <- dgamma(x, shape=.5, scale=1/.5)
d1 <- dgamma(x, shape=1, scale=1/1)
d2 <- dgamma(x, shape=2, scale=1/2)
d5 <- dgamma(x, shape=5, scale=1/5)
plot(rep(x,4), c(d.5, d1, d2, d5), type='n',
 xlab="y", ylab="p(y)")
lines(x, d.5, lty=1, lwd=2)
lines(x, d1, lty=1, lwd=2)
lines(x, d2, lty=1, lwd=2)
lines(x, d5, lty=1, lwd=2)
text (c(0.1,0.2, 0.8, 1.0), c( 1.1, 0.85, 0.65, 0.9), pos=4,
labels=c(expression(alpha == 0.5),
    expression(alpha == 1),expression(alpha == 2),expression(alpha == 5)))
abline(h=0, lty=2)

trans.gamma <- glm(time ~ t1 + t2, family=Gamma(link=identity),
    data=Transact)
S(trans.gamma)

gamma.shape(trans.gamma)

set.seed(12134)

deltaMethod(trans.gamma, "t1/t2")
boot.trans <- bootCase(trans.gamma)
ratio <- boot.trans[, "t1"]/boot.trans[, "t2"]
c("t1/t2"=as.vector(coef(trans.gamma)[2]/coef(trans.gamma)[3]), 
    bootSE=sd(ratio), quantile(ratio, c(0.025, 0.975)))

(phihat <- sum(residuals(mod.ornstein, type="pearson")^2)/
         df.residual(mod.ornstein))

S(mod.ornstein, dispersion=phihat, brief=TRUE)

Anova(mod.ornstein, test.statistic="F")


mod.ornstein.nb <- update(mod.ornstein, family=negative.binomial(1.5))

thetas <- seq(0.5, 2.5, by=0.5)
aics <- rep(0, 5) # allocate vector
for (i in seq(along=thetas)) {
    aics[i] <- AIC(update(mod.ornstein.nb,
         family=negative.binomial(thetas[i])))
}
names(aics) <- thetas
aics

brief(mod.ornstein.nb)

summary(glm.nb(interlocks ~ log2(assets) + nation + sector,
    data=Ornstein))
