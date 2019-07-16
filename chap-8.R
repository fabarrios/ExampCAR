r##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 8                  ##
##-----------------------------------------------------##

library("car")
Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))
prestige.mod.2 <- lm(prestige ~ education + income + type,
    data=Prestige)
brief(prestige.mod.2)

residualPlots(prestige.mod.2)

marginalModelPlots(prestige.mod.2)

avPlots(prestige.mod.2, id=list(n=2, cex=0.6))

mcPlots(prestige.mod.2, ~ education, overlaid=FALSE)

mod.duncan <- lm(prestige ~ income + education, data=Duncan)

set.seed(12345)

qqPlot(mod.duncan, id=list(n=3))

outlierTest(mod.duncan)

influenceIndexPlot(mod.duncan, id=list(n=3))

mod.duncan.2 <- update(mod.duncan,
    subset= rownames(Duncan) != "minister")
compareCoefs(mod.duncan, mod.duncan.2)

influencePlot(mod.duncan, id=list(n=3))

dfbs.duncan <- dfbetas(mod.duncan)
head(dfbs.duncan)  # first few rows

plot(dfbs.duncan[ ,c("income", "education")])  # for b1 and b2
showLabels(dfbs.duncan[ , "income"], dfbs.duncan[ , "education"],
  labels=rownames(Duncan), method=c(6, 16, 27))

avPlots(mod.duncan, id=list(n=3, method="mahal"))

mod.duncan.3 <- update(mod.duncan,
    subset = - whichNames(c("minister", "conductor"), Duncan))
compareCoefs(mod.duncan, mod.duncan.2, mod.duncan.3, se=FALSE)

brief(wool.mod <- lm(cycles ~ len + amp + load, data=Wool))

summary(p1 <- powerTransform(wool.mod))

mod.ornstein <- lm(interlocks  ~ log(assets) + nation + sector,
    data=Ornstein)

summary(p2 <- powerTransform(mod.ornstein, family="bcnPower"))

Ornstein$interlocks.tran <-
  bcnPower(Ornstein$interlocks, lambda=1/3, gamma=0.1)
mod.ornstein.2 <- update(mod.ornstein, interlocks.tran ~ .)

prestige.mod.3 <- lm(prestige ~ income + education + women,
    data=Prestige)
crPlots(prestige.mod.3, order=2)

prestige.mod.4 <- update(prestige.mod.3, ~ . + log2(income) - income)

prestige.mod.5 <- update(prestige.mod.4,
    . ~ . - women + poly(women, 2))
brief(prestige.mod.5, pvalues=TRUE)

library("effects")
plot(Effect("income", prestige.mod.2, partial.residuals=TRUE))

plot(Effect(c("income", "type"), prestige.mod.2,
        partial.residuals=TRUE),
    partial.residuals=list(span=0.9),
    lattice=list(layout=c(3, 1)))

prestige.mod.6 <- update(prestige.mod.2,
    . ~  income*type + education, data=Prestige)
Anova(prestige.mod.6)
plot(Effect(c("income", "type"), prestige.mod.6,
        partial.residuals=TRUE),
    partial.residuals=list(span=0.9),
    lattice=list(layout=c(3, 1)))

mod.transact <- lm(time ~ t1 + t2, data=Transact)
brief(mod.transact)
residualPlots(mod.transact, tests=FALSE, layout=c(1, 3))

(test.t1 <- ncvTest(mod.transact, ~ t1))
(test.t2 <- ncvTest(mod.transact, ~ t2))
(test.t1t2 <- ncvTest(mod.transact, ~ t1 + t2))
ncvTest(mod.transact)

(stat <- test.t1t2$ChiSquare - test.t2$ChiSquare)
(df <- test.t1t2$Df - test.t2$Df)
pchisq(stat, df, lower.tail=FALSE)

mod.working <- glm(partic != "not.work" ~ hincome + children,
     family=binomial, data=Womenlf)
brief(mod.working)

residualPlots(mod.working, layout=c(1, 3))

influenceIndexPlot(mod.working, vars=c("Cook", "hat"), id=list(n=3))

compareCoefs(mod.working, update(mod.working, subset=-c(76, 77)))

mod.ornstein.qp <- glm(interlocks ~ assets + nation + sector,
    family=quasipoisson, data=Ornstein)
crPlots(mod.ornstein.qp, ~ assets)

mod.ornstein.qp.2 <- update(mod.ornstein.qp,
    . ~ log2(assets) + nation + sector)
crPlots(mod.ornstein.qp.2, ~ log2(assets))

mod.mroz <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
    family=binomial, data=Mroz)
crPlots(mod.mroz, "lwg", pch=as.numeric(Mroz$lfp))
legend("bottomleft",c("Estimated lwg", "Observed lwg"),
    pch=1:2, inset=0.01)

cowles.mod <- glm(volunteer ~ sex + neuroticism*extraversion,
    data=Cowles, family=binomial)

plot(predictorEffects(cowles.mod, ~ neuroticism + extraversion,
        partial.residuals=TRUE),
    partial.residuals=list(span=3/4), lattice=list(layout=c(1, 4)))

library("nlme")
Blackmore$tran.exercise <- bcnPower(Blackmore$exercise,
            lambda=0.25, gamma=0.1)
blackmore.mod.6.lme<- lme(tran.exercise ~ I(age - 8)*group,
    random = ~ 1 | subject,
    correlation = corCAR1(form = ~ I(age - 8) | subject),
    data=Blackmore)
S(blackmore.mod.6.lme)

plot(Effect(c("age", "group"), blackmore.mod.6.lme, 
    partial.residuals=TRUE))

system.time(inf.blackmore <- 
    influence(blackmore.mod.6.lme, groups="subject"))


influenceIndexPlot(inf.blackmore)

influenceIndexPlot(inf.blackmore, var="var.cov.comps")

summary(Ericksen)

mod.census <- lm(undercount ~ ., data=Ericksen)
brief(mod.census, pvalues=TRUE)

vif(mod.census)

vif(mod.ornstein)

vif(mod.ornstein.qp.2)
