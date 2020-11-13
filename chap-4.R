##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 4                  ##
##-----------------------------------------------------##

library("car") 
summary(Davis)

davis.mod <- lm(weight ~ repwt, data=Davis)

davis.mod

S(davis.mod)

Confint(davis.mod)

plot(weight ~ repwt, data=Davis)
abline(0, 1, lty="dashed", lwd=2)
abline(davis.mod, lwd=2)
legend("bottomright", c("Unbiased Reporting", "Least Squares"), 
       lty=c("dashed", "solid"), lwd=2, inset=0.02)
with(Davis, showLabels(repwt, weight, n=2, method="mahal"))

davis.mod.2 <- update(davis.mod, subset=-12)
S(davis.mod.2)

compareCoefs(davis.mod, davis.mod.2)
Confint(davis.mod.2)

summary(Prestige)

prestige.mod <- lm(prestige ~ education + log2(income) + women,
    data=Prestige)
S(prestige.mod)

pc <- coef(prestige.mod)
pc2 <- round(pc, 2)
pc4 <- round(pc, 4)

brief(Transact)

S(trans.1 <- lm(time ~ t1 + t2, data=Transact))

Transact.s <- scale(Transact)
round(var(Transact.s), digits=3)
round(colMeans(Transact.s), digits=10)

trans.s <- update(trans.1, data=as.data.frame(Transact.s))
S(trans.s, brief=TRUE)

sapply(Transact, sd)

meanInc <- round(mean(Prestige$income), 0)
meanWo <-  round(mean(Prestige$women), 2)
int <- round(pc[1] + pc[3] * log2(meanInc) + pc[4] * meanWo, 2)

library(effects)

plot(predictorEffects(prestige.mod))

summary(SLID)
nrow(SLID)

scatterplot(log(wages) ~ age, data=SLID, 
    subset = age >= 18 & age <= 65, pch=".")

brief(mod.quad.1 <- lm(log(wages) ~ poly(age, 2, raw=TRUE), 
                  data=SLID, subset = age >= 18 & age <= 65))
brief(mod.quad.2 <- update(mod.quad.1, ~ age + I(age^2)))
brief(mod.quad.3 <- update(mod.quad.1, ~ poly(age, 2)))

plot(predictorEffects(mod.quad.1, residuals=TRUE), 
     partial.residuals=list(cex=0.35, col=gray(0.5), lty=2))

x <- seq(0, 10, length=500)
y <- sin(x+1) + x/5

set.seed(12345)
xx <- runif(50, 0, 10)
yy <- sin(xx+1) + xx/5  + 0.5*rnorm(50)
k <- quantile(xx, c(1/3, 2/3))
group <- cut(xx, breaks=c(-Inf, k, Inf))

par(mfrow=c(2,2))

plot(xx, yy, xlab="u", ylab="y", main="(a)")
mods <- by(data.frame(xx=xx, yy=yy, group=group), group,
           function(df) lm(yy ~ poly(xx, 3), data=df))
lines(x, y, lty=2, lwd=2)
abline(v=k, lty=3, lwd=2)
text(k, 0.2 + rep(par("usr")[4], 2), c(expression(k[1]), expression(k[2])), xpd=TRUE)
kk <- c(0, k, 10)
for (i in 1:3){
    x1 <- x[x >= kk[i] & x < kk[i+1]]
    y1 <- predict(mods[[i]], list(xx=x1))
    lines(x1, y1, lwd=2)
}

xx1 <- xx
xx2 <- (xx - k[1]) * (xx > k[1])
xx3 <- (xx - k[2]) * (xx > k[2])
mod <- lm(yy ~ xx1 + I(xx1^2) + I(xx1^3)
          + xx2 + I(xx2^2) + I(xx2^3)
          + xx3 + I(xx3^2) + I(xx3^3))
plot(xx, yy, xlab="u", ylab="y", main="(b)")
lines(x, y, lty=2, lwd=2)
abline(v=k, lty=3, lwd=2)
text(k, 0.2 + rep(par("usr")[4], 2), c(expression(k[1]), expression(k[2])), xpd=TRUE)
x1 <- x
x2 <- (x - k[1]) * (x > k[1])
x3 <- (x - k[2]) * (x > k[2])
lines(x, predict(mod, list(xx1=x1, xx2=x2, xx3=x3)), lwd=2)

mod <- lm(yy ~ xx1 + I(xx1^2) + I(xx1^3)
          + I(xx2^2) + I(xx2^3)
          + I(xx3^2) + I(xx3^3))
plot(xx, yy, xlab="u", ylab="y", main="(c)")
lines(x, y, lty=2, lwd=2)
abline(v=k, lty=3, lwd=2)
text(k, 0.2 + rep(par("usr")[4], 2), c(expression(k[1]), expression(k[2])), xpd=TRUE)
x1 <- x
x2 <- (x - k[1]) * (x > k[1])
x3 <- (x - k[2]) * (x > k[2])
lines(x, predict(mod, list(xx1=x1, xx2=x2, xx3=x3)), lwd=2)

plot(xx, yy, xlab="u", ylab="y", main="(d)")
lines(x, y, lty=2, lwd=2)
abline(v=k, lty=3, lwd=2)
text(k, 0.2 + rep(par("usr")[4], 2), c(expression(k[1]), expression(k[2])), xpd=TRUE)
mod <- lm(yy ~ xx1 + I(xx1^2) + I(xx1^3)
          + I(xx2^3)
          + I(xx3^3))
x1 <- x
x2 <- (x - k[1]) * (x > k[1])
x3 <- (x - k[2]) * (x > k[2])
lines(x, predict(mod, list(xx1=x1, xx2=x2, xx3=x3)), lwd=2)

library("splines")
mod.spline <- update(mod.quad.1, ~ ns(education, df=5))
plot(predictorEffects(mod.spline, residuals=TRUE), 
     partial=list(cex=0.35, col=gray(0.5), lty=2))

Prestige$type
class(Prestige$type)

Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))
levels(Prestige$type)

x <- c(2, 2, 4, 4, 4, 8, 8, 8)

(fact.x <- factor(x, labels=c(paste(c(2, 4, 8), "hours", sep="-"))))

as.factor(x)

as.numeric(fact.x)

c(2, 4, 8)[as.numeric(fact.x)]

as.character(fact.x)

(z <- factor(rep(c("a", "b", "c", "d"), c(3, 2, 4, 1))))

model.matrix(~ z)

contrasts(z)

summary(Baumann[, c(1, 6)]) 

xtabs(~ group, data=Baumann)

Tapply(post.test.3 ~ group, mean, data=Baumann)
Tapply(post.test.3 ~ group, sd, data=Baumann)

plot(post.test.3 ~ group, data=Baumann, xlab="Group",
    ylab="Reading Score")

S(baum.mod.1 <- lm(post.test.3 ~ group, data=Baumann))

library("emmeans")
emmeans(baum.mod.1, pairwise ~ group)

emmeans(baum.mod.1, trt.vs.ctrl ~ group)

prestige.mod.1 <- update(prestige.mod, ~ . - women + type)
S(prestige.mod.1)

bhat <- coef(prestige.mod.1)
int1 <- round(bhat[1], 2)
int2 <- round(bhat[1] + bhat[4], 2)
int3 <- round(bhat[1] + bhat[5], 2)
sl1 <- round(bhat[2], 2)
sl2 <- round(bhat[3], 2)
bhat <- round(bhat, 2)

plot(predictorEffects(prestige.mod.1, 
      predictors = ~ type + education + income))

emmeans(prestige.mod.1, pairwise ~ type)$contrasts

set.seed(12345)

some(model.matrix(~ type + education + education:type, 
    data=Prestige), 8)

prestige.mod.2 <- update(prestige.mod.1,
    . ~ . + education:type + log2(income):type)
S(prestige.mod.2)

plot(predictorEffects(prestige.mod.2, ~ education), 
     lattice=list(key.args=list(cex=.8, cex.title=.8)),
     lines=list(multiline=TRUE))

plot(predictorEffects(prestige.mod.2, ~ education))


plot(predictorEffects(prestige.mod.2, ~ income),
     lines=list(multiline=TRUE))

plot(predictorEffects(prestige.mod.2, ~ type,
    xlevels=list(income=c(1000, 5000, 8000, 12000),
        education=c(8, 10, 12, 14))), 
    lines=list(multiline=TRUE), confint=list(style="bars"))

mod.slid.int <- lm(log(wages) ~ 
    sex*(ns(education, df=5) + poly(age, degree=2)), data=SLID,
    subset = age >= 18 & age <= 65)
plot(predictorEffects(mod.slid.int, 
        predictors = ~ education + age,
        transformation=list(link=log, inverse=exp)),
     lines=list(multiline=TRUE), confint=list(style="bands"),
     axes=list(y=list(lab="Hourly Wage Rate")))


GSS16 <- na.omit(subset(GSSvocab, year=="2016", 
    select=c("vocab", "ageGroup", "nativeBorn")))

xtabs(~ nativeBorn + ageGroup, data=GSS16)

Tapply(vocab ~ nativeBorn + ageGroup, mean, data=GSS16)
Tapply(vocab ~ nativeBorn + ageGroup, sd, data=GSS16)

require(lattice)
histogram(~ vocab | ageGroup + nativeBorn, data=GSS16,
    breaks=seq(-.5, 10.5, length=12))

mod.vocab.1 <- lm(vocab ~ nativeBorn*ageGroup, data=GSS16)

data.frame(coef.name=paste("b", 0:9, sep=""), 
    estimate=coef(mod.vocab.1))

Effect(c("nativeBorn", "ageGroup"), mod.vocab.1)

plot(Effect(c("nativeBorn", "ageGroup"), mod.vocab.1), 
    main= "(a) Interaction Model", confint=list(style="bars"),
    lines=list(multiline=TRUE), ylim=c(3.5, 6.5))

mod.vocab.2 <- update(mod.vocab.1, . ~ . - nativeBorn:ageGroup)
plot(Effect(c("nativeBorn", "ageGroup"), mod.vocab.2), 
    main="(b) Main Effects Model", lines=list(multiline=TRUE), 
    confint=list(style="bars"), ylim=c(3.5,6.5))

emmeans(mod.vocab.1, pairwise ~ nativeBorn | ageGroup)$contrasts


emmeans(mod.vocab.2, pairwise ~ nativeBorn)$contrasts
emmeans(mod.vocab.2, pairwise ~ ageGroup)$contrasts

mod.vocab.3 <- lm(vocab ~ year + educGroup + gender + 
    nativeBorn*ageGroup, data=GSSvocab)

plot(predictorEffects(mod.vocab.3, ~ . - nativeBorn), 
    axes=list(x=list(rotate=45)), lines=list(multiline=TRUE), 
    confint=list(style="bars"))

mod.slid.fullquad <- lm(log(wages) ~ 
    poly(age, education, degree=2, raw=TRUE), 
    data=SLID, subset = age >= 18 & age <= 65)

plot(predictorEffects(mod.slid.fullquad, 
    xlevel=list(education=4, age=4)), multiline=TRUE)

getOption("contrasts")

with(Prestige, contrasts(type))

Prestige$type1 <- Prestige$type
contrasts(Prestige$type1) <- 
    contr.treatment(levels(Prestige$type1), base=3)
contrasts(Prestige$type1)

Prestige$type2 <- relevel(Prestige$type, ref="prof")

contrasts(Prestige$type2)

contrasts(Prestige$type) <- "contr.SAS"
contrasts(Prestige$type)

contrasts(Prestige$type) <- "contr.helmert"
contrasts(Prestige$type)

contrasts(Prestige$type) <- "contr.sum"
contrasts(Prestige$type)

contrasts(Prestige$type) <- "contr.Treatment"
contrasts(Prestige$type)
contrasts(Prestige$type) <- "contr.Sum"
contrasts(Prestige$type)

options(contrasts=c("contr.Sum", "contr.poly"))

Duncan$type.ord <- ordered(Duncan$type, 
    levels=c("bc", "wc", "prof"))
Duncan$type.ord
round(contrasts(Duncan$type.ord), 3)

S(lm(prestige ~ type.ord, data=Duncan))
Tapply(prestige ~ type.ord, mean, data=Duncan)

dosage <- c(1, 4, 8, 1, 4, 8, 1, 4)

X <- poly(dosage, degree=2)
round(X, 3)
zapsmall(cor(X))

(X2 <- poly(dosage, degree=2, raw=TRUE))
round(cor(X2), 3)

options(contrasts=c("contr.treatment", "contr.poly"))

C <- matrix(c(1, -0.5, -0.5,  0, 1, -1), 3, 2)
colnames(C) <- c(":Basal vs. DRTA & Strat", ":DRTA vs. Strat")
C

contrasts(Baumann$group) <- C
S(lm(post.test.3 ~ group, data=Baumann))

S(prestige.mod.4 <- update(prestige.mod.1, . ~ . - 1))

summary(nutrition)

xtabs(~ race + age, data=nutrition)

mod.nut.1 <- lm(gain ~ race*age, data=nutrition)

S(mod.nut.1, brief=TRUE)

Effect(c("race", "age"), mod.nut.1)

(mod.nut.2 <- update(mod.nut.1, ~ . - race:age))
Effect(c("race", "age"), mod.nut.2)

args(lm)

(lm(prestige ~ I(income + education), data=Duncan))

(f <- factor(rep(LETTERS[1:3], each=3)))
(x <- rep(1:3, 3))
model.matrix(~ f/x)
model.matrix(~ x:f)
