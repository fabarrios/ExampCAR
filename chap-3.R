##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 3                  ##
##-----------------------------------------------------##

set.seed(1234)

library("car") # also loads carData
some(Prestige) # 10 randomly sampled rows

with(Prestige, hist(income))

with(Prestige, hist(income, breaks="FD", col="gray",
    main="Average Income in 1970 (dollars)"))
box()

args("hist.default")

with(Prestige, {
  hist(income, freq=FALSE, ylim=c(0, 1.5e-4), breaks="FD", main="")
  lines(density(income, from=0), lwd=3, lty=2)
  lines(adaptiveKernel(income, from=0), lwd=2, lty=1)
  rug(income)
  legend("topright", c("Fixed bandwidth", "Adaptive bandwidth"),
         lty=2:1, lwd=2, inset=.02)
  box()
})

qqPlot(~ income, data=Prestige, id=list(n=3))

set.seed(124) # for reproducibility
qqPlot(rchisq(100, 3), distribution="chisq", df=3)

Boxplot(~ income, data=Prestige)

with(Prestige, plot(income, prestige))

scatterplot(prestige ~ income, data=Prestige, id=list(n=4))

Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))
scatterplot(prestige ~ income | type, data=Prestige,
    legend=list(coords="bottomright", inset=0.1),
    smooth=list(span=0.9))

xtabs(~type, data=Prestige)

set.seed(1234)

brief(Vocab)  # sample 10 cases

scatterplot(vocabulary ~ education, data=Vocab, main="(a)")

scatterplot(vocabulary ~ education, data=Vocab,
    jitter=list(x=2, y=2), cex=0.01, col="darkgray",
    smooth=list(span=1/3, col.smooth="black", col.spread="black"),
    regLine=list(col="black"), main="(b)")

Boxplot(vocabulary ~ education, data=Vocab, id=FALSE)

brief(Ornstein)

Boxplot(interlocks ~ nation, data=Ornstein, main="(a)")

library("plotrix")
means <- Tapply(interlocks ~ nation, mean, data=Ornstein)
sds <- Tapply(interlocks ~ nation, sd, data=Ornstein)
plotCI(1:4, means, sds, xaxt="n", xlab="Nation of Control",
    ylab="interlocks", main="(b)", ylim=range(Ornstein$interlocks))
lines(1:4, means)
axis(1, at=1:4, labels = names(means))

scatterplotMatrix(~ prestige + income + education + women,
    data=Prestige)

c("natural log"=log(7), "base 2 log"=log2(7),"common log"=log10(7))
log2(7)/log2(exp(1)) # again the natural logarithm

exp(log(50))
log(exp(50))
10^(log10(50))
log10(10^50)

c(log10(7), log(7, base=10), logb(7, 10))

par(mfrow=c(1, 2), mar=c(5, 4, 6, 2) + 0.1)
densityPlot(~ assets, data=Ornstein, xlab="assets", main="(a)")
densityPlot(~ log10(assets), data=Ornstein, adjust=0.65,
    xlab=expression(log[10]~"(assets)"), main="(b)")
basicPowerAxis(0, base=10, side="above", at=10^(2:5),
    axis.title="")

scatterplot(infantMortality ~ ppgdp, data=UN, xlab="GDP per Capita",
     ylab="Infant Mortality Rate (per 1000 births)", main="(a)")

scatterplot(infantMortality ~ ppgdp, data=UN, xlab="GDP per capita",
     ylab="Infant Mortality Rate (per 1000 births)", main="(b)",
     log="xy", id=list(n=3))

brief(lm(log(infantMortality) ~ log(ppgdp), data=UN))

p1 <- powerTransform(infantMortality ~ 1, data=UN, family="bcPower")
summary(p1)

testTransform(p1, lambda=1/2)

summary(p2 <- powerTransform(cbind(income, education) ~ 1,
                data=Prestige, family="bcPower"))

testTransform(p2, lambda=c(1/3, 1))

testTransform(p2, lambda=c(0, 1))

scatterplotMatrix(~ prestige + log(income) + education + women,
     smooth=list(span=0.7), data=Prestige)

summary(p3 <- powerTransform(cbind(income, education) ~ type,
    data=Prestige))
testTransform(p3, c(0, 1))

scatterplotMatrix(~ prestige + log(income) + education + women | type,
    data=Prestige, smooth=FALSE, ellipse=list(levels=0.5),
    legend=list(coords="center"))

p4 <- powerTransform(interlocks ~ 1, data=Ornstein, family="bcnPower")
summary(p4)

symbox(~ ppgdp, data=UN)

spreadLevelPlot(interlocks + 1 ~ nation, data=Ornstein)

par(mar=c(5.1, 4.1, 4.1, 4.1))
Boxplot(log10(interlocks + 1) ~ nation, data=Ornstein)
basicPowerAxis(power=0, base=10, at=c(1, 3, 6, 11, 21, 51, 101),
     start=1, axis.title="Interlocks")

asin(sqrt(seq(0, 1, length=11)))

logit(seq(0.1, 0.9, 0.1))
logit(seq(0, 1, 0.1))

par(mfrow=c(1, 3))
densityPlot(~ women, data=Prestige, from=0, to=100,
    main="(a) Untransformed")
densityPlot(~ logit(women), data=Prestige, adjust=0.7,
    main="(b) Logit")
densityPlot(~ asin(sqrt(women/100)), data=Prestige, adjust=0.7,
    main="(c) Arcsine square-root")
