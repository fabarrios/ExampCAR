##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 9                  ##
##-----------------------------------------------------##

args(plot.default)


plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")

par("col")

names(par())

plot(1:25, pch=1:25, cex=1.5, xlab="Symbol Number", ylab="")
lines(1:25, type="h", lty="dashed")

head(letters) # first 6 lowercase letters
plot(1:26, xlab="letters", ylab="", pch=letters, cex=1.5,
    axes=FALSE, frame.plot=TRUE)

plot(c(1, 7), c(0, 1), type="n", axes=FALSE,
    xlab="Line Type (lty)", ylab="", frame.plot=TRUE)
axis(1, at=1:6)  # x-axis
for (lty in 1:6)
    lines(c(lty, lty, lty + 1), c(0, 0.5, 1), lty=lty, lwd=2)

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
abline(0, 1)
abline(c(1, -1), lty="dashed")
abline(h=seq(0, 1, by=0.1), v=seq(0, 1, by=0.1), col="gray")

library("carData")  # for data
plot(prestige ~ income, type="n", data=Prestige)
grid(lty="solid")
with(Prestige, points(income, prestige, pch=16, cex=1.5))

par(mfrow=c(1, 2))
plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="", frame.plot=TRUE, main="(a)")
text(x=c(0.2, 0.5), y=c(0.2, 0.7),  c("example text", "another string"))
plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="", frame.plot=TRUE, main="(b)")
text(c(0.1, 0.5, 0.8), c(0.2, 0.8, 0.3), c("one","two","three"))

par(mfrow=c(1,2))
plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="", main="(a) arrows")
arrows(x0=1:5, y0=rep(0.1, 5),
    x1=1:5, y1=seq(0.3, 0.9, len=5), code=3)

plot(c(1, 5), c(0, 1), axes=FALSE, type="n",
    xlab="", ylab="", main="(b) segments")
segments(x0=1:5, y0=rep(0.1, 5),
    x1=1:5, y1=seq(0.3, 0.9, len=5))


plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
polygon(c(0.2, 0.8, 0.8), c(0.2, 0.2, 0.8), col="black")
polygon(c(0.2, 0.2, 0.8), c(0.2, 0.8, 0.8))

plot(c(1, 5), c(0, 1), axes=FALSE, type="n",
    xlab="", ylab="", frame.plot=TRUE)
legend(1.2, 0.95, legend=c("group A", "group B", "group C"),
    lty=c(1, 2, 4), pch=1:3)

par(mfrow=c(1,2))
curve(x*cos(25/x), 0.01, pi, n=1000)
curve(sin, 0, 2*pi, ann=FALSE, axes=FALSE, lwd=2)
axis(1, pos=0, at=c(0.1, pi/2, pi, 3*pi/2, 2*pi), lwd.ticks=0,
     labels=c(0, expression(pi/2), expression(pi),
              expression(3*pi/2), expression(2*pi)))
axis(2, pos=0)
curve(cos, add=TRUE, lty="dashed", lwd=2)
legend(pi, 1, lty=1:2, lwd=2, legend=c("sine", "cosine"), bty="n")

rainbow(10)

gray(0:9/9)

head(colors(), 10)
length(colors())

par(mar=rep(0,4))
pie(rep(1,100), col=gray(0:100/100), labels=rep("", 100), mar=rep(0,4))

oldpar <- par(mfrow=c(2, 2), las=1)   # 2 x 2 array of graphs

UN <- na.omit(UN[, c("ppgdp", "infantMortality")])
gdp <- UN$ppgdp/1000
infant <- UN$infantMortality
ord <- order(gdp)   # sort data by gdp
gdp <- gdp[ord]
infant <- infant[ord]

x0 <- gdp[150]          # focal x = x_(150)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[97]     # bandwidth for span of 0.5 (where n = 193)
pick <- dist <= h       # observations within window

plot(gdp, infant, xlab="GDP per Capita ($1000s)", ylab="Infant-Mortality Rate per 1000",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col=gray(0.25))
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window
text(x0, par("usr")[4] + 5, expression(x[(150)]), xpd=TRUE)

plot(range(gdp), c(0,1), xlab="GDP per Capita ($1000s)",
    ylab="Tricube Kernel Weight",
    type="n", main="(b) Tricube Weights")
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)

tricube <- function(x, x0, h) {
    z <- abs(x - x0)/h
    ifelse(z < 1, (1 - z^3)^3, 0)
}
tc <- function(x) tricube(x, x0, h) # to use with curve
curve(tc, min(gdp), max(gdp), n=1000, lwd=2, add=TRUE)
points(gdp[pick], tricube(gdp, x0, h)[pick], pch=16)
abline(h=c(0, 1), col="gray")

plot(gdp, infant, xlab="GDP per Capita ($1000s)", ylab="Infant-Mortality Rate per 1000",
    type="n", main="(c) Local Linear Regression")
points(gdp[pick], infant[pick], col=gray(0.25))
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)
abline(v=c(x0 - h, x0 + h), lty=2)
mod <- lm(infant ~ gdp, weights=tricube(gdp, x0, h))
new <- data.frame(gdp=c(x0 - h, x0, x0 + h))
fit <- predict(mod, newdata=new)
lines(c(x0 - h, x0 + h), fit[c(1, 3)], lwd=3) # local regression line
points(x0, fit[2], pch=16, cex=2)
text(x0, par("usr")[4] + 5, expression(x[(150)]), xpd=TRUE)
text(x0 + 1000, fit[2] + 2.5, expression(hat(y)[(150)]), adj=c(0, 0))

plot(gdp, infant, xlab="GDP per Capita  ($1000s)", ylab="Infant-Mortality Rate (per 1000)",
    main="(d) Complete Local-Linear Estimate", col=gray(0.25))
yhat <- numeric(length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
    x0 <- gdp[i]
    dist <- abs(gdp - x0)
    h <- sort(dist)[97]
    mod <- update(mod, weights=tricube(gdp, x0, h))
    yhat[i] <- predict(mod, newdata=data.frame(gdp=x0))
    }
lines(gdp, yhat, lwd=2)
text(gdp[149], infant[149], paste0(" ", rownames(UN)[149]), adj=c(0, 0))
par(oldpar)

oldpar <- par(mfrow=c(2,2), las=1)

x0 <- gdp[150]          # focal x = x_(150)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[97]     # bandwidth for span of 0.5 (where n = 193)
pick <- dist <= h       # observations within window

plot(gdp, infant, xlab="GDP per Capita ($1000s)", ylab="Infant-Mortality Rate per 1000",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")

plot(gdp, infant, xlab="GDP per Capita ($1000s)", ylab="Infant-Mortality Rate per 1000",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col=gray(0.25))
points(gdp[!pick], infant[!pick], col=gray(0.75))

plot(gdp, infant, xlab="GDP per Capita ($1000s)", ylab="Infant-Mortality Rate per 1000",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col=gray(0.25))
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window

plot(gdp, infant, xlab="GDP per Capita ($1000s)", ylab="Infant-Mortality Rate per 1000",
    type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col=gray(0.25))
points(gdp[!pick], infant[!pick], col=gray(0.75))
abline(v=x0)    # focal x
abline(v=c(x0 - h, x0 + h), lty=2)  # window
text(x0, par("usr")[4] + 5, expression(x[(150)]), xpd=TRUE)

par(oldpar)

plot(1:10, type="n", xlab="", ylab="", axes=FALSE, frame=TRUE)
box("outer")
text(5.5, 5.5, "plotting region", cex=1.5)
text(5.5, 4.5, "standard par() settings")
for (side in 1:4){
    for (line in 0:4){
        mtext(paste("line", line), side=side, line=line, adj=0.70) 
    }
}
for (side in 1:4){
    mtext(paste("margin, side", side), side=side, line=1, 
        adj=0.25, cex=1.25)
}

par(mfrow=c(2, 2), oma=c(1, 1, 2, 1), mar=c(2, 2, 2, 1))
n.lines <- par("mar") - 1
for (plot in 1:4){
    plot(1:10, type="n", xlab="", ylab="", axes=FALSE, frame=TRUE)
    box("figure")
    text(5.5, 5.5, paste("panel", plot), cex=1.5)
    if (plot == 1){
        text(5.5, 3.5, 
        "par(mfrow=c(2, 2),\n  oma=c(1, 1, 2, 1),\n  mar=c(2, 2, 2, 1))", 
        cex=1.25)
    }
    for (side in 1:4){
        for (line in 0:n.lines[side]){
            mtext(paste("inner margin line", line), side=side, 
                line=line, cex=0.75) 
        }
    }
}
box("inner")
box("outer")
n.lines <- par("oma") - 1
for (side in 1:4){
    for (line in 0:n.lines[side]){
    mtext(paste("outer margin line", line), 
        side=side, line=line, outer=TRUE)
    }
}

set.seed(12345)

par(oma=c(0, 0, 1, 0), mar=c(2, 3, 3, 2)) # room in top outer margin
par(fig=c(0, 0.5, 0.5, 1)) # top-left panel
x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)  # expected value of y; reverse values
y <- Ey + 0.1*rnorm(200)  # add errors
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple",
    cex.main=1, xlab="", ylab="", col="darkgray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)  # text in bottom margin
mtext("y ", side=2, at=max(y), las=1)  # text in left margin

par(fig=c(0.5, 1, 0.5, 1), new=TRUE) # top-right panel
x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple",
    cex.main=1, xlab="", ylab="", col="darkgray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(fig=c(0.25, 0.75, 0, 0.5), new=TRUE) # bottom panel
x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple",
    cex.main=1, xlab="", ylab="", col="darkgray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)
title("Nonlinear Relationships", outer=TRUE)

par(oma=c(0, 0, 1, 0), mar=c(2, 3, 3, 2))
n.lines <- par("mar") - 1

par(fig=c(0, 0.5, 0.5, 1)) # upper left
plot(0:1, 0:1, type="n", axes=FALSE, frame=TRUE)
box("figure")
text(0.5, 0.5, "panel 1") 
for (side in 1:4){
    for (line in 0:n.lines[side]){
        mtext(paste("inner margin line", line), side=side, 
              line=line, cex=0.75) 
    }
}

par(fig=c(0.5, 1, 0.5, 1), new=TRUE) # upper right
plot(0:1, 0:1, type="n", axes=FALSE, frame=TRUE)
box("figure")
text(0.5, 0.5, "panel 2") # upper right
for (side in 1:4){
    for (line in 0:n.lines[side]){
        mtext(paste("inner margin line", line), side=side, 
              line=line, cex=0.75) 
    }
}

par(fig=c(0.25, 0.75, 0, 0.5), new=TRUE) # bottom center
plot(0:1, 0:1, type="n", axes=FALSE, frame=TRUE)
text(0.5, 0.5, "panel 3")
box("figure")
for (side in 1:4){
    for (line in 0:n.lines[side]){
        mtext(paste("inner margin line", line), side=side, 
              line=line, cex=0.75) 
    }
}

box("outer")
box("inner")
title("outer margin line 0", outer=TRUE)

library("lattice")

(bwp <- bwplot(salary ~ sex | rank + discipline, data=Salaries,
    scales=list(x=list(rot=45), y=list(log=10, rot=0))))

library("latticeExtra")
useOuterStrips(bwp, strip.left=strip.custom(strip.names=TRUE, 
    var.name="Discipline"))

detach(package:latticeExtra)
library("ggplot2")

library("ggplot2")
qplot(income, prestige, xlab="Average Income", ylab="Prestige Score",
      geom=c("point", "smooth"), color=I("black"), data=Prestige)


(plt <- ggplot(Prestige, aes(x=income, y=prestige)))
(plt <- plt + labs(x="Average Income", y="Prestige Score"))
(plt <- plt + geom_point())
plt + geom_smooth(color="black")

head(Depredations)

library("maps")
par(mfrow=c(1, 2))
map("county", "minnesota", col=gray(0.4))
with(Depredations, points(longitude, latitude,
     cex=sqrt(early), pch=20))
title("Depredations, 1976-1991", cex.main=1.5)
map("county", "minnesota", col=grey(0.4))
with(Depredations,points(longitude, latitude,
     cex=sqrt(late), pch=20))
title("Depredations, 1992-1998", cex.main=1.5)

library("dplyr")
MplsStops %>% group_by(neighborhood) %>% 
    summarize(nstops = n(), 
        ntraffic = sum(problem == "traffic"), 
        nNoCitation = sum(problem == "traffic" & 
          citationIssued == "no", na.rm=TRUE),
        nYesCitation = sum(problem == "traffic" & 
          citationIssued == "yes", na.rm=TRUE),
        lat = mean(lat),
        long = mean(long)) -> Neighborhood
Neigh.combined <- merge(Neighborhood, MplsDemo, by="neighborhood")

head(Neigh.combined[, c("neighborhood", "long", "lat", "nstops")])
nrow(Neigh.combined)
