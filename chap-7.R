##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 7                  ##
##-----------------------------------------------------##

library("car")
library("nlme")


brief(MathAchieve, cols=c(6, 0))

brief(MathAchSchool)

library("dplyr")
MathAchieve %>% group_by(School) %>%
    summarize(mean.ses = mean(SES)) -> Temp
Temp <- merge(MathAchSchool, Temp, by="School")
brief(Temp)

HSB <- merge(Temp[, c("School", "Sector", "mean.ses")],
    MathAchieve[, c("School", "SES", "MathAch")], by="School")
names(HSB) <- tolower(names(HSB))

HSB$cses <- with(HSB, ses - mean.ses)
brief(HSB)

set.seed(12345) # for reproducibility
cat <- with(HSB, sample(unique(school[sector == "Catholic"]), 20))
Cat.20 <- HSB[is.element(HSB$school, cat), ]
dim(Cat.20)
pub <- with(HSB, sample(unique(school[sector == "Public"]), 20))
Pub.20 <- HSB[is.element(HSB$school, pub), ]
dim(Pub.20)

library("lattice")
xyplot(mathach ~ cses | school, data=Cat.20, main="Catholic",
    panel=function(x, y){
        panel.points(x, y)
        panel.lmline(x, y, lty=2, lwd=2, col="darkgray")
        panel.loess(x, y, span=1, lwd=2)
    }
)

xyplot(mathach ~ cses | school, data=Pub.20, main="Public",
    panel=function(x, y){
        panel.points(x, y)
        panel.lmline(x, y, lty=2, lwd=2, col="darkgray")
        panel.loess(x, y, span=1, lwd=2)
    }
)

cat.list <- lmList(mathach ~ cses | school,
    subset = sector == "Catholic", data=HSB)
pub.list <- lmList(mathach ~ cses | school,
    subset = sector == "Public", data=HSB)

brief(pub.list[[1]])

cat.coef <- coef(cat.list)
head(cat.coef)  # first 6 Catholic schools
pub.coef <- coef(pub.list)

old <- par(mfrow=c(1, 2))
boxplot(cat.coef[, 1], pub.coef[, 1], main="Intercepts",
    names=c("Catholic", "Public"))
boxplot(cat.coef[, 2], pub.coef[, 2], main="Slopes",
    names=c("Catholic", "Public"))
par(old) # restore

cat.coef <- merge(cat.coef, Temp[, c("School", "mean.ses")],
                  by.x="row.names", by.y="School")
pub.coef <- merge(pub.coef, Temp[, c("School", "mean.ses")],
                  by.x="row.names", by.y="School")
colnames(pub.coef)[2] <- colnames(cat.coef)[2] <- "Intercept"
head(cat.coef)

scatterplot(Intercept ~ mean.ses, data=cat.coef, boxplots=FALSE,
    main="Catholic")
scatterplot(cses ~ mean.ses, data=cat.coef, boxplots=FALSE,
    main="Catholic")
scatterplot(Intercept ~ mean.ses, data=pub.coef, boxplots=FALSE,
    main="Public")
scatterplot(cses ~ mean.ses, data=pub.coef, boxplots=FALSE,
    main="Public")

brief(lm(Intercept ~ mean.ses, data=cat.coef))
brief(lm(Intercept ~ mean.ses, data=pub.coef))

HSB$sector <- factor(HSB$sector, levels=c("Public", "Catholic"))

hsb.lme.1 <- lme(mathach ~ mean.ses*cses + sector*cses,
    random = ~ cses | school, data=HSB)

S(hsb.lme.1)

library("effects")

plot(predictorEffects(hsb.lme.1, ~ cses,
        xlevels=list(mean.ses=round(seq(-1.2, 0.8, length=6), 1))),
    lines=list(multiline=TRUE, lwd=4), confint=list(style="bands"),
    axes=list(x=list(rug=FALSE)),
    lattice=list(key.args=list(space="right",  columns=1)))

hsb.lme.2 <- update(hsb.lme.1,
    random = ~ 1 | school)  # omitting random effect of cses
anova(hsb.lme.1, hsb.lme.2)

hsb.lme.3 <- update(hsb.lme.1,
    random = ~ cses - 1 | school)  # omitting random intercept
anova(hsb.lme.1, hsb.lme.3)

pvalCorrected <- function(chisq, df){
    (pchisq(chisq, df, lower.tail=FALSE) +
            pchisq(chisq, df - 1, lower.tail=FALSE))/2
}
pvalCorrected(1.1241, df=2)
pvalCorrected(220.56, df=2)

compareCoefs(hsb.lme.1, hsb.lme.2)

library("lme4")
hsb.lmer.1 <- lmer(mathach ~ mean.ses*cses + sector*cses
    + (cses | school), data=HSB)
S(hsb.lmer.1)

hsb.lmer.2 <- lmer(mathach ~ mean.ses*cses + sector*cses
    + (1 | school), data=HSB) # omitting random effect of cses
anova(hsb.lmer.1, hsb.lmer.2)

anova(hsb.lmer.1, hsb.lmer.2, refit=FALSE)

Anova(hsb.lmer.2)

fixef(hsb.lmer.2)

school.effects <- ranef(hsb.lmer.2)$school
head(school.effects)

school.effects$sector <- MathAchSchool$Sector
school.effects$intercept <- school.effects$"(Intercept)" + 
    ifelse(school.effects$sector == "Public",
        fixef(hsb.lmer.2)[1], sum(fixef(hsb.lmer.2)[c(1, 4)]))
head(school.effects)

school.effects <- school.effects[order(school.effects$intercept), ]
head(school.effects)
tail(school.effects)

brief(Blackmore, rows=c(5, 3))

Blackmore$agegroup <- with(Blackmore,
    cut(age, quantile(age, c(0, 0.25, 0.5, 0.75, 1)),
        include.lowest=TRUE))
xtabs(~ group + agegroup, data=Blackmore)

densityplot( ~ exercise | agegroup + group,
    panel = function(x){
         res <- adaptiveKernel(x, from=0)
         llines(res$x, res$y, lwd=2, col="darkgray")
         panel.rug(x)
         p <- sum(x == 0)/length(x)
         llines(c(0, 0), c(0, p))
         lpoints(0, p, pch=16)
    },
    strip=strip.custom(strip.names=c(TRUE, TRUE)),
    data=Blackmore, xlab="Exercise (hours/week)")

blackmore.mod.1 <- lmer(exercise ~ age*group + (age|subject), 
    data=Blackmore)

Blackmore$tran.exercise <- bcnPower(Blackmore$exercise,
            lambda=0.25, gamma=0.1)
tzero <- min(Blackmore$tran.exercise) # transformed 0
densityplot( ~ tran.exercise | agegroup + group,
    panel = function(x){
         res <- adaptiveKernel(x, from=tzero)
         llines(res$x, res$y, lwd=2, col="darkgray")
         panel.rug(x)
         p <- sum(x == tzero)/length(x)
         llines(c(tzero, tzero), c(0, p))
         lpoints(tzero, p, pch=16)
    },
    strip=strip.custom(strip.names=c(TRUE, TRUE)),
    data=Blackmore, xlab="Transformed Exercise")

set.seed(12345) # for reproducibility
pat.sample <- with(Blackmore,
            sample(unique(subject[group == "patient"]), 20))
con.sample <- with(Blackmore,
            sample(unique(subject[group == "control"]), 20))
print(xyplot(tran.exercise ~ age|subject, data=Blackmore,
       subset=(subject %in% con.sample),
       type=c("p", "r", "g"),
       ylab="Transformed minutes of exercise",
       main="Control Subjects",
       ylim=1.2*range(Blackmore$tran.exercise),
       layout=c(5, 4), aspect=1.0),
       position=c(0, 0, 0.5, 1), more=TRUE)
print(xyplot(tran.exercise ~ age|subject, data=Blackmore,
       subset=(subject %in% pat.sample),
       type=c("p", "r", "g"),
       ylab="Transformed minutes of exercise",
       main="Patients",
       ylim=1.2*range(Blackmore$tran.exercise),
       layout=c(5, 4), aspect=1.0),
       position=c(0.5, 0, 1, 1))

pat.list <- nlme::lmList(tran.exercise ~ I(age - 8) | subject,
                   subset = group=="patient", data=Blackmore)
con.list <- nlme::lmList(tran.exercise ~ I(age - 8) | subject,
                   subset = group=="control", data=Blackmore)
pat.coef <- coef(pat.list)
con.coef <- coef(con.list)
old <- par(mfrow=c(1, 2))
boxplot(pat.coef[,1], con.coef[,1], main="Intercepts",
        names=c("Patients", "Controls"))
boxplot(pat.coef[,2], con.coef[,2], main="Slopes",
        names=c("Patients", "Controls"))
par(old)

blackmore.mod.2.lmer <- lmer(tran.exercise ~ I(age - 8)*group + 
    (I(age - 8) | subject), data=Blackmore)
S(blackmore.mod.2.lmer)
Anova(blackmore.mod.2.lmer, test.statistic="F")

blackmore.mod.2.lme <- lme(tran.exercise ~ I(age - 8)*group,
    random = ~ 1 + I(age - 8) | subject, data=Blackmore)
fixef(blackmore.mod.2.lme)

bm.eff <- Effect(c("age", "group"), blackmore.mod.2.lmer,
    xlevels=list(age=seq(8, 18, by=2)),
    transformation=list(
        link=function(x) bcnPower(x, lambda=0.25, gamma=0.1),
        inverse=function(z) 
            bcnPowerInverse(z, lambda=0.25, gamma=0.1)))
plot(bm.eff,
    axes=list(y=list(type="response"), x=list(rug=FALSE)),
    lines=list(multiline=TRUE, lwd=2),
    confint=list(style="bands"),
    lattice=list(key.args=list(x = 0.20, y = 0.75,
        corner = c(0, 0), padding.text = 1.25)),
    xlab="Age (years)", ylab="Exercise (hours/week)", main="")

blackmore.mod.3.lmer <- lmer(tran.exercise ~ I(age - 8)*group +  
    (1 | subject), data=Blackmore) # no random slopes
anova(blackmore.mod.2.lmer, blackmore.mod.3.lmer, refit=FALSE)
pvalCorrected(35.1, 2)

blackmore.mod.4.lmer <- lmer(tran.exercise ~ I(age - 8)*group + 
    (age - 1 | subject), Blackmore) # no random intercepts
anova(blackmore.mod.2.lmer, blackmore.mod.4.lmer, refit=FALSE) 
pvalCorrected(28.5, 2)


blackmore.mod.6.lme<- update(blackmore.mod.2.lme, 
    random = ~ 1|subject, # random intercepts
    correlation = corCAR1(form = ~ I(age - 8) | subject))
blackmore.mod.7.lme<- update(blackmore.mod.2.lme, 
    random = ~ I(age - 8) - 1 | subject, # random slopes
    correlation = corCAR1(form = ~ I(age - 8) | subject))

AIC(blackmore.mod.2.lme, blackmore.mod.6.lme, blackmore.mod.7.lme)
BIC(blackmore.mod.2.lme, blackmore.mod.6.lme, blackmore.mod.7.lme)

compareCoefs(blackmore.mod.2.lme, blackmore.mod.6.lme, 
    blackmore.mod.7.lme)

hsb.lm <- lm(mathach ~ mean.ses*cses + sector*cses, data=HSB)

Mpls <- merge(MplsStops, MplsDemo, by="neighborhood")
nrow(Mpls)
names(Mpls)

Mpls <- subset(Mpls, 
    subset = problem == "suspicious" & MDC == "MDC",
    select=c("neighborhood", "race", "gender", "black", 
        "personSearch"))
summary(Mpls)

Mpls$race <- Recode(Mpls$race, 
    ' "White" = "White"; "Black" = "Black"; 
        "Native American" = "Native American"; else=NA ')
Mpls$gender[Mpls$gender == "Unknown"] <- NA
Mpls$gender <- droplevels(Mpls$gender)
Mpls <- na.omit(Mpls)
Mpls$neighborhood <- droplevels(Mpls$neighborhood)
Mpls$race <- factor(Mpls$race, 
    levels=c("White", "Black", "Native American"))
summary(Mpls)
nrow(Mpls)

ftable(round(100*
        prop.table(xtabs(~ race + gender + personSearch, data=Mpls), 
            margin=c(1, 2)), 
        1))

summary(as.vector(xtabs(~ neighborhood, data=Mpls)))
summary(as.vector(xtabs(~ neighborhood + gender + race, data=Mpls)))

mod.mpls <- glmer(personSearch ~ race*gender + black 
        + (1 | neighborhood), data=Mpls, family=binomial)

Anova(mod.mpls)
S(mod.mpls)

plot(Effect(c("race", "gender"), mod.mpls), 
     lines=list(multiline=TRUE), confint=list(style="bars"))
