##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##               Script for Chapter 2                  ##
##-----------------------------------------------------##

options(show.signif.stars=FALSE)

library("car") # loads car and carData packages

class(Davis)
brief(Davis)

data("Challeng", package="alr4")
brief("Challeng")

cooperation <- c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29,
    27, 58, 52, 41, 30, 40, 39, 44, 34, 44)

(condition <- rep(c("public", "anonymous"), c(10, 10)))
(sex <- rep(rep(c("male", "female"), each=5), 2))

rep(5, 3)
rep(c(1, 2, 3), 2)

rep(1:3, 3:1)

brief(Guyer <- data.frame(cooperation, condition, sex))

Guyer <- data.frame(
  cooperation = c(49, 64, 37, 52, 68, 54, 61, 79, 64, 29, 
                  27, 58, 52, 41, 30, 40, 39, 44, 34, 44),
  condition = rep(c("public", "anonymous"), c(10, 10)),
  sex = rep(rep(c("male", "female"), each=5), 2)
)

str(Guyer)

Duncan <- read.table(file="Duncan.txt", header=TRUE)
brief(Duncan)  # first 3 and last 2 rows

library("tidyverse")  # loads all of the tidyverse packages
Duncan.tibble <- as_tibble(Duncan)
print(Duncan.tibble, n=5)  # note print() method
brief(as.data.frame(Duncan.tibble))

str(Duncan)

search()

Duncan$prestige
mean(Duncan$prestige)
mean(Duncan[ , "prestige"]) # equivalent, column by name
mean(Duncan[ , 4]) # equivalent, column by number

with(Duncan, mean(prestige, trim=0.1))

mod.duncan <- lm(prestige ~ income + education, data=Duncan)

brief(Freedman)

head(Freedman$density, 20)  # first 20 values

library("mice")

library("mice")
md.pattern(Freedman)

median(Freedman$density)

median(Freedman$density, na.rm=TRUE)

with(Freedman, {
    plot(density, crime, main="(a)")
    showLabels(density, crime, labels=row.names(Freedman), 
        n=5, method="x")
})

log(c(1, 10, NA, 100), base=10)
c(1, NA, 3, 4) * c(2, 3, 4, NA)


with(Freedman, plot(log(density, base=10), crime, main="(b)"))
abline(lm(crime ~ log(density, base=10), data=Freedman), lty="dashed", lwd=2)
good <- with(Freedman, complete.cases(crime, density))
with(Freedman,
    lines(lowess(log(density[good], base=10), crime[good], f=1.0), lwd=2))
legend("bottomright", legend=c("ols", "lowess"), 
       lty=c("dashed", "solid"), lwd=2, inset=.02)

lm(crime ~ log(density, base=10), data=Freedman)


good <- with(Freedman, complete.cases(crime, density))
head(good, 20)  # first 20 values


Freedman.good <- na.omit(Freedman)
brief(Freedman.good)

NA == c(1, 2, NA, 4, NA)

is.na(c(1, 2, NA, 4, NA))

sum(is.na(Freedman))
sum(is.na(Freedman.good))

perc.coop <- 100*Guyer$cooperation/120

remove("perc.coop") # from the global environment
Guyer$perc.coop <- 100*Guyer$cooperation/120 
brief(Guyer)  # first 3 rows and last 2 rows

Guyer$cooperation <- with(Guyer, log(perc.coop/(100 - perc.coop)))
brief(Guyer)

Guyer$coop.4 <- cut(Guyer$perc.coop, breaks=4)
summary(Guyer$coop.4)

Guyer$coop.thirds <- with(Guyer, cut(perc.coop,
     quantile(perc.coop, c(0, 1/3, 2/3, 1)),
     labels=c("low", "med", "high"),
     include.lowest=TRUE))
summary(Guyer$coop.thirds)

(Guyer$coop.2 <- Recode(Guyer$perc.coop, 
    ' lo:50="low"; 50:hi="high" '))

set.seed(12345)  # for reproducibility
(sample.20 <- sort(sample(nrow(Womenlf), 20)))  # 20 random cases
Womenlf[sample.20, ]  # 20 randomly selected rows

(seed <- sample(2^31 - 1, 1))
set.seed(seed)

Womenlf$working <- Recode(Womenlf$partic,
    ' c("parttime", "fulltime")="yes"; "not.work"="no" ')
Womenlf$working.alt <- Recode(Womenlf$partic,
    ' c("parttime", "fulltime")="yes"; else="no" ')
Womenlf$working[sample.20]  # the 20 sampled cases
with(Womenlf, all(working == working.alt))  # check

Womenlf$fulltime <- Recode(Womenlf$partic,
    ' "fulltime"="yes"; "parttime"="no"; "not.work"=NA ')
Womenlf$fulltime[sample.20]  # the 20 sampled cases

Womenlf$region.4 <- Recode(Womenlf$region,
    ' c("Prairie", "BC")="West" ')
Womenlf$region.4[sample.20]  # the 20 sampled cases

Womenlf$working.alt.2 <- factor(with(Womenlf,
    ifelse(partic %in% c("parttime", "fulltime"), "yes", "no")))
with(Womenlf, all.equal(working, working.alt.2))

husbands.income <- c(10, 30, 50, 20, 120) # imagine in $1000s
wifes.income    <- c(15, 20, 45, 22,  90)
ifelse (husbands.income > wifes.income, 
    husbands.income, wifes.income) # larger of the two

pmax(husbands.income, wifes.income)

Womenlf$fulltime.alt <- factor(with(Womenlf,
    ifelse(partic == "fulltime", "yes",
        ifelse(partic == "parttime", "no", NA))))
with(Womenlf, all.equal(fulltime, fulltime.alt))

with(Womenlf, all(fulltime == fulltime.alt))

remove(Guyer, Womenlf)

brief(Guyer.male <- Guyer[Guyer$sex == "male", ], 
    rows=c(2, 1))
brief(Guyer.female <- Guyer[Guyer$sex == "female", ], 
    rows=c(2, 1))

brief(Guyer.reordered <- rbind(Guyer.male, Guyer.female))

brief(Guyer.reordered <- cbind(Guyer.reordered, 
    pctcoop=round(100*Guyer.reordered$cooperation/120, 2)))

brief(MplsStops)

brief(MplsDemo)

MplsStops %>% group_by(neighborhood) %>% 
    summarize(nstops = n(), 
        ntraffic = sum(problem == "traffic"), 
        nNoCitation = sum(problem == "traffic" & 
          citationIssued == "no", na.rm=TRUE),
        nYesCitation = sum(problem == "traffic" & 
          citationIssued == "yes", na.rm=TRUE),
        lat = mean(lat),
        long = mean(long)) -> Neighborhood

Neighborhood

Neigh.combined <- merge(Neighborhood, MplsDemo, by="neighborhood")
brief(Neigh.combined)

Neighborhood$neighborhood[!(Neighborhood$neighborhood 
    %in% MplsDemo$neighborhood)]

Neigh.combined.2 <- merge(Neighborhood, MplsDemo, 
    by="neighborhood", all.x=TRUE)
dim(Neigh.combined.2)

MplsStops.2 <- merge(MplsStops, Neigh.combined.2, by="neighborhood")
brief(MplsStops.2)

head(OBrienKaiser, 2) # first two subjects
nrow(OBrienKaiser)

OBK.L <- reshape(OBrienKaiser, direction="long", 
    varying=list(response=3:17), 
    v.names="response", idvar="subject")
nrow(OBK.L)
head(OBK.L, 16) # first measurment for each subject

ord <- with(OBK.L, order(subject, time))
OBK.L <- OBK.L[ord, ]
head(OBK.L, 15) # rows for subject 1

OBK.L$phase <- factor(rep(rep(c("pre", "post", "fup"), 
    each=5), 16))
OBK.L$hour <- factor(rep(rep(1:5, 3), 16))
head(OBK.L, 15)

OBK.W <- reshape(OBK.L, direction="wide", idvar="subject", 
    v.names="response", drop=c("phase", "hour"),
    varying=list(response=paste0(
        rep(c("pre", "post", "fup"), each=5), ".", rep(1:5, 3))))
head(OBK.W, 2)
nrow(OBK.W)
    # ignore subject in column 3 of OBK.W:
all(OBrienKaiser == OBK.W[, c(1:2, 4:18)]) 

(A <- matrix(1:12, nrow=3, ncol=4))
(B <- matrix(c("a", "b", "c"), 4, 3, 
    byrow=TRUE)) # 4 rows, 3 columns

dim(A)
dim(B)

set.seed(54321) # for reproducibility
(v <- sample(10, 10))  # permutation of 1 to 10

length(v)
dim(v)

as.matrix(v)

matrix(v, nrow=1)

(array.3 <- array(1:24, 
    dim=c(4, 3, 2)))  # 4 rows, 3 columns, 2 layers

(list.1 <- list(mat.1=A, mat.2=B, vec=v))  # a 3-item list

v  # previously defined, permutation of 1:10
v[2] # second element
v[c(4, 2, 6)]  # selected out of order
v[c(4, 2, 4)]  # selecting 4th element twice

v[-c(2, 4, 6, 8, 10)]  # equivalent to v[c(1, 3, 5, 7, 9)]

names(v) <- letters[1:10]
v
v[c("f", "i", "g")]

v < 6
v[v < 6]  # all elements that are less than 6

(vv <- v)  # make a copy of v
vv[c(1, 3, 5)] <- 1:3  # replace elments 1, 3, 5
vv
vv[c("b", "d", "f", "h", "j")] <- NA
vv
remove(vv) # clean up

A  # previously defined
A[2, 3]  # element in row 2, column 3
A[c(1, 2), 2]  # rows 1 and 2, column 2 (returns a vector)
A[c(1, 2), c(2, 3)] # rows 1 and 2, columns 2 and 3 (submatrix)
A[1:2, ]  # rows 1 and 2, all columns

A[ , 1]  # produces a vector
A[ , 1, drop=FALSE]  # produces a 1-column matrix

A[ , -c(1, 3)]  # omit columns 1 and 3
A[-1, -2]       # omit row 1 and column 2
rownames(A) <- c("one", "two", "three")  # set row names
colnames(A) <- c("w", "x", "y", "z")     # set column names
A
A[c("one", "two"), c("x", "y")]
A[c(TRUE, FALSE, TRUE), ]  # select 1st and 3rd rows

(AA <- A)     # make a copy of A
AA[1, ] <- 0  # set first row to zeros
AA[2, 3:4] <- NA  
AA
remove(AA)

list.1  # previously defined

list.1[c(2, 3)]  # elements 2 and 3
list.1[2]        # returns a one-element list

list.1[[2]]  # returns a matrix

list.1["mat.1"]  # produces a one-element list
list.1[["mat.1"]]  #  extracts a single element

list.1$mat.1

list.1$mat.1 <- matrix(7:10, 2, 2)   # replace element
list.1$title <- "an arbitrary list"  # new element
list.1$mat.2 <- NULL                 # delete element
list.1

list.1["title"] <- list(NULL)
list.1

list.1$vec[3]
list.1[["mat.1"]][2, 1]

list.1$foo

brief(Guyer)

rownames(Guyer)

Guyer[ , 1]  # first column, returned as a vector
Guyer[ , "cooperation"]  # equivalent
Guyer[1:3, ]  # first 3 rows
Guyer[c("1", "2"), "cooperation"] # by row and column names
Guyer[-(6:20), ]  # drop rows 6 through 20
with(Guyer, Guyer[sex == "female" & condition == "public", ])

subset(Guyer, sex == "female" & condition == "public")

Guyer$cooperation
Guyer[["cooperation"]] # equivalent
Guyer[[1]]             # equivalent
head(Guyer["cooperation"]) # first six rows

c.starts <- c("02/27/2017", "02/24/2016", "05/14/1984")
c.ends <-   c("27-03-17", "3-1-17", "1-11-85")

library("lubridate")

library("lubridate")
(Dates <- data.frame(starts=mdy(c.starts), ends=dmy(c.ends)))

with(Dates, ends - starts)

weekdays(Dates$starts, abbreviate=TRUE)
months(Dates$starts, abbreviate=FALSE)
quarters(Dates$starts, abbreviate=TRUE)
as.numeric(format(Dates$starts, "%Y"))
as.numeric(format(Dates$starts, "%m"))

(time1 <- hms("17:5:3"))
(time2 <- hm("7:4"))
(time3 <- ms("7:4"))
time1 - time2
hour(time1)
minute(time1)
second(time1)

(date.time <- mdy_hms("7/10/18 23:05:01"))

MplsStops$wkday <- factor(weekdays(MplsStops$date, abbreviate=TRUE), 
    levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
(tab1 <- xtabs(~ problem + wkday, data=MplsStops))
100*round(prop.table(tab1, margin=1), 3) # row percents
chisq.test(tab1)

daynight <- with(MplsStops,
    ifelse(hour(date) < 6 | hour(date) >= 18,
        "night", "day"))
(tab2 <- xtabs(~ problem + daynight, MplsStops))
100*round(prop.table(tab2, margin=1), 3)
chisq.test(tab2)

Hamlet <- readLines("Hamlet.txt") # from the current directory
head(Hamlet)       # first 6 lines
length(Hamlet)     # number of lines
nchar(Hamlet)      # number of characters per line
sum(nchar(Hamlet)) # number of characters in all

lines.1_6 <- paste(Hamlet[1:6], collapse=" ")

strwrap(lines.1_6)

substring(lines.1_6, 22, 41)


characters <- strsplit(lines.1_6, "")[[1]]
length(characters)    # number of characters
head(characters, 20)  # first 20 characters

all.lines <- paste(Hamlet, collapse=" ")
words <- strsplit(all.lines, " ")[[1]]
length(words)    # number of words
head(words, 20)  # first 20 words

words <- sub("[,;:.?!]", "", words)
head(words, 20)

sub("me", "you", "It's all, 'me, me, me' with you!")
gsub("me", "you", "It's all, 'me, me, me' with you!")

head(words <- tolower(words), 20)  # first 20 words
word.counts <- sort(table(words), decreasing=TRUE)
word.counts[word.counts > 2]  # words used more than twice
head(sort(unique(words)), 20)  # first 20 unique words
length(unique(words))  # number of unique words

grep("-", words)
words[grep("-", words)]

grep("^-", words)
words <- words[- grep("^-", words)] # negative index to delete
head(sort(unique(words)), 20)

grep("!$", c("!10", "wow!"))
grep("^and$", c("android", "sand", "and", "random"))
grep("and", c("android", "sand", "and", "random"))

data <- c("-123.45", "three hundred", "7550", 
    "three hundred 23", "Fred")
data[grep("^[0-9.-]*$", data)]

data[grep("^[^0-9.-]*$", data)]

words[grep("^(the|a|an)$", words)]

length(unique(words[- grep("^(the|a|an)$", words)]))

grep("\\$", c("$100.00", "100 dollars"))

remove(list=objects())
