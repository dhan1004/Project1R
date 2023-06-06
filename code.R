data <- read.csv("femaleMiceWeights.csv")
install.packages("dplyr")
library(dplyr)

controls <- filter(data, Diet=="chow")
controls <- select(controls, Bodyweight)
# puts controls into a vector
unlist(controls)

# piping operator allows you to call functions on the object returned by a
# previous function
controls <- filter(data, Diet=="chow") %>% select(Bodyweight) %>% unlist

treatment <- filter(data, Diet=="hf") %>% select(Bodyweight) %>% unlist

population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)

# get a random sample
sample(population, 12)
mean(sample(population, 12))

obs <- mean(treatment) - mean(controls)

# null hypothesis is true
n <- 10000
nulls <- vector("numeric", n)
for (i in 1:n) {
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  nulls[i] <- mean(treatment) - mean(control)
}

# proportion of times nulls is greater than observation 
mean(nulls > obs)

# p-value: probability that an outcome from the null distribution 
# is bigger than what we observed when the null hypothesis is true
mean(abs(nulls) > obs)


install.packages("ggplot2")
library(ggplot2)
data("msleep")

msleep <- read.csv("msleep_ggplot2.csv")
class(msleep)
primates <- filter(msleep, order=="Primates")
nrow(primates)
class(primates)
select(primates, sleep_total) %>% unlist %>% mean

filter(msleep, order=="Primates") %>% summarize(mean(sleep_total))

load("skew.RData")
dim(dat)
# create 9 plots (3 by 3) for data
par(mfrow = c(3,3))
# for each of 9 plots make a normal graph for each column
for (i in 1:9) {
  qqnorm(dat[,i])
  qqline(dat[,i])
}
hist(dat[,4])

head(InsectSprays)
f <- 3
boxplot(split(InsectSprays[,1], InsectSprays[,2]))
boxplot(InsectSprays$count ~ InsectSprays$spray)

install.packages("UsingR")
library(UsingR)
data(nym.2002, package="UsingR")

# filtering datasets and creating histogram & boxplot for each gender
men <- filter(nym.2002, gender=="Male")
hist(men$time)
boxplot(men$time)
women <- filter(nym.2002, gender=="Female")
hist(women$time)
boxplot(women$time)

install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(1)

m <- 10000
nulls_mice <- vector("numeric", m)
for (i in 1:10000) {
  sample_mice <- sample(x, 5)
  nulls_mice[i] <- mean(sample_mice)
}
mean(abs(mean(x) - nulls_mice) > 1)

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
mean(averages50<25 & averages50>23)

pnorm(25, 23.9, 0.43) - pnorm(23, 23.9, 0.43)

# probability distribution exercises
install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

data1952 = gapminder[gapminder$year==1952,]
x = data1952$lifeExp
mean(x<= 40)


prop = function(q) {
  mean(x <= q)
}
prop(40)
qs = seq(from=min(x), to=max(x), length=20)
qs
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
plot(ecdf(x))

dat <- read.csv("mice_pheno.csv")
dat <- na.omit( dat )
library(dplyr)
x <- filter(dat, Diet=="chow") %>% filter(Sex=="F") %>% select(Bodyweight) %>% unlist
mean(x)
library(rafalib)
popsd(x)
set.seed(2)
X <- sample(x,25)
mean(X)
y <- filter(dat, Diet=="hf") %>% filter(Sex=="F") %>% select(Bodyweight) %>% unlist
mean(y)
popsd(y)
set.seed(2)
Y <- sample(y,25)
mean(Y)
abs(mean(x)-mean(y)) - abs(mean(X) - mean(Y))
pnorm(3, 0, 1) - pnorm(-3, 0, 1)
y <- filter(dat, Sex=="M") %>% filter(Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <= 3)

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)

dat <- read.csv("femaleMiceWeights.csv")

set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)


X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
sd(X)
2*(1 - pnorm(2/sd(X) * sqrt(12)))


se <- sqrt((sd(X)**2)/12 + (sd(Y)**2)/12)
t.test(Y,X)
(mean(Y) - mean(X))/se
pnorm(-2.0552) + (1 - pnorm(2.0552))
