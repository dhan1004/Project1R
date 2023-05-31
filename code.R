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

