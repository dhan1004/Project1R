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
