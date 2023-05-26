data <- read.csv("femaleMiceWeights.csv")
ncol(data)
name(data[,2])
colnames(data)
data[12,2]
weight <- data$Bodyweight
weight[11]
length(data)
length(weight)
data

# calculate mean weight of mice on hf diet
mean(weight[13:24])

set.seed(1)
sample(weight[13:24], 1)
