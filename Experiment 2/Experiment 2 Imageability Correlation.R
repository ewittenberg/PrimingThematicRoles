## Experiment 2 Imageability Correlation

##### CORRELATION #####

# load data
data<-read.csv("Experiment 2 Imageability Ratings.csv")
data$X<-NULL
length(unique(data$item)) # should be 80 (20*2*2)

# check for normality of variables

op <- par(mfrow = c(1, 2))
hist(data$imageability)
qqnorm(data$imageability)
qqline(data$imageability)
par(op)

op <- par(mfrow = c(1, 2))
hist(data$priming)
qqnorm(data$priming)
qqline(data$priming)
par(op)

# proceed with pearson correlation (normal enough)
cor.test(data$imageability,data$priming)
