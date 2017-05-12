## Experiment 2 Picture Ratings

##### PACKAGES #####

if (!require(lme4)) {install.packages("lme4"); require(lme4)} # for mixed-effects models
if (!require(Rmisc)) {install.packages("Rmisc"); require(Rmisc)} # for data summaries

##### PICTURES #####

# load data
data2<-read.csv("Experiment 2 Picture Ratings.csv")
data2$X<-NULL

fit2<-aov(X9.rating~condition,data=data2)
summary(fit2)

summ2<-summarySE(data=data2,measurevar="X9.rating",groupvars=c("condition"),
                    na.rm=T,conf.interval=0.95)
summ2
