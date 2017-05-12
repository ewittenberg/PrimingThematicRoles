## Experiment 2 Sentence Ratings

##### PACKAGES #####

if (!require(lme4)) {install.packages("lme4"); require(lme4)} # for mixed-effects models
if (!require(Rmisc)) {install.packages("Rmisc"); require(Rmisc)} # for data summaries

##### SENTENCES #####

# load data
data1<-read.csv("Experiment 2 Sentence Ratings.csv")
data1$X<-NULL

fit1<-aov(X4.rating~condition*prime,data=data1)
summary(fit1)

summ1<-summarySE(data=data1,measurevar="X4.rating",groupvars=c("condition","prime"),
                 na.rm=T,conf.interval=0.95)
summ1

mean(data1$X4.rating[data1$condition=="L"])
mean(data1$X4.rating[data1$condition=="N"])

mean(data1$X4.rating[data1$condition=="N"])-mean(data1$X4.rating[data1$condition=="L"])

# dative
dat<-subset(data1,condition=="N")

fit.dat<-aov(X4.rating~prime,data=dat)
summary(fit.dat)

# light
lig<-subset(data1,condition=="L")

fit.lig<-aov(X4.rating~prime,data=lig)
summary(fit.lig)
