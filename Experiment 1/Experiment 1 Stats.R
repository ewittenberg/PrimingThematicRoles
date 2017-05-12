## Experiment 1 Stats

##### PACKAGES #####

if (!require(lme4)) {install.packages("lme4"); require(lme4)} # for mixed-effects models
if (!require(Rmisc)) {install.packages("Rmisc"); require(Rmisc)} # for data summaries
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)} # for plotting

##### LOGISTIC REGRESSION #####

# load data
data<-read.csv("Experiment 1 Data.csv")
data$X<-NULL
length(unique(data$subject)) # should be 192

# trial loss
trial.total<-nrow(data)
loss.total<-nrow(data)-as.numeric(table(data$response=="D")[2]+table(data$response=="P")[2])
missing.total<-as.numeric(table(data$response=="MISSING")[2])
trial.adjusted<-trial.total-missing.total
loss.adjusted<-loss.total-missing.total
loss.percentage<-round(loss.adjusted/trial.adjusted*100,2)
loss.percentage
use.percentage<-round((trial.adjusted-loss.adjusted)/trial.adjusted*100,2)
use.percentage

# binarization (D=1,P=0)
data$priming<-ifelse(data$response=="D",1,ifelse(data$response=="P",0,NA))

## effect coding
# prime type (1=do,-1=po)
data$prime_type.e<-factor(data$prime_type,levels=c("DO","PO"))
contrasts(data$prime_type.e)<-contr.sum(2)
# condition (1=nlvc,0=idiom,-1=lvc;0=nlvc,1=idiom,-1=lvc)
data$condition.e<-factor(data$condition,levels=c("NLVC","LVC","IDIOM"),labels=c("DATIVE","LIGHT","IDIOM"))
contrasts(data$condition.e)<-contr.sum(3)

##### MODEL SELECTION #####

## BASE MODEL
base<-glmer(priming~prime_type.e*condition.e+(1|target)+(1|subject),
           family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
summary(base)

## MAXIMAL MODEL (that converges)
max<-glmer(priming~prime_type.e*condition.e+(1|target)+(1|subject)+(1+prime_type.e|target),
             # (1+prime_type.e|subject)+(1+condition.e|target)+(1+prime_type.e*condition.e|target) # didn't converge with these in
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
summary(max)

anova(base,max) # same

## summaries

data.base<-summarySE(data=data,measurevar="priming",groupvars=c("prime_type.e","condition.e"),
                    na.rm=T,conf.interval=0.95)
data.base

# percent priming by condition
priming_pers<-ddply(data.base,.(condition.e),summarize,priming=priming[prime_type.e=="DO"]-priming[prime_type.e=="PO"])
priming_pers

##### FORWARD MODEL COMPARISON #####

# m1
m1<-glmer(priming~(1|target)+(1|subject),
            family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m1)

## 2
m2<-glmer(priming~prime_type.e+(1|target)+(1|subject),
           family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m2)

## 3
m3<-glmer(priming~prime_type.e+condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m3)

## 4
m4<-glmer(priming~prime_type.e*condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m4)

anova(m1,m2)
anova(m1,m2)[2,2]-anova(m1,m2)[1,2]
anova(m2,m3)
anova(m2,m3)[2,2]-anova(m2,m3)[1,2]
anova(m3,m4)
anova(m3,m4)[2,2]-anova(m3,m4)[1,2]

##### PAIRWISES #####

# dative vs. light
nlvc_lvc<-subset(data,!(condition=="IDIOM"))
nlvc_lvc$condition.e<-factor(nlvc_lvc$condition,levels=c("NLVC","LVC"))
contrasts(nlvc_lvc$condition.e)<-contr.sum(2)
fit_glmer_1<-glmer(priming~prime_type.e*condition.e+(1|target)+(1|subject),
                   # (1+prime_type.e|subject)+(1+condition.e|target)+(1+prime_type.e*condition.e|target) # didn't converge with these in
                   family=binomial,control=glmerControl(optimizer="bobyqa"),data=nlvc_lvc)
summary(fit_glmer_1)

data.dativelight<-summarySE(data=nlvc_lvc,measurevar="priming",groupvars=c("prime_type.e","condition.e"),
                    na.rm=T,conf.interval=0.95)
data.dativelight

# dative vs. idiom
nlvc_idiom<-subset(data,!(condition=="LVC"))
nlvc_idiom$condition.e<-factor(nlvc_idiom$condition,levels=c("NLVC","IDIOM"))
contrasts(nlvc_idiom$condition.e)<-contr.sum(2)
fit_glmer_2<-glmer(priming~prime_type.e*condition.e+(1|target)+(1|subject),
                   # (1+prime_type.e|subject)+(1+condition.e|target)+(1+prime_type.e*condition.e|target) # didn't converge with these in
                   family=binomial,control=glmerControl(optimizer="bobyqa"),data=nlvc_idiom)
summary(fit_glmer_2)

data.dativeidiom<-summarySE(data=nlvc_idiom,measurevar="priming",groupvars=c("prime_type.e","condition.e"),
                            na.rm=T,conf.interval=0.95)
data.dativeidiom

# idiom vs. light
lvc_idiom<-subset(data,!(condition=="NLVC"))
lvc_idiom$condition.e<-factor(lvc_idiom$condition,levels=c("IDIOM","LVC"))
contrasts(lvc_idiom$condition.e)<-contr.sum(2)
fit_glmer_3<-glmer(priming~prime_type.e*condition.e+(1|target)+(1|subject),
                   # (1+prime_type.e|subject)+(1+condition.e|target)+(1+prime_type.e*condition.e|target) # didn't converge with these in
                   family=binomial,control=glmerControl(optimizer="bobyqa"),data=lvc_idiom)
summary(fit_glmer_3)

data.lightidiom<-summarySE(data=lvc_idiom,measurevar="priming",groupvars=c("prime_type.e","condition.e"),
                            na.rm=T,conf.interval=0.95)
data.lightidiom

##### VISUALIZATION #####

# add ps and stars
data.base$p<-c(0,0,0,1,1,1)
data.base$star<-""
data.base$star[data.base$p<=.05]<-"*"
data.base$star[data.base$p<=.01]<-"**"
data.base$star[data.base$p<=.001]<-"***"

# set positions for asterisks
max.prime.se<-max(data.base$priming)+max(data.base$se)
y.coords1<-c(max.prime.se+.02,max.prime.se+.06,max.prime.se+.06)
y.coords2<-c(max.prime.se+.06,max.prime.se+.06,max.prime.se+.02)
y.max<-c(max.prime.se+.04)

g<-ggplot(data.base,aes(x=prime_type.e,y=priming,fill=prime_type.e))+
  geom_bar(position=position_dodge(),stat="identity",
           colour="black", # Use black outlines
           size=.3)+ # Thinner lines
  facet_grid(.~condition.e)+
  geom_errorbar(aes(ymin=priming-se,ymax=priming+se),
                width=.2,
                position=position_dodge(.9))+
  xlab("Prime Form")+
  ylab("Proportion of DO responses")+
  # guides(fill=F)+
  # scale_fill_hue(name="Prime Type", # Legend label, use darker colors
  #                breaks=c("DO","PO"),
  #                labels=c("Double-object (DO)","Prepositional-object (PO)"))+
  # ggtitle("title")+
  ylim(0,1)+
  # theme_gray()+
  theme_set(theme_gray(base_size=14))
g

# prune graph/add asterisks
g+theme(axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        legend.position="bottom",
        legend.title=element_blank())+
        annotate("segment",x=c(1,1,2),xend=c(1,2,2),y=y.coords1,yend=y.coords2)+
        geom_text(aes(y=y.max,label=star),colour="black",vjust=0,size=10,nudge_x=0.5)
