## Experiment 2 Stats

##### PACKAGES #####

if (!require(lme4)) {install.packages("lme4"); require(lme4)} # for mixed-effects models
if (!require(Rmisc)) {install.packages("Rmisc"); require(Rmisc)} # for data summaries
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)} # for plotting

##### LOGISTIC REGRESSION #####

# load data
data<-read.csv("Experiment 2 Data.csv")
data$X<-NULL
length(unique(data$participant)) # should be 64

# trial loss
trial.total<-2560
loss.total<-2560-nrow(data)
loss.percentage<-round(loss.total/trial.total*100,2)
loss.percentage
use.percentage<-round((trial.total-loss.total)/trial.total*100,2)
use.percentage

# binarization (D=1,P=0)
data$priming<-ifelse(data$target.form=="DO",1,ifelse(data$target.form=="PO",0,NA))

## effect coding
# prime type (1=do,-1=po)
data$prime_type.e<-factor(data$prime.form,levels=c("DO","PO"))
contrasts(data$prime_type.e)<-contr.sum(2)
# prime condition (1=nlvc,-1=lvc)
data$p.condition.e<-factor(data$prime.condition,levels=c("NL","L"),labels=c("DATIVE","LIGHT"))
contrasts(data$p.condition.e)<-contr.sum(2)
# target condition (1=nlvc,-1=lvc)
data$t.condition.e<-factor(data$target.condition,levels=c("NL","L"),labels=c("DATIVE","LIGHT"))
contrasts(data$t.condition.e)<-contr.sum(2)

# rename participant and target.item for consistency
colnames(data)[which(colnames(data)=="participant")]<-"subject"
colnames(data)[which(colnames(data)=="target.item")]<-"target"

##### MODEL SELECTION #####

## BASE MODEL = MINIMAL MODEL (that converges)
max<-glmer(priming~prime_type.e*p.condition.e*t.condition.e+(1|target)+(1|subject),
             # (1+prime_type.e|target)+(1+prime_type.e|subject)+ # didn't converge with these in
             # (1+p.condition.e|target)+(1+p.condition.e|subject)+
             # (1+t.condition.e|target)+(1+t.condition.e|subject)+
             # (1+prime_type.e*p.condition.e|target)+(1+prime_type.e*p.condition.e|subject)+
             # (1+prime_type.e*t.condition.e|target)+(1+prime_type.e*t.condition.e|subject)+
             # (1+p.condition.e*t.condition.e|target)+(1+p.condition.e*t.condition.e|subject)+
             # (1+prime_type.e*p.condition.e*t.condition.e|target)+(1+prime_type.e*p.condition.e*t.condition.e|subject),
           family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
summary(max)

## summaries

data.max<-summarySE(data=data,measurevar="priming",groupvars=c("prime_type.e","p.condition.e","t.condition.e"),
                    na.rm=T,conf.interval=0.95)
data.max

# percent priming by condition
priming_pers<-ddply(data.max,.(p.condition.e,t.condition.e),summarize,priming=priming[prime_type.e=="DO"]-priming[prime_type.e=="PO"])
priming_pers

##### FORWARD MODEL COMPARISON #####

# m1
m1<-glmer(priming~(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m1)

## 2
m2<-glmer(priming~t.condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m2)

## 3
m3<-glmer(priming~t.condition.e+prime_type.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m3)

## 4
m4<-glmer(priming~t.condition.e+prime_type.e+p.condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m4)

## 5
m5<-glmer(priming~t.condition.e*prime_type.e+p.condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m5)

## 6
m6<-glmer(priming~t.condition.e*prime_type.e+t.condition.e*p.condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m6)

## 7
m7<-glmer(priming~t.condition.e*prime_type.e+t.condition.e*p.condition.e+prime_type.e*p.condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m7)

## 8
m8<-glmer(priming~t.condition.e*prime_type.e*p.condition.e+(1|target)+(1|subject),
          family=binomial,control=glmerControl(optimizer="bobyqa"),data=data)
# summary(m8)

anova(m1,m2)
anova(m1,m2)[2,2]-anova(m1,m2)[1,2]
anova(m2,m3)
anova(m2,m3)[2,2]-anova(m2,m3)[1,2]
anova(m3,m4)
anova(m3,m4)[2,2]-anova(m3,m4)[1,2]
anova(m4,m5)
anova(m4,m5)[2,2]-anova(m4,m5)[1,2]
anova(m5,m6)
anova(m5,m6)[2,2]-anova(m5,m6)[1,2]
anova(m6,m7)
anova(m6,m7)[2,2]-anova(m6,m7)[1,2]
anova(m7,m8)
anova(m7,m8)[2,2]-anova(m7,m8)[1,2]

##### PAIRWISES #####

# dative targets only
t.dative<-subset(data,target.condition=="NL")
t.dative$p.condition.e<-factor(t.dative$prime.condition,levels=c("NL","L"))
contrasts(t.dative$p.condition.e)<-contr.sum(2)
fit_glmer_1<-glmer(priming~prime_type.e*p.condition.e+(1|target)+(1|subject),
                   family=binomial,control=glmerControl(optimizer="bobyqa"),data=t.dative)
summary(fit_glmer_1)

data.t.dative<-summarySE(data=t.dative,measurevar="priming",groupvars=c("prime_type.e","p.condition.e"),
                            na.rm=T,conf.interval=0.95)
data.t.dative<-data.t.dative[-5,]
data.t.dative

# light targets only
t.light<-subset(data,target.condition=="L")
t.light$p.condition.e<-factor(t.light$prime.condition,levels=c("NL","L"))
contrasts(t.light$p.condition.e)<-contr.sum(2)
fit_glmer_2<-glmer(priming~prime_type.e*p.condition.e+(1|target)+(1|subject),
                   family=binomial,control=glmerControl(optimizer="bobyqa"),data=t.light)
summary(fit_glmer_2)

data.t.light<-summarySE(data=t.light,measurevar="priming",groupvars=c("prime_type.e","p.condition.e"),
                         na.rm=T,conf.interval=0.95)
data.t.light<-data.t.light[-5,]
data.t.light

##### VISUALIZATION #####

# add ps and stars
data.max$p<-c(0,0,0,0,1,1,1,1)
data.max$star<-""
data.max$star[data.max$p<=.05]<-"*"
data.max$star[data.max$p<=.01]<-"**"
data.max$star[data.max$p<=.001]<-"***"

# set positions for asterisks
max.prime.se<-max(data.max$priming)+max(data.max$se)
y.coords1<-c(max.prime.se+.02,max.prime.se+.06,max.prime.se+.06)
y.coords2<-c(max.prime.se+.06,max.prime.se+.06,max.prime.se+.02)
y.max<-c(max.prime.se+.04)

g<-ggplot(data.max,aes(x=prime_type.e,y=priming,fill=prime_type.e))+
  geom_bar(position=position_dodge(),stat="identity",
           colour="black", # Use black outlines
           size=.3)+ # Thinner lines
  facet_grid(.~t.condition.e*p.condition.e)+
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
  ylim(0,1.2)+
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
