#### Robustness checks ####
# This version:   09-12-2015
# First version:  13-08-2013
rm(list=ls(all=TRUE))
setwd("[DIR]/Publications/OEP_2016") 
options(scipen=4)                 

source("code/functions.R") 
source("code/clean.R")

##### Table: Fixed effects ####

## Country indicators
f1<-glm.nb(outcome~index.a+factor(iso3c),main)
clse(f1,1,ccode)
AIC(f1);roc(as.numeric(f1$y>0),fitted(f1))

## Year indicators
f2<-glm.nb(outcome~index.a+factor(year),main)
clse(f2,1,ccode)
AIC(f2);roc(as.numeric(f2$y>0),fitted(f2))

## Country-specific time trend
f3<-glm.nb(outcome~index.a+factor(iso3c)*year,main)
clse(f3,1,ccode)
AIC(f3);roc(as.numeric(f3$y>0),fitted(f3))

## Month indicators
f4<-glm.nb(outcome~index.a+factor(month),main)
clse(f4,1,ccode)
AIC(f4);roc(as.numeric(f4$y>0),fitted(f4))

##### Table: Other estimation methods ####

## Poisson model
p1<-glm(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,main,family="poisson")
p2<-glm(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year),
        main,family="poisson")

clse(p1,1,ccode)
AIC(p1);auc(as.numeric(p1$model$outcome>0),fitted(p1))[1]
clse(p2,1,ccode)
AIC(p2);auc(as.numeric(p2$model$outcome>0),fitted(p2))[1]

## Quasi-Poisson model 
qp1<-glm(outcome.ln~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,
         main,family="quasipoisson")
qp2<-glm(outcome.ln~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year),
        main,family="quasipoisson")

clse(qp1,1,ccode)
AIC(qp1);auc(as.numeric(qp1$model$outcome>0),fitted(qp1))[1]
clse(qp2,1,ccode)
AIC(qp2);auc(as.numeric(qp2$model$outcome>0),fitted(qp2))[1]

## Logit estimation
l1<-glm(outcome.b~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,
         main,family="binomial"(link="logit"))
l2<-glm(outcome.b~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year),
        main,family="binomial"(link="logit"))

clse(l1,1,ccode)
AIC(l1);auc(as.numeric(l1$model$outcome.b),fitted(l1))[1]
clse(l2,1,ccode)
AIC(l2);auc(as.numeric(l2$model$outcome.b),fitted(l2))[1]

rl1<-relogit(outcome.b~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,main)
rl2<-relogit(outcome.b~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+
               factor(year),main)

clse(rl1,1,ccode)
AIC(rl1);auc(rl1$model[,1],as.vector(fitted(rl1)))[1]
clse(rl2,1,ccode)
AIC(rl2);auc(rl2$model[,1],as.vector(fitted(rl2)))[1]

## OLS estimation
lm1<-lm(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,main)
lm2<-lm(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year),
        main)

clse(lm1,1,ccode)
AIC(lm1);auc(as.numeric(lm1$model$outcome>0),fitted(lm1))[1]
clse(lm2,1,ccode)
AIC(lm2);auc(as.numeric(lm2$model$outcome>0),fitted(lm2))[1]

ll1<-lm(outcome.ln~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,main)
ll2<-lm(outcome.ln~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year),
        main)

clse(ll1,1,ccode)
AIC(ll1);auc(as.numeric(ll1$model$outcome>0),fitted(ll1))[1]
clse(ll2,1,ccode)
AIC(ll2);auc(as.numeric(ll2$model$outcome>0),fitted(ll2))[1]

lpm1<-lm(outcome.b~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,main)
lpm2<-lm(outcome.b~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year),
        main)

clse(lpm1,1,ccode)
AIC(lpm1);auc(lpm1$model$outcome.b,fitted(lpm1))[1]
clse(lpm2,1,ccode)
AIC(lpm2);auc(lpm2$model$outcome.b,fitted(lpm2))[1]

#### Table: Robustness checks ####

## Really simple model
r0<-glm.nb(violence~violence.l+index.a+regime.l+year,simple)
clse(r0,1,factor(simple$iso3c))
AIC(r0);auc(as.numeric(r0$model$violence>0),fitted(r0))[1]

## Using incidence rather than onset (omitting fixed effects)
r1<-glm.nb(violence~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+violence.l+
             factor(year),incd)
clse(r1,1,factor(incd$iso3c))
AIC(r1);auc(as.numeric(r1$model$violence>0),fitted(r1))[1]

## Accounting for oil prices 
r2<-glm.nb(outcome~index.a+violence.cl+oil.dt+factor(iso3c)+factor(year),rbst)
r3<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+oilindex+
             factor(iso3c)+factor(year),rbst)

clse(r2,1,factor(rbst$iso3c))
AIC(r2);auc(as.numeric(r2$model$outcome>0),fitted(r2))[1]
clse(r3,1,factor(rbst$iso3c))
AIC(r3);auc(as.numeric(r3$model$outcome>0),fitted(r3))[1]

## Including income level
r4<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+gdp.l+
             factor(iso3c)+factor(year),rbst)
clse(r4,1,factor(rbst$iso3c))
AIC(r4);auc(as.numeric(r4$model$outcome>0),fitted(r4))[1]

## Accounting for food aid
r5<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+food.aid+
             factor(iso3c)+factor(year),rbst)
clse(r5,1,factor(rbst$iso3c))
AIC(r5);auc(as.numeric(r5$model$outcome>0),fitted(r5))[1]

## Effect of landlocked countries
r6<-glm.nb(outcome~index.a*landlocked+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),rbst)
clse(r6,1,factor(rbst$iso3c))
AIC(r6);auc(as.numeric(r6$model$outcome>0),fitted(r6))[1]

## Effect on oil exporters
r8<-glm.nb(outcome~index.a*oil.d+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),rbst)
clse(r8,1,factor(rbst$iso3c))
AIC(r8);auc(as.numeric(r8$model$outcome>0),fitted(r8))[1]

## Excluding Nigeria and South Africa
r9<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),rbst2)
clse(r9,1,factor(rbst2$iso3c))
AIC(r9);auc(as.numeric(r9$model$outcome>0),fitted(r9))[1]

## Excluding North Africa
r10<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),rbst3)
clse(r10,1,factor(rbst3$iso3c))
AIC(r10);auc(as.numeric(r10$model$outcome>0),fitted(r10))[1]

#### Table: Other outcome variables ####
re<-list()

## Excluding escalation
o1<-glm.nb(violence0~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[1]]<-clse(o1,1,o1$model[,7])
AIC(o10)
roc(as.numeric(o10$y>0),fitted(o10))
nobs(o10)

## No pro-government violence
o2<-glm.nb(violence.no.pg~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[2]]<-clse(o2,1,o2$model[,7])

## Minor violence
o3<-glm.nb(violence.m~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[3]]<-clse(o3,1,o3$model[,7])

## Non-violence
o4<-glm.nb(non~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[4]]<-clse(o4,1,o4$model[,7])

## Civil conflict
o5<-glm.nb(cw~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[5]]<-clse(o5,1,o5$model[,7])

## Government-targeted
o6<-glm.nb(gtarg~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[6]]<-clse(o6,1,o6$model[,7])

## All events
o7<-glm.nb(event~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[7]]<-clse(o7,1,o7$model[,7])

## All urban events
o8<-glm.nb(event.urban~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[8]]<-clse(o8,1,o8$model[,7])

## Violent urban events
o9<-glm.nb(violence.urban~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[9]]<-clse(o9,1,o9$model[,7])

## Violent rural events
o10<-glm.nb(violence.rural~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),outc)
re[[10]]<-clse(o10,1,o10$model[,7])

## Plot results
outc<-c("No escalation","No pro-government","Minor violence","Non-violent",
        "Civil conflict","Government-targeted","All unrest",
        "All urban unrest","Urban violence","Rural violence")
beta<-c()
se<-c()

for(i in 1:10){
  beta[i]=re[[i]][2,1]
  se[i]=re[[i]][2,2]
}

df<-data.frame(outcome=outc,beta=beta,se=se)
df$o<-df$beta-qnorm(0.975)*df$se
df<-df[order(-df$o),]
y.axis<-c(length(df[,3]):1)
x<-c(min(df$beta-qnorm(0.975)*df$se),max(df$beta+qnorm(0.975)*df$se))

## Plot 
par(mar=c(4,11.5,2,1),family="serif")
plot(df$beta,y.axis,type="n",axes=F,xlab="", ylab="",main="",xlim=x)
rect( 0.289986-qnorm(.975)*0.081859,0,
      0.289986+qnorm(.975)*0.081859,y.axis,col="grey90",lwd=0)
abline(v=0,lwd=1)
abline(h=y.axis,lty=2,lwd=.5,col="dimgrey")
abline(v=0,lty=2)

# Segments and points
segments(df$beta-qnorm(.975)*df$se,y.axis,df$beta+qnorm(.975)*df$se,y.axis,
         lwd=3,col="grey40",lend=1)
segments(df$beta-qnorm(.84)*df$se,y.axis,df$beta+qnorm(.84)*df$se,y.axis,
         lwd=4,col="grey20",lend=1)
points(df$beta,y.axis,type="p",xlab="", ylab="",main="", 
     pch=19,cex=1.5,col="black",xlim=x)

# Format axis
axis(1,cex.axis=1.5,tick=FALSE)
axis(2,at=y.axis,label=df$outcome,las=1,tick=F,mgp=c(2,.6,0),cex.axis=1.5) 

#### Table: Political heterogeneity ####

## Effect of countries in conflict
p1<-glm.nb(outcome~index.a*conflict+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),pol)
clse(p1,1,factor(pol$iso3c))
AIC(p1);roc(as.numeric(p1$model$outcome>0),fitted(p1))

## Autocracies
p2<-glm.nb(outcome~index.a*auto+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),pol)
clse(p2,1,factor(pol$iso3c))
AIC(p2);roc(as.numeric(p2$model$outcome>0),fitted(p2))

## Anocracies
p3<-glm.nb(outcome~index.a*border+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),pol)
clse(p3,1,factor(pol$iso3c))
AIC(p3);roc(as.numeric(p3$model$outcome>0),fitted(p3))

## Democratic change
p4<-glm.nb(outcome~index.a*change+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),pol)
clse(p4,1,factor(pol$iso3c))
AIC(p4);roc(as.numeric(p4$model$outcome>0),fitted(p4))

## Democracies
p5<-glm.nb(outcome~index.a*demo+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),pol)
clse(p5,1,factor(pol$iso3c))
AIC(p5);roc(as.numeric(p5$model$outcome>0),fitted(p5))

##### Table: Other food price measures ####

## Lag food price index
a1<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+index.a.l+
             factor(iso3c)+factor(year),lvl)
clse(a1,1,factor(lvl$iso3c))
AIC(a1);roc(as.numeric(a1$model$outcome>0),fitted(a1))

## Growth rates
a2<-glm.nb(outcome~index.g+violence.cl+gdp.ppp.gl+regime.l+pop.l+index.gl+
             factor(iso3c)+factor(year),lvl)
clse(a2,1,factor(lvl$iso3c))
AIC(a2);roc(as.numeric(a2$y>0),fitted(a2))

## 5-month growth rate
a3<-glm.nb(outcome~index.g5+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),lvl)
clse(a3,1,factor(lvl$iso3c))
AIC(a3);roc(as.numeric(a3$y>0),fitted(a3))

## Levels
a4<-glm.nb(outcome~index.dt+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             dt.l+dt.l2+dt.l3+dt.l4+dt.l5+
             factor(iso3c)+factor(year),lvl)
clse(a4,1,factor(lvl$iso3c))
AIC(a4);roc(as.numeric(a4$y>0),fitted(a4))

## Shocks
a5<-glm.nb(outcome~index.dt*fif.d+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),lvl)
clse(a5,1,factor(lvl$iso3c))
AIC(a5);roc(as.numeric(a5$y>0),fitted(a5))
