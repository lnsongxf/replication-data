#### Regression analysis ####
# This version:   26-11-2015
# First version:  13-08-2013
rm(list=ls(all=TRUE))             # Clear workspace
setwd("[DIR]/Publications/OEP_2016") 
options(scipen=4)                 

source("code/functions.R") 
source("code/clean.R")

##### Table: Main regression results ####

## Parsimonuous model, NB-GLM
m1<-glm.nb(outcome~index.a,main)
clse(m1,1,factor(main$iso3c))
summary(m1)
AIC(m1);roc(as.numeric(m1$model$outcome>0),fitted(m1))

## Include country and time indicators
m2<-glm.nb(outcome~index.a+factor(iso3c)+factor(year)+factor(iso3c)*year,main)
clse(m2,1,m2$model[,3])
summary(m2)
AIC(m2);roc(as.numeric(m2$model$outcome>0),fitted(m2))

## Include other explanatory variables
m3<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l,main)
clse(m3,1,factor(main$iso3c))
summary(m3)
AIC(m3);roc(as.numeric(m3$model$outcome>0),fitted(m3))

# Full model, NB-GLM
m4<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),main)
clse(m4,1,factor(main$iso3c))
summary(m4)
AIC(m4);roc(as.numeric(m4$model$outcome>0),fitted(m4))

#### Timing of effect ####
## Estimate model
t1<-glm.nb(outcome~index.a+index.a.l+index.a.l2+index.a.l3+index.a.l4+
             index.a.l5+index.a.l6+index.a.l7+index.a.l8+index.a.l9+
             index.a.l10+index.a.f+index.a.f2+index.a.f3+index.a.f4+
             index.a.f5+index.a.f6+index.a.f7+index.a.f8+index.a.f9+
             index.a.f10+factor(iso3c)+factor(year),timing)

## Get estimates 
tr<-clse(t1,1,factor(timing$iso3c))
est<-data.frame(tr[2:22,1:2])
colnames(est)<-c("estimate","error")

## Calculate 95% interval
est$upper<-est$estimate+qnorm(.975)*est$error
est$lower<-est$estimate-qnorm(.975)*est$error

est$order<-c(11:1,12:21)
est<-est[order(est$order),]

## Plot results
par(mar=c(5,5,2,2),family="serif")
plot(est$estimate,ylim=c(min(est$lower),max(est$upper)),type="n",
     axes=FALSE,xlab="",ylab="")
abline(h=0,lwd=2,col="black")
abline(v=11,lwd=1,lty=2)
polygon(c(rev(est$order),est$order),
        c(rev(est$lower),est$upper),col=adjustcolor("black",alpha.f=0.1), border = NA)
lines(est$order,est$estimate,lwd=2,type="b")
axis(1,cex.axis=1.2,tick=FALSE,at=1:21,
     label=c("t-10","t-9","t-8","t-7","t-6","t-5","t-4","t-3","t-2","t-1","t",
             "t+1","t+2","t+3","t+4","t+5","t+6","t+7","t+8","t+9","t+10"))
axis(2,cex.axis=1.5,tick=FALSE,las=1)
mtext(expression(beta),2,line=3,cex=1.7,las=1)

#### Effect of sample period ####

## End year
beta<-c()
err<-c()
yr<-rev(as.vector(unique(main$year)))
yr<-yr[1:(length(yr)-2)]
N<-length(yr)

for(i in 1:N){
  mt<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),main[main$year<=yr[i],])
  beta[i]=clse(mt,1,factor(main[main$year<=yr[i],]$iso3c))[2,1]
  err[i]=clse(mt,1,factor(main[main$year<=yr[i],]$iso3c))[2,2] 
}

hi<-beta+qnorm(.975)*err
lo<-beta-qnorm(.975)*err

## Plot results
par(mar=c(5,5,2,2),family="serif")
plot(yr,beta,ylim=c(min(lo),max(hi)),type="n",
     axes=FALSE,xlab="",ylab="")
abline(h=0,lwd=2,col="black",lty=2)
polygon(c(rev(yr),yr),
        c(rev(lo),hi),col=adjustcolor("black",alpha.f=0.1), border = NA)
lines(yr,beta,lwd=2,type="b")
axis(1,cex.axis=1.5,tick=FALSE,at=1992:2011)
mtext("End year (start year = 1990)",1,line=3,cex=1.7) 
axis(2,cex.axis=1.5,tick=FALSE,las=1)
mtext(expression(beta),2,line=3,cex=1.7,las=1)

## Start year
beta<-c()
err<-c()
yr<-as.vector(unique(main$year))
yr<-yr[1:(length(yr)-2)]
N<-length(yr)

for(i in 1:N){
  mt<-glm.nb(outcome~index.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),main[main$year>=yr[i],])
  beta[i]=clse(mt,1,factor(main[main$year>=yr[i],]$iso3c))[2,1]
  err[i]=clse(mt,1,factor(main[main$year>=yr[i],]$iso3c))[2,2] 
}

hi<-beta+qnorm(.975)*err
lo<-beta-qnorm(.975)*err

## Plot results
par(mar=c(5,5,2,2),family="serif")
plot(yr,beta,ylim=c(min(lo),max(hi)),type="n",
     axes=FALSE,xlab="",ylab="")
abline(h=0,lwd=2,col="black",lty=2)
polygon(c(rev(yr),yr),
        c(rev(lo),hi),col=adjustcolor("black",alpha.f=0.1), border = NA)
lines(yr,beta,lwd=2,type="b")
axis(1,cex.axis=1.2,tick=FALSE,at=1990:2009)
mtext("Start year (end year = 2011)",1,line=3,cex=1.5) 
axis(2,cex.axis=1.2,tick=FALSE,las=1)
mtext(expression(beta),2,line=3,cex=1.5,las=1)

#### Different specifications of the FPI ####
index<-c("N.a","R.a","G.a","UN.a","C.a","Food.a","Cer.a","W.a")
expl<-c("violence.cl+gdp.ppp.gl+regime.l+pop.l+factor(iso3c)+factor(year)")
beta.i<-c()
se.i<-c()
N<-length(index)
s<-list()

for(i in 1:N){
  # Define formula and estimate model
  form<-paste("outcome~",paste(index[i],expl,sep="+"))
  s[[i]]<-glm.nb(form,spec)
  
  
  # Write estimates to vector
  beta.i[i]=clse(s[[i]],1,factor(spec$iso3c))[2,1]
  se.i[i]=clse(s[[i]],1,factor(spec$iso3c))[2,2]
}

## Add specification with imports and exports
s0<-glm.nb(outcome~imports.a+exports.a+violence.cl+gdp.ppp.gl+regime.l+pop.l+
             factor(iso3c)+factor(year),spec)
beta.i[9]<-clse(s0,1,factor(spec$iso3c))[2,1]
beta.i[10]<-clse(s0,1,factor(spec$iso3c))[3,1]
se.i[9]=clse(s0,1,factor(spec$iso3c))[2,2]
se.i[10]=clse(s0,1,factor(spec$iso3c))[3,2]

v.names<-c("Nominal prices","Real prices","Average weight","Trade balance",
           "No weights","Generic food price index","Main cereals","Wheat only",
           "Imports","Exports")

## Plot results
d<-data.frame(coef=beta.i,se=se.i,var.names=v.names)
d<-d[order(-d$coef),]
y.axis<-c(length(d[,3]):1)
x<-c(min(d$coef-qnorm(0.975)*d$se),max(d$coef+qnorm(0.975)*d$se))

## Plot 
par(mar=c(5,13.5,6,1),family="serif")
plot(d$coef,y.axis,type="n",axes=F,xlab="", ylab="",main="",xlim=x)
rect( 0.289986-qnorm(.975)*0.081859,0,
      0.289986+qnorm(.975)*0.081859,y.axis,col="grey90",lwd=0)
segments(d$coef-qnorm(.975)*d$se,y.axis,d$coef+qnorm(.975)*d$se,y.axis,
         lwd=3,col="grey40",lend=1)
segments(d$coef-qnorm(.84)*d$se,y.axis,d$coef+qnorm(.84)*d$se,y.axis,
         lwd=4,col="grey20",lend=1)
points(d$coef,y.axis,type="p",xlab="", ylab="",main="", 
     pch=19,cex=1.5,col="black",xlim=x)

# Format axis
axis(1,at=seq(-1,1,.1),labels=seq(-1,1,.1),cex.axis=1.5,tick=FALSE)
abline(h=y.axis,lty=2,lwd=.5,col="dimgrey")
abline(v=0)
axis(2,at=y.axis,label=d$var.names,las=1,tick=F,mgp=c(2,.6,0),cex.axis=1.5) 
minimalrug(d$coef, side=1, line=.5,lwd=2)

#### FIN ####
