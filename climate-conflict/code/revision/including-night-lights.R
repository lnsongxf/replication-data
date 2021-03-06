#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Accounting for local wealth levels
# Request from R1
# Using nigh light emissions
# Last update 2018 05 03
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source("code/load.R")
library(plyr)

# Functions 
stan<-function(x){(x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))}
ihs<-function(x){log(x + sqrt(x^2 + 1))}

# 1) Prepare data
load('data/night_lights.RData')

# Aggregate
d<-nlight[nlight$year %in% c(1992,1993,1997,1998),]
d$period<-ifelse(d$year>1993,1,0)
d<-ddply(d,.(nrow,period),summarise,value=mean(value))
d<-data.frame(nrow=unique(d$nrow),
              nlight0=d$value[d$period==0],
              nlight1=d$value[d$period==1])

# Inverse hyperbolice sine transformation and standardise
d$nightlight<-stan(ihs(d$nlight1)-ihs(d$nlight0))

# Need to add row names
load('raw_data/gaul_adm2.RData')
n<-data.frame(nrow=1:nrow(gaul),row_name=as.numeric(row.names(gaul)))
d<-merge(d,n)

X<-merge(X,d[,c('row_name','nightlight')])

# 2) Specify model
M<-function (){
  for(i in 1:N){
    mu[i]<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]+ 
      b4*pop[i]+b5*lz[i] +b6*noaa[i]
    lambda[i]<-exp(mu[i])
    p[i]<-r/(r+lambda[i])
    conflict[i]~dnegbin(p[i],r)     
    fitted[i]<-mu[i] # Predicted values
  }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b3~dnorm(0,.01)   # Rainfall
  b4~dnorm(0,.01)   # Population density
  b5~dnorm(0,.01)   # Livelihood zone
  b6~dnorm(0,.01)   # Night light emissions
  r~dgamma(.01,.01) # Gamma distribution 
}

# 3) Estimate model
set.seed(42);nl<-jags(data=list(conflict=X$events.com,
                                conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,
                                pop=X$p00.km2,lz=X$pastoral,noaa=X$nightlight,
                               N=N),
                      parameters.to.save=c("a0","b1","b2","b3","b4","b5","b6",
                                           "fitted"),
                      model.file=M,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)

# Results
print(nl$BUGSoutput$summary[1:8,c(1,2,3,7)],digits.summary=3) # Col. 4
save.image('output/accounting_local_wealth.RData')

## FIN