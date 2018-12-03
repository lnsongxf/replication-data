#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Fit Poisson model
# Last update 2018 03 22
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
source("code/load.R")       # data

# 1) Poisson model
P1<-function (){
  for(i in 1:N){
    log(lambda[i])<- a0+b1*conflict.l[i]+b2*conflict.w[i]+b3*rain[i]
    
    conflict[i]~dpois(lambda[i])     
    fitted[i]<-lambda[i] # Predicted values
  }  
  
  # Priors
  a0~dnorm(0,.01)   # Intercept
  b1~dnorm(0,.01)   # Temporal lag
  b2~dnorm(0,.01)   # Spatial lag
  b3~dnorm(0,.01)   # Rainfall
}

# 2) Estimate model
set.seed(42);p<-jags(data=list(conflict=X$events.com,conflict.l=X$events.com.l,
                                conflict.w=X$W.events.com,rain=X$belg.g,N=N),
                      parameters.to.save=c("a0","b1","b2","b3"),
                      model.file=P1,n.chains=3,n.iter=2500,
                      n.burnin=1000,n.thin=2)
print(p)

# 3) Plot distribution
load("output/fitted_models.RData")

a<-as.vector(m1$BUGSoutput$sims.list$b3)
b<-as.vector(p$BUGSoutput$sims.list$b3)
par(mar=c(5,5,3,2),las=1,bty='n',cex.lab=2,cex.axis=2,cex.main=2)
plot(density(a,from=-3.2,to=0.21),ylim=c(0,4.5),axes=F,xlab='',ylab='',main='',
     lwd=2)
lines(density(b),col='steelblue4',lwd=2)
axis(1,tick=F)

## FIN