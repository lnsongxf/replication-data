#### Functions used for estimation and analysis ####

#### Standardise input variables ####
# See: 
# http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf
stan<-function(x){
  (x-mean(x,na.rm=TRUE))/(2*sd(x,na.rm=TRUE))  
}

#### Robust clustered standard errors ####
# Source:
# http://people.su.se/~ma/mcluster.R
# http://people.su.se/~ma/clustering.pdf

# Script written by Mahmood Arai
clse <- 
  function(fm, dfcw, cluster){
    # R-codes (www.r-project.org) for computing
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
    
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
    
    # reweighting the var-cov matrix for the within model
    library(sandwich);library(lmtest)
    M <- length(unique(cluster))   
    N <- length(cluster)           
    K <- fm$rank                        
    dfc <- (M/(M-1))*((N-1)/(N-K))  
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
    coeftest(fm, vcovCL) }
    
#### Minimalrug for plot ####
# Draw a rug plot, but ommit the baseline (actually, draw over it)
minimalrug <- function(x, lwd=0.7, tcl=0.3, ...) {
  # x:   a numeric vector
  # ...: parameters passed to rug()
  
  # Rounded ends don't work well with erasing one end
  oldlend <- par(lend = "butt")
  on.exit(par(oldlend))

  # Used for overwriting the axis line to leave tickmarks
  bg <- par("bg")
  if (bg == "transparent")
    bg <- "white"

  # Draw the rug
  rug(x, ticksize=NA, lwd=lwd, ...)
  # Acrobat shows "shadows" around a line erased with a line
  #  of similar width, so use a thicker line
  overlwd=1
  # Remove the baseline (... is put first to allow other the other
  #  parameters to override it)
  axis(..., at=x, col=bg, tcl=0, label=FALSE, lwd=overlwd)
}

#### Plot prediction intervals ####
predPlot<-function(x){
  fig<-data.frame(x)
  # Calculate intervals
  fig$upr<-fig$fit+qnorm(.975)*fig$se.fit
  fig$lwr<-fig$fit-qnorm(.975)*fig$se.fit
  
  # Aggregate data
  fig$iso3c<-df.n$iso3c
  fig<-ddply(fig,.(iso3c),summarise,
            upr=sum(upr),lwr=sum(lwr))
  
  yhat<-merge(obs,fig)
  yhat<-na.omit(yhat[order(yhat$lwr,yhat$violence),])
  
  # Plot
  par(mar=c(4.5,4.5,1,3),family="serif",pty="m",mgp=c(5,1,0))
  plot(1+yhat$violence,1:length(yhat$violence),type="n",log="x",
       axes=FALSE,xlab="Number of violent events",ylab="",main="",cex.lab=1.5)
  segments(1+yhat$lwr,1:length(yhat$violence),
          1+yhat$upr,lwd=10,col=alpha("grey",.3),lend=2)
  points(1+yhat$violence,1:length(yhat$violence),pch=19,cex=.9)
  
  axis(1,tick=F,cex=1.5)
  axis(2,at=1:nrow(yhat),label=yhat$name,las=1,tick=F,mgp=c(1,0,0),cex.axis=.8) 
}
