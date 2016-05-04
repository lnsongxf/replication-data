#### Data preparation for regression analysis ####
setwd("[DIR]/Publications/OEP_2016")
set.seed(42); runif(1)

## Libraries
library(countrycode)
library(MASS)         
library(pROC)         
library(psych)        
library(scales)
library(Zelig)

## Data
load("tidy_data/allData.Rdata") 
allData<-allData[order(allData$iso3c,allData$date),]

#### Main table ####
mvars<-c("iso3c","year","date","month","violence","violence.l","event.cl",
         "violence.cl","index.a","gdp.ppp.gl","regime.l","pop.l",
         "oilindex","oil.dt")
main<-allData[,mvars]
main$outcome<-main$violence
main[main$violence>=1 & main$violence.l>=1,]$outcome<-NA
main[6:14]<-lapply(main[6:14],stan)
main<-na.omit(main) # Keep number of observations steady
ccode<-as.vector(factor(main$iso3c))

#### Timing of effect ####
tvars<-c("iso3c","year","date","month","violence","violence.l","index.a",
         "index.a.l","index.a.l2","index.a.l3","index.a.l4","index.a.l5",
         "index.a.l6","index.a.l7","index.a.l8","index.a.l9","index.a.l10",
         "index.a.f","index.a.f2","index.a.f3","index.a.f4","index.a.f5",
         "index.a.f6","index.a.f7","index.a.f8","index.a.f9","index.a.f10")
timing<-allData[,tvars]
timing$outcome<-timing$violence
timing[timing$violence>=1 & timing$violence.l>=1,]$outcome<-NA
timing[c(-1:-6,-28)]<-lapply(timing[c(-1:-6,-28)],stan)
timing<-na.omit(timing) 

#### Different specifications FPI ####
svars<-c("iso3c","year","date","month","violence","violence.l",
         "violence.cl","oilindex","oil.dt","gdp.ppp.gl","pop.l","regime.l",
         "N.a","R.a","G.a","UN.a","C.a","Food.a","Cer.a","W.a",
         "imports.a","exports.a")
spec<-allData[,svars]
spec[is.na(spec$exports.a),]$exports.a<-0
spec$outcome<-spec$violence
spec[spec$violence>=1 & spec$violence.l>=1,]$outcome<-NA
spec[7:22]<-lapply(spec[7:22],stan)
spec<-na.omit(spec) 

#### Other measures for food prices ####
fvars<-c("iso3c","year","date","month","violence","violence.l","event.cl",
         "violence.cl","index.a","gdp.ppp.gl","regime.l","pop.l",
         "index.dt","dt.l","dt.l2","dt.l3","dt.l4","dt.l5",
         "index.g","index.gl","index.a.l","index.g3","index.g5","fif.d")
lvl<-allData[,fvars]
lvl$outcome<-lvl$violence
lvl[lvl$violence>=1 & lvl$violence.l>=1,]$outcome<-NA

lvl[7:23]<-lapply(lvl[7:23],stan)
lvl<-na.omit(lvl) 

#### Different estimation methods ####
main$outcome.b<-as.numeric(main$outcome>0)
main$outcome.ln<-log(1+main$outcome)

#### Other outcome variables ####
dvars<-c("iso3c","year","date","violence0","violence.no.pg","violence.m",
         "non","cw","gtarg","event","event.urban","violence.urban",
         "violence.rural","event.cl","violence.cl","index.a","gdp.ppp.gl",
         "regime.l","pop.l","violence0.l","violence.no.pg.l","violence.m.l",
         "non.l","cw.l","gtarg.l","event.l","event.urban.l","violence.urban.l",
         "violence.rural.l")
outc<-allData[,dvars]

outc[outc$violence0>=1 & outc$violence0.l>=1,]$violence0<-NA
outc[outc$violence.no.pg>=1 & outc$violence.no.pg.l>=1,]$violence.no.pg<-NA
outc[outc$violence.m>=1 & outc$violence.m.l>=1,]$violence.m<-NA
outc[outc$non>=1 & outc$non.l>=1,]$non<-NA
outc[outc$cw>=1 & outc$cw.l>=1,]$cw<-NA
outc[outc$gtarg>=1 & outc$gtarg.l>=1,]$gtarg<-NA
outc[outc$event>=1 & outc$event.l>=1,]$event<-NA
outc[outc$event.urban>=1 & outc$event.urban.l>=1,]$event.urban<-NA
outc[outc$violence.urban>=1 & outc$violence.urban.l>=1,]$violence.urban<-NA
outc[outc$violence.rural>=1 & outc$violence.rural.l>=1,]$violence.rural<-NA

outc<-outc[,-20:-29]
outc[14:19]<-lapply(outc[14:19],stan)

#### Robustness checks and country characteristics ####
rvars<-c("iso3c","year","date","month","violence","violence.l","event.cl",
         "violence.cl","index.a","gdp.ppp.gl","regime.l","pop.l","gdp.l",
         "oilindex","oil.dt","oil.d","food.aid","landlocked")
rbst<-allData[,rvars]
incd<-na.omit(rbst) # For estimation with incidence indicator

rbst$outcome<-rbst$violence
rbst[rbst$violence>=1 & rbst$violence.l>=1,]$outcome<-NA

rbst2<-rbst[rbst$iso3c!="NGA" & rbst$iso3c!="ZAF",] 
rbst3<-rbst[rbst$iso3c!="MAR" & rbst$iso3c!="DZA" & rbst$iso3c!="TUN" &
              rbst$iso3c!="LBY" & rbst$iso3c!="EGY",]

rbst[6:16]<-lapply(rbst[6:16],stan)
rbst2[6:16]<-lapply(rbst2[6:16],stan)
rbst3[6:16]<-lapply(rbst3[6:16],stan)
incd[6:16]<-lapply(incd[6:16],stan)

rbst<-na.omit(rbst)
rbst2<-na.omit(rbst2)
rbst3<-na.omit(rbst3)

#### Political heterogeneity ####
pvars<-c("iso3c","year","date","month","violence","violence.l","event.cl",
         "violence.cl","index.a","gdp.ppp.gl","regime.l","pop.l","conflict",
         "d_autoc","d_democ","d_full","d_partial","d_border","d_reverse",
         "d_ev")
pol<-allData[,pvars]
pol$outcome<-pol$violence
pol[pol$violence>=1 & pol$violence.l>=1,]$outcome<-NA
pol[6:12]<-lapply(pol[6:12],stan)

pol$auto<-as.numeric(pol$d_autoc==1|pol$d_reverse==1)
pol$demo<-as.numeric(pol$d_democ==1|pol$d_full==1)
pol$border<-as.numeric(pol$d_partial==1 | pol$d_border==1)
pol$change<-as.numeric(pol$d_partial==1 | pol$d_border==1|
                     pol$d_reverse==1| pol$d_full==1)

pol<-na.omit(pol)

#### Simple model ####
simple<-allData[,c("iso3c","violence","violence.l","index.a","regime.l","year")]
simple[,-1:-2]<-lapply(simple[,-1:-2],stan)

#### Data frame with short country names ####
country<-data.frame(ccode=unique(allData$iso3c))
country$name<-countrycode(country$ccode,"iso3c","country.name",warn=TRUE)

## Add country names
country[country$name %in% "Congo, the Democratic Republic of the"
        , "name"]<-"DRC" 
country[country$name %in% "Tanzania, United Republic of"
        , "name"]<-"Tanzania"
country[country$name %in% "Libyan Arab Jamahiriya"  
        , "name"]<-"Libya"
country[country$name %in% "Congo, Republic of"
        , "name"]<-"Congo"
country[country$name %in% "Central African Republic"
        , "name"]<-"CAR"
