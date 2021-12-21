dir()

Lati <- read.table("R.latirostris.txt", header=T)
str(Lati)
Lati$CP
Lati$P
Lati$ANO
Lati$Longitude
Lati$Latitude
Lati$Amb

Lati

summary(Lati)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 3106   Min.   : 4.800   Min.   : 1.000   Min.   : 1.00   Min.   : 1.000   Min.   :1999  
## 1st Qu.: 4608   1st Qu.: 6.600   1st Qu.: 1.250   1st Qu.: 5.25   1st Qu.: 3.250   1st Qu.:2001  
## Median : 8020   Median : 8.100   Median : 3.500   Median :15.50   Median : 8.000   Median :2004  
## Mean   :10424   Mean   : 8.629   Mean   : 7.235   Mean   :14.24   Mean   : 6.882   Mean   :2006  
## 3rd Qu.:15239   3rd Qu.: 9.975   3rd Qu.: 9.000   3rd Qu.:21.00   3rd Qu.:10.000   3rd Qu.:2011  
## Max.   :21102   Max.   :19.600   Max.   :58.000   Max.   :30.00   Max.   :12.000   Max.   :2013  
##
## state               Amb              Longitude         Latitude     
## Length:34          Length:34          Min.   :-51.30   Min.   :-24.84  
## Class :character   Class :character   1st Qu.:-50.02   1st Qu.:-21.74  
## Mode  :character   Mode  :character   Median :-49.46   Median :-21.02  
##                                       Mean   :-49.13   Mean   :-21.41  
##                                       3rd Qu.:-48.51   3rd Qu.:-20.97  
##                                       Max.   :-46.82   Max.   :-19.54 


## R.latirostris_Geral - teste de normalidade --------------------------------##
shapiro.test(Lati$CP) ## W = 0.86998, p-value = 0.0008093
shapiro.test(Lati$P)  ## W = 0.58799, p-value = 1.382e-08
shapiro.test(Lati$ANO) ## W = 0.86703, p-value = 0.0006889


## R.latirostris_Geral - cor e cov -------------------------------------------##
cor(Lati$ANO, Lati$CP) ## 0.4656765
cor(Lati$ANO, Lati$P)  ## 0.3657144
cor(Lati$ANO, Lati$CP, method="spearman") ## 0.5186777
cor(Lati$ANO, Lati$P, method="spearman")  ## 0.5645945

cov(Lati$ANO, Lati$CP) ## 6.86738
cov(Lati$ANO, Lati$P)  ## 19.13904


## R.latirostris_Geral - CP --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Lati.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.cp.gamm.exp$gam)
summary(model.Lati.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Lati.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati)
summary(model.Lati.cp.gamm.lin$gam)
summary(model.Lati.cp.gamm.lin$lme)

model.Lati.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati)

model.Lati.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati)

model.Lati.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Lati.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.cp.gam.exp$gam)
summary(model.Lati.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Lati.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.cp.gamm.exp$gam)
summary(model.Lati.cp.gamm.exp$lme)

model.Lati.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati)
summary(model.Lati.cp.gamm.lin$gam)
summary(model.Lati.cp.gamm.lin$lme)

model.Lati.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati)

model.Lati.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati)

model.Lati.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati)



### modelos log sem componente randômico e com correlação espacial ###

model.Lati.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.cp.gam.exp$gam)
summary(model.Lati.cp.gam.exp$lme)




## R.latirostris_Geral - P ---------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Lati.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.p.gamm.exp$gam)
summary(model.Lati.p.gamm.exp$lme)

model.Lati.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati)
summary(model.Lati.p.gamm.lin$gam)
summary(model.Lati.p.gamm.lin$lme)

model.Lati.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati)

model.Lati.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati)

model.Lati.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Lati.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.p.gam.exp$gam)
summary(model.Lati.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Lati.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.p.gamm.exp$gam)
summary(model.Lati.p.gamm.exp$lme)

model.Lati.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati)
summary(model.Lati.p.gamm.lin$gam)
summary(model.Lati.p.gamm.lin$lme)

model.Lati.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati)

model.Lati.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati)

model.Lati.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati)



### modelos log sem componente randômico e com correlação espacial ###

model.Lati.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati)
summary(model.Lati.p.gam.exp$gam)
summary(model.Lati.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Lati2 <- read.table("R.latirostris_AmbPeque.txt", header=T)
str(Lati2)
Lati2$CP
Lati2$P
Lati2$ANO
Lati2$Longitude
Lati2$Latitude
Lati2$Amb

Lati2

summary(Lati2)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 3106   Min.   : 5.300   Min.   : 1.000   Min.   : 1.00   Min.   : 1.000   Min.   :1999  
## 1st Qu.: 3905   1st Qu.: 6.225   1st Qu.: 1.000   1st Qu.: 5.25   1st Qu.: 3.000   1st Qu.:2000  
## Median : 6277   Median : 6.750   Median : 2.000   Median :11.50   Median : 7.500   Median :2003  
## Mean   : 7977   Mean   : 7.941   Mean   : 5.727   Mean   :13.91   Mean   : 6.227   Mean   :2004  
## 3rd Qu.:13391   3rd Qu.: 8.825   3rd Qu.: 5.500   3rd Qu.:23.75   3rd Qu.: 9.500   3rd Qu.:2006  
## Max.   :19227   Max.   :19.600   Max.   :58.000   Max.   :30.00   Max.   :12.000   Max.   :2013  
## state               Amb              Longitude         Latitude     
## Length:22          Length:22          Min.   :-50.02   Min.   :-24.05  
## Class :character   Class :character   1st Qu.:-50.02   1st Qu.:-21.05  
## Mode  :character   Mode  :character   Median :-49.46   Median :-21.02  
##                                       Mean   :-49.35   Mean   :-21.25  
##                                       3rd Qu.:-49.28   3rd Qu.:-20.97  
##                                       Max.   :-46.82   Max.   :-20.97


## R.latirostris_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Lati2$CP) ## W = 0.68608, p-value = 1.309e-05
shapiro.test(Lati2$P)  ## W = 0.40321, p-value = 1.824e-08
shapiro.test(Lati2$ANO) ## W = 0.82476, p-value = 0.001254


## R.latirostris_AmbPeque - cor e cov ----------------------------------------##
cor(Lati2$ANO, Lati2$CP) ## 0.3443308
cor(Lati2$ANO, Lati2$P)  ## 0.2628919
cor(Lati2$ANO, Lati2$CP, method="spearman") ## 0.3492801
cor(Lati2$ANO, Lati2$P, method="spearman")  ## 0.3462233

cov(Lati2$ANO, Lati2$CP) ## 4.872511
cov(Lati2$ANO, Lati2$P)  ## 14.67532


## R.latirostris_AmbPeque - CP -----------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Lati2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.cp.gamm.exp$gam)
summary(model.Lati2.cp.gamm.exp$lme)

## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##   A term has fewer unique covariate combinations than specified maximum degrees of freedom

model.Lati2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati2)
summary(model.Lati2.cp.gamm.lin$gam)
summary(model.Lati2.cp.gamm.lin$lme)

model.Lati2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati2)

model.Lati2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati2)

model.Lati2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Lati2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.cp.gam.exp$gam)
summary(model.Lati2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Lati2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.cp.gamm.exp$gam)
summary(model.Lati2.cp.gamm.exp$lme)

model.Lati2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati2)
summary(model.Lati2.cp.gamm.lin$gam)
summary(model.Lati2.cp.gamm.lin$lme)

model.Lati2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati2)

model.Lati2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati2)

model.Lati2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati2)



### modelos log sem componente randômico e com correlação espacial ###

model.Lati2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.cp.gam.exp$gam)
summary(model.Lati2.cp.gam.exp$lme)




## R.latirostris_AmbPeque - P ------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Lati2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.p.gamm.exp$gam)
summary(model.Lati2.p.gamm.exp$lme)

model.Lati2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati2)
summary(model.Lati2.p.gamm.lin$gam)
summary(model.Lati2.p.gamm.lin$lme)

model.Lati2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati2)

model.Lati2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati2)

model.Lati2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Lati2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.p.gam.exp$gam)
summary(model.Lati2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Lati2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.p.gamm.exp$gam)
summary(model.Lati2.p.gamm.exp$lme)

model.Lati2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Lati2)
summary(model.Lati2.p.gamm.lin$gam)
summary(model.Lati2.p.gamm.lin$lme)

model.Lati2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Lati2)

model.Lati2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Lati2)

model.Lati2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Lati2)



### modelos log sem componente randômico e com correlação espacial ###

model.Lati2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Lati2)
summary(model.Lati2.p.gam.exp$gam)
summary(model.Lati2.p.gam.exp$lme)

