dir()

Macu <- read.table("P.maculatus.txt", header=T)
str(Macu)
Macu$CP
Macu$P
Macu$ANO
Macu$Longitude
Macu$Latitude
Macu$Amb

Macu

summary(Macu)
## catalognumber         CP              P               day            month             ANO      
## Min.   : 1272   Min.   : 3.70   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1900  
## 1st Qu.: 9022   1st Qu.: 5.70   1st Qu.:  4.00   1st Qu.: 6.25   1st Qu.: 2.000   1st Qu.:2006  
## Median :11178   Median : 9.60   Median : 16.00   Median :27.00   Median : 7.000   Median :2006  
## Mean   :13080   Mean   :10.31   Mean   : 34.89   Mean   :18.39   Mean   : 5.553   Mean   :2000  
## 3rd Qu.:18973   3rd Qu.:14.00   3rd Qu.: 53.75   3rd Qu.:27.00   3rd Qu.: 8.000   3rd Qu.:2013  
## Max.   :21344   Max.   :25.50   Max.   :252.00   Max.   :30.00   Max.   :12.000   Max.   :2017  
## state               Amb              Longitude         Latitude     
## Length:38          Length:38          Min.   :-53.06   Min.   :-23.87  
## Class :character   Class :character   1st Qu.:-51.30   1st Qu.:-22.37  
## Mode  :character   Mode  :character   Median :-50.23   Median :-20.28  
##                                       Mean   :-49.85   Mean   :-21.03  
##                                       3rd Qu.:-49.24   3rd Qu.:-20.28  
##                                       Max.   :-44.35   Max.   :-16.97


## P.maculatus_Geral - teste de normalidade ----------------------------------##
shapiro.test(Macu$CP) ## W = 0.91479, p-value = 0.00685
shapiro.test(Macu$P)  ## W = 0.68837, p-value = 1.078e-07
shapiro.test(Macu$ANO) ## W = 0.41132, p-value = 3.52e-11


## P.maculatus_Geral - cor e cov ---------------------------------------------##
cor(Macu$ANO, Macu$CP) ## -0.1795058
cor(Macu$ANO, Macu$P)  ## -0.3820677
cor(Macu$ANO, Macu$CP, method="spearman") ## 0.356876
cor(Macu$ANO, Macu$P, method="spearman")  ## 0.3354065

cov(Macu$ANO, Macu$CP) ## -29.52546
cov(Macu$ANO, Macu$P)  ## -560.3812


## P.maculatus_Geral - CP ----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Macu.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.cp.gamm.exp$gam)
summary(model.Macu.cp.gamm.exp$lme)

## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##   A term has fewer unique covariate combinations than specified maximum degrees of freedom

model.Macu.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu)
summary(model.Macu.cp.gamm.lin$gam)
summary(model.Macu.cp.gamm.lin$lme)

model.Macu.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu)

model.Macu.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu)

model.Macu.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Macu.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.cp.gam.exp$gam)
summary(model.Macu.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Macu.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.cp.gamm.exp$gam)
summary(model.Macu.cp.gamm.exp$lme)

model.Macu.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu)
summary(model.Macu.cp.gamm.lin$gam)
summary(model.Macu.cp.gamm.lin$lme)

model.Macu.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu)

model.Macu.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu)

model.Macu.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu)



### modelos log sem componente randômico e com correlação espacial ###

model.Macu.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.cp.gam.exp$gam)
summary(model.Macu.cp.gam.exp$lme)




## P.maculatus_Geral - P -----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Macu.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.p.gamm.exp$gam)
summary(model.Macu.p.gamm.exp$lme)

model.Macu.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu)
summary(model.Macu.p.gamm.lin$gam)
summary(model.Macu.p.gamm.lin$lme)

model.Macu.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu)

model.Macu.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu)

model.Macu.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Macu.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.p.gam.exp$gam)
summary(model.Macu.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Macu.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.p.gamm.exp$gam)
summary(model.Macu.p.gamm.exp$lme)

model.Macu.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu)
summary(model.Macu.p.gamm.lin$gam)
summary(model.Macu.p.gamm.lin$lme)

model.Macu.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu)

model.Macu.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu)

model.Macu.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu)



### modelos log sem componente randômico e com correlação espacial ###

model.Macu.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu)
summary(model.Macu.p.gam.exp$gam)
summary(model.Macu.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Macu2 <- read.table("P.maculatus_AmbPeque.txt", header=T)
str(Macu2)
Macu2$CP
Macu2$P
Macu2$ANO
Macu2$Longitude
Macu2$Latitude
Macu2$Amb

Macu2

summary(Macu2)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 5474   Min.   : 3.700   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :2003  
## 1st Qu.: 9022   1st Qu.: 5.475   1st Qu.:  3.25   1st Qu.:11.00   1st Qu.: 3.250   1st Qu.:2006  
## Median :11178   Median : 9.600   Median : 16.00   Median :27.00   Median : 8.000   Median :2006  
## Mean   :13354   Mean   :10.021   Mean   : 29.79   Mean   :19.76   Mean   : 5.971   Mean   :2009  
## 3rd Qu.:18973   3rd Qu.:14.000   3rd Qu.: 53.75   3rd Qu.:27.00   3rd Qu.: 8.000   3rd Qu.:2013  
## Max.   :21344   Max.   :25.500   Max.   :120.00   Max.   :30.00   Max.   :12.000   Max.   :2017  
## state               Amb              Longitude         Latitude     
## Length:34          Length:34          Min.   :-51.30   Min.   :-23.53  
## Class :character   Class :character   1st Qu.:-51.30   1st Qu.:-21.77  
## Mode  :character   Mode  :character   Median :-50.29   Median :-20.28  
##                                       Mean   :-50.06   Mean   :-20.89  
##                                       3rd Qu.:-49.24   3rd Qu.:-20.28  
##                                       Max.   :-45.90   Max.   :-16.97


## P.maculatus_AmbPeque - teste de normalidade -------------------------------##
shapiro.test(Macu2$CP) ## W = 0.90512, p-value = 0.006282
shapiro.test(Macu2$P)  ## W = 0.8008, p-value = 2.655e-05
shapiro.test(Macu2$ANO) ## W = 0.83824, p-value = 0.0001545


## P.maculatus_AmbPeque - cor e cov ------------------------------------------##
cor(Macu2$ANO, Macu2$CP) ## 0.5380568
cor(Macu2$ANO, Macu2$P)  ## 0.2818733
cor(Macu2$ANO, Macu2$CP, method="spearman") ## 0.5352451
cor(Macu2$ANO, Macu2$P, method="spearman")  ## 0.512899

cov(Macu2$ANO, Macu2$CP) ## 11.44474
cov(Macu2$ANO, Macu2$P)  ## 38.24955


## P.maculatus_AmbPeque - CP -------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Macu2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.cp.gamm.exp$gam)
summary(model.Macu2.cp.gamm.exp$lme)

## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##   A term has fewer unique covariate combinations than specified maximum degrees of freedom

model.Macu2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu2)
summary(model.Macu2.cp.gamm.lin$gam)
summary(model.Macu2.cp.gamm.lin$lme)

model.Macu2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu2)

model.Macu2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu2)

model.Macu2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Macu2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.cp.gam.exp$gam)
summary(model.Macu2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Macu2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.cp.gamm.exp$gam)
summary(model.Macu2.cp.gamm.exp$lme)

model.Macu2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu2)
summary(model.Macu2.cp.gamm.lin$gam)
summary(model.Macu2.cp.gamm.lin$lme)

model.Macu2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu2)

model.Macu2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu2)

model.Macu2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu2)



### modelos log sem componente randômico e com correlação espacial ###

model.Macu2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.cp.gam.exp$gam)
summary(model.Macu2.cp.gam.exp$lme)




## P.maculatus_AmbPeque - P --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Macu2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.p.gamm.exp$gam)
summary(model.Macu2.p.gamm.exp$lme)

model.Macu2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu2)
summary(model.Macu2.p.gamm.lin$gam)
summary(model.Macu2.p.gamm.lin$lme)

model.Macu2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu2)

model.Macu2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu2)

model.Macu2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Macu2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.p.gam.exp$gam)
summary(model.Macu2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Macu2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.p.gamm.exp$gam)
summary(model.Macu2.p.gamm.exp$lme)

model.Macu2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Macu2)
summary(model.Macu2.p.gamm.lin$gam)
summary(model.Macu2.p.gamm.lin$lme)

model.Macu2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Macu2)

model.Macu2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Macu2)

model.Macu2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Macu2)



### modelos log sem componente randômico e com correlação espacial ###

model.Macu2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Macu2)
summary(model.Macu2.p.gam.exp$gam)
summary(model.Macu2.p.gam.exp$lme)

