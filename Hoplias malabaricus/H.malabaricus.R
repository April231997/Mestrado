dir()

Mala <- read.table("H.malabaricus.txt", header=T)
str(Mala)
Mala$CP
Mala$P
Mala$ANO
Mala$Longitude
Mala$Latitude
Mala$Amb

Mala

summary(Mala)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  592   Min.   : 3.000   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 6062   1st Qu.: 5.700   1st Qu.:  3.00   1st Qu.:12.00   1st Qu.: 5.000   1st Qu.:2003  
## Median : 8826   Median : 8.500   Median : 12.00   Median :19.00   Median : 7.000   Median :2005  
## Mean   : 8988   Mean   : 9.787   Mean   : 38.09   Mean   :18.09   Mean   : 6.916   Mean   :2003  
## 3rd Qu.:10913   3rd Qu.:12.900   3rd Qu.: 44.00   3rd Qu.:26.00   3rd Qu.: 9.000   3rd Qu.:2007  
## Max.   :21330   Max.   :28.000   Max.   :543.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## state               Amb              Longitude         Latitude     
## Length:453         Length:453         Min.   :-53.82   Min.   :-24.69  
## Class :character   Class :character   1st Qu.:-50.62   1st Qu.:-21.19  
## Mode  :character   Mode  :character   Median :-49.55   Median :-20.65  
##                                       Mean   :-49.65   Mean   :-20.88  
##                                       3rd Qu.:-49.27   3rd Qu.:-20.33  
##                                       Max.   :-42.77   Max.   :-16.62


## H.malabaricus_Geral - teste de normalidade --------------------------------##
shapiro.test(Mala$CP) ## W = 0.92324, p-value = 1.898e-14
shapiro.test(Mala$P)  ## W = 0.59142, p-value < 2.2e-16
shapiro.test(Mala$ANO) ## W = 0.80655, p-value < 2.2e-16


## H.malabaricus_Geral - cor e cov -------------------------------------------##
cor(Mala$ANO, Mala$CP) ## -0.006001329
cor(Mala$ANO, Mala$P)  ## -0.0105746
cor(Mala$ANO, Mala$CP, method="spearman") ## -0.03484835
cor(Mala$ANO, Mala$P, method="spearman")  ## -0.04315964

cov(Mala$ANO, Mala$CP) ## -0.2267699
cov(Mala$ANO, Mala$P)  ## -5.208702


## H.malabaricus_Geral - CP --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Mala.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.cp.gamm.exp$gam)
summary(model.Mala.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Mala.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala)
summary(model.Mala.cp.gamm.lin$gam)
summary(model.Mala.cp.gamm.lin$lme)

model.Mala.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala)

model.Mala.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala)

model.Mala.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Mala.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.cp.gam.exp$gam)
summary(model.Mala.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Mala.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.cp.gamm.exp$gam)
summary(model.Mala.cp.gamm.exp$lme)

model.Mala.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala)
summary(model.Mala.cp.gamm.lin$gam)
summary(model.Mala.cp.gamm.lin$lme)

model.Mala.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala)

model.Mala.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala)

model.Mala.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala)



### modelos log sem componente randômico e com correlação espacial ###

model.Mala.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.cp.gam.exp$gam)
summary(model.Mala.cp.gam.exp$lme)




## H.malabaricus_Geral - P ---------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Mala.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.p.gamm.exp$gam)
summary(model.Mala.p.gamm.exp$lme)

model.Mala.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala)
summary(model.Mala.p.gamm.lin$gam)
summary(model.Mala.p.gamm.lin$lme)

model.Mala.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala)

model.Mala.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala)

model.Mala.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Mala.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.p.gam.exp$gam)
summary(model.Mala.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Mala.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.p.gamm.exp$gam)
summary(model.Mala.p.gamm.exp$lme)

model.Mala.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala)
summary(model.Mala.p.gamm.lin$gam)
summary(model.Mala.p.gamm.lin$lme)

model.Mala.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala)

model.Mala.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala)

model.Mala.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala)



### modelos log sem componente randômico e com correlação espacial ###

model.Mala.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala)
summary(model.Mala.p.gam.exp$gam)
summary(model.Mala.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Mala2 <- read.table("H.malabaricus_AmbGran.txt", header=T)
str(Mala2)
Mala2$CP
Mala2$P
Mala2$ANO
Mala2$Longitude
Mala2$Latitude
Mala2$Amb

Mala2

summary(Mala2)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  592   Min.   : 3.200   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 5923   1st Qu.: 6.600   1st Qu.:  6.00   1st Qu.:10.50   1st Qu.: 4.000   1st Qu.:2003  
## Median : 7264   Median : 9.100   Median : 16.00   Median :16.00   Median : 7.000   Median :2004  
## Mean   : 8238   Mean   : 9.967   Mean   : 36.01   Mean   :16.77   Mean   : 6.466   Mean   :2002  
## 3rd Qu.:10322   3rd Qu.:12.550   3rd Qu.: 41.50   3rd Qu.:24.00   3rd Qu.: 8.000   3rd Qu.:2006  
## Max.   :19286   Max.   :26.000   Max.   :454.00   Max.   :31.00   Max.   :12.000   Max.   :2013  
## state               Amb              Longitude         Latitude     
## Length:223         Length:223         Min.   :-53.82   Min.   :-24.61  
## Class :character   Class :character   1st Qu.:-50.41   1st Qu.:-21.18  
## Mode  :character   Mode  :character   Median :-49.56   Median :-20.69  
##                                       Mean   :-49.73   Mean   :-20.78  
##                                       3rd Qu.:-49.35   3rd Qu.:-20.44  
##                                       Max.   :-45.74   Max.   :-16.62


## H.malabaricus_AmbGran - teste de normalidade ------------------------------##
shapiro.test(Mala2$CP) ## W = 0.94134, p-value = 8.104e-08
shapiro.test(Mala2$P)  ## W = 0.59897, p-value < 2.2e-16
shapiro.test(Mala2$ANO) ## W = 0.77991, p-value < 2.2e-16


## H.malabaricus_AmbGran - cor e cov -----------------------------------------##
cor(Mala2$ANO, Mala2$CP) ## 0.04571224
cor(Mala2$ANO, Mala2$P)  ## -0.008482954
cor(Mala2$ANO, Mala2$CP, method="spearman") ## 0.003618576
cor(Mala2$ANO, Mala2$P, method="spearman")  ## 0.01169663

cov(Mala2$ANO, Mala2$CP) ## 1.569066
cov(Mala2$ANO, Mala2$P)  ## -3.72979


## H.malabaricus_AmbGran - CP ------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Mala2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.cp.gamm.exp$gam)
summary(model.Mala2.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Mala2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala2)
summary(model.Mala2.cp.gamm.lin$gam)
summary(model.Mala2.cp.gamm.lin$lme)

model.Mala2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala2)

model.Mala2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala2)

model.Mala2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Mala2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.cp.gam.exp$gam)
summary(model.Mala2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Mala2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.cp.gamm.exp$gam)
summary(model.Mala2.cp.gamm.exp$lme)

model.Mala2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala2)
summary(model.Mala2.cp.gamm.lin$gam)
summary(model.Mala2.cp.gamm.lin$lme)

model.Mala2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala2)

model.Mala2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala2)

model.Mala2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala2)



### modelos log sem componente randômico e com correlação espacial ###

model.Mala2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.cp.gam.exp$gam)
summary(model.Mala2.cp.gam.exp$lme)




## H.malabaricus_AmbGran - P -------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Mala2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.p.gamm.exp$gam)
summary(model.Mala2.p.gamm.exp$lme)

model.Mala2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala2)
summary(model.Mala2.p.gamm.lin$gam)
summary(model.Mala2.p.gamm.lin$lme)

model.Mala2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala2)

model.Mala2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala2)

model.Mala2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Mala2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.p.gam.exp$gam)
summary(model.Mala2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Mala2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.p.gamm.exp$gam)
summary(model.Mala2.p.gamm.exp$lme)

model.Mala2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala2)
summary(model.Mala2.p.gamm.lin$gam)
summary(model.Mala2.p.gamm.lin$lme)

model.Mala2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala2)

model.Mala2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala2)

model.Mala2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala2)



### modelos log sem componente randômico e com correlação espacial ###

model.Mala2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala2)
summary(model.Mala2.p.gam.exp$gam)
summary(model.Mala2.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Mala3 <- read.table("H.malabaricus_AmbPeque.txt", header=T)
str(Mala3)
Mala3$CP
Mala3$P
Mala3$ANO
Mala3$Longitude
Mala3$Latitude
Mala3$Amb

Mala3

summary(Mala3)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  949   Min.   : 3.000   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 7984   1st Qu.: 5.200   1st Qu.:  2.00   1st Qu.:14.00   1st Qu.: 6.000   1st Qu.:2005  
## Median : 9006   Median : 7.850   Median :  9.00   Median :23.00   Median : 8.000   Median :2006  
## Mean   : 9715   Mean   : 9.612   Mean   : 40.11   Mean   :19.38   Mean   : 7.352   Mean   :2005  
## 3rd Qu.:12803   3rd Qu.:13.000   3rd Qu.: 44.75   3rd Qu.:26.00   3rd Qu.: 9.000   3rd Qu.:2010  
## Max.   :21330   Max.   :28.000   Max.   :543.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:230         Length:230         Min.   :-53.78   Min.   :-24.69  
## Class :character   Class :character   1st Qu.:-51.06   1st Qu.:-21.29  
## Mode  :character   Mode  :character   Median :-49.52   Median :-20.60  
##                                       Mean   :-49.56   Mean   :-20.98  
##                                       3rd Qu.:-49.22   3rd Qu.:-20.28  
##                                       Max.   :-42.77   Max.   :-16.62


## H.malabaricus_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Mala3$CP) ##  W = 0.89518, p-value = 1.427e-11
shapiro.test(Mala3$P)  ## W = 0.58732, p-value < 2.2e-16
shapiro.test(Mala3$ANO) ## W = 0.78161, p-value < 2.2e-16


## H.malabaricus_AmbPeque - cor e cov ----------------------------------------##
cor(Mala3$ANO, Mala3$CP) ## -0.03799798
cor(Mala3$ANO, Mala3$P)  ## -0.02286134
cor(Mala3$ANO, Mala3$CP, method="spearman") ## 0.01605942
cor(Mala3$ANO, Mala3$P, method="spearman")  ## 0.00228451

cov(Mala3$ANO, Mala3$CP) ## -1.508641
cov(Mala3$ANO, Mala3$P)  ## -11.97015


## H.malabaricus_AmbPeque - CP -----------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Mala3.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.cp.gamm.exp$gam)
summary(model.Mala3.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Mala3.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala3)
summary(model.Mala3.cp.gamm.lin$gam)
summary(model.Mala3.cp.gamm.lin$lme)

model.Mala3.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala3)

model.Mala3.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala3)

model.Mala3.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Mala3.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.cp.gam.exp$gam)
summary(model.Mala3.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Mala3.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.cp.gamm.exp$gam)
summary(model.Mala3.cp.gamm.exp$lme)

model.Mala3.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala3)
summary(model.Mala3.cp.gamm.lin$gam)
summary(model.Mala3.cp.gamm.lin$lme)

model.Mala3.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala3)

model.Mala3.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala3)

model.Mala3.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala3)



### modelos log sem componente randômico e com correlação espacial ###

model.Mala3.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.cp.gam.exp$gam)
summary(model.Mala3.cp.gam.exp$lme)


## H.malabaricus_AmbPeque - P ------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Mala3.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.p.gamm.exp$gam)
summary(model.Mala3.p.gamm.exp$lme)

model.Mala3.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala3)
summary(model.Mala3.p.gamm.lin$gam)
summary(model.Mala3.p.gamm.lin$lme)

model.Mala3.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala3)

model.Mala3.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala3)

model.Mala3.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Mala3.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.p.gam.exp$gam)
summary(model.Mala3.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Mala3.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.p.gamm.exp$gam)
summary(model.Mala3.p.gamm.exp$lme)

model.Mala3.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Mala3)
summary(model.Mala3.p.gamm.lin$gam)
summary(model.Mala3.p.gamm.lin$lme)

model.Mala3.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Mala3)

model.Mala3.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Mala3)

model.Mala3.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Mala3)



### modelos log sem componente randômico e com correlação espacial ###

model.Mala3.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Mala3)
summary(model.Mala3.p.gam.exp$gam)
summary(model.Mala3.p.gam.exp$lme)
