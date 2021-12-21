dir()

Quel <- read.table("R.quelen.txt", header=T)
str(Quel)
Quel$CP
Quel$P
Quel$ANO
Quel$Longitude
Quel$Latitude
Quel$Amb

Quel

summary(Quel)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  716   Min.   : 3.200   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1985  
## 1st Qu.: 7437   1st Qu.: 6.000   1st Qu.:  3.00   1st Qu.:12.00   1st Qu.: 4.000   1st Qu.:2004  
## Median : 8672   Median : 8.000   Median :  8.00   Median :16.00   Median : 7.000   Median :2005  
## Mean   : 9901   Mean   : 9.003   Mean   : 19.51   Mean   :16.88   Mean   : 6.207   Mean   :2006  
## 3rd Qu.:10546   3rd Qu.:11.300   3rd Qu.: 23.00   3rd Qu.:24.00   3rd Qu.: 8.000   3rd Qu.:2007  
## Max.   :21349   Max.   :21.700   Max.   :167.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:434         Length:434         Min.   :-56.02   Min.   :-24.60  
## Class :character   Class :character   1st Qu.:-50.30   1st Qu.:-21.21  
## Mode  :character   Mode  :character   Median :-49.49   Median :-20.81  
##                                       Mean   :-49.54   Mean   :-20.88  
##                                       3rd Qu.:-48.73   3rd Qu.:-20.35  
##                                       Max.   :-43.78   Max.   :-15.70


## R.quelen_Geral - teste de normalidade -------------------------------------##
shapiro.test(Quel$CP) ## W = 0.92447, p-value = 5.739e-14
shapiro.test(Quel$P)  ## W = 0.67434, p-value < 2.2e-16
shapiro.test(Quel$ANO) ## W = 0.84668, p-value < 2.2e-16


## R.quelen_Geral - cor e cov ------------------------------------------------##
cor(Quel$ANO, Quel$CP) ## 0.07251275
cor(Quel$ANO, Quel$P)  ## 0.105339
cor(Quel$ANO, Quel$CP, method="spearman") ## 0.02196141
cor(Quel$ANO, Quel$P, method="spearman")  ## 0.01032484

cov(Quel$ANO, Quel$CP) ## 0.9241802
cov(Quel$ANO, Quel$P)  ## 9.32952


## R.quelen_Geral - CP -------------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Quel.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.cp.gamm.exp$gam)
summary(model.Quel.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Quel.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel)
summary(model.Quel.cp.gamm.lin$gam)
summary(model.Quel.cp.gamm.lin$lme)

model.Quel.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel)

model.Quel.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel)

model.Quel.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Quel.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.cp.gam.exp$gam)
summary(model.Quel.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Quel.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.cp.gamm.exp$gam)
summary(model.Quel.cp.gamm.exp$lme)

model.Quel.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel)
summary(model.Quel.cp.gamm.lin$gam)
summary(model.Quel.cp.gamm.lin$lme)

model.Quel.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel)

model.Quel.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel)

model.Quel.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel)



### modelos log sem componente randômico e com correlação espacial ###

model.Quel.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.cp.gam.exp$gam)
summary(model.Quel.cp.gam.exp$lme)




## R.quelen_Geral - P --------------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Quel.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.p.gamm.exp$gam)
summary(model.Quel.p.gamm.exp$lme)

model.Quel.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel)
summary(model.Quel.p.gamm.lin$gam)
summary(model.Quel.p.gamm.lin$lme)

model.Quel.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel)

model.Quel.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel)

model.Quel.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Quel.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.p.gam.exp$gam)
summary(model.Quel.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Quel.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.p.gamm.exp$gam)
summary(model.Quel.p.gamm.exp$lme)

model.Quel.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel)
summary(model.Quel.p.gamm.lin$gam)
summary(model.Quel.p.gamm.lin$lme)

model.Quel.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel)

model.Quel.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel)

model.Quel.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel)



### modelos log sem componente randômico e com correlação espacial ###

model.Quel.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel)
summary(model.Quel.p.gam.exp$gam)
summary(model.Quel.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Quel2 <- read.table("R.quelen_AmbGran.txt", header=T)
str(Quel2)
Quel2$CP
Quel2$P
Quel2$ANO
Quel2$Longitude
Quel2$Latitude
Quel2$Amb

Quel2

summary(Quel2)
## catalognumber         CP              P               day           month             ANO      
## Min.   :  716   Min.   : 3.20   Min.   :  1.00   Min.   : 1.0   Min.   : 1.000   Min.   :1985  
## 1st Qu.: 7214   1st Qu.: 5.90   1st Qu.:  3.00   1st Qu.:12.0   1st Qu.: 4.000   1st Qu.:2004  
## Median : 8545   Median : 7.80   Median :  8.00   Median :16.0   Median : 7.000   Median :2005  
## Mean   : 9442   Mean   : 8.73   Mean   : 18.04   Mean   :16.2   Mean   : 6.405   Mean   :2005  
## 3rd Qu.: 9993   3rd Qu.:10.82   3rd Qu.: 21.25   3rd Qu.:24.0   3rd Qu.: 8.000   3rd Qu.:2006  
## Max.   :19506   Max.   :21.40   Max.   :157.00   Max.   :31.0   Max.   :12.000   Max.   :2013  
## 
## state               Amb              Longitude         Latitude     
## Length:348         Length:348         Min.   :-56.02   Min.   :-23.18  
## Class :character   Class :character   1st Qu.:-50.68   1st Qu.:-21.00  
## Mode  :character   Mode  :character   Median :-49.64   Median :-20.79  
##                                       Mean   :-49.84   Mean   :-20.68  
##                                       3rd Qu.:-49.34   3rd Qu.:-20.21  
##                                       Max.   :-46.77   Max.   :-15.95


## R.quelen_AmbGran - teste de normalidade -----------------------------------##
shapiro.test(Quel2$CP) ## W = 0.92165, p-value = 1.627e-12
shapiro.test(Quel2$P)  ## W = 0.65474, p-value < 2.2e-16
shapiro.test(Quel2$ANO) ## W = 0.81817, p-value < 2.2e-16


## R.quelen_AmbGran - cor e cov ----------------------------------------------##
cor(Quel2$ANO, Quel2$CP) ## -0.09790428
cor(Quel2$ANO, Quel2$P)  ## -0.0578888
cor(Quel2$ANO, Quel2$CP, method="spearman") ## -0.05633103
cor(Quel2$ANO, Quel2$P, method="spearman")  ## -0.07368102

cov(Quel2$ANO, Quel2$CP) ## -1.064056
cov(Quel2$ANO, Quel2$P)  ## -4.34125


## R.quelen_AmbGran - CP -----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Quel2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.cp.gamm.exp$gam)
summary(model.Quel2.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Quel2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel2)
summary(model.Quel2.cp.gamm.lin$gam)
summary(model.Quel2.cp.gamm.lin$lme)

model.Quel2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel2)

model.Quel2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel2)

model.Quel2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Quel2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.cp.gam.exp$gam)
summary(model.Quel2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Quel2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.cp.gamm.exp$gam)
summary(model.Quel2.cp.gamm.exp$lme)

model.Quel2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel2)
summary(model.Quel2.cp.gamm.lin$gam)
summary(model.Quel2.cp.gamm.lin$lme)

model.Quel2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel2)

model.Quel2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel2)

model.Quel2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel2)



### modelos log sem componente randômico e com correlação espacial ###

model.Quel2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.cp.gam.exp$gam)
summary(model.Quel2.cp.gam.exp$lme)




## R.quelen_AmbGran - P ------------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Quel2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.p.gamm.exp$gam)
summary(model.Quel2.p.gamm.exp$lme)

model.Quel2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel2)
summary(model.Quel2.p.gamm.lin$gam)
summary(model.Quel2.p.gamm.lin$lme)

model.Quel2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel2)

model.Quel2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel2)

model.Quel2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Quel2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.p.gam.exp$gam)
summary(model.Quel2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Quel2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.p.gamm.exp$gam)
summary(model.Quel2.p.gamm.exp$lme)

model.Quel2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel2)
summary(model.Quel2.p.gamm.lin$gam)
summary(model.Quel2.p.gamm.lin$lme)

model.Quel2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel2)

model.Quel2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel2)

model.Quel2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel2)



### modelos log sem componente randômico e com correlação espacial ###

model.Quel2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel2)
summary(model.Quel2.p.gam.exp$gam)
summary(model.Quel2.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Quel3 <- read.table("R.quelen_AmbPeque.txt", header=T)
str(Quel3)
Quel3$CP
Quel3$P
Quel3$ANO
Quel3$Longitude
Quel3$Latitude
Quel3$Amb

Quel3

summary(Quel3)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 5561   Min.   : 4.000   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :2003  
## 1st Qu.: 7977   1st Qu.: 6.325   1st Qu.:  4.00   1st Qu.:12.00   1st Qu.: 3.000   1st Qu.:2004  
## Median :10179   Median : 9.350   Median : 12.50   Median :22.50   Median : 6.000   Median :2007  
## Mean   :11757   Mean   :10.103   Mean   : 25.47   Mean   :19.64   Mean   : 5.407   Mean   :2008  
## 3rd Qu.:15357   3rd Qu.:12.825   3rd Qu.: 35.50   3rd Qu.:28.00   3rd Qu.: 8.000   3rd Qu.:2011  
## Max.   :21349   Max.   :21.700   Max.   :167.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:86          Length:86          Min.   :-55.26   Min.   :-24.60  
## Class :character   Class :character   1st Qu.:-49.49   1st Qu.:-22.46  
## Mode  :character   Mode  :character   Median :-47.89   Median :-21.95  
##                                       Mean   :-48.34   Mean   :-21.69  
##                                       3rd Qu.:-47.70   3rd Qu.:-20.99  
##                                       Max.   :-43.78   Max.   :-15.70 



## R.quelen_AmbPeque - teste de normalidade ----------------------------------##
shapiro.test(Quel3$CP) ##  W = 0.94017, p-value = 0.0006001
shapiro.test(Quel3$P)  ## W = 0.74703, p-value = 7.252e-11
shapiro.test(Quel3$ANO) ## W = 0.89009, p-value = 2.364e-06


## R.quelen_AmbPeque - cor e cov ---------------------------------------------##
cor(Quel3$ANO, Quel3$CP) ## 0.3712187
cor(Quel3$ANO, Quel3$P)  ## 0.4127415
cor(Quel3$ANO, Quel3$CP, method="spearman") ## 0.2570606
cor(Quel3$ANO, Quel3$P, method="spearman")  ## 0.2628654

cov(Quel3$ANO, Quel3$CP) ## 6.290643
cov(Quel3$ANO, Quel3$P)  ## 50.3171


## R.quelen_AmbPeque - CP ----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Quel3.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.cp.gamm.exp$gam)
summary(model.Quel3.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Quel3.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel3)
summary(model.Quel3.cp.gamm.lin$gam)
summary(model.Quel3.cp.gamm.lin$lme)

model.Quel3.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel3)

model.Quel3.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel3)

model.Quel3.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Quel3.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.cp.gam.exp$gam)
summary(model.Quel3.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Quel3.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.cp.gamm.exp$gam)
summary(model.Quel3.cp.gamm.exp$lme)

model.Quel3.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel3)
summary(model.Quel3.cp.gamm.lin$gam)
summary(model.Quel3.cp.gamm.lin$lme)

model.Quel3.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel3)

model.Quel3.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel3)

model.Quel3.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel3)



### modelos log sem componente randômico e com correlação espacial ###

model.Quel3.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.cp.gam.exp$gam)
summary(model.Quel3.cp.gam.exp$lme)


## R.quelen_AmbPeque - P -----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Quel3.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.p.gamm.exp$gam)
summary(model.Quel3.p.gamm.exp$lme)

model.Quel3.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel3)
summary(model.Quel3.p.gamm.lin$gam)
summary(model.Quel3.p.gamm.lin$lme)

model.Quel3.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel3)

model.Quel3.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel3)

model.Quel3.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Quel3.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.p.gam.exp$gam)
summary(model.Quel3.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Quel3.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.p.gamm.exp$gam)
summary(model.Quel3.p.gamm.exp$lme)

model.Quel3.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Quel3)
summary(model.Quel3.p.gamm.lin$gam)
summary(model.Quel3.p.gamm.lin$lme)

model.Quel3.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Quel3)

model.Quel3.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Quel3)

model.Quel3.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Quel3)



### modelos log sem componente randômico e com correlação espacial ###

model.Quel3.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Quel3)
summary(model.Quel3.p.gam.exp$gam)
summary(model.Quel3.p.gam.exp$lme)
