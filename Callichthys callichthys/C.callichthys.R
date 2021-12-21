dir()

Calli <- read.table("C.callichthys.txt", header=T)
str(Calli)
Calli$CP
Calli$P
Calli$ANO
Calli$Longitude
Calli$Latitude
Calli$Amb

Calli

summary(Calli)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 1405   Min.   : 2.800   Min.   : 1.000   Min.   : 1.00   Min.   : 1.000   Min.   :1968  
## 1st Qu.: 7298   1st Qu.: 4.100   1st Qu.: 2.000   1st Qu.: 2.00   1st Qu.: 4.000   1st Qu.:2003  
## Median : 8058   Median : 5.300   Median : 5.000   Median :15.00   Median : 5.000   Median :2004  
## Mean   : 9315   Mean   : 5.884   Mean   : 9.284   Mean   :13.47   Mean   : 5.901   Mean   :2001  
## 3rd Qu.:11300   3rd Qu.: 7.400   3rd Qu.:13.000   3rd Qu.:20.00   3rd Qu.: 7.000   3rd Qu.:2006  
## Max.   :19307   Max.   :11.400   Max.   :42.000   Max.   :30.00   Max.   :12.000   Max.   :2013  
## 
## state               Amb              Longitude         Latitude     
## Length:81          Length:81          Min.   :-52.17   Min.   :-24.12  
## Class :character   Class :character   1st Qu.:-49.67   1st Qu.:-22.54  
## Mode  :character   Mode  :character   Median :-49.48   Median :-21.34  
##                                       Mean   :-49.15   Mean   :-20.62  
##                                       3rd Qu.:-47.65   3rd Qu.:-20.85  
##                                       Max.   :-46.92   Max.   : 20.83 


## C.callichthys_Geral - teste de normalidade --------------------------------##
shapiro.test(Calli$CP) ## W = 0.94908, p-value = 0.002835
shapiro.test(Calli$P)  ## W = 0.78598, p-value = 1.666e-09
shapiro.test(Calli$ANO) ## W = 0.62009, p-value = 3.679e-13


## C.callichthys_Geral - cor e cov -------------------------------------------##
cor(Calli$ANO, Calli$CP) ## 0.1333969
cor(Calli$ANO, Calli$P)  ## 0.1638562
cor(Calli$ANO, Calli$CP, method="spearman") ## 0.1168345
cor(Calli$ANO, Calli$P, method="spearman")  ## 0.09643201

cov(Calli$ANO, Calli$CP) ## 3.541296
cov(Calli$ANO, Calli$P)  ## 20.0088


## C.callichthys_Geral - CP --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Calli.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.cp.gamm.exp$gam)
summary(model.Calli.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Calli.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli)
summary(model.Calli.cp.gamm.lin$gam)
summary(model.Calli.cp.gamm.lin$lme)

model.Calli.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli)

model.Calli.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli)

model.Calli.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Calli.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.cp.gam.exp$gam)
summary(model.Calli.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Calli.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.cp.gamm.exp$gam)
summary(model.Calli.cp.gamm.exp$lme)

model.Calli.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli)
summary(model.Calli.cp.gamm.lin$gam)
summary(model.Calli.cp.gamm.lin$lme)

model.Calli.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli)

model.Calli.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli)

model.Calli.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli)



### modelos log sem componente randômico e com correlação espacial ###

model.Calli.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.cp.gam.exp$gam)
summary(model.Calli.cp.gam.exp$lme)




## C.callichthys_Geral - P ---------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Calli.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.p.gamm.exp$gam)
summary(model.Calli.p.gamm.exp$lme)

model.Calli.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli)
summary(model.Calli.p.gamm.lin$gam)
summary(model.Calli.p.gamm.lin$lme)

model.Calli.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli)

model.Calli.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli)

model.Calli.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Calli.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.p.gam.exp$gam)
summary(model.Calli.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Calli.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.p.gamm.exp$gam)
summary(model.Calli.p.gamm.exp$lme)

model.Calli.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli)
summary(model.Calli.p.gamm.lin$gam)
summary(model.Calli.p.gamm.lin$lme)

model.Calli.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli)

model.Calli.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli)

model.Calli.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli)



### modelos log sem componente randômico e com correlação espacial ###

model.Calli.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli)
summary(model.Calli.p.gam.exp$gam)
summary(model.Calli.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Calli2 <- read.table("C.callichthys_AmbGran.txt", header=T)
str(Calli2)
Calli2$CP
Calli2$P
Calli2$ANO
Calli2$Longitude
Calli2$Latitude
Calli2$Amb

Calli2

summary(Calli2)
## catalognumber         CP               P              day            month            ANO      
## Min.   : 1405   Min.   : 2.800   Min.   : 1.00   Min.   : 1.00   Min.   : 1.00   Min.   :1988  
## 1st Qu.: 7298   1st Qu.: 5.000   1st Qu.: 4.00   1st Qu.: 4.25   1st Qu.: 4.00   1st Qu.:2003  
## Median : 8058   Median : 6.400   Median : 8.50   Median :16.00   Median : 7.00   Median :2004  
## Mean   : 8925   Mean   : 6.422   Mean   :11.63   Mean   :13.89   Mean   : 6.37   Mean   :2004  
## 3rd Qu.: 8411   3rd Qu.: 7.925   3rd Qu.:18.00   3rd Qu.:19.00   3rd Qu.: 8.00   3rd Qu.:2005  
## Max.   :19193   Max.   :11.400   Max.   :42.00   Max.   :30.00   Max.   :12.00   Max.   :2012  
## 
## state               Amb              Longitude         Latitude     
## Length:46          Length:46          Min.   :-51.54   Min.   :-25.58  
## Class :character   Class :character   1st Qu.:-49.67   1st Qu.:-21.65  
## Mode  :character   Mode  :character   Median :-49.57   Median :-20.85  
##                                       Mean   :-49.36   Mean   :-19.40  
##                                       3rd Qu.:-48.97   3rd Qu.:-20.66  
##                                       Max.   :-47.63   Max.   : 20.83
 

## C.callichthys_AmbGran - teste de normalidade ------------------------------##
shapiro.test(Calli2$CP) ## W = 0.9794, p-value = 0.5817
shapiro.test(Calli2$P)  ## W = 0.85155, p-value = 3.374e-05
shapiro.test(Calli2$ANO) ## W = 0.78157, p-value = 7.75e-07


## C.callichthys_AmbGran - cor e cov -----------------------------------------##
cor(Calli2$ANO, Calli2$CP) ## -0.01302228
cor(Calli2$ANO, Calli2$P)  ## -0.08634117 
cor(Calli2$ANO, Calli2$CP, method="spearman") ## 0.1882735
cor(Calli2$ANO, Calli2$P, method="spearman")  ## 0.1856703

cov(Calli2$ANO, Calli2$CP) ## -0.1072464
cov(Calli2$ANO, Calli2$P)  ## -3.432367


## C.callichthys_AmbGran - CP ------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Calli2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.cp.gamm.exp$gam)
summary(model.Calli2.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Calli2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli2)
summary(model.Calli2.cp.gamm.lin$gam)
summary(model.Calli2.cp.gamm.lin$lme)

model.Calli2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli2)

model.Calli2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli2)

model.Calli2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Calli2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.cp.gam.exp$gam)
summary(model.Calli2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Calli2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.cp.gamm.exp$gam)
summary(model.Calli2.cp.gamm.exp$lme)

model.Calli2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli2)
summary(model.Calli2.cp.gamm.lin$gam)
summary(model.Calli2.cp.gamm.lin$lme)

model.Calli2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli2)

model.Calli2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli2)

model.Calli2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli2)



### modelos log sem componente randômico e com correlação espacial ###

model.Calli2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.cp.gam.exp$gam)
summary(model.Asty2.cp.gam.exp$lme)




## C.callichthys_AmbGran - P -------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Calli2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.p.gamm.exp$gam)
summary(model.Calli2.p.gamm.exp$lme)

model.Calli2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli2)
summary(model.Calli2.p.gamm.lin$gam)
summary(model.Calli2.p.gamm.lin$lme)

model.Calli2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli2)

model.Calli2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli2)

model.Calli2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Calli2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.p.gam.exp$gam)
summary(model.Calli2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Calli2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.p.gamm.exp$gam)
summary(model.Calli2.p.gamm.exp$lme)

model.Calli2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli2)
summary(model.Calli2.p.gamm.lin$gam)
summary(model.Calli2.p.gamm.lin$lme)

model.Calli2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli2)

model.Calli2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli2)

model.Calli2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli2)



### modelos log sem componente randômico e com correlação espacial ###

model.Calli2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli2)
summary(model.Calli2.p.gam.exp$gam)
summary(model.Calli2.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Calli3 <- read.table("C.callichthys_AmbPeque.txt", header=T)
str(Calli3)
Calli3$CP
Calli3$P
Calli3$ANO
Calli3$Longitude
Calli3$Latitude
Calli3$Amb

Calli3


summary(Calli3)
## CP               P               day            month             ANO          state          
## Min.   : 2.800   Min.   : 1.000   Min.   : 1.00   Min.   : 1.000   Min.   :1968   Length:36         
## 1st Qu.: 3.800   1st Qu.: 1.750   1st Qu.: 2.00   1st Qu.: 4.000   1st Qu.:1988   Class :character  
## Median : 4.500   Median : 3.000   Median :10.50   Median : 4.000   Median :2004   Mode  :character  
## Mean   : 5.117   Mean   : 6.056   Mean   :12.83   Mean   : 5.389   Mean   :1997                     
## 3rd Qu.: 5.525   3rd Qu.: 5.250   3rd Qu.:25.00   3rd Qu.: 6.250   3rd Qu.:2006                     
## Max.   :11.000   Max.   :39.000   Max.   :30.00   Max.   :12.000   Max.   :2013                     
## 
## Amb              Longitude         Latitude     
## Length:36          Min.   :-52.17   Min.   :-24.12  
## Class :character   1st Qu.:-49.54   1st Qu.:-22.62  
## Mode  :character   Median :-49.05   Median :-22.54  
##                    Mean   :-48.91   Mean   :-22.31  
##                    3rd Qu.:-47.62   3rd Qu.:-21.34  
##                    Max.   :-46.92   Max.   :-20.39 
 

## C.callichthys_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Calli3$CP) ##  W = 0.86913, p-value = 0.0005417
shapiro.test(Calli3$P)  ##  W = 0.63469, p-value = 3.075e-08
shapiro.test(Calli3$ANO) ## W = 0.69668, p-value = 2.51e-07


## C.callichthys_AmbPeque - cor e cov ----------------------------------------##
cor(Calli3$ANO, Calli3$CP) ## 0.06709635
cor(Calli3$ANO, Calli3$P)  ## 0.1740796
cor(Calli3$ANO, Calli3$CP, method="spearman") ## 0.03973902
cor(Calli3$ANO, Calli3$P, method="spearman")  ## -0.01105208

cov(Calli3$ANO, Calli3$CP) ## 2.274762
cov(Calli3$ANO, Calli3$P)  ## 24.93492


## C.callichthys_AmbPeque - CP -----------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Calli3.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.cp.gamm.exp$gam)
summary(model.Calli3.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Calli3.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli3)
summary(model.Calli3.cp.gamm.lin$gam)
summary(model.Calli3.cp.gamm.lin$lme)

model.Calli3.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli3)

model.Calli3.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli3)

model.Calli3.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Calli3.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.cp.gam.exp$gam)
summary(model.Calli3.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Calli3.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.cp.gamm.exp$gam)
summary(model.Calli3.cp.gamm.exp$lme)

model.Calli3.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli3)
summary(model.Calli3.cp.gamm.lin$gam)
summary(model.Calli3.cp.gamm.lin$lme)

model.Calli3.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli3)

model.Calli3.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli3)

model.Calli3.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli3)



### modelos log sem componente randômico e com correlação espacial ###

model.Calli3.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.cp.gam.exp$gam)
summary(model.Calli3.cp.gam.exp$lme)


## C.callichthys_AmbPeque - P ------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Calli3.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.p.gamm.exp$gam)
summary(model.Calli3.p.gamm.exp$lme)

model.Calli3.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli3)
summary(model.Calli3.p.gamm.lin$gam)
summary(model.Calli3.p.gamm.lin$lme)

model.Calli3.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli3)

model.Calli3.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli3)

model.Calli3.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Calli3.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.p.gam.exp$gam)
summary(model.Calli3.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Calli3.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.p.gamm.exp$gam)
summary(model.Calli3.p.gamm.exp$lme)

model.Calli3.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Calli3)
summary(model.Calli3.p.gamm.lin$gam)
summary(model.Calli3.p.gamm.lin$lme)

model.Calli3.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Calli3)

model.Calli3.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Calli3)

model.Calli3.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Calli3)



### modelos log sem componente randômico e com correlação espacial ###

model.Calli3.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Calli3)
summary(model.Calli3.p.gam.exp$gam)
summary(model.Calli3.p.gam.exp$lme)
