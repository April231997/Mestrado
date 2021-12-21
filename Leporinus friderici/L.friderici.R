dir()

Frid <- read.table("L.friderici.txt", header=T)
str(Frid)
Frid$CP
Frid$P
Frid$ANO
Frid$Longitude
Frid$Latitude
Frid$Amb

Frid

summary(Frid)
## catalognumber         CP               P               day            month             ANO      
## Min.   :    2   Min.   : 2.700   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1981  
## 1st Qu.: 3916   1st Qu.: 5.725   1st Qu.:  4.00   1st Qu.:11.25   1st Qu.: 3.000   1st Qu.:2000  
## Median : 8708   Median : 9.200   Median : 17.00   Median :18.00   Median : 6.000   Median :2006  
## Mean   : 9860   Mean   : 9.992   Mean   : 43.81   Mean   :17.66   Mean   : 6.076   Mean   :2004  
## 3rd Qu.:17492   3rd Qu.:13.950   3rd Qu.: 62.00   3rd Qu.:23.75   3rd Qu.: 9.000   3rd Qu.:2008  
## Max.   :22777   Max.   :22.500   Max.   :286.00   Max.   :31.00   Max.   :12.000   Max.   :2018  
##
## state               Amb              Longitude         Latitude     
## Length:118         Length:118         Min.   :-53.73   Min.   :-23.43  
## Class :character   Class :character   1st Qu.:-50.21   1st Qu.:-21.25  
## Mode  :character   Mode  :character   Median :-49.75   Median :-20.79  
##                                       Mean   :-49.61   Mean   :-20.83  
##                                       3rd Qu.:-49.27   3rd Qu.:-20.42  
##                                       Max.   :-47.59   Max.   :-17.21


## L.friderici_Geral - teste de normalidade ----------------------------------##
shapiro.test(Frid$CP) ## W = 0.92886, p-value = 9.596e-06
shapiro.test(Frid$P)  ## W = 0.70431, p-value = 4.23e-14
shapiro.test(Frid$ANO) ## W = 0.89047, p-value = 8.027e-08


## L.friderici_Geral - cor e cov ---------------------------------------------##
cor(Frid$ANO, Frid$CP) ## -0.03447602
cor(Frid$ANO, Frid$P)  ## 0.0734879
cor(Frid$ANO, Frid$CP, method="spearman") ## 0.01257859
cor(Frid$ANO, Frid$P, method="spearman")  ## 0.01949557

cov(Frid$ANO, Frid$CP) ## -1.427988
cov(Frid$ANO, Frid$P)  ## 37.15863


## L.friderici_Geral - CP ----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Frid.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.cp.gamm.exp$gam)
summary(model.Frid.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Frid.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid)
summary(model.Frid.cp.gamm.lin$gam)
summary(model.Frid.cp.gamm.lin$lme)

model.Frid.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid)

model.Frid.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid)

model.Frid.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Frid.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.cp.gam.exp$gam)
summary(model.Frid.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Frid.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.cp.gamm.exp$gam)
summary(model.Frid.cp.gamm.exp$lme)

model.Frid.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid)
summary(model.Frid.cp.gamm.lin$gam)
summary(model.Frid.cp.gamm.lin$lme)

model.Frid.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid)

model.Frid.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid)

model.Frid.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid)



### modelos log sem componente randômico e com correlação espacial ###

model.Frid.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.cp.gam.exp$gam)
summary(model.Frid.cp.gam.exp$lme)




## L.friderici_Geral - P -----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Frid.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.p.gamm.exp$gam)
summary(model.Frid.p.gamm.exp$lme)

model.Frid.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid)
summary(model.Frid.p.gamm.lin$gam)
summary(model.Frid.p.gamm.lin$lme)

model.Frid.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid)

model.Frid.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid)

model.Frid.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Frid.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.p.gam.exp$gam)
summary(model.Frid.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Frid.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.p.gamm.exp$gam)
summary(model.Frid.p.gamm.exp$lme)

model.Frid.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid)
summary(model.Frid.p.gamm.lin$gam)
summary(model.Frid.p.gamm.lin$lme)

model.Frid.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid)

model.Frid.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid)

model.Frid.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid)



### modelos log sem componente randômico e com correlação espacial ###

model.Frid.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid)
summary(model.Frid.p.gam.exp$gam)
summary(model.Frid.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Frid2 <- read.table("L.friderici_AmbGran.txt", header=T)
str(Frid2)
Frid2$CP
Frid2$P
Frid2$ANO
Frid2$Longitude
Frid2$Latitude
Frid2$Amb

Frid2

summary(Frid2)
## catalognumber         CP               P               day            month             ANO      
## Min.   :    2   Min.   : 2.700   Min.   :  1.00   Min.   : 5.00   Min.   : 1.000   Min.   :1981  
## 1st Qu.: 6169   1st Qu.: 4.400   1st Qu.:  2.00   1st Qu.:11.00   1st Qu.: 4.000   1st Qu.:2003  
## Median : 9003   Median : 7.200   Median :  8.00   Median :16.00   Median : 7.000   Median :2006  
## Mean   : 9262   Mean   : 8.578   Mean   : 31.32   Mean   :18.76   Mean   : 6.683   Mean   :2002  
## 3rd Qu.:17492   3rd Qu.:12.200   3rd Qu.: 33.00   3rd Qu.:26.00   3rd Qu.: 9.000   3rd Qu.:2008  
## Max.   :19275   Max.   :22.500   Max.   :286.00   Max.   :31.00   Max.   :12.000   Max.   :2013  
##
## state               Amb              Longitude         Latitude     
## Length:41          Length:41          Min.   :-53.73   Min.   :-22.48  
## Class :character   Class :character   1st Qu.:-50.93   1st Qu.:-21.25  
## Mode  :character   Mode  :character   Median :-49.55   Median :-20.61  
##                                       Mean   :-50.00   Mean   :-20.87  
##                                       3rd Qu.:-49.39   3rd Qu.:-20.52  
##                                       Max.   :-47.70   Max.   :-20.21


## L.friderici_AmbGran - teste de normalidade --------------------------------##
shapiro.test(Frid2$CP) ## W = 0.88438, p-value = 0.0005922
shapiro.test(Frid2$P)  ## W = 0.60541, p-value = 2.775e-09
shapiro.test(Frid2$ANO) ## W = 0.78402, p-value = 2.562e-06


## L.friderici_AmbGran - cor e cov -------------------------------------------##
cor(Frid2$ANO, Frid2$CP) ## -0.3995912
cor(Frid2$ANO, Frid2$P)  ## -0.2306737
cor(Frid2$ANO, Frid2$CP, method="spearman") ## -0.2603442
cor(Frid2$ANO, Frid2$P, method="spearman")  ## -0.2612185

cov(Frid2$ANO, Frid2$CP) ## -20.94043
cov(Frid2$ANO, Frid2$P)  ## -130.6744


## L.friderici_AmbGran - CP --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Frid2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.cp.gamm.exp$gam)
summary(model.Frid2.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Frid2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid2)
summary(model.Frid2.cp.gamm.lin$gam)
summary(model.Frid2.cp.gamm.lin$lme)

model.Frid2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid2)

model.Frid2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid2)

model.Frid2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Frid2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.cp.gam.exp$gam)
summary(model.Frid2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Frid2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.cp.gamm.exp$gam)
summary(model.Frid2.cp.gamm.exp$lme)

model.Frid2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid2)
summary(model.Frid2.cp.gamm.lin$gam)
summary(model.Frid2.cp.gamm.lin$lme)

model.Frid2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid2)

model.Frid2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid2)

model.Frid2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid2)



### modelos log sem componente randômico e com correlação espacial ###

model.Frid2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.cp.gam.exp$gam)
summary(model.Frid2.cp.gam.exp$lme)




## L.friderici_AmbGran - P ---------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Frid2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.p.gamm.exp$gam)
summary(model.Frid2.p.gamm.exp$lme)

model.Frid2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid2)
summary(model.Frid2.p.gamm.lin$gam)
summary(model.Frid2.p.gamm.lin$lme)

model.Frid2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid2)

model.Frid2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid2)

model.Frid2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Frid2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.p.gam.exp$gam)
summary(model.Frid2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Frid2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.p.gamm.exp$gam)
summary(model.Frid2.p.gamm.exp$lme)

model.Frid2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid2)
summary(model.Frid2.p.gamm.lin$gam)
summary(model.Frid2.p.gamm.lin$lme)

model.Frid2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid2)

model.Frid2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid2)

model.Frid2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid2)



### modelos log sem componente randômico e com correlação espacial ###

model.Frid2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid2)
summary(model.Frid2.p.gam.exp$gam)
summary(model.Frid2.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Frid3 <- read.table("L.friderici_AmbPeque.txt", header=T)
str(Frid3)
Frid3$CP
Frid3$P
Frid3$ANO
Frid3$Longitude
Frid3$Latitude
Frid3$Amb

Frid3

summary(Frid3)


## L.friderici_AmbPeque - teste de normalidade -------------------------------##
shapiro.test(Frid3$CP) ## W = 0.92416, p-value = 0.0002704
shapiro.test(Frid3$P)  ## W = 0.74247, p-value = 4.488e-10
shapiro.test(Frid3$ANO) ## W = 0.91144, p-value = 7.295e-05


## L.friderici_AmbPeque - cor e cov ------------------------------------------##
cor(Frid3$ANO, Frid3$CP) ## 0.2143102
cor(Frid3$ANO, Frid3$P)  ## 0.2409742
cor(Frid3$ANO, Frid3$CP, method="spearman") ## 0.1977796
cor(Frid3$ANO, Frid3$P, method="spearman")  ## 0.2203198

cov(Frid3$ANO, Frid3$CP) ## 6.019252
cov(Frid3$ANO, Frid3$P)  ## 88.10885


## L.friderici_AmbPeque - CP -------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Frid3.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.cp.gamm.exp$gam)
summary(model.Frid3.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Frid3.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid3)
summary(model.Frid3.cp.gamm.lin$gam)
summary(model.Frid3.cp.gamm.lin$lme)

model.Frid3.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid3)

model.Frid3.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid3)

model.Frid3.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Frid3.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.cp.gam.exp$gam)
summary(model.Frid3.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Frid3.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.cp.gamm.exp$gam)
summary(model.Frid3.cp.gamm.exp$lme)

model.Frid3.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid3)
summary(model.Frid3.cp.gamm.lin$gam)
summary(model.Frid3.cp.gamm.lin$lme)

model.Frid3.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid3)

model.Frid3.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid3)

model.Frid3.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid3)



### modelos log sem componente randômico e com correlação espacial ###

model.Frid3.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.cp.gam.exp$gam)
summary(model.Frid3.cp.gam.exp$lme)


## L.friderici_AmbPeque - P --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Frid3.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.p.gamm.exp$gam)
summary(model.Frid3.p.gamm.exp$lme)

model.Frid3.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid3)
summary(model.Frid3.p.gamm.lin$gam)
summary(model.Frid3.p.gamm.lin$lme)

model.Frid3.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid3)

model.Frid3.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid3)

model.Frid3.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Frid3.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.p.gam.exp$gam)
summary(model.Frid3.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Frid3.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.p.gamm.exp$gam)
summary(model.Frid3.p.gamm.exp$lme)

model.Frid3.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Frid3)
summary(model.Frid3.p.gamm.lin$gam)
summary(model.Frid3.p.gamm.lin$lme)

model.Frid3.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Frid3)

model.Frid3.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Frid3)

model.Frid3.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Frid3)



### modelos log sem componente randômico e com correlação espacial ###

model.Frid3.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Frid3)
summary(model.Frid3.p.gam.exp$gam)
summary(model.Frid3.p.gam.exp$lme)
