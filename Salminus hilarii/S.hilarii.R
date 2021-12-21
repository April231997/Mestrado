dir()

Hila <- read.table("S.hilarii.txt", header=T)
str(Hila)
Hila$CP
Hila$P
Hila$ANO
Hila$Longitude
Hila$Latitude
Hila$Amb

Hila

summary(Hila)
## catalognumber         CP              P              day            month           ANO      
## Min.   :  531   Min.   : 6.40   Min.   :  5.0   Min.   : 1.00   Min.   :1.00   Min.   :1980  
## 1st Qu.: 1678   1st Qu.:10.65   1st Qu.: 19.5   1st Qu.: 9.25   1st Qu.:2.75   1st Qu.:1989  
## Median : 9415   Median :13.20   Median : 32.0   Median :16.00   Median :3.00   Median :2006  
## Mean   :10748   Mean   :14.46   Mean   : 77.6   Mean   :16.90   Mean   :3.60   Mean   :2000  
## 3rd Qu.:18807   3rd Qu.:20.30   3rd Qu.:154.2   3rd Qu.:29.00   3rd Qu.:4.00   3rd Qu.:2009  
## Max.   :21436   Max.   :24.30   Max.   :289.0   Max.   :31.00   Max.   :7.00   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:20          Length:20          Min.   :-50.23   Min.   :-23.40  
## Class :character   Class :character   1st Qu.:-49.42   1st Qu.:-21.19  
## Mode  :character   Mode  :character   Median :-49.32   Median :-20.46  
##                                       Mean   :-49.06   Mean   :-20.97  
##                                       3rd Qu.:-48.91   3rd Qu.:-20.39  
##                                       Max.   :-47.59   Max.   :-19.99  


## S.hilarii_Geral - teste de normalidade ------------------------------------##
shapiro.test(Hila$CP) ## W = 0.8986, p-value = 0.03882
shapiro.test(Hila$P)  ## W = 0.76286, p-value = 0.0002538
shapiro.test(Hila$ANO) ## W = 0.88312, p-value = 0.02014


## S.hilarii_Geral - cor e cov -----------------------------------------------##
cor(Hila$ANO, Hila$CP) ## 0.2751821
cor(Hila$ANO, Hila$P)  ## 0.2206498
cor(Hila$ANO, Hila$CP, method="spearman") ## 0.3332311
cor(Hila$ANO, Hila$P, method="spearman")  ## 0.2938945

cov(Hila$ANO, Hila$CP) ## 19.5
cov(Hila$ANO, Hila$P)  ## 251.6842


## S.hilarii_Geral - CP ------------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Hila.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.cp.gamm.exp$gam)
summary(model.Hila.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Hila.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila)
summary(model.Hila.cp.gamm.lin$gam)
summary(model.Hila.cp.gamm.lin$lme)

model.Hila.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila)

model.Hila.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila)

model.Hila.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Hila.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.cp.gam.exp$gam)
summary(model.Hila.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Hila.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.cp.gamm.exp$gam)
summary(model.Hila.cp.gamm.exp$lme)

model.Hila.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila)
summary(model.Hila.cp.gamm.lin$gam)
summary(model.Hila.cp.gamm.lin$lme)

model.Hila.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila)

model.Hila.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila)

model.Hila.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila)



### modelos log sem componente randômico e com correlação espacial ###

model.Hila.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.cp.gam.exp$gam)
summary(model.Hila.cp.gam.exp$lme)




## S.hilarii_Geral - P -------------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Hila.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.p.gamm.exp$gam)
summary(model.Hila.p.gamm.exp$lme)

model.Hila.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila)
summary(model.Hila.p.gamm.lin$gam)
summary(model.Hila.p.gamm.lin$lme)

model.Hila.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila)

model.Hila.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila)

model.Hila.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Hila.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.p.gam.exp$gam)
summary(model.Hila.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Hila.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.p.gamm.exp$gam)
summary(model.Hila.p.gamm.exp$lme)

model.Hila.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila)
summary(model.Hila.p.gamm.lin$gam)
summary(model.Hila.p.gamm.lin$lme)

model.Hila.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila)

model.Hila.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila)

model.Hila.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila)



### modelos log sem componente randômico e com correlação espacial ###

model.Hila.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila)
summary(model.Hila.p.gam.exp$gam)
summary(model.Hila.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Hila2 <- read.table("S.hilarii_AmbPeque.txt", header=T)
str(Hila2)
Hila2$CP
Hila2$P
Hila2$ANO
Hila2$Longitude
Hila2$Latitude
Hila2$Amb

Hila2

summary(Hila2)
## catalognumber         CP              P              day           month        ANO      
## Min.   :  531   Min.   : 9.60   Min.   : 17.0   Min.   : 1.0   Min.   :1   Min.   :1984  
## 1st Qu.: 9360   1st Qu.:12.70   1st Qu.: 29.0   1st Qu.:10.0   1st Qu.:3   1st Qu.:1998  
## Median :17809   Median :14.20   Median : 48.0   Median :16.0   Median :4   Median :2006  
## Mean   :14052   Mean   :16.18   Mean   : 98.6   Mean   :16.6   Mean   :4   Mean   :2005  
## 3rd Qu.:20980   3rd Qu.:21.35   3rd Qu.:168.0   3rd Qu.:26.0   3rd Qu.:5   3rd Qu.:2016  
## Max.   :21436   Max.   :24.30   Max.   :289.0   Max.   :31.0   Max.   :7   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:15          Length:15          Min.   :-50.23   Min.   :-23.40  
## Class :character   Class :character   1st Qu.:-49.40   1st Qu.:-20.66  
## Mode  :character   Mode  :character   Median :-49.28   Median :-20.41  
##                                       Mean   :-48.95   Mean   :-20.97  
##                                       3rd Qu.:-47.85   3rd Qu.:-20.38  
##                                       Max.   :-47.59   Max.   :-19.99  


## S.hilarii_AmbPeque - teste de normalidade ---------------------------------##
shapiro.test(Hila2$CP) ## W = 0.88169, p-value = 0.05028
shapiro.test(Hila2$P)  ## W = 0.81974, p-value = 0.006664
shapiro.test(Hila2$ANO) ## W = 0.8383, p-value = 0.01191


## S.hilarii_AmbPeque - cor e cov --------------------------------------------##
cor(Hila2$ANO, Hila2$CP) ## -0.06803891
cor(Hila2$ANO, Hila2$P)  ## -0.05038113
cor(Hila2$ANO, Hila2$CP, method="spearman") ## 0.2002648
cor(Hila2$ANO, Hila2$P, method="spearman")  ## 0.119424

cov(Hila2$ANO, Hila2$CP) ## -4.038571
cov(Hila2$ANO, Hila2$P)  ## -53.77143


## S.hilarii_AmbPeque - CP ---------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Hila2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.cp.gamm.exp$gam)
summary(model.Hila2.cp.gamm.exp$lme)

## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##   A term has fewer unique covariate combinations than specified maximum degrees of freedom

model.Hila2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila2)
summary(model.Hila2.cp.gamm.lin$gam)
summary(model.Hila2.cp.gamm.lin$lme)

model.Hila2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila2)

model.Hila2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila2)

model.Hila2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Hila2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.cp.gam.exp$gam)
summary(model.Hila2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Hila2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.cp.gamm.exp$gam)
summary(model.Hila2.cp.gamm.exp$lme)

model.Hila2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila2)
summary(model.Hila2.cp.gamm.lin$gam)
summary(model.Hila2.cp.gamm.lin$lme)

model.Hila2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila2)

model.Hila2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila2)

model.Hila2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila2)



### modelos log sem componente randômico e com correlação espacial ###

model.Hila2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.cp.gam.exp$gam)
summary(model.Hila2.cp.gam.exp$lme)




## S.hilarii_AmbPeque - P ----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Hila2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.p.gamm.exp$gam)
summary(model.Hila2.p.gamm.exp$lme)

model.Hila2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila2)
summary(model.Hila2.p.gamm.lin$gam)
summary(model.Hila2.p.gamm.lin$lme)

model.Hila2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila2)

model.Hila2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila2)

model.Hila2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Hila2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.p.gam.exp$gam)
summary(model.Hila2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Hila2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.p.gamm.exp$gam)
summary(model.Hila2.p.gamm.exp$lme)

model.Hila2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Hila2)
summary(model.Hila2.p.gamm.lin$gam)
summary(model.Hila2.p.gamm.lin$lme)

model.Hila2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Hila2)

model.Hila2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Hila2)

model.Hila2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Hila2)



### modelos log sem componente randômico e com correlação espacial ###

model.Hila2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Hila2)
summary(model.Hila2.p.gam.exp$gam)
summary(model.Hila2.p.gam.exp$lme)

