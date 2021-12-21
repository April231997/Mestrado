dir()

Line <- read.table("P.lineatus.txt", header=T)
str(Line)
Line$CP
Line$P
Line$ANO
Line$Longitude
Line$Latitude
Line$Amb

Line

summary(Line)
## catalognumber         CP              P               day            month             ANO      
## Min.   : 7686   Min.   : 3.60   Min.   :  1.00   Min.   : 3.00   Min.   : 1.000   Min.   :2005  
## 1st Qu.: 9426   1st Qu.: 9.30   1st Qu.: 24.75   1st Qu.:21.00   1st Qu.: 2.000   1st Qu.:2005  
## Median : 9696   Median : 9.90   Median : 29.00   Median :21.00   Median : 3.000   Median :2005  
## Mean   :10956   Mean   :10.44   Mean   : 38.79   Mean   :20.85   Mean   : 4.787   Mean   :2006  
## 3rd Qu.:10056   3rd Qu.:10.80   3rd Qu.: 37.00   3rd Qu.:23.00   3rd Qu.: 9.000   3rd Qu.:2006  
## Max.   :21385   Max.   :21.50   Max.   :291.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:136         Length:136         Min.   :-50.11   Min.   :-23.33  
## Class :character   Class :character   1st Qu.:-49.27   1st Qu.:-20.42  
## Mode  :character   Mode  :character   Median :-49.27   Median :-20.42  
##                                       Mean   :-49.18   Mean   :-20.54  
##                                       3rd Qu.:-49.27   3rd Qu.:-20.42  
##                                       Max.   :-47.77   Max.   :-17.21


## P.lineatus_Geral - teste de normalidade -----------------------------------##
shapiro.test(Line$CP) ## W = 0.76139, p-value = 1.361e-13
shapiro.test(Line$P)  ## W = 0.54058, p-value < 2.2e-16
shapiro.test(Line$ANO) ## W = 0.43596, p-value < 2.2e-16


## P.lineatus_Geral - cor e cov ----------------------------------------------##
cor(Line$ANO, Line$CP) ## 0.4021166
cor(Line$ANO, Line$P)  ## 0.4128143
cor(Line$ANO, Line$CP, method="spearman") ## 0.0004002544
cor(Line$ANO, Line$P, method="spearman")  ## -0.02275495

cov(Line$ANO, Line$CP) ## 
cov(Line$ANO, Line$P)  ## 


## P.lineatus_Geral - CP -----------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Line.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.cp.gamm.exp$gam)
summary(model.Line.cp.gamm.exp$lme)

## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##   A term has fewer unique covariate combinations than specified maximum degrees of freedom

model.Line.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line)
summary(model.Line.cp.gamm.lin$gam)
summary(model.Line.cp.gamm.lin$lme)

model.Line.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line)

model.Line.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line)

model.Line.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Line.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.cp.gam.exp$gam)
summary(model.Line.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Line.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.cp.gamm.exp$gam)
summary(model.Line.cp.gamm.exp$lme)

model.Line.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line)
summary(model.Line.cp.gamm.lin$gam)
summary(model.Line.cp.gamm.lin$lme)

model.Line.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line)

model.Line.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line)

model.Line.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line)



### modelos log sem componente randômico e com correlação espacial ###

model.Line.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.cp.gam.exp$gam)
summary(model.Line.cp.gam.exp$lme)




## P.lineatus_Geral - P ------------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Line.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.p.gamm.exp$gam)
summary(model.Line.p.gamm.exp$lme)

model.Line.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line)
summary(model.Line.p.gamm.lin$gam)
summary(model.Line.p.gamm.lin$lme)

model.Line.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line)

model.Line.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line)

model.Line.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Line.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.p.gam.exp$gam)
summary(model.Line.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Line.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.p.gamm.exp$gam)
summary(model.Line.p.gamm.exp$lme)

model.Line.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line)
summary(model.Line.p.gamm.lin$gam)
summary(model.Line.p.gamm.lin$lme)

model.Line.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line)

model.Line.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line)

model.Line.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line)



### modelos log sem componente randômico e com correlação espacial ###

model.Line.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line)
summary(model.Line.p.gam.exp$gam)
summary(model.Line.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Line2 <- read.table("P.lineatus_AmbPeque.txt", header=T)
str(Line2)
Line2$CP
Line2$P
Line2$ANO
Line2$Longitude
Line2$Latitude
Line2$Amb

Line2

summary(Line2)
## catalognumber         CP              P               day            month            ANO      
## Min.   : 7686   Min.   : 6.40   Min.   :  8.00   Min.   : 3.00   Min.   : 1.00   Min.   :2005  
## 1st Qu.: 9426   1st Qu.: 9.30   1st Qu.: 25.00   1st Qu.:21.00   1st Qu.: 2.00   1st Qu.:2005  
## Median : 9503   Median : 9.90   Median : 29.00   Median :21.00   Median : 3.00   Median :2005  
## Mean   :10715   Mean   :10.41   Mean   : 37.57   Mean   :20.94   Mean   : 4.48   Mean   :2006  
## 3rd Qu.:10056   3rd Qu.:10.70   3rd Qu.: 37.00   3rd Qu.:23.00   3rd Qu.: 9.00   3rd Qu.:2006  
## Max.   :21385   Max.   :18.50   Max.   :186.00   Max.   :27.00   Max.   :12.00   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:125         Length:125         Min.   :-50.11   Min.   :-23.33  
## Class :character   Class :character   1st Qu.:-49.27   1st Qu.:-20.42  
## Mode  :character   Mode  :character   Median :-49.27   Median :-20.42  
## Mean   :-49.20   Mean   :-20.51  
## 3rd Qu.:-49.27   3rd Qu.:-20.42  
## Max.   :-47.77   Max.   :-17.21


## P.lineatus_AmbPeque - teste de normalidade --------------------------------##
shapiro.test(Line2$CP) ## W = 0.73075, p-value = 7.612e-14
shapiro.test(Line2$P)  ## W = 0.58839, p-value < 2.2e-16
shapiro.test(Line2$ANO) ## W = 0.39646, p-value < 2.2e-16


## P.lineatus_AmbPeque - cor e cov -------------------------------------------##
cor(Line2$ANO, Line2$CP) ## 0.5068479
cor(Line2$ANO, Line2$P)  ## 0.5482054
cor(Line2$ANO, Line2$CP, method="spearman") ## 0.0537058
cor(Line2$ANO, Line2$P, method="spearman")  ## 0.02074117

cov(Line2$ANO, Line2$CP) ## 2.925735
cov(Line2$ANO, Line2$P)  ## 42.15671


## P.lineatus_AmbPeque - CP --------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Line2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.cp.gamm.exp$gam)
summary(model.Line2.cp.gamm.exp$lme)

model.Line2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line2)
summary(model.Line2.cp.gamm.lin$gam)
summary(model.Line2.cp.gamm.lin$lme)

model.Line2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line2)

model.Line2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line2)

model.Line2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Line2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.cp.gam.exp$gam)
summary(model.Line2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Line2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.cp.gamm.exp$gam)
summary(model.Line2.cp.gamm.exp$lme)

model.Line2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line2)
summary(model.Line2.cp.gamm.lin$gam)
summary(model.Line2.cp.gamm.lin$lme)

model.Line2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line2)

model.Line2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line2)

model.Line2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line2)



### modelos log sem componente randômico e com correlação espacial ###

model.Line2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.cp.gam.exp$gam)
summary(model.Line2.cp.gam.exp$lme)




## P.lineatus_AmbPeque - P ---------------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Line2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.p.gamm.exp$gam)
summary(model.Line2.p.gamm.exp$lme)

model.Line2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line2)
summary(model.Line2.p.gamm.lin$gam)
summary(model.Line2.p.gamm.lin$lme)

model.Line2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line2)

model.Line2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line2)

model.Line2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Line2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.p.gam.exp$gam)
summary(model.Line2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Line2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.p.gamm.exp$gam)
summary(model.Line2.p.gamm.exp$lme)

model.Line2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Line2)
summary(model.Line2.p.gamm.lin$gam)
summary(model.Line2.p.gamm.lin$lme)

model.Line2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Line2)

model.Line2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Line2)

model.Line2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Line2)



### modelos log sem componente randômico e com correlação espacial ###

model.Line2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Line2)
summary(model.Line2.p.gam.exp$gam)
summary(model.Line2.p.gam.exp$lme)

