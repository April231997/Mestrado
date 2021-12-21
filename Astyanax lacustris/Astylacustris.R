dir()

Asty <- read.table("Astylacustris.txt", header=T)
str(Asty)
Asty$CP
Asty$P
Asty$ANO
Asty$Longitude
Asty$Latitude
Asty$Amb

Asty

summary(Asty)
## catalognumber         CP              P               day            month             ANO      
## Min.   :21017   Min.   :2.700   Min.   : 1.000   Min.   : 5.00   Min.   : 1.000   Min.   :2012  
## 1st Qu.:21321   1st Qu.:3.550   1st Qu.: 1.000   1st Qu.:10.00   1st Qu.: 2.000   1st Qu.:2016  
## Median :22755   Median :4.500   Median : 2.000   Median :11.00   Median : 3.000   Median :2017  
## Mean   :22338   Mean   :4.587   Mean   : 3.522   Mean   :14.73   Mean   : 4.881   Mean   :2016  
### rd Qu.:22821   3rd Qu.:5.300   3rd Qu.: 4.500   3rd Qu.:19.50   3rd Qu.: 7.000   3rd Qu.:2017  
## Max.   :22865   Max.   :8.300   Max.   :15.000   Max.   :28.00   Max.   :12.000   Max.   :2019  
## 
## state               Amb              Longitude         Latitude     
## Length:67          Length:67          Min.   :-51.48   Min.   :-23.49  
## Class :character   Class :character   1st Qu.:-48.58   1st Qu.:-23.46  
## Mode  :character   Mode  :character   Median :-48.41   Median :-23.34  
##                                       Mean   :-48.84   Mean   :-22.23  
##                                       3rd Qu.:-48.16   3rd Qu.:-20.42  
##                                       Max.   :-47.77   Max.   :-18.92 

## Astyanax lacustris_Geral - teste de normalidade ---------------------------##
shapiro.test(Asty$CP) ## W = 0.94443, p-value = 0.004788  
shapiro.test(Asty$P)  ## W = 0.80069, p-value = 4.134e-08  
shapiro.test(Asty$ANO) ## W = 0.71855, p-value = 4.99e-10  


## Astyanax lacustris_Geral - cor e cov --------------------------------------##
cor(Asty$ANO, Asty$CP) ## 0.0860064
cor(Asty$ANO, Asty$P)  ## 0.08535767
cor(Asty$ANO, Asty$CP, method="spearman") ## 0.120057
cor(Asty$ANO, Asty$P, method="spearman")  ## 0.09106156

cov(Asty$ANO, Asty$CP) ## 0.2296246
cov(Asty$ANO, Asty$P)  ## 0.5162822


## Astyanax lacustris_Geral - CP ---------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Asty.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.cp.gamm.exp$gam)
summary(model.Asty.cp.gamm.exp$lme)

## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model.Asty.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty)
summary(model.Asty.cp.gamm.lin$gam)
summary(model.Asty.cp.gamm.lin$lme)

model.Asty.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty)

model.Asty.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty)

model.Asty.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Asty.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.cp.gam.exp$gam)
summary(model.Asty.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Asty.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.cp.gamm.exp$gam)
summary(model.Asty.cp.gamm.exp$lme)

model.Asty.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty)
summary(model.Asty.cp.gamm.lin$gam)
summary(model.Asty.cp.gamm.lin$lme)

model.Asty.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty)

model.Asty.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty)

model.Asty.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty)



### modelos log sem componente randômico e com correlação espacial ###

model.Asty.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.cp.gam.exp$gam)
summary(model.Asty.cp.gam.exp$lme)




## Astyanax lacustris_Geral - P ---------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Asty.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.p.gamm.exp$gam)
summary(model.Asty.p.gamm.exp$lme)

model.Asty.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty)
summary(model.Asty.p.gamm.lin$gam)
summary(model.Asty.p.gamm.lin$lme)

model.Asty.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty)

model.Asty.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty)

model.Asty.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Asty.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.p.gam.exp$gam)
summary(model.Asty.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Asty.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.p.gamm.exp$gam)
summary(model.Asty.p.gamm.exp$lme)

model.Asty.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty)
summary(model.Asty.p.gamm.lin$gam)
summary(model.Asty.p.gamm.lin$lme)

model.Asty.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty)

model.Asty.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty)

model.Asty.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty)



### modelos log sem componente randômico e com correlação espacial ###

model.Asty.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty)
summary(model.Asty.p.gam.exp$gam)
summary(model.Asty.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Asty2 <- read.table("Astylacustris_AmbGran.txt", header=T)
str(Asty2)
Asty2$CP
Asty2$P
Asty2$ANO
Asty2$Longitude
Asty2$Latitude
Asty2$Amb

Asty2

summary(Asty2)
## catalognumber         CP              P              day            month            ANO      
## Min.   :21017   Min.   :3.100   Min.   :1.000   Min.   : 8.00   Min.   :2.000   Min.   :2016  
## 1st Qu.:21023   1st Qu.:3.700   1st Qu.:2.000   1st Qu.:11.00   1st Qu.:2.000   1st Qu.:2016  
## Median :22753   Median :4.600   Median :3.000   Median :19.00   Median :5.000   Median :2017  
## Mean   :22164   Mean   :4.622   Mean   :3.348   Mean   :16.57   Mean   :4.565   Mean   :2017  
## 3rd Qu.:22791   3rd Qu.:5.450   3rd Qu.:4.500   3rd Qu.:21.00   3rd Qu.:7.000   3rd Qu.:2018  
## Max.   :22829   Max.   :6.700   Max.   :8.000   Max.   :22.00   Max.   :7.000   Max.   :2018  
## 
## state               Amb              Longitude         Latitude     
## Length:23          Length:23          Min.   :-48.58   Min.   :-23.49  
## Class :character   Class :character   1st Qu.:-48.50   1st Qu.:-23.49  
## Mode  :character   Mode  :character   Median :-48.41   Median :-23.46  
##                                       Mean   :-48.40   Mean   :-21.89  
##                                       3rd Qu.:-48.28   3rd Qu.:-18.92  
##                                       Max.   :-48.22   Max.   :-18.92 

## Astyanax lacustris_AmbGran - teste de normalidade -------------------------##
shapiro.test(Asty2$CP) ## W = 0.94894, p-value = 0.2782  
shapiro.test(Asty2$P)  ## W = 0.88222, p-value = 0.01107  
shapiro.test(Asty2$ANO) ## W = 0.80716, p-value = 0.0004999  


## Astyanax lacustris_AmbGran - cor e cov ------------------------------------##
cor(Asty2$ANO, Asty2$CP) ## 0.3483976
cor(Asty2$ANO, Asty2$P)  ## 0.149331
cor(Asty2$ANO, Asty2$CP, method="spearman") ## 0.3415192
cor(Asty2$ANO, Asty2$P, method="spearman")  ## 0.2783456

cov(Asty2$ANO, Asty2$CP) ## 0.3019763
cov(Asty2$ANO, Asty2$P)  ## 0.2588933


## Astyanax lacustris_AmbGran - CP -------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Asty2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.cp.gamm.exp$gam)
summary(model.Asty2.cp.gamm.exp$lme)

model.Asty2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty2)
summary(model.Asty2.cp.gamm.lin$gam)
summary(model.Asty2.cp.gamm.lin$lme)

model.Asty2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty2)

model.Asty2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty2)

model.Asty2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Asty2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.cp.gam.exp$gam)
summary(model.Asty2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Asty2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.cp.gamm.exp$gam)
summary(model.Asty2.cp.gamm.exp$lme)

model.Asty2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty2)
summary(model.Asty2.cp.gamm.lin$gam)
summary(model.Asty2.cp.gamm.lin$lme)

model.Asty2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty2)

model.Asty2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty2)

model.Asty2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty2)



### modelos log sem componente randômico e com correlação espacial ###

model.Asty2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.cp.gam.exp$gam)
summary(model.Asty2.cp.gam.exp$lme)




## Astyanax lacustris_AmbGran - P -------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Asty2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.p.gamm.exp$gam)
summary(model.Asty2.p.gamm.exp$lme)

model.Asty2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty2)
summary(model.Asty2.p.gamm.lin$gam)
summary(model.Asty2.p.gamm.lin$lme)

model.Asty2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty2)

model.Asty2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty2)

model.Asty2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty2)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Asty2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.p.gam.exp$gam)
summary(model.Asty2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Asty2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.p.gamm.exp$gam)
summary(model.Asty2.p.gamm.exp$lme)

model.Asty2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty2)
summary(model.Asty2.p.gamm.lin$gam)
summary(model.Asty2.p.gamm.lin$lme)

model.Asty2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty2)

model.Asty2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty2)

model.Asty2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty2)



### modelos log sem componente randômico e com correlação espacial ###

model.Asty2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty2)
summary(model.Asty2.p.gam.exp$gam)
summary(model.Asty2.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Asty3 <- read.table("Astylacustris_AmbPeque.txt", header=T)
str(Asty3)
Asty3$CP
Asty3$P
Asty3$ANO
Asty3$Longitude
Asty3$Latitude
Asty3$Amb

Asty3

summary(Asty3)
## catalognumber         CP              P               day            month             ANO      
## Min.   :21204   Min.   :2.700   Min.   : 1.000   Min.   : 5.00   Min.   : 1.000   Min.   :2012  
## 1st Qu.:22375   1st Qu.:3.400   1st Qu.: 1.000   1st Qu.:10.00   1st Qu.: 2.000   1st Qu.:2015  
## Median :22755   Median :4.300   Median : 2.000   Median :10.50   Median : 2.000   Median :2017  
## Mean   :22429   Mean   :4.568   Mean   : 3.614   Mean   :13.77   Mean   : 5.045   Mean   :2016  
## 3rd Qu.:22836   3rd Qu.:5.225   3rd Qu.: 4.250   3rd Qu.:17.00   3rd Qu.:11.250   3rd Qu.:2017  
## Max.   :22865   Max.   :8.300   Max.   :15.000   Max.   :28.00   Max.   :12.000   Max.   :2019  
## state               Amb              Longitude         Latitude     
## Length:44          Length:44          Min.   :-51.48   Min.   :-23.47  
## Class :character   Class :character   1st Qu.:-49.95   1st Qu.:-23.38  
## Mode  :character   Mode  :character   Median :-48.42   Median :-23.33  
##                                       Mean   :-49.06   Mean   :-22.41  
##                                       3rd Qu.:-48.16   3rd Qu.:-20.71  
##                                       Max.   :-47.77   Max.   :-20.42 

## Astyanax lacustris_AmbPeque - teste de normalidade ------------------------##
shapiro.test(Asty3$CP) ## W = 0.92044, p-value = 0.004924  
shapiro.test(Asty3$P)  ## W = 0.77048, p-value = 7.055e-07
shapiro.test(Asty3$ANO) ## W = 0.70678, p-value = 4.599e-08


## Astyanax lacustris_AmbPeque - cor e cov -----------------------------------##
cor(Asty3$ANO, Asty3$CP) ## 0.05281316
cor(Asty3$ANO, Asty3$P)  ## 0.09414422
cor(Asty3$ANO, Asty3$CP, method="spearman") ## -0.01588296
cor(Asty3$ANO, Asty3$P, method="spearman")  ## -0.01393925

cov(Asty3$ANO, Asty3$CP) ## 0.1799154
cov(Asty3$ANO, Asty3$P)  ## 0.7494715


## Astyanax lacustris_AmbPeque - CP ------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Asty3.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.cp.gamm.exp$gam)
summary(model.Asty3.cp.gamm.exp$lme)

model.Asty3.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty3)
summary(model.Asty3.cp.gamm.lin$gam)
summary(model.Asty3.cp.gamm.lin$lme)

model.Asty3.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty3)

model.Asty3.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty3)

model.Asty3.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Asty3.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.cp.gam.exp$gam)
summary(model.Asty3.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Asty3.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.cp.gamm.exp$gam)
summary(model.Asty3.cp.gamm.exp$lme)

model.Asty3.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty3)
summary(model.Asty3.cp.gamm.lin$gam)
summary(model.Asty3.cp.gamm.lin$lme)

model.Asty3.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty3)

model.Asty3.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty3)

model.Asty3.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty3)



### modelos log sem componente randômico e com correlação espacial ###

model.Asty3.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.cp.gam.exp$gam)
summary(model.Asty3.cp.gam.exp$lme)


## Astyanax lacustris_AmbPeque - P ------------------------------------------##

### modelos naturais com componente randômico e correlação espacial ###

model.Asty3.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.p.gamm.exp$gam)
summary(model.Asty3.p.gamm.exp$lme)

model.Asty3.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty3)
summary(model.Asty3.p.gamm.lin$gam)
summary(model.Asty3.p.gamm.lin$lme)

model.Asty3.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty3)

model.Asty3.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty3)

model.Asty3.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty3)



### modelos naturais sem componente randômico e com correlação espacial ###

model.Asty3.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.p.gam.exp$gam)
summary(model.Asty3.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Asty3.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.p.gamm.exp$gam)
summary(model.Asty3.p.gamm.exp$lme)

model.Asty3.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Asty3)
summary(model.Asty3.p.gamm.lin$gam)
summary(model.Asty3.p.gamm.lin$lme)

model.Asty3.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Asty3)

model.Asty3.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Asty3)

model.Asty3.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Asty3)



### modelos log sem componente randômico e com correlação espacial ###

model.Asty3.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Asty3)
summary(model.Asty3.p.gam.exp$gam)
summary(model.Asty3.p.gam.exp$lme)

