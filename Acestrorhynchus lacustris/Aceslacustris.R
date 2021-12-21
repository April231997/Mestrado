library('here')
library('mgcv')
library('gratia')
library('gamair')
library('ggplot2')
library('purrr')
library('mvnfast')
library("tibble")
library('gganimate')
library('cowplot')
library('tidyr')
library("knitr")
library("viridis")
library('readr')
library('dplyr')
library('nlme')

ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()

Aces <- read.table("Aceslacustris.txt", header=T)
str(Aces)
Aces$CP
Aces$P
Aces$ANO
Aces$Longitude
Aces$Latitude
Aces$Amb
Aces$state

length(Aces$CP)

names(Aces)
Aces


summary(Aces)
## catalognumber         CP              P               day           month             ANO      
## Min.   :  541   Min.   : 3.30   Min.   :  1.00   Min.   : 1.0   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 3393   1st Qu.:11.95   1st Qu.: 22.00   1st Qu.:15.0   1st Qu.: 3.000   1st Qu.:1990  
## Median : 9320   Median :13.20   Median : 31.00   Median :21.0   Median : 7.000   Median :2005  
## Mean   : 8927   Mean   :13.29   Mean   : 41.69   Mean   :19.7   Mean   : 6.472   Mean   :2001  
## 3rd Qu.: 9574   3rd Qu.:15.75   3rd Qu.: 57.00   3rd Qu.:25.0   3rd Qu.:10.000   3rd Qu.:2006  
## Max.   :21347   Max.   :22.50   Max.   :180.00   Max.   :31.0   Max.   :12.000   Max.   :2017  
## state               Amb              Longitude         Latitude     
## Length:123         Length:123         Min.   :-51.86   Min.   :-23.33  
## Class :character   Class :character   1st Qu.:-49.46   1st Qu.:-20.65  
## Mode  :character   Mode  :character   Median :-49.28   Median :-20.42  
##                                       Mean   :-49.37   Mean   :-20.74  
##                                       3rd Qu.:-49.27   3rd Qu.:-20.42  
##                                       Max.   :-47.50   Max.   :-17.21

## Acestrorhynchus lacustris_Geral - teste de normalidade --------------------##
shapiro.test(Aces$CP) ## W = 0.95439, p-value = 0.0003827  
shapiro.test(Aces$P)  ## W = 0.84302, p-value = 4.182e-10  
shapiro.test(Aces$year) ## W = 0.83474, p-value = 2.012e-10  


## Acestrorhynchus lacustris_Geral - cor e cov -------------------------------##
cor(Aces$ANO, Aces$CP) ## 0.05891344
cor(Aces$ANO, Aces$P)  ## -0.01630575
cor(Aces$ANO, Aces$CP, method="spearman") ## 0.01145827
cor(Aces$ANO, Aces$P, method="spearman")  ## -0.03576729

cov(Aces$ANO, Aces$CP) ## 2.262795
cov(Aces$ANO, Aces$P)  ## -5.704118


## Acestrorhynchus lacustris_Geral - CP --------------------------------------##



## modelos naturais com componente randômico e correlação espacial
model.Aces.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data = Aces)
summary(model.Aces.cp.gamm.exp$gam)
summary(model.Aces.cp.gamm.exp$lme)

## Error in getCovariate.corSpatial(object, data = data) : 
##   cannot have zero distances in "corSpatial"

model.Aces.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces)
summary(model.Aces.cp.gamm.lin$gam)
summary(model.Aces.cp.gamm.lin$lme)

model.Aces.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces)

model.Aces.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces)

model.Aces.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces)


## modelos naturais sem componente randômico e com correlação espacial
model.Aces.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces)
summary(model.Aces.cp.gam.exp$gam)
summary(model.Aces.cp.gam.exp$lme)


## modelos log com componente randômico e correlação espacial
model.Aces.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces)
summary(model.Aces.cp.gamm.exp$gam)
summary(model.Aces.cp.gamm.exp$lme)

model.Aces1.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces)
summary(model.Aces.cp.gamm.lin$gam)
summary(model.Aces.cp.gamm.lin$lme)

model.Aces.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces)

model.Aces.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces)

model.Aces.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces)


## modelos log sem componente randômico e com correlação espacial
model.Aces.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces)
summary(model.Aces.cp.gam.exp$gam)
summary(model.Aces.cp.gam.exp$lme)



## Acestrorhynchus lacustris_Geral - P --------------------------------------##



## modelos naturais com componente randômico e correlação espacial
model.Aces.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data = Aces)
summary(model.Aces.p.gamm.exp$gam)
summary(model.Aces.p.gamm.exp$lme)

model.Aces.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces)
summary(model.Aces.p.gamm.lin$gam)
summary(model.Aces.p.gamm.lin$lme)

model.Aces.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces)

model.Aces.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces)

model.Aces.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces)


## modelos naturais sem componente randômico e com correlação espacial
model.Aces.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces)
summary(model.Aces.p.gam.exp$gam)
summary(model.Aces.p.gam.exp$lme)


## modelos log com componente randômico e correlação espacial
model.Aces.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces)
summary(model.Aces.p.gamm.exp$gam)
summary(model.Aces.p.gamm.exp$lme)

model.Aces1.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces)
summary(model.Aces.p.gamm.lin$gam)
summary(model.Aces.p.gamm.lin$lme)

model.Aces.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces)

model.Aces.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces)

model.Aces.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces)


## modelos log sem componente randômico e com correlação espacial
model.Aces.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces)
summary(model.Aces.p.gam.exp$gam)
summary(model.Aces.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Aces2 <- read.table("Aceslacustris_AmbGran.txt", header=T)
str(Aces2)
Aces2$CP
Aces2$P
Aces2$ANO
Aces2$Longitude
Aces2$Latitude

summary(Aces2)
## catalognumber         CP              P               day            month             ANO          state               Amb           
## Min.   :  541   Min.   : 4.00   Min.   :  1.00   Min.   : 2.00   Min.   : 1.000   Min.   :1980   Length:29          Length:29         
## 1st Qu.:  549   1st Qu.: 9.60   1st Qu.: 10.00   1st Qu.: 9.00   1st Qu.: 8.000   1st Qu.:1982   Class :character   Class :character  
## Median :  557   Median :12.90   Median : 27.00   Median :22.00   Median : 8.000   Median :1985   Mode  :character   Mode  :character  
## Mean   : 5296   Mean   :11.52   Mean   : 29.14   Mean   :18.69   Mean   : 8.207   Mean   :1992                                        
## 3rd Qu.: 9007   3rd Qu.:13.90   3rd Qu.: 40.00   3rd Qu.:25.00   3rd Qu.:11.000   3rd Qu.:2005                                        
## Max.   :19264   Max.   :19.80   Max.   :131.00   Max.   :31.00   Max.   :12.000   Max.   :2013                                        
## Longitude         Latitude     
## Min.   :-51.35   Min.   :-21.55  
## 1st Qu.:-49.42   1st Qu.:-20.66  
## Median :-49.42   Median :-20.61  
## Mean   :-49.33   Mean   :-20.81  
## 3rd Qu.:-49.40   3rd Qu.:-20.61  
## Max.   :-47.70   Max.   :-20.27

## Acestrorhynchus lacustris_AmbGran - teste de normalidade --------------------##
shapiro.test(Aces2$CP) ## W = 0.91834, p-value = 0.02768  
shapiro.test(Aces2$P)  ## W = 0.80523, p-value = 0.0001038  
shapiro.test(Aces2$ANO) ## W = 0.77176, p-value = 2.71e-05  


## Acestrorhynchus lacustris_AmbGran - cor e cov -------------------------------##
cor(Aces2$ANO, Aces2$CP) ## -0.276793
cor(Aces2$ANO, Aces2$P)  ## -0.2293384
cor(Aces2$ANO, Aces2$CP, method="spearman") ## -0.3332164
cor(Aces2$ANO, Aces2$P, method="spearman")  ## -0.3952822

cov(Aces2$ANO, Aces2$CP) ## -13.9564
cov(Aces2$ANO, Aces2$P)  ## -76.7968



## Acestrorhynchus lacustris_AmbGran - CP --------------------------------------##



### modelos naturais com componente randômico e correlação espacial ###

model.Aces2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.cp.gamm.exp$gam)
summary(model.Aces2.cp.gamm.exp$lme)

model.Aces2.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.cp.gamm.exp$gam)
summary(model.Aces2.cp.gamm.exp$lme)

model.Aces2.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces2)
summary(model.Aces2.cp.gamm.lin$gam)
summary(model.Aces2.cp.gamm.lin$lme)


model.Aces2.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces2)

model.Aces2.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces2)

model.Aces2.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces2)


### modelos naturais sem componente randômico e com correlação espacial ###

model.Aces2.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces2.cp.gam.exp$gam)
summary(model.Aces2.cp.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Aces2.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.cp.gamm.exp$gam)
summary(model.Aces2.cp.gamm.exp$lme)

model.Aces2.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces2)
summary(model.Aces2.cp.gamm.lin$gam)
summary(model.Aces2.cp.gamm.lin$lme)

model.Aces2.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces2)

model.Aces2.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces2)

model.Aces2.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces2)


### modelos log sem componente randômico e com correlação espacial ###

model.Aces2.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.cp.gam.exp$gam)
summary(model.Aces2.cp.gam.exp$lme)




## Acestrorhynchus lacustris_AmbGran - P --------------------------------------##



### modelos naturais com componente randômico e correlação espacial ###

model.Aces2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.p.gamm.exp$gam)
summary(model.Aces2.p.gamm.exp$lme)

model.Aces2.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.p.gamm.exp$gam)
summary(model.Aces2.p.gamm.exp$lme)

model.Aces2.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces2)
summary(model.Aces2.p.gamm.lin$gam)
summary(model.Aces2.p.gamm.lin$lme)


model.Aces2.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces2)

model.Aces2.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces2)

model.Aces2.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces2)


### modelos naturais sem componente randômico e com correlação espacial ###

model.Aces2.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces2.p.gam.exp$gam)
summary(model.Aces2.p.gam.exp$lme)



### modelos log com componente randômico e correlação espacial ###

model.Aces2.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.p.gamm.exp$gam)
summary(model.Aces2.p.gamm.exp$lme)

model.Aces2.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces2)
summary(model.Aces2.p.gamm.lin$gam)
summary(model.Aces2.p.gamm.lin$lme)

model.Aces2.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces2)

model.Aces2.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces2)

model.Aces2.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces2)


### modelos log sem componente randômico e com correlação espacial ###

model.Aces2.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces2)
summary(model.Aces2.p.gam.exp$gam)
summary(model.Aces2.p.gam.exp$lme)




##----------------------------------------------------------------------------##

dir()

Aces3 <- read.table("Aceslacustris_AmbPeque.txt", header=T)
str(Aces3)
Aces3$CP
Aces3$P
Aces3$ANO
Aces3$Longitude
Aces3$Latitude
Aces3$Amb

summary(Aces3)
## catalognumber         CP              P               day            month             ANO          state               Amb           
## Min.   :  545   Min.   : 3.30   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1983   Length:94          Length:94         
## 1st Qu.: 9274   1st Qu.:12.10   1st Qu.: 23.00   1st Qu.:15.00   1st Qu.: 3.000   1st Qu.:2005   Class :character   Class :character  
## Median : 9346   Median :13.30   Median : 34.00   Median :21.00   Median : 5.000   Median :2005   Mode  :character   Mode  :character  
## Mean   :10047   Mean   :13.84   Mean   : 45.56   Mean   :20.01   Mean   : 5.936   Mean   :2003                                        
## 3rd Qu.: 9804   3rd Qu.:16.00   3rd Qu.: 61.75   3rd Qu.:25.00   3rd Qu.: 9.000   3rd Qu.:2006                                        
## Max.   :21347   Max.   :22.50   Max.   :180.00   Max.   :31.00   Max.   :12.000   Max.   :2017                                        
## Longitude         Latitude     
## Min.   :-51.86   Min.   :-23.33  
## 1st Qu.:-49.46   1st Qu.:-20.62  
## Median :-49.28   Median :-20.42  
## Mean   :-49.38   Mean   :-20.72  
## 3rd Qu.:-49.27   3rd Qu.:-20.38  
## Max.   :-47.50   Max.   :-17.21

## Acestrorhynchus lacustris_AmbPeque - teste de normalidade -----------------##
shapiro.test(Aces3$CP) ## W = 0.95885, p-value = 0.004798  
shapiro.test(Aces3$P)  ## W = 0.84453, p-value = 1.613e-08  
shapiro.test(Aces3$ANO) ## W = 0.78414, p-value = 1.996e-10  


## Acestrorhynchus lacustris_AmbPeque - cor e cov ----------------------------##
cor(Aces3$ANO, Aces3$CP) ## 0.0304709
cor(Aces3$ANO, Aces3$P)  ## -0.1075716
cor(Aces3$ANO, Aces3$CP, method="spearman") ## 0.002769193
cor(Aces3$ANO, Aces3$P, method="spearman")  ## -0.04228625

cov(Aces3$ANO, Aces3$CP) ## 0.824102
cov(Aces3$ANO, Aces3$P)  ## -29.46889


## Acestrorhynchus lacustris_AmbPeque - CP -----------------------------------##


### modelos naturais com componente randômico e correlação espacial ###

model.Aces3.cp.gamm.exp<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.cp.gamm.exp$gam)
summary(model.Aces3.cp.gamm.exp$lme)

model.Aces3.cp.gamm.lin<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces3)
summary(model.Aces3.cp.gamm.lin$gam)
summary(model.Aces3.cp.gamm.lin$lme)

model.Aces3.cp.gamm.rat<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces3)

model.Aces3.cp.gamm.sph<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces3)

model.Aces3.cp.gamm.gau<-gamm(CP~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces3)


### modelos naturais sem componente randômico e com correlação espacial ###

model.Aces3.cp.gam.exp<-gamm(CP~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.cp.gam.exp$gam)
summary(model.Aces3.cp.gam.exp$lme)


### modelos log com componente randômico e correlação espacial ###

model.Aces3.cp.gamm.exp<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.cp.gamm.exp$gam)
summary(model.Aces3.cp.gamm.exp$lme)

model.Aces3.cp.gamm.lin<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces3)
summary(model.Aces3.cp.gamm.lin$gam)
summary(model.Aces3.cp.gamm.lin$lme)

model.Aces3.cp.gamm.rat<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces3)

model.Aces3.cp.gamm.sph<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces3)

model.Aces3.cp.gamm.gau<-gamm(log(CP)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces3)


### modelos log sem componente randômico e com correlação espacial ###

model.Aces3.cp.gam.exp<-gamm(log(CP)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.cp.gam.exp$gam)
summary(model.Aces3.cp.gam.exp$lme)




## Acestrorhynchus lacustris_AmbPeque - P -----------------------------------##


### modelos naturais com componente randômico e correlação espacial ###

model.Aces3.p.gamm.exp<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.p.gamm.exp$gam)
summary(model.Aces3.p.gamm.exp$lme)

model.Aces3.p.gamm.lin<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces3)
summary(model.Aces3.p.gamm.lin$gam)
summary(model.Aces3.p.gamm.lin$lme)

model.Aces3.p.gamm.rat<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces3)

model.Aces3.p.gamm.sph<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces3)

model.Aces3.p.gamm.gau<-gamm(P~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces3)


### modelos naturais sem componente randômico e com correlação espacial ###

model.Aces3.p.gam.exp<-gamm(P~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.p.gam.exp$gam)
summary(model.Aces3.p.gam.exp$lme)


### modelos log com componente randômico e correlação espacial ###

model.Aces3.p.gamm.exp<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.p.gamm.exp$gam)
summary(model.Aces3.p.gamm.exp$lme)

model.Aces3.p.gamm.lin<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="lin", metric="euc"), data=Aces3)
summary(model.Aces3.p.gamm.lin$gam)
summary(model.Aces3.p.gamm.lin$lme)

model.Aces3.p.gamm.rat<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="rat", metric="euc"), data=Aces3)

model.Aces3.p.gamm.sph<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="sph", metric="euc"), data=Aces3)

model.Aces3.p.gamm.gau<-gamm(log(P)~s(ANO), random=list(Amb=~1), corSpatial(form=~Latitude+Longitude, type="gau", metric="euc"), data=Aces3)


### modelos log sem componente randômico e com correlação espacial ###

model.Aces3.p.gam.exp<-gamm(log(P)~s(ANO), correlation=corSpatial(.1, form=~Latitude+Longitude, type="exp", metric="euc"), data=Aces3)
summary(model.Aces3.p.gam.exp$gam)
summary(model.Aces3.p.gam.exp$lme)




