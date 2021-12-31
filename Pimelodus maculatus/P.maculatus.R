dir()

Macu <- read.table("P.maculatus.txt", header=T)

Macu

summary(Macu)
## catalognumber         CP              P               day            month             ANO      
## Min.   : 1272   Min.   : 3.70   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1900  
## 1st Qu.: 9022   1st Qu.: 5.70   1st Qu.:  4.00   1st Qu.: 6.25   1st Qu.: 2.000   1st Qu.:2006  
## Median :11178   Median : 9.60   Median : 16.00   Median :27.00   Median : 7.000   Median :2006  
## Mean   :13080   Mean   :10.31   Mean   : 34.89   Mean   :18.39   Mean   : 5.553   Mean   :2000  
## 3rd Qu.:18973   3rd Qu.:14.00   3rd Qu.: 53.75   3rd Qu.:27.00   3rd Qu.: 8.000   3rd Qu.:2013  
## Max.   :21344   Max.   :25.50   Max.   :252.00   Max.   :30.00   Max.   :12.000   Max.   :2017  
## 
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

model.Macu.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data=Macu)
summary(model.Macu.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Macu.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Macu)
summary(model.Macu.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Macu.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Macu)
summary(model3.Macu.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ ANO
## 
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 75.72053   59.75404   1.267    0.213
## ANO         -0.03270    0.02987  -1.095    0.281
##
## R-sq.(adj) =  0.00534   
## Scale est. = 28.233    n = 38

model4.Macu.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Macu)
summary(model4.Macu.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  8.295811   5.858569   1.416    0.165
## ANO         -0.003049   0.002928  -1.041    0.305
##
## R-sq.(adj) =  0.00228   
## Scale est. = 0.2714    n = 38

model5.Macu.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Macu)
summary(model5.Macu.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 238.4
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.17733 -0.81097 -0.06334  0.74835  2.88188 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.0     0.000   
## Residual             29.8     5.459   
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) 75.72053   59.75404 36.00000   1.267    0.213
## ANO         -0.03270    0.02987 36.00000  -1.095    0.281
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Macu.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Macu)
summary(model6.Macu.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
##Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 71.2
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.6264 -0.8190  0.1677  0.8932  2.0428 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.0000   0.0000  
## Residual             0.2865   0.5352  
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  8.295811   5.858569 36.000000   1.416    0.165
## ANO         -0.003049   0.002928 36.000000  -1.041    0.305
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



## P.maculatus_Geral - P ------------------------------------------------------##

model.Macu.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Macu)
summary(model.Macu.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Macu.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Macu)
summary(model2.Macu.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Macu.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Macu)
summary(model3.Macu.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 1276.3998   500.5410   2.550   0.0152 *
##   ANO           -0.6206     0.2502  -2.481   0.0179 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.122   
## Scale est. = 1981.1    n = 38

model4.Macu.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Macu)
summary(model4.Macu.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 21.282244  16.227500   1.311    0.198
## ANO         -0.009326   0.008111  -1.150    0.258
##
## R-sq.(adj) =  0.00863   
## Scale est. = 2.0822    n = 38

model5.Macu.p.lmer<-lmer(P~ANO + (1|Amb), data = Macu)
summary(model5.Macu.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 391.5
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.8861 -0.6225 -0.2541  0.4738  3.3841 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)    0      0.00   
## Residual             2091     45.73   
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)  
## (Intercept) 1276.3998   500.5410   36.0000   2.550   0.0152 *
##   ANO           -0.6206     0.2502   36.0000  -2.481   0.0179 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Macu.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Macu)
summary(model6.Macu.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 144.6
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.7362 -0.8011  0.1638  0.9432  1.4994 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.000    0.000   
## Residual             2.198    1.483   
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 21.282244  16.227500 36.000000   1.311    0.198
## ANO         -0.009326   0.008111 36.000000  -1.150    0.258
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



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
## 
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

model.Macu2.cp.gamm<-gamm(CP~s(ANO), data=Macu2)
summary(model.Macu2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Macu2.cp.gamm<-gamm(log(CP)~s(ANO), data=Macu2)
summary(model2.Macu2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Macu2.cp.lm<-lm(CP~ANO, data = Macu2)
summary(model3.Macu2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Macu2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -5.914 -3.107 -1.959  2.088 11.075 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -1452.2235   404.9469  -3.586  0.00110 **
##   ANO             0.7278     0.2015   3.611  0.00103 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 4.591 on 32 degrees of freedom
## Multiple R-squared:  0.2895,	Adjusted R-squared:  0.2673 
## F-statistic: 13.04 on 1 and 32 DF,  p-value: 0.00103

model4.Macu2.cp.lm<-lm(log(CP)~ANO, data = Macu2)
summary(model4.Macu2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Macu2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -0.6035 -0.2999 -0.1453  0.2722  1.1379 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -158.78695   38.87905  -4.084 0.000276 ***
##   ANO            0.08011    0.01935   4.140 0.000236 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4408 on 32 degrees of freedom
## Multiple R-squared:  0.3488,	Adjusted R-squared:  0.3284 
## F-statistic: 17.14 on 1 and 32 DF,  p-value: 0.0002361



## P.maculatus_AmbPeque - P --------------------------------------------------##

model.Macu2.p.gamm<-gamm(P~s(ANO), data = Macu2)
summary(model.Macu2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Macu2.p.gamm<-gamm(log(P)~s(ANO), data=Macu2)
summary(model2.Macu2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Macu2.p.lm<-lm(P~ANO, data = Macu2)
summary(model3.Macu2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Macu2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -32.82 -20.07 -17.08  22.62  95.50 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -4857.183   2940.595  -1.652    0.108
## ANO             2.432      1.464   1.662    0.106
##
## Residual standard error: 33.34 on 32 degrees of freedom
## Multiple R-squared:  0.07945,	Adjusted R-squared:  0.05069 
## F-statistic: 2.762 on 1 and 32 DF,  p-value: 0.1063

model4.Macu2.p.lm<-lm(log(P)~ANO, data = Macu2)
summary(model4.Macu2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Macu2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.8978 -0.7992 -0.2884  0.7181  3.3311 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -401.90560  110.48549  -3.638 0.000958 ***
##   ANO            0.20130    0.05499   3.661 0.000899 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.253 on 32 degrees of freedom
## Multiple R-squared:  0.2952,	Adjusted R-squared:  0.2731 
## F-statistic:  13.4 on 1 and 32 DF,  p-value: 0.0008992


