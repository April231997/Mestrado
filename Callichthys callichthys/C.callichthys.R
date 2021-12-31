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

model.Calli.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Calli)
summary(model.Calli.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.8052     0.5901   9.838 2.78e-15 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##       edf Ref.df     F p-value
## s(ANO) 2.562  2.562 3.628   0.113
##
## R-sq.(adj) =  0.0398   
## Scale est. = 3.7957    n = 81

model2.Calli.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Calli)
summary(model2.Calli.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.6924     0.1027   16.48   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value
## s(ANO) 1.86   1.86 1.621   0.188
##
## R-sq.(adj) =  0.00418   
## Scale est. = 0.11481   n = 81

model3.Calli.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Calli)
summary(model3.Calli.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -16.19838   39.25762  -0.413    0.681
## ANO           0.01101    0.01962   0.561    0.576
##
##
## R-sq.(adj) =  0.000327   
## Scale est. = 4.2206    n = 81

model4.Calli.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Calli)
summary(model4.Calli.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 0.1724533  6.6838886   0.026    0.979
## ANO         0.0007613  0.0033407   0.228    0.820
##
##
## R-sq.(adj) =  -0.00785   
## Scale est. = 0.12183   n = 81

model5.Calli.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Calli)
summary(model5.Calli.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Calli
##
## REML criterion at convergence: 354.7
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.7490 -0.6845 -0.2091  0.6177  2.7390 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.7682   0.8765  
## Residual             4.2717   2.0668  
## Number of obs: 81, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept) -11.537493  39.275590  78.995352  -0.294     0.77
## ANO           0.008675   0.019630  78.997516   0.442     0.66
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Calli.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Calli)
summary(model6.Calli.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Calli
##
## REML criterion at convergence: 74.8
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.20420 -0.64233 -0.03618  0.67533  2.30135 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.02669  0.1634  
## Residual             0.12332  0.3512  
## Number of obs: 81, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 8.961e-01  6.680e+00 7.899e+01   0.134    0.894
## ANO         3.988e-04  3.339e-03 7.899e+01   0.119    0.905
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



## C.callichthys_Geral - P ---------------------------------------------------##

model.Calli.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Calli)
summary(model.Calli.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    8.983      2.345   3.831  0.00026 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 3.64   3.64 5.111  0.0034 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.147   
## Scale est. = 73.278    n = 81

model5.Calli.p.lmer<-lmer(P~ANO + (1|Amb), data = Calli)
summary(model5.Calli.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Calli
##
## REML criterion at convergence: 596.6
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.1084 -0.6450 -0.3396  0.2873  3.3190 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 11.58    3.403   
## Residual             91.61    9.571   
## Number of obs: 81, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept) -144.01369  181.46335   78.81262  -0.794    0.430
## ANO            0.07649    0.09070   78.82202   0.843    0.402
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



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

model.Calli2.cp.gamm<-gamm(CP~s(ANO), data=Calli2)
summary(model.Calli2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   6.4217     0.2913   22.04   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##         edf Ref.df     F p-value  
## s(ANO) 2.15   2.15 3.674  0.0271 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.151   
## Scale est. = 3.8192    n = 46

model2.Calli2.cp.gamm<-gamm(log(CP)~s(ANO), data=Calli2)
summary(model2.Calli2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
## 
## Parametric coefficients:
##         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.7998     0.0503   35.78   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##         edf Ref.df     F p-value  
## s(ANO) 1.944  1.944 2.417  0.0936 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0964   
## Scale est. = 0.11385   n = 46

model3.Calli2.cp.lm<-lm(CP~ANO, data = Calli2)
summary(model3.Calli2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Calli2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.6315 -1.4315 -0.0205  1.5009  4.8585 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  21.119998 170.143980   0.124    0.902
## ANO          -0.007333   0.084888  -0.086    0.932
##
## Residual standard error: 2.178 on 44 degrees of freedom
## Multiple R-squared:  0.0001696,	Adjusted R-squared:  -0.02255 
## F-statistic: 0.007463 on 1 and 44 DF,  p-value: 0.9316

model4.Calli2.cp.lm<-lm(log(CP)~ANO, data = Calli2)
summary(model4.Calli2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Calli2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.76559 -0.18577  0.05542  0.27122  0.69021 
##
## Coefficients:
##            Estimate Std. Error t value Pr(>|t|)
## (Intercept) -5.123040  28.470109  -0.180    0.858
## ANO          0.003454   0.014204   0.243    0.809
##
## Residual standard error: 0.3644 on 44 degrees of freedom
## Multiple R-squared:  0.001342,	Adjusted R-squared:  -0.02135 
## F-statistic: 0.05913 on 1 and 44 DF,  p-value: 0.809



## C.callichthys_AmbGran - P -------------------------------------------------##

model.Calli2.p.gamm<-gamm(P~s(ANO), data = Calli2)
summary(model.Calli2.p.gamm$gam)
##Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   11.630      1.351   8.607 7.12e-11 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 2.334  2.334 5.054  0.0065 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.216   
## Scale est. = 82.176    n = 46

model2.Calli2.p.gamm<-gamm(log(P)~s(ANO), data=Calli2)
summary(model2.Calli2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
## 
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.0079     0.1466    13.7   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df   F p-value  
## s(ANO) 1.931  1.931 2.3  0.0986 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0949   
## Scale est. = 0.96655   n = 46

model3.Calli2.p.lm<-lm(P~ANO, data = Calli2)
summary(model3.Calli2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Calli2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -10.942  -7.942  -2.651   6.293  30.058 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 482.0409   818.2927   0.589    0.559
## ANO          -0.2347     0.4083  -0.575    0.568
##
## Residual standard error: 10.47 on 44 degrees of freedom
## Multiple R-squared:  0.007455,	Adjusted R-squared:  -0.0151 
## F-statistic: 0.3305 on 1 and 44 DF,  p-value: 0.5683

model4.Calli2.p.lm<-lm(log(P)~ANO, data = Calli2)
summary(model4.Calli2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Calli2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.0340 -0.6360  0.1199  0.8190  1.9842 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -29.22535   82.81493  -0.353    0.726
## ANO           0.01558    0.04132   0.377    0.708
##
## Residual standard error: 1.06 on 44 degrees of freedom
## Multiple R-squared:  0.003222,	Adjusted R-squared:  -0.01943 
## F-statistic: 0.1422 on 1 and 44 DF,  p-value: 0.7079



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

model.Calli3.cp.gamm<-gamm(CP~s(ANO), data=Calli3)
summary(model.Calli3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.1167     0.3081   16.61   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##         edf Ref.df     F p-value
## s(ANO) 1.813  1.813 1.995   0.123
##
## R-sq.(adj) =  0.119   
## Scale est. = 3.3226    n = 36

model2.Calli3.cp.gamm<-gamm(log(CP)~s(ANO), data=Calli3)
summary(model2.Calli3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.56965    0.05531   28.38   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.718  1.718 1.597   0.216
##
## R-sq.(adj) =  0.0901   
## Scale est. = 0.10706   n = 36

model3.Calli3.cp.lm<-lm(CP~ANO, data = Calli3)
summary(model3.Calli3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Calli3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.3891 -1.3146 -0.4843  0.4800  5.7644 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -10.340452  39.420846  -0.262    0.795
## ANO           0.007742   0.019743   0.392    0.697
##
## Residual standard error: 2.002 on 34 degrees of freedom
##Multiple R-squared:  0.004502,	Adjusted R-squared:  -0.02478 
## F-statistic: 0.1538 on 1 and 34 DF,  p-value: 0.6974

model4.Calli3.cp.lm<-lm(log(CP)~ANO, data = Calli3)
summary(model4.Calli3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Calli3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.53927 -0.23337 -0.06644  0.13966  0.82950 
##
## Coefficients:
##            Estimate Std. Error t value Pr(>|t|)
## (Intercept)  1.732e+00  6.979e+00   0.248    0.805
## ANO         -8.145e-05  3.495e-03  -0.023    0.982
##
## Residual standard error: 0.3544 on 34 degrees of freedom
## Multiple R-squared:  1.597e-05,	Adjusted R-squared:  -0.0294 
## F-statistic: 0.0005431 on 1 and 34 DF,  p-value: 0.9815



## C.callichthys_AmbPeque - P ------------------------------------------------##

model.Calli3.p.gamm<-gamm(P~s(ANO), data = Calli3)
summary(model.Calli3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    6.056      1.240   4.883  2.6e-05 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   2      2 4.503  0.0189 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =    0.2   
## Scale est. = 53.82     n = 36

model2.Calli3.p.gamm<-gamm(log(P)~s(ANO), data=Calli3)
summary(model2.Calli3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.1905     0.1666   7.144 3.26e-08 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##     edf Ref.df     F p-value
## s(ANO) 1.634  1.634 1.426   0.297
##
## R-sq.(adj) =  0.074   
## Scale est. = 0.97188   n = 36

model3.Calli3.p.lm<-lm(P~ANO, data = Calli3)
summary(model3.Calli3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Calli3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -6.444 -4.985 -3.265  0.569 31.641 
##
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -163.37845  164.37915  -0.994    0.327
## ANO            0.08486    0.08232   1.031    0.310
##
## Residual standard error: 8.349 on 34 degrees of freedom
## Multiple R-squared:  0.0303,	Adjusted R-squared:  0.001783 
## F-statistic: 1.063 on 1 and 34 DF,  p-value: 0.3099

model4.Calli3.p.lm<-lm(log(P)~ANO, data = Calli3)
summary(model4.Calli3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Calli3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.1977 -0.7177 -0.1694  0.4661  2.5146 
##
## Coefficients:
##          Estimate Std. Error t value Pr(>|t|)
## (Intercept)  6.595655  20.824376   0.317    0.753
## ANO         -0.002707   0.010429  -0.260    0.797
##
## Residual standard error: 1.058 on 34 degrees of freedom
## Multiple R-squared:  0.001978,	Adjusted R-squared:  -0.02738 
## F-statistic: 0.06737 on 1 and 34 DF,  p-value: 0.7968


