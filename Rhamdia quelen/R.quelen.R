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

model.Quel.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Quel)
summary(model.Quel.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   9.2749     0.4299   21.57   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.819   0.366
##
## R-sq.(adj) =  0.00217   
## Scale est. = 14.36     n = 434

model2.Quel.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Quel)
summary(model2.Quel.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.14011    0.04432   48.28   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.071    0.79
##
## R-sq.(adj) =  -0.00145   
## Scale est. = 0.16539   n = 434

model3.Quel.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Quel)
summary(model3.Quel.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -93.37778  113.61950  -0.822    0.412
## ANO           0.05118    0.05663   0.904    0.367
##
## R-sq.(adj) =  0.00217   
## Scale est. = 14.36     n = 434

model4.Quel.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Quel)
summary(model4.Quel.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1.099968  12.180685  -0.090    0.928
## ANO          0.001615   0.006072   0.266    0.790
##
## R-sq.(adj) =  -0.00145   
## Scale est. = 0.16539   n = 434

model5.Quel.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Quel)
summary(model5.Quel.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Quel
##
## REML criterion at convergence: 2394.4
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.5553 -0.8038 -0.2413  0.6034  3.3334 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.704   0.8391  
## Residual             14.390   3.7934  
## Number of obs: 434, groups:  Amb, 2
##
## Fixed effects:
##  Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -79.18197  114.27397 413.04058  -0.693    0.489
## ANO           0.04413    0.05696 413.49111   0.775    0.439
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Quel.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Quel)
summary(model6.Quel.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Quel
##
## REML criterion at convergence: 465.9
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.27429 -0.81295 -0.06245  0.75880  2.39154 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.007497 0.08659 
## Residual             0.165722 0.40709 
## Number of obs: 434, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 4.957e-01  1.226e+01 4.098e+02   0.040    0.968
## ANO         8.233e-04  6.110e-03 4.103e+02   0.135    0.893
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



## R.quelen_Geral - P --------------------------------------------------------##

model.Quel.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data=Quel)
summary(model.Quel.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   19.512      1.264   15.43   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 1.982  1.982 3.348  0.0279 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0179   
## Scale est. = 692.16    n = 434

model2.Quel.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Quel)
summary(model.Quel.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
## 
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   19.512      1.264   15.43   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 1.982  1.982 3.348  0.0279 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0179   
## Scale est. = 692.16    n = 434

model3.Quel.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Quel)
summary(model3.Quel.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -1666.7392   765.8941  -2.176   0.0301 *
##   ANO             0.8407     0.3819   2.202   0.0282 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.00881   
## Scale est. = 697.42    n = 434

model4.Quel.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Quel)
summary(model4.Quel.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1.322875  36.225784  -0.037    0.971
## ANO          0.001807   0.018058   0.100    0.920
##
## R-sq.(adj) =  -0.0021   
## Scale est. = 1.4786    n = 434

model5.Quel.p.lmer<-lmer(P~ANO + (1|Amb), data = Quel)
summary(model5.Quel.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Quel
##
## REML criterion at convergence: 4070
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.9893 -0.5780 -0.3905  0.1540  5.3236 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  11.61    3.407  
## Residual             697.27   26.406  
## Number of obs: 434, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)  
## (Intercept) -1382.6981   788.8607   326.8004  -1.753   0.0806 .
## ANO             0.6997     0.3932   328.2408   1.780   0.0761 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Quel.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Quel)
summary(model6.Quel.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Quel
##
## REML criterion at convergence: 1411.9
##
## Scaled residuals: 
##  Min       1Q   Median       3Q      Max 
## -2.02066 -0.89044 -0.08964  0.74058  2.35807 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.04829  0.2197  
## Residual             1.48128  1.2171  
## Number of obs: 434, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)   4.580270  36.570489 391.564374   0.125    0.900
## ANO          -0.001123   0.018228 392.323417  -0.062    0.951
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000



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

model.Quel2.cp.gamm<-gamm(CP~s(ANO), data=Quel2)
summary(model.Quel2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.7305     0.1941   44.98   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 3.358  0.0677 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.00672   
## Scale est. = 13.076    n = 348

model2.Quel2.cp.gamm<-gamm(log(CP)~s(ANO), data=Quel2)
summary(model2.Quel2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.08637    0.02121   98.37   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##  edf Ref.df     F p-value  
## s(ANO)   1      1 4.369  0.0373 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.00958   
## Scale est. = 0.15611   n = 348

model3.Quel2.cp.lm<-lm(CP~ANO, data = Quel2)
summary(model3.Quel2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Quel2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -5.6703 -2.8558 -0.8414  1.9231 12.6490 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 247.89307  130.69608   1.897   0.0587 .
## ANO          -0.11927    0.06518  -1.830   0.0681 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 3.626 on 346 degrees of freedom
## Multiple R-squared:  0.009585,	Adjusted R-squared:  0.006723 
## F-statistic: 3.349 on 1 and 346 DF,  p-value: 0.06812

model4.Quel2.cp.lm<-lm(log(CP)~ANO, data = Quel2)
summary(model4.Quel2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Quel2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.94065 -0.31204 -0.02101  0.28319  0.97446 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 31.891703  14.280309   2.233   0.0262 *
##   ANO         -0.014864   0.007122  -2.087   0.0376 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.3962 on 346 degrees of freedom
## Multiple R-squared:  0.01243,	Adjusted R-squared:  0.00958 
## F-statistic: 4.356 on 1 and 346 DF,  p-value: 0.0376



## R.quelen_AmbGran - P ------------------------------------------------------##

model.Quel2.p.gamm<-gamm(P~s(ANO), data = Quel2)
summary(model.Quel2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   18.040      1.344   13.43   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.167   0.281
##
## R-sq.(adj) =  0.000471   
## Scale est. = 626.48    n = 348

model2.Quel2.p.gamm<-gamm(log(P)~s(ANO), data=Quel2)
summary(model2.Quel2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.17606    0.06393   34.04   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value  
## s(ANO)   1      1 5.35  0.0213 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0123   
## Scale est. = 1.4183    n = 348

model3.Quel2.p.lm<-lm(P~ANO, data = Quel2)
summary(model3.Quel2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Quel2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -20.017 -14.591 -10.124   2.518 139.849 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 993.8012   904.6538   1.099    0.273
## ANO          -0.4866     0.4512  -1.079    0.282
##
## Residual standard error: 25.1 on 346 degrees of freedom
## Multiple R-squared:  0.003351,	Adjusted R-squared:  0.0004706 
## F-statistic: 1.163 on 1 and 346 DF,  p-value: 0.2815

model4.Quel2.p.lm<-lm(log(P)~ANO, data = Quel2)
summary(model4.Quel2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Quel2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.2838 -0.8975 -0.1224  0.8299  2.9708 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 101.59856   43.04455    2.36   0.0188 *
##   ANO          -0.04958    0.02147   -2.31   0.0215 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.194 on 346 degrees of freedom
## Multiple R-squared:  0.01518,	Adjusted R-squared:  0.01234 
## F-statistic: 5.335 on 1 and 346 DF,  p-value: 0.02149



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

model.Quel3.cp.gamm<-gamm(CP~s(ANO), data=Quel3)
summary(model.Quel3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.1035     0.4363   23.16   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F  p-value    
## s(ANO)   1      1 13.59 0.000403 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.128   
## Scale est. = 16.181    n = 86

model2.Quel3.cp.gamm<-gamm(log(CP)~s(ANO), data=Quel3)
summary(model2.Quel3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.21944    0.04489   49.44   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value   
## s(ANO)   1      1 9.715  0.0025 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0919   
## Scale est. = 0.17132   n = 86

model3.Quel3.cp.lm<-lm(CP~ANO, data = Quel3)
summary(model3.Quel3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Quel3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -7.6123 -3.3935 -0.8928  3.1202 10.2036 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -824.9578   227.9045  -3.620 0.000503 ***
##   ANO            0.4159     0.1135   3.664 0.000434 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 4.07 on 84 degrees of freedom
## Multiple R-squared:  0.1378,	Adjusted R-squared:  0.1275 
## F-statistic: 13.43 on 1 and 84 DF,  p-value: 0.0004335

model4.Quel3.cp.lm<-lm(log(CP)~ANO, data = Quel3)
summary(model4.Quel3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Quel3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.91822 -0.32428 -0.00431  0.36006  0.73667 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -70.44380   23.45091  -3.004  0.00351 **
##   ANO           0.03619    0.01168   3.099  0.00265 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4188 on 84 degrees of freedom
## Multiple R-squared:  0.1026,	Adjusted R-squared:  0.09189 
## F-statistic: 9.601 on 1 and 84 DF,  p-value: 0.002645



## R.quelen_AmbPeque - P -----------------------------------------------------##

model.Quel3.p.gamm<-gamm(P~s(ANO), data = Quel3)
summary(model.Quel3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   25.465      3.079   8.271 1.74e-12 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F  p-value    
## s(ANO)   1      1 17.45 7.23e-05 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =   0.16   
## Scale est. = 805.79    n = 86

model2.Quel3.p.gamm<-gamm(log(P)~s(ANO), data=Quel3)
summary(model2.Quel3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.5139     0.1294   19.42   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO)   1      1 10.28 0.00191 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0972   
## Scale est. = 1.4242    n = 86

model3.Quel3.p.lm<-lm(P~ANO, data = Quel3)
summary(model3.Quel3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Quel3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -43.588 -18.188  -8.645   9.764 130.393 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -6653.9574  1608.3033  -4.137 8.30e-05 ***
##   ANO             3.3270     0.8011   4.153 7.84e-05 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 28.72 on 84 degrees of freedom
## Multiple R-squared:  0.1704,	Adjusted R-squared:  0.1605 
## F-statistic: 17.25 on 1 and 84 DF,  p-value: 7.842e-05

model4.Quel3.p.lm<-lm(log(P)~ANO, data = Quel3)
summary(model4.Quel3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Quel3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.9806 -0.9994  0.0195  1.0037  2.2447 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -212.96776   67.61595  -3.150  0.00226 **
##   ANO            0.10733    0.03368   3.187  0.00202 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.208 on 84 degrees of freedom
## Multiple R-squared:  0.1079,	Adjusted R-squared:  0.09724 
## F-statistic: 10.16 on 1 and 84 DF,  p-value: 0.00202


