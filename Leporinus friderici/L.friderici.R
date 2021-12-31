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

model.Frid.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Frid)
summary(model.Frid.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    9.502      1.303   7.293 4.49e-11 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 4.239  4.239 3.618  0.0077 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0617   
## Scale est. = 21.339    n = 118

model2.Frid.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Frid)
summary(model2.Frid.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.1128     0.1455   14.52   <2e-16 ***
##   ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 3.047  3.047 4.878 0.00293 **
##  ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0413   
## Scale est. = 0.22874   n = 118

model3.Frid.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Frid)
summary(model3.Frid.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  93.15516  116.80847   0.798    0.427
## ANO          -0.04161    0.05830  -0.714    0.477
##
##
## R-sq.(adj) =  -0.00845   
## Scale est. = 24.962    n = 118

model4.Frid.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Frid)
summary(model4.Frid.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 19.019047  11.918636   1.596    0.113
## ANO         -0.008426   0.005948  -1.417    0.159
##
##
## R-sq.(adj) =  -0.00442   
## Scale est. = 0.25801   n = 118

model5.Frid.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Frid)
summary(model5.Frid.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Frid
##
## REML criterion at convergence: 718.8
##
## Scaled residuals: 
##  Min      1Q  Median      3Q     Max 
## -1.3674 -0.8149 -0.2491  0.7477  2.7751 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  2.308   1.519   
## Residual             25.166   5.017   
## Number of obs: 118, groups:  Amb, 2
##
## Fixed effects:
##  Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 104.64676  116.83228 115.99822   0.896    0.372
## ANO          -0.04738    0.05831 115.99824  -0.813    0.418
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000

model6.Frid.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Frid)
summary(model6.Frid.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Frid
##
## REML criterion at convergence: 189
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.88503 -0.79424 -0.02145  0.83662  2.25427 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.04207  0.2051  
## Residual             0.26019  0.5101  
## Number of obs: 118, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)  
## (Intercept)  19.908372  11.900232 115.876451   1.673    0.097 .
## ANO          -0.008873   0.005939 115.867686  -1.494    0.138  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



## L.friderici_Geral - P -----------------------------------------------------##

model.Frid.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Frid)
summary(model.Frid.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    40.45      10.87   3.722 0.000309 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 3.729  3.729 2.293  0.0724 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0583   
## Scale est. = 3446.5    n = 118

model2.Frid.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Frid)
summary(model2.Frid.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.5418     0.4673   5.439 3.13e-07 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 3.844  3.844 4.505 0.00225 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0507   
## Scale est. = 1.9952    n = 118

model3.Frid.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Frid)
summary(model3.Frid.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1084.0443  1421.1418  -0.763    0.447
## ANO             0.5628     0.7091   0.794    0.429
##
##
## R-sq.(adj) =  -0.00317   
## Scale est. = 3818.8    n = 118

model4.Frid.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Frid)
summary(model4.Frid.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 48.30991   35.89607   1.346    0.181
## ANO         -0.02280    0.01792  -1.273    0.206
##
##
## R-sq.(adj) =  -0.00644   
## Scale est. = 2.3402    n = 118

model5.Frid.p.lmer<-lmer(P~ANO + (1|Amb), data = Frid)
summary(model5.Frid.p.lmer)
## 

model6.Frid.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Frid)
summary(model6.Frid.p.lmer)
## 



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

model.Frid2.cp.gamm<-gamm(CP~s(ANO), data=Frid2)
summary(model.Frid2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.5780     0.7148      12 1.15e-14 ***
##   ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value   
## s(ANO)   1      1 7.601 0.00883 **
##   ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.138   
## Scale est. = 20.439    n = 41

model2.Frid2.cp.gamm<-gamm(log(CP)~s(ANO), data=Frid2)
summary(model2.Frid2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.99147    0.07789   25.57   <2e-16 ***
##   ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value  
## s(ANO) 1.477  1.477 9.136  0.0101 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.219   
## Scale est. = 0.24268   n = 41

model3.Frid2.cp.lm<-lm(CP~ANO, data = Frid2)
summary(model3.Frid2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Frid2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -4.649 -3.500 -1.030  4.088 14.960 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 389.06987  139.77421   2.784  0.00825 **
##   ANO          -0.19010    0.06983  -2.722  0.00964 **
##   ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.635 on 39 degrees of freedom
## Multiple R-squared:  0.1597,	Adjusted R-squared:  0.1381 
## F-statistic: 7.411 on 1 and 39 DF,  p-value: 0.009644

model4.Frid2.cp.lm<-lm(log(CP)~ANO, data = Frid2)
summary(model4.Frid2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Frid2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.84715 -0.38179  0.01496  0.36031  1.24974 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 48.770840  15.586092   3.129  0.00331 **
##  ANO         -0.023372   0.007787  -3.001  0.00467 **
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.5169 on 39 degrees of freedom
## Multiple R-squared:  0.1876,	Adjusted R-squared:  0.1668 
## F-statistic: 9.008 on 1 and 39 DF,  p-value: 0.004669



## L.friderici_AmbGran - P ---------------------------------------------------##

model.Frid2.p.gamm<-gamm(P~s(ANO), data = Frid2)
summary(model.Frid2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   31.317      8.202   3.818  0.00047 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 2.248   0.142
##
## R-sq.(adj) =  0.0289   
## Scale est. = 2691      n = 41

model2.Frid2.p.gamm<-gamm(log(P)~s(ANO), data=Frid2)
summary(model2.Frid2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
## log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.1824     0.2286   9.548 1.08e-11 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##         edf Ref.df     F p-value   
## s(ANO) 1.559  1.559 9.276 0.00931 **
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =   0.23   
## Scale est. = 2.0898    n = 41

model3.Frid2.p.lm<-lm(P~ANO, data = Frid2)
summary(model3.Frid2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Frid2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -41.68 -26.21 -19.02  12.91 261.16 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 2405.6974  1603.8083    1.50    0.142
## ANO           -1.1863     0.8013   -1.48    0.147
##
## Residual standard error: 53.19 on 39 degrees of freedom
## Multiple R-squared:  0.05321,	Adjusted R-squared:  0.02893 
## F-statistic: 2.192 on 1 and 39 DF,  p-value: 0.1468

model4.Frid2.p.lm<-lm(log(P)~ANO, data = Frid2)
summary(model4.Frid2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Frid2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.0802 -1.3870  0.0049  1.0442  3.8553 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 142.03408   45.92598   3.093  0.00366 **
##   ANO          -0.06987    0.02295  -3.045  0.00415 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.523 on 39 degrees of freedom
## Multiple R-squared:  0.1921,	Adjusted R-squared:  0.1714 
## F-statistic: 9.273 on 1 and 39 DF,  p-value: 0.004154



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

model.Frid3.cp.gamm<-gamm(CP~s(ANO), data=Frid3)
summary(model.Frid3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   10.753      0.557    19.3   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 2.588  2.588 4.224  0.0412 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.122   
## Scale est. = 22.646    n = 74

model2.Frid3.cp.gamm<-gamm(log(CP)~s(ANO), data=Frid3)
summary(model2.Frid3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.2595     0.0561   40.27   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df    F p-value  
## s(ANO)   1      1 2.97  0.0891 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0258   
## Scale est. = 0.22978   n = 74

model3.Frid3.cp.lm<-lm(CP~ANO, data = Frid3)
summary(model3.Frid3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Frid3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -6.7578 -5.0818 -0.3071  3.5171 11.1424 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -391.0053   215.7980  -1.812   0.0742 .
## ANO            0.2003     0.1076   1.862   0.0667 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 5.039 on 72 degrees of freedom
## Multiple R-squared:  0.04593,	Adjusted R-squared:  0.03268 
## F-statistic: 3.466 on 1 and 72 DF,  p-value: 0.06672

model4.Frid3.cp.lm<-lm(log(CP)~ANO, data = Frid3)
summary(model4.Frid3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Frid3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.90724 -0.48841  0.07578  0.40126  0.84357 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -33.36225   20.81227  -1.603   0.1133  
## ANO           0.01776    0.01038   1.712   0.0913 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.486 on 72 degrees of freedom
## Multiple R-squared:  0.0391,	Adjusted R-squared:  0.02575 
## F-statistic:  2.93 on 1 and 72 DF,  p-value: 0.09128



## L.friderici_AmbPeque - P --------------------------------------------------##

model.Frid3.p.gamm<-gamm(P~s(ANO), data = Frid3)
summary(model.Frid3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   51.473      7.333   7.019 1.08e-09 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df    F p-value  
## s(ANO) 1.91   1.91 3.14  0.0319 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.102   
## Scale est. = 3925.8    n = 74

model2.Frid3.p.gamm<-gamm(log(P)~s(ANO), data=Frid3)
summary(model2.Frid3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.004      0.170   17.68   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value  
## s(ANO)   1      1 3.675  0.0592 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0347   
## Scale est. = 2.1089    n = 74

model3.Frid3.p.lm<-lm(P~ANO, data = Frid3)
summary(model3.Frid3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Frid3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -67.07 -44.62 -18.26  17.16 221.31 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -5829.396   2791.361  -2.088   0.0403 *
##   ANO             2.932      1.392   2.107   0.0386 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 65.18 on 72 degrees of freedom
## Multiple R-squared:  0.05807,	Adjusted R-squared:  0.04499 
## F-statistic: 4.439 on 1 and 72 DF,  p-value: 0.03862

model4.Frid3.p.lm<-lm(log(P)~ANO, data = Frid3)
summary(model4.Frid3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Frid3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.9461 -1.5598  0.2689  1.2376  2.7009 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -117.02867   63.05098  -1.856   0.0675 .
## ANO            0.05984    0.03143   1.904   0.0609 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.472 on 72 degrees of freedom
## Multiple R-squared:  0.04792,	Adjusted R-squared:  0.0347 
## F-statistic: 3.624 on 1 and 72 DF,  p-value: 0.06094


