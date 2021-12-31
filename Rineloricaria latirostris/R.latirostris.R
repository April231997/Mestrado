dir()

Lati <- read.table("R.latirostris.txt", header=T)
str(Lati)
Lati$CP
Lati$P
Lati$ANO
Lati$Longitude
Lati$Latitude
Lati$Amb

Lati

summary(Lati)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 3106   Min.   : 4.800   Min.   : 1.000   Min.   : 1.00   Min.   : 1.000   Min.   :1999  
## 1st Qu.: 4608   1st Qu.: 6.600   1st Qu.: 1.250   1st Qu.: 5.25   1st Qu.: 3.250   1st Qu.:2001  
## Median : 8020   Median : 8.100   Median : 3.500   Median :15.50   Median : 8.000   Median :2004  
## Mean   :10424   Mean   : 8.629   Mean   : 7.235   Mean   :14.24   Mean   : 6.882   Mean   :2006  
## 3rd Qu.:15239   3rd Qu.: 9.975   3rd Qu.: 9.000   3rd Qu.:21.00   3rd Qu.:10.000   3rd Qu.:2011  
## Max.   :21102   Max.   :19.600   Max.   :58.000   Max.   :30.00   Max.   :12.000   Max.   :2013  
##
## state               Amb              Longitude         Latitude     
## Length:34          Length:34          Min.   :-51.30   Min.   :-24.84  
## Class :character   Class :character   1st Qu.:-50.02   1st Qu.:-21.74  
## Mode  :character   Mode  :character   Median :-49.46   Median :-21.02  
##                                       Mean   :-49.13   Mean   :-21.41  
##                                       3rd Qu.:-48.51   3rd Qu.:-20.97  
##                                       Max.   :-46.82   Max.   :-19.54 



## R.latirostris_Geral - teste de normalidade --------------------------------##
shapiro.test(Lati$CP) ## W = 0.86998, p-value = 0.0008093
shapiro.test(Lati$P)  ## W = 0.58799, p-value = 1.382e-08
shapiro.test(Lati$ANO) ## W = 0.86703, p-value = 0.0006889



## R.latirostris_Geral - cor e cov -------------------------------------------##
cor(Lati$ANO, Lati$CP) ## 0.4656765
cor(Lati$ANO, Lati$P)  ## 0.3657144
cor(Lati$ANO, Lati$CP, method="spearman") ## 0.5186777
cor(Lati$ANO, Lati$P, method="spearman")  ## 0.5645945

cov(Lati$ANO, Lati$CP) ## 6.86738
cov(Lati$ANO, Lati$P)  ## 19.13904



## R.latirostris_Geral - CP --------------------------------------------------##

model.Lati.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data=Lati)
summary(model.Lati.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.6294     0.4553   18.95   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value   
## s(ANO)   1      1 9.138  0.0049 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.192   
## Scale est. = 6.8408    n = 34

model2.Lati.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Lati)
summary(model.Lati.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.6294     0.4553   18.95   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO)   1      1 9.138  0.0049 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.192   
## Scale est. = 6.8408    n = 34

model3.Lati.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Lati)
summary(model3.Lati.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -561.36057  191.48306  -2.932  0.00618 **
##   ANO            0.28419    0.09547   2.977  0.00551 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.192   
## Scale est. = 6.8408    n = 34

model4.Lati.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Lati)
summary(model4.Lati.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -61.365023  19.570181  -3.136  0.00366 **
##   ANO           0.031645   0.009757   3.243  0.00276 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.224   
## Scale est. = 0.071455  n = 34

model5.Lati.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Lati)
summary(model5.Lati.cp.lmer)
## boundary (singular) fit: see ?isSingular

model6.Lati.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Lati)
summary(model6.Lati.cp.lmer)
## boundary (singular) fit: see ?isSingular



## R.latirostris_Geral - P ---------------------------------------------------##

model.Lati.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data=Lati)
summary(model.Lati.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    7.235      1.699   4.258 0.000169 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 5.095   0.031 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.107   
## Scale est. = 95.291    n = 34

model2.Lati.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Lati)
summary(model.Lati.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    7.235      1.699   4.258 0.000169 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 5.095   0.031 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.107   
## Scale est. = 95.291    n = 34

model3.Lati.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Lati)
summary(model3.Lati.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -1581.2978   714.6655  -2.213   0.0342 *
##   ANO             0.7920     0.3563   2.223   0.0334 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.107   
## Scale est. = 95.291    n = 34

model4.Lati.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Lati)
summary(model4.Lati.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -245.21224   68.23716  -3.594  0.00108 **
##   ANO            0.12292    0.03402   3.613  0.00102 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.268   
## Scale est. = 0.86873   n = 34

model5.Lati.p.lmer<-lmer(P~ANO + (1|Amb), data = Lati)
summary(model5.Lati.p.lmer)
## boundary (singular) fit: see ?isSingular

model6.Lati.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Lati)
summary(model6.Lati.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Lati
##
## REML criterion at convergence: 98.1
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.90429 -0.64526  0.08833  0.53251  2.85088 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.08387  0.2896  
## Residual             0.89278  0.9449  
## Number of obs: 34, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)   
## (Intercept) -215.53774   72.91093   22.89818  -2.956  0.00710 **
##   ANO            0.10815    0.03634   22.94127   2.976  0.00678 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



##----------------------------------------------------------------------------##

dir()

Lati2 <- read.table("R.latirostris_AmbPeque.txt", header=T)
str(Lati2)
Lati2$CP
Lati2$P
Lati2$ANO
Lati2$Longitude
Lati2$Latitude
Lati2$Amb

Lati2

summary(Lati2)
## catalognumber         CP               P               day            month             ANO      
## Min.   : 3106   Min.   : 5.300   Min.   : 1.000   Min.   : 1.00   Min.   : 1.000   Min.   :1999  
## 1st Qu.: 3905   1st Qu.: 6.225   1st Qu.: 1.000   1st Qu.: 5.25   1st Qu.: 3.000   1st Qu.:2000  
## Median : 6277   Median : 6.750   Median : 2.000   Median :11.50   Median : 7.500   Median :2003  
## Mean   : 7977   Mean   : 7.941   Mean   : 5.727   Mean   :13.91   Mean   : 6.227   Mean   :2004  
## 3rd Qu.:13391   3rd Qu.: 8.825   3rd Qu.: 5.500   3rd Qu.:23.75   3rd Qu.: 9.500   3rd Qu.:2006  
## Max.   :19227   Max.   :19.600   Max.   :58.000   Max.   :30.00   Max.   :12.000   Max.   :2013  
## state               Amb              Longitude         Latitude     
## Length:22          Length:22          Min.   :-50.02   Min.   :-24.05  
## Class :character   Class :character   1st Qu.:-50.02   1st Qu.:-21.05  
## Mode  :character   Mode  :character   Median :-49.46   Median :-21.02  
##                                       Mean   :-49.35   Mean   :-21.25  
##                                       3rd Qu.:-49.28   3rd Qu.:-20.97  
##                                       Max.   :-46.82   Max.   :-20.97



## R.latirostris_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Lati2$CP) ## W = 0.68608, p-value = 1.309e-05
shapiro.test(Lati2$P)  ## W = 0.40321, p-value = 1.824e-08
shapiro.test(Lati2$ANO) ## W = 0.82476, p-value = 0.001254



## R.latirostris_AmbPeque - cor e cov ----------------------------------------##
cor(Lati2$ANO, Lati2$CP) ## 0.3443308
cor(Lati2$ANO, Lati2$P)  ## 0.2628919
cor(Lati2$ANO, Lati2$CP, method="spearman") ## 0.3492801
cor(Lati2$ANO, Lati2$P, method="spearman")  ## 0.3462233

cov(Lati2$ANO, Lati2$CP) ## 4.872511
cov(Lati2$ANO, Lati2$P)  ## 14.67532



## R.latirostris_AmbPeque - CP -----------------------------------------------##

model.Lati2.cp.gamm<-gamm(CP~s(ANO), data=Lati2)
summary(model.Lati2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Lati2.cp.gamm<-gamm(log(CP)~s(ANO), data=Lati2)
summary(model2.Lati2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Lati2.cp.lm<-lm(CP~ANO, data = Lati2)
summary(model3.Lati2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Lati2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.2543 -1.6761 -0.3674  0.3663 10.9500 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -445.0969   276.2105  -1.611    0.123
## ANO            0.2261     0.1378   1.640    0.117
##
## Residual standard error: 2.932 on 20 degrees of freedom
## Multiple R-squared:  0.1186,	Adjusted R-squared:  0.07449 
## F-statistic:  2.69 on 1 and 20 DF,  p-value: 0.1166

model4.Lati2.cp.lm<-lm(log(CP)~ANO, data = Lati2)
summary(model4.Lati2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Lati2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.35757 -0.20928 -0.01916  0.09465  0.87539 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -47.06140   26.28288  -1.791   0.0885 .
## ANO           0.02450    0.01312   1.868   0.0765 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.279 on 20 degrees of freedom
## Multiple R-squared:  0.1485,	Adjusted R-squared:  0.1059 
## F-statistic: 3.488 on 1 and 20 DF,  p-value: 0.07655



## R.latirostris_AmbPeque - P ------------------------------------------------##

model.Lati2.p.gamm<-gamm(P~s(ANO), data = Lati2)
summary(model.Lati2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Lati2.p.gamm<-gamm(log(P)~s(ANO), data=Lati2)
summary(model2.Lati2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Lati2.p.lm<-lm(P~ANO, data = Lati2)
summary(model3.Lati2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Lati2)
##
## Residuals:
##  Min     1Q Median     3Q    Max 
## -9.587 -4.139 -1.587 -1.096 50.137 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1358.7596  1119.7651  -1.213    0.239
## ANO             0.6809     0.5588   1.219    0.237
##
## Residual standard error: 11.89 on 20 degrees of freedom
## Multiple R-squared:  0.06911,	Adjusted R-squared:  0.02257 
## F-statistic: 1.485 on 1 and 20 DF,  p-value: 0.2372

model4.Lati2.p.lm<-lm(log(P)~ANO, data = Lati2)
summary(model4.Lati2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Lati2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.57738 -0.81559  0.07345  0.60638  2.83131 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -173.50455   97.43298  -1.781   0.0901 .
## ANO            0.08706    0.04862   1.791   0.0885 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.034 on 20 degrees of freedom
## Multiple R-squared:  0.1382,	Adjusted R-squared:  0.09507 
## F-statistic: 3.206 on 1 and 20 DF,  p-value: 0.08851


