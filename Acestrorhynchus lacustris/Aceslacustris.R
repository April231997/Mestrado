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
library('vegan')
library('lmerTest')
library('r2glmm')



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
## 
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

model.Aces.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Aces)
summary(model.Aces.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  12.7447     0.8966   14.21   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.303   0.583
##
## R-sq.(adj) =  -0.0175   
## Scale est. = 13.174    n = 123

model2.Aces.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Aces)
summary(model2.Aces.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.48594    0.08079   30.77   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.027    0.87
##
## R-sq.(adj) =  -0.0113   
## Scale est. = 0.12254   n = 123

model3.Aces.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Aces)
summary(model3.Aces.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 52.14138   71.82495   0.726    0.469
## ANO         -0.01969    0.03594  -0.548    0.585
##
##
## R-sq.(adj) =  -0.0175   
## Scale est. = 13.174    n = 123

model4.Aces.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Aces)
summary(model4.Aces.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  3.6115562  6.9013176   0.523    0.602
## ANO         -0.0005626  0.0034535  -0.163    0.871
##
##
## R-sq.(adj) =  -0.0113   
## Scale est. = 0.12254   n = 123

model5.Aces.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Aces)
summary(model5.Aces.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 672.7
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -3.00957 -0.46668  0.01668  0.56931  2.29000 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  3.201   1.789   
## Residual             13.269   3.643   
## Number of obs: 123, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  65.04620   72.53720 116.67206   0.897    0.372
## ANO          -0.02619    0.03630 116.70365  -0.721    0.472
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000

model6.Aces.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Aces)
summary(model6.Aces.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 106.6
##
## Scaled residuals: 
##  Min      1Q  Median      3Q     Max 
## -4.0126 -0.2517  0.1513  0.5872  1.7120 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.02613  0.1617  
## Residual             0.12340  0.3513  
## Number of obs: 123, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)   4.956852   6.984210 115.048303   0.710    0.479
## ANO          -0.001240   0.003495 115.045769  -0.355    0.724
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



## Acestrorhynchus lacustris_Geral - P --------------------------------------##

model.Aces.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Aces)
summary(model.Aces.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
## 
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   37.526      7.268   5.163 9.66e-07 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.285   0.259
##
## R-sq.(adj) =  -0.017   
## Scale est. = 1111.7    n = 123

model2.Aces.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Aces)
summary(model2.Aces.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.1624     0.2716   11.64   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.991   0.321
##
## R-sq.(adj) =  -0.0225   
## Scale est. = 1.0818    n = 123

model3.Aces.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Aces)
summary(model3.Aces.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##  Estimate Std. Error t value Pr(>|t|)
## (Intercept) 777.7775   655.0551   1.187    0.237
## ANO          -0.3700     0.3278  -1.129    0.261
##
##
## R-sq.(adj) =  -0.017   
## Scale est. = 1111.7    n = 123

model4.Aces.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Aces)
summary(model4.Aces.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 23.64817   20.63857   1.146    0.254
## ANO         -0.01024    0.01033  -0.991    0.323
##
##
## R-sq.(adj) =  -0.0225   
## Scale est. = 1.0818    n = 123

model5.Aces.p.lmer<-lmer(P~ANO + (1|Amb), data = Aces)
summary(model5.Aces.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 1209.1
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.5567 -0.6181 -0.3000  0.3726  3.8460 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  212.9   14.59   
## Residual             1119.4   33.46   
## Number of obs: 123, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) 914.7119   664.2372 113.3829   1.377    0.171
## ANO          -0.4389     0.3324 113.3447  -1.320    0.189
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Aces.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Aces)
summary(model6.Aces.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 370.4
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -3.5347 -0.3326  0.1162  0.6306  1.8772 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.2927   0.5411  
## Residual             1.0898   1.0439  
## Number of obs: 123, groups:  Amb, 2
##
## Fixed effects:
##  Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  27.12219   20.81234 117.73062   1.303    0.195
## ANO          -0.01199    0.01042 117.78330  -1.151    0.252
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



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
## 
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

model.Aces2.cp.gamm<-gamm(CP~s(ANO), data=Aces2)
summary(model.Aces2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   11.524      0.733   15.72 4.11e-15 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 2.323   0.139
##
## R-sq.(adj) =  0.0424   
## Scale est. = 15.046    n = 29

model2.Aces2.cp.gamm<-gamm(log(CP)~s(ANO), data=Aces2)
summary(model2.Aces2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.36334    0.07901   29.91   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 2.429   0.131
##
## R-sq.(adj) =  0.0457   
## Scale est. = 0.17478   n = 29

model3.Aces2.cp.gamm<-gamm(CP~ANO, data=Aces2)
summary(model2.Aces2.cp.gamm$gam)
## Error in gamm(CP ~ ANO, data = Aces2) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Aces2.cp.gamm<-gamm(log(CP)~ANO, data=Aces2)
summary(model4.Aces2.cp.gamm$gam)
## Error in gamm(CP ~ ANO, data = Aces2) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Aces2.cp.lm<-lm(CP~ANO, data = Aces2)
summary(model5.Aces2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Aces2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -7.7567 -1.3935  0.6373  1.6507  8.7520 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 196.05225  123.28922   1.590    0.123
## ANO          -0.09264    0.06190  -1.497    0.146
##
## Residual standard error: 4.02 on 27 degrees of freedom
## Multiple R-squared:  0.07661,	Adjusted R-squared:  0.04241 
## F-statistic:  2.24 on 1 and 27 DF,  p-value: 0.1461

model6.Aces2.cp.lm<-lm(log(CP)~ANO, data = Aces2)
summary(model6.Aces2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Aces2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.95144 -0.04985  0.12137  0.18250  0.75018 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 22.699507  13.288157   1.708   0.0991 .
## ANO         -0.010210   0.006671  -1.530   0.1375  
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4333 on 27 degrees of freedom
## Multiple R-squared:  0.07982,	Adjusted R-squared:  0.04574 
## F-statistic: 2.342 on 1 and 27 DF,  p-value: 0.1375

## Acestrorhynchus lacustris_AmbGran - P --------------------------------------##

model.Aces2.p.gamm<-gamm(P~s(ANO), data = Aces2)
summary(model.Aces2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   29.138      4.931   5.909 2.69e-06 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.554   0.223
##
## R-sq.(adj) =  0.0175   
## Scale est. = 680.87    n = 29

model2.Aces2.p.gamm<-gamm(log(P)~s(ANO), data=Aces2)
summary(model2.Aces2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.8350     0.2137   13.27 2.41e-13 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 4.947  0.0347 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.119   
## Scale est. = 1.2782    n = 29

model3.Aces2.p.gamm<-gamm(P~ANO, data=Aces2)
summary(model3.Aces2.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Aces2) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Aces2.p.gamm<-gamm(log(P)~ANO, data=Aces2)
summary(model4.Aces2.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Aces2) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Aces2.p.lm<-lm(P~ANO, data = Aces2)
summary(model5.Aces2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Aces2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -30.618 -14.364  -4.658   5.852 100.421 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1044.5261   829.3712   1.259    0.219
## ANO           -0.5098     0.4164  -1.224    0.231
##
## Residual standard error: 27.04 on 27 degrees of freedom
## Multiple R-squared:  0.0526,	Adjusted R-squared:  0.01751 
## F-statistic: 1.499 on 1 and 27 DF,  p-value: 0.2314

model6.Aces2.p.lm<-lm(log(P)~ANO, data = Aces2)
summary(model6.Aces2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Aces2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.4109 -0.3421  0.2349  0.4666  2.2245 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 81.32011   35.93447   2.263   0.0319 *
##   ANO         -0.03940    0.01804  -2.184   0.0378 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.172 on 27 degrees of freedom
## Multiple R-squared:  0.1502,	Adjusted R-squared:  0.1187 
## F-statistic: 4.771 on 1 and 27 DF,  p-value: 0.03781



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

model.Aces3.cp.gamm<-gamm(CP~s(ANO), data=Aces3)
summary(model.Aces3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
## 
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  13.8351     0.3593    38.5   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.086   0.769
##
## R-sq.(adj) =  -0.00993   
## Scale est. = 12.008    n = 94

model2.Aces3.cp.gamm<-gamm(log(CP)~s(ANO), data=Aces3)
summary(model2.Aces3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.58615    0.03248   79.63   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.132    0.29
##
## R-sq.(adj) =  0.00129   
## Scale est. = 0.098093  n = 94

model3.Aces3.cp.gamm<-gamm(CP~ANO, data=Aces3)
summary(model3.Aces3.cp.gamm$gam)
## Error in gamm(CP ~ ANO, data = Aces3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Aces3.cp.gamm<-gamm(log(CP)~ANO, data=Aces3)
summary(model4.Aces3.cp.gamm$gam)
## Error in gamm(CP ~ ANO, data = Aces3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Aces3.cp.lm<-lm(CP~ANO, data = Aces3)
summary(model5.Aces3.cp.lm)
## Call:
##   lm(formula = CP ~ ANO, data = Aces3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -10.2839  -1.7321  -0.6214   2.3423   8.8613 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -13.58558   93.77794  -0.145    0.885
## ANO           0.01369    0.04681   0.292    0.771
##
## Residual standard error: 3.503 on 92 degrees of freedom
## Multiple R-squared:  0.0009285,	Adjusted R-squared:  -0.009931 
## F-statistic: 0.0855 on 1 and 92 DF,  p-value: 0.7706

model6.Aces3.cp.lm<-lm(log(CP)~ANO, data = Aces3)
summary(model6.Aces3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Aces3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.31007 -0.08871  0.01353  0.19148  0.59162 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -6.382507   8.475758  -0.753    0.453
## ANO          0.004477   0.004231   1.058    0.293
##
## Residual standard error: 0.3166 on 92 degrees of freedom
## Multiple R-squared:  0.01202,	Adjusted R-squared:  0.001286 
## F-statistic:  1.12 on 1 and 92 DF,  p-value: 0.2928



## Acestrorhynchus lacustris_AmbPeque - P -----------------------------------##


model.Aces3.p.gamm<-gamm(P~s(ANO), data = Aces3)
summary(model.Aces3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    45.56       3.62   12.59   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.089   0.299

model2.Aces3.p.gamm<-gamm(log(P)~s(ANO), data=Aces3)
summary(model2.Aces3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
## 
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   3.4844     0.1008   34.58   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.203   0.653
##
## R-sq.(adj) =  -0.00867   
## Scale est. = 0.9444    n = 94

model3.Aces3.p.gamm<-gamm(P~ANO, data=Aces3)
summary(model3.Aces3.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Aces3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Aces3.p.gamm<-gamm(log(P)~ANO, data=Aces3)
summary(model4.Aces3.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Aces3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Aces3.p.lm<-lm(P~ANO, data = Aces3)
summary(model5.Aces3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Aces3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -53.55 -22.27 -10.82  15.01 127.41 
###
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1026.0943   944.8115   1.086    0.280
## ANO           -0.4894     0.4716  -1.038    0.302
##
## Residual standard error: 35.29 on 92 degrees of freedom
## Multiple R-squared:  0.01157,	Adjusted R-squared:  0.0008279 
## F-statistic: 1.077 on 1 and 92 DF,  p-value: 0.3021

model6.Aces3.p.lm<-lm(log(P)~ANO, data = Aces3)
summary(model6.Aces3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Aces3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.5353 -0.3500  0.0637  0.6365  1.7930 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -8.299686  26.298896  -0.316    0.753
## ANO          0.005882   0.013127   0.448    0.655
##
## Residual standard error: 0.9823 on 92 degrees of freedom
## Multiple R-squared:  0.002178,	Adjusted R-squared:  -0.008668 
## F-statistic: 0.2008 on 1 and 92 DF,  p-value: 0.6551


