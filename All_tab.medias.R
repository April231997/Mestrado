ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()

Aces <- read.table("Aceslacustris_Geral2.txt", header=T)
str(Aces)
Aces$CP
Aces$P
Aces$ANO
Aces$longitude
Aces$latitude
Aces$Amb
Aces$state

length(Aces$CP)

names(Aces)
Aces

summary(Aces)
##       CP              P               ANO          state               Amb              Longitude     
## Min.   : 3.50   Min.   :  1.00   Min.   :1980   Length:47          Length:47          Min.   :-51.86  
## 1st Qu.:11.85   1st Qu.: 22.00   1st Qu.:1988   Class :character   Class :character   1st Qu.:-49.85  
## Median :13.10   Median : 31.20   Median :2002   Mode  :character   Mode  :character   Median :-49.42  
## Mean   :12.87   Mean   : 39.36   Mean   :1998                                         Mean   :-49.64  
## 3rd Qu.:14.90   3rd Qu.: 45.75   3rd Qu.:2006                                         3rd Qu.:-49.28  
## Max.   :22.00   Max.   :175.50   Max.   :2017                                         Max.   :-47.50  
## NA's   :2       
##
##    Latitude     
## Min.   :-23.33  
##  1st Qu.:-21.07  
##  Median :-20.62  
##  Mean   :-20.81  
##  3rd Qu.:-20.42  
##  Max.   :-17.21  
##  NA's   :2    

## Acestrorhynchus lacustris_Geral - teste de normalidade --------------------##
shapiro.test(Aces$CP) ## W = 0.9428, p-value = 0.02279 
shapiro.test(Aces$P)  ## W = 0.81003, p-value = 2.683e-06  
shapiro.test(Aces$ANO) ## W = 0.89546, p-value = 0.0005174 



## Acestrorhynchus lacustris_Geral - cor e cov -------------------------------##
cor(Aces$ANO, Aces$CP) ## 0.1219209
cor(Aces$ANO, Aces$P)  ## 0.054245
cor(Aces$ANO, Aces$CP, method="spearman") ## 0.1346513
cor(Aces$ANO, Aces$P, method="spearman")  ## 0.07106232

cov(Aces$ANO, Aces$CP) ## 5.440657
cov(Aces$ANO, Aces$P)  ## 20.87303



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
## (Intercept)  12.8702     0.5864   21.95   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.694   0.409
## 
## R-sq.(adj) =  -0.00703   
## Scale est. = 15.82     n = 47

model2.Aces.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Aces)
summary(model2.Aces.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.48984    0.05784   43.05   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.845   0.363
##
## R-sq.(adj) =  -0.00379   
## Scale est. = 0.15387   n = 47

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
## (Intercept) -76.69390  108.69366  -0.706    0.484
## ANO           0.04483    0.05440   0.824    0.414
##
## R-sq.(adj) =  -0.00703   
## Scale est. = 15.82     n = 47

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
## (Intercept) -7.253865  10.719853  -0.677    0.502
## ANO          0.004877   0.005365   0.909    0.368
##
## R-sq.(adj) =  -0.00379   
## Scale est. = 0.15387   n = 47

model5.Aces.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Aces)
summary(model5.Aces.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 672.7
##
## Scaled residuals: 
## Min       1Q   Median       3Q      Max 
## -2.17811 -0.23764  0.08008  0.50304  2.33490 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.07786 0.279   
## Residual             16.49186 4.061   
## Number of obs: 47, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -73.34503  109.20536  28.70918  -0.672    0.507
## ANO           0.04314    0.05466  28.42371   0.789    0.436
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Aces.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Aces)
summary(model6.Aces.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 106.6
##
## Scaled residuals: 
## Min       1Q   Median       3Q      Max 
## -2.92793 -0.04439  0.26893  0.49604  1.60891 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.0000   0.0000  
## Residual             0.1607   0.4009  
## Number of obs: 47, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -7.253865  10.719853 45.000000  -0.677    0.502
## ANO          0.004877   0.005365 45.000000   0.909    0.368
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



model.Aces.cp.gam <- gam(CP ~ s(ANO, k = 21), data = Aces, method = "REML", sp = 0.0001)
summary(model.Aces.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  12.8702     0.5254    24.5   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 1.507   0.149
##
## R-sq.(adj) =  0.209   Deviance explained = 54.2%
## -REML = 166.74  Scale est. = 12.976    n = 47

gam.check(model.Aces.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.827918e-06,-1.827918e-06]
## (score 166.7415 & scale 12.97562).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.33    0.98



model2.Aces.cp.gam <- gam(log(CP) ~ s(ANO, k = 21), data = Aces, method = "REML", sp = 0.0001)
summary(model2.Aces.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.48984    0.05134   48.49   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 1.578   0.126
##
## R-sq.(adj) =  0.226   Deviance explained = 55.2%
## -REML = 61.986  Scale est. = 0.1239    n = 47

gam.check(model2.Aces.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.279008e-06,-1.279008e-06]
## (score 61.98573 & scale 0.1238958).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.33    0.98



model3.Aces.cp.gam <- gam(CP ~ s(ANO, k = 21), random=list(Amb=~1), data = Aces, method = "REML", sp = 0.0001)
summary(model3.Aces.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  12.8702     0.5254    24.5   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 1.507   0.149
##
## R-sq.(adj) =  0.209   Deviance explained = 54.2%
## -REML = 166.74  Scale est. = 12.976    n = 47

gam.check(model3.Aces.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.827918e-06,-1.827918e-06]
## (score 166.7415 & scale 12.97562).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.33    0.97



model4.Aces.cp.gam <- gam(log(CP) ~ s(ANO, k = 21), random=list(Amb=~1), data = Aces, method = "REML", sp = 0.0001)
summary(model4.Aces.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## Intercept)  2.48984    0.05134   48.49   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 1.578   0.126
##
## R-sq.(adj) =  0.226   Deviance explained = 55.2%
## -REML = 61.986  Scale est. = 0.1239    n = 47

gam.check(model4.Aces.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.279008e-06,-1.279008e-06]
## (score 61.98573 & scale 0.1238958).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.33    0.98



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
## (Intercept)   39.360      5.087   7.737 8.35e-10 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.136   0.714
##
## R-sq.(adj) =  -0.0192   
## Scale est. = 1190.5    n = 47

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
## (Intercept)   3.2045     0.1739   18.42   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.266   0.609
##
## R-sq.(adj) =  -0.0163   
## Scale est. = 1.3917    n = 47

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
## (Intercept) -304.2524   942.9070  -0.323    0.748
## ANO            0.1720     0.4719   0.364    0.717
##
## R-sq.(adj) =  -0.0192   
## Scale est. = 1190.5    n = 47

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
## (Intercept) -13.237340  32.239051  -0.411    0.683
## ANO           0.008229   0.016136   0.510    0.613
##
## R-sq.(adj) =  -0.0163   
## Scale est. = 1.3917    n = 47

model5.Aces.p.lmer<-lmer(P~ANO + (1|Amb), data = Aces)
summary(model5.Aces.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 460.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.1406 -0.5058 -0.1791  0.2064  3.8919 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)    8.78   2.963  
## Residual             1239.92  35.213  
## Number of obs: 47, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -261.5204   949.2552   29.7703  -0.276    0.785
## ANO            0.1505     0.4751   29.4929   0.317    0.754
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Aces.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Aces)
summary(model6.Aces.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Aces
##
## REML criterion at convergence: 157
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -2.7195 -0.0951  0.2464  0.5454  1.6896 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.000    0.000   
## Residual             1.454    1.206   
## Number of obs: 47, groups:  Amb, 2
##
## Fixed effects:
## Estimate Std. Error         df t value Pr(>|t|)
## (Intercept) -13.237340  32.239051  45.000000  -0.411    0.683
## ANO           0.008229   0.016136  45.000000   0.510    0.613
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



model.Aces.p.gam <- gam(P ~ s(ANO, k = 21), data = Aces, method = "REML", sp = 0.0001)
summary(model.Aces.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  39.360      4.124   9.543 4.42e-10 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 2.052  0.0379 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.345   Deviance explained = 62.1%
## -REML = 260.24  Scale est. = 799.49    n = 47

gam.check(model.Aces.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.964151e-07,-1.964151e-07]
## (score 260.2357 & scale 799.4897).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.44       1



model2.Aces.p.gam <- gam(log(P) ~ s(ANO, k = 21), data = Aces, method = "REML", sp = 0.0001)
summary(model2.Aces.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.204      0.158   20.28   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 1.413   0.189
##
## R-sq.(adj) =  0.179   Deviance explained = 52.5%
## -REML = 112.54  Scale est. = 1.1739    n = 47

gam.check(model2.Aces.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.760261e-06,-2.760261e-06]
## (score 112.5412 & scale 1.173856).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.39    0.99



model3.Aces.p.gam <- gam(P ~ s(ANO, k = 21), random=list(Amb=~1), data = Aces, method = "REML", sp = 0.0001)
summary(model3.Aces.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  39.360      4.124   9.543 4.42e-10 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 2.052  0.0379 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.345   Deviance explained = 62.1%
## -REML = 260.24  Scale est. = 799.49    n = 47

gam.check(model3.Aces.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.964151e-07,-1.964151e-07]
## (score 260.2357 & scale 799.4897).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.44    0.99



model4.Aces.p.gam <- gam(log(P) ~ s(ANO, k = 21), random=list(Amb=~1), data = Aces, method = "REML", sp = 0.0001)
summary(model4.Aces.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 21)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## Intercept)  3.204      0.158   20.28   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 19.39  19.94 1.413   0.189
##
## R-sq.(adj) =  0.179   Deviance explained = 52.5%
## -REML = 112.54  Scale est. = 1.1739    n = 47

gam.check(model4.Aces.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.760261e-06,-2.760261e-06]
## (score 112.5412 & scale 1.173856).
## Hessian positive definite, eigenvalue range [22.5,22.5].
## Model rank =  21 / 21 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 20.0 19.4    1.39    0.99



##----------------------------------------------------------------------------##



Aces2 <- filter(Aces, Amb == "Pequeno")
Aces3 <- filter(Aces, Amb == "Grande")

str(Aces2)
Aces2$CP
Aces2$P
Aces2$ANO
Aces2$Longitude
Aces2$Latitude
Aces2$Amb

summary(Aces2)
##        CP              P               ANO          state               Amb              Longitude     
## Min.   : 3.50   Min.   :  1.00   Min.   :1983   Length:32          Length:32          Min.   :-51.86  
## 1st Qu.:12.05   1st Qu.: 23.00   1st Qu.:1990   Class :character   Class :character   1st Qu.:-49.84  
## Median :13.40   Median : 33.00   Median :2005   Mode  :character   Mode  :character   Median :-49.46  
## Mean   :13.38   Mean   : 43.30   Mean   :2001                                         Mean   :-49.61  
## 3rd Qu.:15.53   3rd Qu.: 55.62   3rd Qu.:2006                                         3rd Qu.:-49.28  
## Max.   :22.00   Max.   :175.50   Max.   :2017                                         Max.   :-47.50  
## NA's   :2       
##
## Latitude     
## Min.   :-23.33  
## 1st Qu.:-21.14  
## Median :-20.62  
## Mean   :-20.84  
## 3rd Qu.:-20.42  
## Max.   :-17.21  
## NA's   :2     



## Acestrorhynchus lacustris_AmbPeque - teste de normalidade -----------------##
shapiro.test(Aces2$CP) ## W = 0.93262, p-value = 0.04635 
shapiro.test(Aces2$P)  ## W = 0.83791, p-value = 0.0002296 
shapiro.test(Aces2$ANO) ## W = 0.89371, p-value = 0.004299 



## Acestrorhynchus lacustris_AmbPeque - cor e cov ----------------------------##
cor(Aces2$ANO, Aces2$CP) ## 0.1209257
cor(Aces2$ANO, Aces2$P)  ## 0.0516827
cor(Aces2$ANO, Aces2$CP, method="spearman") ## 0.1993202
cor(Aces2$ANO, Aces2$P, method="spearman")  ## 0.1235982

cov(Aces2$ANO, Aces2$CP) ## 4.784778
cov(Aces2$ANO, Aces2$P)  ## 18.21079



## Acestrorhynchus lacustris_AmbPeque - CP -----------------------------------##

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
## (Intercept)  13.3844     0.7207   18.57   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO)   1      1 0.46   0.503
##
## R-sq.(adj) =  -0.0182   
## Scale est. = 16.102    n = 32

model2.Aces2.cp.gamm<-gamm(log(CP)~s(ANO), data=Aces2)
summary(model2.Aces2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
## Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.53120    0.06995   36.19   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.458   0.504
##
## R-sq.(adj) =  -0.0183   
## Scale est. = 0.15167   n = 32

model3.Aces2.cp.lm<-lm(CP~ANO, data = Aces2)
summary(model3.Aces2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Aces2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -9.077 -0.935 -0.017  2.354  9.216 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -89.75467  154.57883  -0.581    0.566
## ANO           0.05155    0.07726   0.667    0.510
##
## Residual standard error: 4.144 on 30 degrees of freedom
## Multiple R-squared:  0.01462,	Adjusted R-squared:  -0.01822 
## F-statistic: 0.4452 on 1 and 30 DF,  p-value: 0.5097

model4.Aces2.cp.lm<-lm(log(CP)~ANO, data = Aces2)
summary(model4.Aces2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Aces2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.20024 -0.00362  0.06692  0.22831  0.61806 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -7.461348  15.002095  -0.497    0.623
## ANO          0.004995   0.007499   0.666    0.510
##
## Residual standard error: 0.4022 on 30 degrees of freedom
## Multiple R-squared:  0.01457,	Adjusted R-squared:  -0.01827 
## F-statistic: 0.4437 on 1 and 30 DF,  p-value: 0.5104



model.Aces2.cp.gam <- gam(CP ~ s(ANO, k = 18), data = Aces2, method = "REML", sp = 0.0001)
summary(model.Aces2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  13.3844     0.7619   17.57 5.85e-11 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.95     17 0.829    0.65
##
## R-sq.(adj) =  -0.101   Deviance explained = 50.1%
## -REML = 142.01  Scale est. = 18.577    n = 32

gam.check(model.Aces2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.399441e-06,-3.399441e-06]
## (score 142.0134 & scale 18.57749).
## Hessian positive definite, eigenvalue range [15,15].
## Model rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 17.0 16.9    1.58       1



model2.Aces2.cp.gam <- gam(log(CP) ~ s(ANO, k=18), data = Aces2, method = "REML", sp = 0.0001)
summary(model2.Aces2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.53120    0.07483   33.83 7.26e-15 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.95     17 0.79   0.683
##
## R-sq.(adj) =  -0.128   Deviance explained = 48.9%
## -REML = 72.388  Scale est. = 0.17918   n = 32

gam.check(model2.Aces2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.542119e-06,-4.542119e-06]
## (score 72.38802 & scale 0.1791783).
## Hessian positive definite, eigenvalue range [15,15].
## Model rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 17.0 16.9    1.56       1



## Acestrorhynchus lacustris_AmbPeque - P ------------------------------------##

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
## (Intercept)   43.303      6.457   6.707 1.97e-07 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.083   0.775
##
## R-sq.(adj) =  -0.0306   
## Scale est. = 1292.4    n = 32

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
## (Intercept)   3.2992     0.2204   14.97 1.86e-15 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.135   0.716
##
## R-sq.(adj) =  -0.0289   
## Scale est. = 1.5061    n = 32

model3.Aces2.p.lm<-lm(P~ANO, data = Aces2)
summary(model3.Aces2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Aces2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -43.548 -18.926  -9.095  11.273 134.484 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -349.2424  1384.8677  -0.252    0.803
## ANO            0.1962     0.6922   0.283    0.779
##
## Residual standard error: 37.13 on 30 degrees of freedom
## Multiple R-squared:  0.002671,	Adjusted R-squared:  -0.03057 
## F-statistic: 0.08035 on 1 and 30 DF,  p-value: 0.7788

model4.Aces2.p.lm<-lm(log(P)~ANO, data = Aces2)
summary(model4.Aces2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Aces2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.3533 -0.1051  0.2400  0.6726  1.9678 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -13.757642  47.275705  -0.291    0.773
## ANO           0.008526   0.023630   0.361    0.721
##
## Residual standard error: 1.267 on 30 degrees of freedom
## Multiple R-squared:  0.00432,	Adjusted R-squared:  -0.02887 
## F-statistic: 0.1302 on 1 and 30 DF,  p-value: 0.7208



model.Aces2.p.gam <- gam(P ~ s(ANO, k = 18), data = Aces2, method = "REML", sp = 0.001)
summary(model.Aces2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  43.303      6.095   7.104  4.4e-06 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.53  16.96 1.195   0.397
##
## R-sq.(adj) =  0.111   Deviance explained = 58.5%
## -REML = 187.33  Scale est. = 1189      n = 32

gam.check(model.Aces2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.290156e-07,-5.290156e-07]
## (score 187.3273 & scale 1188.954).
## Hessian positive definite, eigenvalue range [15,15].
## Model rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 17.0 16.5    1.67       1



model2.Aces2.p.gam <- gam(log(P) ~ s(ANO, k = 18), data = Aces2, method = "REML", sp = 0.0001)
summary(model2.Aces2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.2992     0.2287   14.43  8.1e-10 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.95     17 0.874    0.61
##
## R-sq.(adj) =  -0.0716   Deviance explained = 51.4%
## -REML = 105.91  Scale est. = 1.6732    n = 32

gam.check(model2.Aces2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.404253e-06,-2.404253e-06]
## (score 105.9066 & scale 1.673226).
## Hessian positive definite, eigenvalue range [15,15].
## Model rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 17.0 16.9    1.57       1



##----------------------------------------------------------------------------##

summary(Aces3)
##       CP              P               ANO          state               Amb              Longitude     
## Min.   : 4.50   Min.   :  2.00   Min.   :1980   Length:15          Length:15          Min.   :-51.35  
## 1st Qu.:10.20   1st Qu.: 12.00   1st Qu.:1983   Class :character   Class :character   1st Qu.:-49.82  
## Median :12.90   Median : 31.00   Median :1988   Mode  :character   Mode  :character   Median :-49.42  
## Mean   :11.77   Mean   : 30.95   Mean   :1992                                         Mean   :-49.70  
## 3rd Qu.:13.25   3rd Qu.: 33.75   3rd Qu.:2004                                         3rd Qu.:-49.40  
## Max.   :19.80   Max.   :131.00   Max.   :2013                                         Max.   :-47.70  
## Latitude     
## Min.   :-21.55  
## 1st Qu.:-20.66  
## Median :-20.61  
## Mean   :-20.74  
## 3rd Qu.:-20.61  
## Max.   :-20.27

## Acestrorhynchus lacustris_AmbGran - teste de normalidade -----------------##
shapiro.test(Aces3$CP) ## W = 0.90021, p-value = 0.09589 
shapiro.test(Aces3$P)  ## W = 0.6872, p-value = 0.0001819
shapiro.test(Aces3$ANO) ## W = 0.83745, p-value = 0.01159  



## Acestrorhynchus lacustris_AmbGran - cor e cov ----------------------------##
cor(Aces3$ANO, Aces3$CP) ## -0.05651074
cor(Aces3$ANO, Aces3$P)  ## -0.1300188
cor(Aces3$ANO, Aces3$CP, method="spearman") ## -0.2017938
cor(Aces3$ANO, Aces3$P, method="spearman")  ## -0.3151299

cov(Aces3$ANO, Aces3$CP) ## -2.578095
cov(Aces3$ANO, Aces3$P)  ## -47.36333



## Acestrorhynchus lacustris_AmbGran - CP -----------------------------------##

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
## (Intercept)  11.7733     0.9872   11.93 2.25e-08 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.045   0.836
##
## R-sq.(adj) =  -0.0735   
## Scale est. = 13.644    n = 15

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
## (Intercept)   2.4016     0.1039   23.12 6.07e-12 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO)   1      1 0.01   0.924
##
## R-sq.(adj) =  -0.0762   
## Scale est. = 0.1511    n = 15

model3.Aces3.cp.lm<-lm(CP~ANO, data = Aces3)
summary(model3.Aces3.cp.lm)
## Call:
##   lm(formula = CP ~ ANO, data = Aces3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -7.405 -1.374  1.049  1.481  7.967 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  47.96355  177.33774   0.270    0.791
## ANO          -0.01817    0.08901  -0.204    0.841
##
## Residual standard error: 3.968 on 13 degrees of freedom
## Multiple R-squared:  0.003193,	Adjusted R-squared:  -0.07348 
## F-statistic: 0.04165 on 1 and 13 DF,  p-value: 0.8415

model4.Aces3.cp.lm<-lm(log(CP)~ANO, data = Aces3)
summary(model4.Aces3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Aces3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.89111 -0.09658  0.16293  0.18900  0.58697 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 6.462e-01  1.866e+01   0.035    0.973
## ANO         8.811e-04  9.367e-03   0.094    0.926
##
## Residual standard error: 0.4176 on 13 degrees of freedom
## Multiple R-squared:  0.0006801,	Adjusted R-squared:  -0.07619 
## F-statistic: 0.008847 on 1 and 13 DF,  p-value: 0.9265



model.Aces3.cp.gam <- gam(CP ~ s(ANO, k = 12), data = Aces3, method = "REML", sp = 0.0001)
summary(model.Aces3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  11.7733     0.1426   82.57 2.02e-06 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.81  10.99 59.06 0.00315 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

gam.check(model.Aces3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.428339e-06,-2.428339e-06]
## (score 46.29217 & scale 0.3049601).
## Hessian positive definite, eigenvalue range [6.500002,6.500002].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.8    1.74       1



model2.Aces3.cp.gam <- gam(log(CP) ~ s(ANO, k=12), data = Aces3, method = "REML", sp = 0.0001)
summary(model2.Aces3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.40159    0.01346   178.5 1.73e-07 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.81  10.99 74.08 0.00226 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.983   Deviance explained = 99.6%
## -REML =  14.94  Scale est. = 0.0027166  n = 15

gam.check(model2.Aces3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-7.54851e-07,-7.54851e-07]
## (score 14.94011 & scale 0.002716637).
## Hessian positive definite, eigenvalue range [6.500001,6.500001].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.8    1.76    0.99



## Acestrorhynchus lacustris_AmbGran - P -------------------------------------##

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
## (Intercept)   30.947      7.828   3.953  0.00165 **
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.241   0.632
##
## R-sq.(adj) =  -0.0587   
## Scale est. = 857.92    n = 15

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
## (Intercept)   3.0024     0.2801   10.72 7.99e-08 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.007   0.935
##
## R-sq.(adj) =  -0.0764   
## Scale est. = 1.0983    n = 15

model3.Aces3.p.lm<-lm(P~ANO, data = Aces3)
summary(model3.Aces3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Aces3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -31.372 -14.862  -2.705   3.037  98.963 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  695.8132  1406.2469   0.495    0.629
## ANO           -0.3337     0.7058  -0.473    0.644
##
## Residual standard error: 31.46 on 13 degrees of freedom
## Multiple R-squared:  0.0169,	Adjusted R-squared:  -0.05872 
## F-statistic: 0.2235 on 1 and 13 DF,  p-value: 0.6442

model4.Aces3.p.lm<-lm(log(P)~ANO, data = Aces3)
summary(model4.Aces3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Aces3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.3239 -0.5023  0.4149  0.4921  1.8662 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  7.031099  50.316102    0.14    0.891
## ANO         -0.002022   0.025255   -0.08    0.937
##
## Residual standard error: 1.126 on 13 degrees of freedom
## Multiple R-squared:  0.0004929,	Adjusted R-squared:  -0.07639 
## F-statistic: 0.006411 on 1 and 13 DF,  p-value: 0.9374



model.Aces3.p.gam <- gam(P ~ s(ANO, k = 12), data = Aces3, method = "REML", sp = 0.0001)
summary(model.Aces3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  30.947      5.083   6.088  0.00744 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.81  10.99 2.672   0.228
##
## R-sq.(adj) =  0.585   Deviance explained = 90.6%
## -REML = 83.702  Scale est. = 387.61    n = 15

gam.check(model.Aces3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.977562e-05,-1.977562e-05]
## (score 83.702 & scale 387.615).
## Hessian positive definite, eigenvalue range [6.50002,6.50002].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.8    1.72       1



model2.Aces3.p.gam <- gam(log(P) ~ s(ANO, k=12), data = Aces3, method = "REML", sp = 0.0001)
summary(model2.Aces3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.00240    0.03197   93.91 1.34e-06 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.81  10.99 94.8 0.00156 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.987   Deviance explained = 99.7%
## -REML = 28.838  Scale est. = 0.015332  n = 15

gam.check(model2.Aces3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.792655e-10,-1.792655e-10]
## (score 28.83765 & scale 0.01533156).
## Hessian positive definite, eigenvalue range [6.5,6.5].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.8    1.52    0.96




## ---------------------------------------------------------------------------##
                             ## C.callichthys ##

dir()
Calli <- read.table("C.callichthys_Geral2.txt", header=T)
str(Calli)
Calli$CP
Calli$P
Calli$ANO
Calli$Longitude
Calli$Latitude
Calli$Amb
Calli$state


summary(Calli)
##       CP               P               ANO          state               Amb              Longitude     
## Min.   : 2.900   Min.   : 1.000   Min.   :1968   Length:38          Length:38          Min.   :-52.17  
## 1st Qu.: 4.225   1st Qu.: 2.775   1st Qu.:2003   Class :character   Class :character   1st Qu.:-49.70  
## Median : 6.100   Median : 8.050   Median :2005   Mode  :character   Mode  :character   Median :-49.38  
## Mean   : 6.200   Mean   :10.884   Mean   :2004                                         Mean   :-49.24  
## 3rd Qu.: 7.625   3rd Qu.:15.400   3rd Qu.:2007                                         3rd Qu.:-48.60  
## Max.   :11.400   Max.   :42.000   Max.   :2013                                         Max.   :-46.92  
##                                                                                        NA's   :1       
##    Latitude     
##  Min.   :-24.12  
## 1st Qu.:-22.29  
## Median :-21.06  
## Mean   :-20.36  
## 3rd Qu.:-20.85  
## Max.   : 20.83  
## NA's   :1  


## C.callichthys_Geral2 - teste de normalidade --------------------------------##
shapiro.test(Calli$CP) ## W = 0.95858, p-value = 0.1707
shapiro.test(Calli$P)  ## W = 0.84501, p-value = 9.957e-05
shapiro.test(Calli$ANO) ## W = 0.73831, p-value = 7.216e-07


## C.callichthys_Geral2 - cor e cov -------------------------------------------##
cor(Calli$ANO, Calli$CP) ## 0.0562421
cor(Calli$ANO, Calli$P)  ## 0.02853436
cor(Calli$ANO, Calli$CP, method="spearman") ## 0.1004748
cor(Calli$ANO, Calli$P, method="spearman")  ## 0.07305374

cov(Calli$ANO, Calli$CP) ## 1.040541
cov(Calli$ANO, Calli$P)  ## 2.413087


## C.callichthys_Geral2 - CP --------------------------------------------------##

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
## (Intercept)   6.1720     0.4091   15.09   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df   F p-value
## s(ANO)   1      1 0.1   0.754
##
## R-sq.(adj) =  -0.0245   
## Scale est. = 5.1486    n = 38

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
## (Intercept)  1.72836    0.09694   17.83   <2e-16 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.497  1.497 0.361   0.579
##
## R-sq.(adj) =  0.0104   
## Scale est. = 0.13402   n = 38

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
## (Intercept) -23.79488   96.17029  -0.247    0.806
## ANO           0.01495    0.04799   0.312    0.757
##
## R-sq.(adj) =  -0.0245   
## Scale est. = 5.1486    n = 38

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
## (Intercept) -2.501079  15.967508  -0.157    0.876
## ANO          0.002114   0.007968   0.265    0.792
##
## R-sq.(adj) =  -0.0243   
## Scale est. = 0.14129   n = 38

model5.Calli.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Calli)
summary(model5.Calli.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Calli
##
## REML criterion at convergence: 174.3
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.5586 -0.7690 -0.1105  0.5747  2.2680 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.4664   0.683   
## Residual             5.2683   2.295   
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -15.36026   95.01779  35.38653  -0.162    0.872
## ANO           0.01070    0.04742  35.39134   0.226    0.823
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
## REML criterion at convergence: 45.3
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.9796 -0.6779  0.0452  0.7366  1.9715 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.02259  0.1503  
## Residual             0.14499  0.3808  
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -1.451580  15.778452 35.276707  -0.092    0.927
## ANO          0.001585   0.007874 35.278291   0.201    0.842
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Calli.cp.gam <- gam(CP ~ s(ANO, k = 15), data = Calli, method = "REML", sp = 0.0001)
summary(model.Calli.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 15)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.2000     0.3118   19.89   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 6.14  7.223 2.64    0.03 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =   0.31   Deviance explained = 42.5%
## -REML = 85.928  Scale est. = 3.6936    n = 38

gam.check(model.Calli.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.999133e-05,-5.999133e-05]
## (score 85.92845 & scale 3.693618).
## Hessian positive definite, eigenvalue range [18.00006,18.00006].
## Model rank =  15 / 15 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'   edf k-index p-value
## s(ANO) 14.00  6.14    1.13    0.78



model2.Calli.cp.gam <- gam(log(CP) ~ s(ANO, k = 15), data = Calli, method = "REML", sp = 0.0001)
summary(model2.Calli.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 15)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.75343    0.05362    32.7   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 6.14  7.223 2.343  0.0498 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.281   Deviance explained =   40%
## -REML = 22.529  Scale est. = 0.10925   n = 38

gam.check(model2.Calli.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.231602e-10,-2.231602e-10]
## (score 22.52864 & scale 0.1092504).
## Hessian positive definite, eigenvalue range [18,18].
## Model rank =  15 / 15 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'   edf k-index p-value
## s(ANO) 14.00  6.14    1.02    0.47



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
## (Intercept)   10.884      1.715   6.346 2.41e-07 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO)   1      1 0.03   0.863
##
## R-sq.(adj) =  -0.0269   
## Scale est. = 108.86    n = 38

model2.Calli.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Calli)
summary(model2.Calli.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   1.7457     0.3181   5.487 3.43e-06 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.22   1.22 0.082   0.861
##
## R-sq.(adj) =  -0.0146   
## Scale est. = 1.216     n = 38

model3.Calli.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Calli)
summary(model3.Calli.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -64.77126  441.72042  -0.147    0.884
## ANO           0.03775    0.22042   0.171    0.865
##
## R-sq.(adj) =  -0.0269   
## Scale est. = 108.86    n = 38

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
## (Intercept) -2.501079  15.967508  -0.157    0.876
## ANO          0.002114   0.007968   0.265    0.792
##
## R-sq.(adj) =  -0.0243   
## Scale est. = 0.14129   n = 38

model5.Calli.p.lmer<-lmer(P~ANO + (1|Amb), data = Calli)
summary(model5.Calli.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Calli
##
## REML criterion at convergence: 284.3
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.9551 -0.7747 -0.2801  0.3992  2.9359 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)   1.529   1.237  
## Residual             114.187  10.686  
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -53.66428  440.96468  35.69120  -0.122    0.904
## ANO           0.03215    0.22005  35.70096   0.146    0.885
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Calli.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Calli)
summary(model6.Calli.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Calli
##
## REML criterion at convergence: 123.9
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.8379 -0.7984  0.1173  0.7230  2.0248 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.3028   0.5502  
## Residual             1.2744   1.1289  
## Number of obs: 38, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  2.037729  46.806087 35.206983   0.044    0.966
## ANO         -0.000155   0.023359 35.205601  -0.007    0.995
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Calli.p.gam <- gam(P ~ s(ANO, k = 12), data = Calli, method = "REML", sp = 0.001)
summary(model.Calli.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.884      1.326   8.206 7.73e-09 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 9.777  10.58 3.208 0.00799 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.402   Deviance explained =   56%
## -REML = 147.56  Scale est. = 66.86     n = 38

gam.check(model.Calli.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.173196e-06,-1.173196e-06]
## (score 147.5567 & scale 66.85995).
## Hessian positive definite, eigenvalue range [18,18].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'   edf k-index p-value
## s(ANO) 11.00  9.78    1.39    0.98



model2.Calli.p.gam <- gam(log(P) ~ s(ANO, k = 14), data = Calli, method = "REML", sp = 0.0001)
summary(model2.Calli.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 14)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.8369     0.1589   11.56 1.96e-11 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 12.39  12.91 2.149  0.0501 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.304   Deviance explained = 53.7%
## -REML = 79.946  Scale est. = 0.95986   n = 38

gam.check(model2.Calli.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.959481e-06,-1.959481e-06]
## (score 79.94615 & scale 0.9598554).
## Hessian positive definite, eigenvalue range [18,18].
## Model rank =  14 / 14 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##        k'  edf k-index p-value
## s(ANO) 13.0 12.4    1.05    0.54



##----------------------------------------------------------------------------##

dir()

Calli2 <- filter(Calli, Amb == "Grande")
Calli3 <- filter(Calli, Amb == "Pequeno")


Calli2$CP
Calli2$P
Calli2$ANO
Calli2$Longitude
Calli2$Latitude
Calli2$Amb

Calli2

summary(Calli2)
##       CP               P              ANO          state               Amb              Longitude     
## Min.   : 2.900   Min.   : 1.00   Min.   :1988   Length:25          Length:25          Min.   :-51.54  
## 1st Qu.: 5.200   1st Qu.: 4.00   1st Qu.:2003   Class :character   Class :character   1st Qu.:-49.66  
## Median : 6.300   Median : 9.50   Median :2005   Mode  :character   Mode  :character   Median :-49.38  
## Mean   : 6.632   Mean   :12.28   Mean   :2005                                         Mean   :-49.30  
## 3rd Qu.: 7.800   3rd Qu.:18.00   3rd Qu.:2006                                         3rd Qu.:-49.01  
## Max.   :11.400   Max.   :42.00   Max.   :2012                                         Max.   :-47.63  
## Latitude     
## Min.   :-22.56  
## 1st Qu.:-21.21  
## Median :-20.87  
## Mean   :-19.41  
## 3rd Qu.:-20.82  
## Max.   : 20.83

## C.callichthys_AmbGran - teste de normalidade ------------------------------##
shapiro.test(Calli2$CP) ## W = 0.98445, p-value = 0.9569
shapiro.test(Calli2$P)  ## W = 0.88785, p-value = 0.01007
shapiro.test(Calli2$ANO) ## W = 0.83766, p-value = 0.001036


## C.callichthys_AmbGran - cor e cov -----------------------------------------##
cor(Calli2$ANO, Calli2$CP) ## -0.1432591
cor(Calli2$ANO, Calli2$P)  ## -0.2831597
cor(Calli2$ANO, Calli2$CP, method="spearman") ## 0.026929
cor(Calli2$ANO, Calli2$P, method="spearman")  ## -0.01795959

cov(Calli2$ANO, Calli2$CP) ## -1.485167
cov(Calli2$ANO, Calli2$P)  ## -13.93033


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
## (Intercept)   6.6320     0.3673   18.06  1.2e-14 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 2.075  2.075 3.808   0.038 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.237   
## Scale est. = 3.2375    n = 25

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
## (Intercept)  1.83964    0.06243   29.46   <2e-16 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.839  1.839 2.158   0.179
##
## R-sq.(adj) =  0.133   
## Scale est. = 0.093553  n = 25

model3.Calli2.cp.lm<-lm(CP~ANO, data = Calli2)
summary(model3.Calli2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Calli2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.650 -1.736 -0.377  1.126  4.061 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 131.23931  179.49636   0.731    0.472
## ANO          -0.06216    0.08954  -0.694    0.495
##
## Residual standard error: 2.144 on 23 degrees of freedom
## Multiple R-squared:  0.02052,	Adjusted R-squared:  -0.02206 
## F-statistic: 0.4819 on 1 and 23 DF,  p-value: 0.4945

model4.Calli2.cp.lm<-lm(log(CP)~ANO, data = Calli2)
summary(model4.Calli2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Calli2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.76653 -0.22710 -0.00977  0.21016  0.53270 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 14.58874   28.83184   0.506    0.618
## ANO         -0.00636    0.01438  -0.442    0.662
##
## Residual standard error: 0.3444 on 23 degrees of freedom
## Multiple R-squared:  0.00843,	Adjusted R-squared:  -0.03468 
## F-statistic: 0.1955 on 1 and 23 DF,  p-value: 0.6625



model.Calli2.cp.gam <- gam(CP ~ s(ANO, k = 9), data = Calli2, method = "REML", sp = 0.001)
summary(model.Calli2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.6320     0.3083   21.51 1.61e-13 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.467  7.902 3.543  0.0138 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.472   Deviance explained = 63.6%
## -REML = 57.789  Scale est. = 2.3764    n = 25

gam.check(model.Calli2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-7.513987e-08,-7.513987e-08]
## (score 57.78859 & scale 2.376418).
## Hessian positive definite, eigenvalue range [11.5,11.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.47     1.4    0.95



model2.Calli2.cp.gam <- gam(log(CP) ~ s(ANO, k = 9), data = Calli2, method = "REML", sp = 0.001)
summary(model2.Calli2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 9)
##
## Parametric coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.83964    0.05285   34.81   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.467  7.902 2.783  0.0367 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.391   Deviance explained =   58%
## -REML = 17.198  Scale est. = 0.069826  n = 25

gam.check(model2.Calli2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-6.041954e-07,-6.041954e-07]
## (score 17.19845 & scale 0.06982573).
## Hessian positive definite, eigenvalue range [11.5,11.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.47    1.46    0.99



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
## (Intercept)   12.284      1.587    7.74 1.11e-07 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value   
## s(ANO) 2.275  2.275 6.14 0.00574 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.367   
## Scale est. = 60.447    n = 25

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
## (Intercept)   2.1335     0.1836   11.62 6.22e-11 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO) 1.715  1.715 1.63   0.295
##
## R-sq.(adj) =  0.0964   
## Scale est. = 0.80864   n = 25

model3.Calli2.p.lm<-lm(P~ANO, data = Calli2)
summary(model3.Calli2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Calli2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -11.097  -7.931  -2.097   5.319  20.401 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 1181.0561   825.4432   1.431    0.166
## ANO           -0.5830     0.4118  -1.416    0.170
##
## Residual standard error: 9.86 on 23 degrees of freedom
## Multiple R-squared:  0.08018,	Adjusted R-squared:  0.04019 
## F-statistic: 2.005 on 1 and 23 DF,  p-value: 0.1702

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
## -2.1270 -0.7205  0.1243  0.7431  1.3619 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 42.77342   83.02705   0.515    0.611
## ANO         -0.02027    0.04142  -0.489    0.629
##
## Residual standard error: 0.9918 on 23 degrees of freedom
## Multiple R-squared:  0.01031,	Adjusted R-squared:  -0.03272 
## F-statistic: 0.2396 on 1 and 23 DF,  p-value: 0.6291



model.Calli2.p.gam <- gam(P ~ s(ANO, k = 9), data = Calli2, method = "REML", sp = 0.001)
summary(model.Calli2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  12.284      1.311   9.372 5.07e-08 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.467  7.902 4.948 0.00285 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.576   Deviance explained = 70.8%
## -REML = 91.164  Scale est. = 42.953    n = 25

gam.check(model.Calli2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.709141e-09,-1.709141e-09]
## (score 91.16391 & scale 42.95268).
## Hessian positive definite, eigenvalue range [11.5,11.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.47    1.33     0.9



model2.Calli2.p.gam <- gam(log(P) ~ s(ANO, k = 9), data = Calli2, method = "REML", sp = 0.0001)
summary(model2.Calli2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 9)
##
## Parametric coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.1335     0.1532   13.93 2.19e-10 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.936  7.998 2.846  0.0358 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.384   Deviance explained = 58.8%
## -REML = 48.704  Scale est. = 0.58685   n = 25

gam.check(model2.Calli2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.732733e-07,-2.732733e-07]
## (score 48.70426 & scale 0.5868499).
## Hessian positive definite, eigenvalue range [11.5,11.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.94    1.39    0.97



##----------------------------------------------------------------------------##

str(Calli3)
Calli3$CP
Calli3$P
Calli3$ANO
Calli3$Longitude
Calli3$Latitude
Calli3$Amb

Calli3


summary(Calli3)
##        CP               P               ANO          state               Amb              Longitude     
## Min.   : 2.900   Min.   : 1.000   Min.   :1968   Length:13          Length:13          Min.   :-52.17  
## 1st Qu.: 3.200   1st Qu.: 1.000   1st Qu.:2002   Class :character   Class :character   1st Qu.:-50.19  
## Median : 4.200   Median : 2.100   Median :2006   Mode  :character   Mode  :character   Median :-49.05  
## Mean   : 5.369   Mean   : 8.192   Mean   :2003                                         Mean   :-49.10  
## 3rd Qu.: 6.500   3rd Qu.: 9.300   3rd Qu.:2010                                         3rd Qu.:-47.44  
## Max.   :11.000   Max.   :39.000   Max.   :2013                                         Max.   :-46.92  
##                                                                                          NA's   :1       
##     Latitude     
## Min.   :-24.12  
## 1st Qu.:-22.72  
## Median :-22.52  
## Mean   :-22.35  
## 3rd Qu.:-21.79  
## Max.   :-20.39  
## NA's   :1  



## C.callichthys_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Calli3$CP) ##  W = 0.87308, p-value = 0.05752
shapiro.test(Calli3$P)  ##  W = 0.70001, p-value = 0.0005551
shapiro.test(Calli3$ANO) ## W = 0.75984, p-value = 0.002362


## C.callichthys_AmbPeque - cor e cov ----------------------------------------##
cor(Calli3$ANO, Calli3$CP) ## 0.1414498
cor(Calli3$ANO, Calli3$P)  ## 0.2119638
cor(Calli3$ANO, Calli3$CP, method="spearman") ## 0.3139699
cor(Calli3$ANO, Calli3$P, method="spearman")  ## 0.2553934

cov(Calli3$ANO, Calli3$CP) ## 4.320513
cov(Calli3$ANO, Calli3$P)  ## 29.28013


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
## (Intercept)   5.3692     0.6502   8.258 6.56e-06 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.509  1.509 0.421   0.494
##
## R-sq.(adj) =  0.0906   
## Scale est. = 5.0731    n = 13

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
## (Intercept)   1.5876     0.1121   14.17 3.89e-08 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.593  1.593 0.637   0.436
##
## R-sq.(adj) =  0.114   
## Scale est. = 0.15071   n = 13

model3.Calli3.cp.lm<-lm(CP~ANO, data = Calli3)
summary(model3.Calli3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Calli3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.569 -2.015 -1.210  1.749  5.354 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -53.78938  124.83540  -0.431    0.675
## ANO           0.02954    0.06234   0.474    0.645
##
## Residual standard error: 2.611 on 11 degrees of freedom
## Multiple R-squared:  0.02001,	Adjusted R-squared:  -0.06908 
## F-statistic: 0.2246 on 1 and 11 DF,  p-value: 0.6448

model4.Calli3.cp.lm<-lm(log(CP)~ANO, data = Calli3)
summary(model4.Calli3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Calli3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.5348 -0.4261 -0.1574  0.3746  0.7774 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -5.417518  21.880831  -0.248    0.809
## ANO          0.003498   0.010926   0.320    0.755
##
## Residual standard error: 0.4577 on 11 degrees of freedom
## Multiple R-squared:  0.009232,	Adjusted R-squared:  -0.08084 
## F-statistic: 0.1025 on 1 and 11 DF,  p-value: 0.7548



model.Calli3.cp.gam <- gam(CP ~ s(ANO, k = 9), data = Calli3, method = "REML", sp = 0.0001)
summary(model.Calli3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.3692     0.5956   9.014 0.000217 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 6.748  7.458 1.394   0.408
##
## R-sq.(adj) =  0.277   Deviance explained = 68.4%
## -REML = 37.334  Scale est. = 4.6122    n = 13

gam.check(model.Calli3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.880408e-07,-2.880408e-07]
## (score 37.33378 & scale 4.612212).
## Hessian positive definite, eigenvalue range [5.5,5.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 6.75    1.67    0.99



model2.Calli3.cp.gam <- gam(log(CP) ~ s(ANO, k = 9), data = Calli3, method = "REML", sp = 0.0001)
summary(model2.Calli3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 9)
##
## Parametric coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.5876     0.1073    14.8 1.75e-05 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 6.748  7.458 1.277   0.441
## 
##  R-sq.(adj) =  0.228   Deviance explained = 66.2%
## -REML =  18.16  Scale est. = 0.14965   n = 13

gam.check(model2.Calli3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.195875e-07,-3.195875e-07]
## (score 18.15967 & scale 0.1496522).
## Hessian positive definite, eigenvalue range [5.5,5.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 6.75    1.57    0.95



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
## (Intercept)    8.192      3.022   2.711   0.0206 *
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.238  1.238 0.305   0.526
## 
## R-sq.(adj) =  0.0256   
## Scale est. = 109.56    n = 13

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
## (Intercept)   1.2665     0.3594   3.524  0.00499 **
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO) 1.34   1.34 0.18   0.731
##
## R-sq.(adj) =  0.014   
## Scale est. = 1.5497    n = 13

model3.Calli3.p.lm<-lm(P~ANO, data = Calli3)
summary(model3.Calli3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Calli3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -8.771 -7.670 -5.468  3.931 28.929 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -392.7257   557.3433  -0.705    0.496
## ANO            0.2002     0.2783   0.719    0.487
##
## Residual standard error: 11.66 on 11 degrees of freedom
## Multiple R-squared:  0.04493,	Adjusted R-squared:  -0.0419 
## F-statistic: 0.5175 on 1 and 11 DF,  p-value: 0.4869

model4.Calli3.p.lm<-lm(log(P)~ANO, data = Calli3)
summary(model4.Calli3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Calli3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.3012 -1.2261 -0.5311  1.1260  2.3529 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -8.135124  67.160086  -0.121    0.906
## ANO          0.004695   0.033536   0.140    0.891
##
## Residual standard error: 1.405 on 11 degrees of freedom
## Multiple R-squared:  0.001778,	Adjusted R-squared:  -0.08897 
## F-statistic: 0.0196 on 1 and 11 DF,  p-value: 0.8912



model.Calli3.p.gam <- gam(P ~ s(ANO, k = 9), data = Calli3, method = "REML", sp = 0.0001)
summary(model.Calli3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.192      2.513   3.259   0.0209 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 6.748  7.458 1.664   0.336
##
## R-sq.(adj) =  0.371   Deviance explained = 72.5%
## -REML = 53.725  Scale est. = 82.124    n = 13

gam.check(model.Calli3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.68325e-07,-1.68325e-07]
## (score 53.72475 & scale 82.12382).
## Hessian positive definite, eigenvalue range [5.5,5.5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 6.75    1.81    0.99



model2.Calli3.p.gam <- gam(log(P) ~ s(ANO, k = 9), data = Calli3, method = "REML", sp = 0.0001)
summary(model2.Calli3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 9)
##
## Parametric coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.2665     0.3357   3.773   0.0119 *
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 6.748  7.458 1.173   0.485
## 
##  R-sq.(adj) =  0.192   Deviance explained = 64.6%
## -REML = 30.827  Scale est. = 1.4647    n = 13

gam.check(model2.Calli3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-7.89683e-07,-7.89683e-07]
## (score 30.82677 & scale 1.464737).
## Hessian positive definite, eigenvalue range [5.500001,5.500001].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 6.75     1.6    0.97



## ---------------------------------------------------------------------------##
                              ## H.malabaricus ##

dir()
Mala <- read.table("H.malabaricus_Geral2.txt", header=T)

str(Mala)
Mala$CP
Mala$P
Mala$ANO
Mala$Longitude
Mala$Latitude
Mala$Amb
Mala$state

Mala

summary(Mala)
##       CP              P                ANO          state               Amb              Longitude     
## Min.   : 3.20   Min.   :  1.000   Min.   :1980   Length:222         Length:222         Min.   :-53.82  
## 1st Qu.: 6.80   1st Qu.:  6.125   1st Qu.:2003   Class :character   Class :character   1st Qu.:-50.48  
## Median : 9.40   Median : 18.400   Median :2005   Mode  :character   Mode  :character   Median :-49.47  
## Mean   :10.23   Mean   : 41.570   Mean   :2003                                         Mean   :-49.65  
## 3rd Qu.:12.90   3rd Qu.: 48.650   3rd Qu.:2007                                         3rd Qu.:-49.27  
## Max.   :28.00   Max.   :454.000   Max.   :2017                                         Max.   :-42.77  
##                                                                                        NA's   :9       
##     Latitude     
## Min.   :-24.69  
## 1st Qu.:-21.19  
## Median :-20.80  
## Mean   :-20.95  
## 3rd Qu.:-20.40  
## Max.   :-16.62  
## NA's   :9  



## H.malabaricus_Geral2 - teste de normalidade ----------------------------------##
shapiro.test(Mala$CP) ## W = 0.939, p-value = 5.231e-08
shapiro.test(Mala$P)  ## W = 0.6436, p-value < 2.2e-16
shapiro.test(Mala$ANO) ## W = 0.82699, p-value = 5.394e-15

## H.malabaricus_Geral2 - cor e cov ---------------------------------------------##
cor(Mala$ANO, Mala$CP) ## 0.03034292
cor(Mala$ANO, Mala$P)  ## 0.009416907
cor(Mala$ANO, Mala$CP, method="spearman") ## 0.04387951
cor(Mala$ANO, Mala$P, method="spearman")  ## 0.03954667

cov(Mala$ANO, Mala$CP) ## 1.065362
cov(Mala$ANO, Mala$P)  ## 4.465138



## H.malabaricus_Geral2 - CP ----------------------------------------------------##

model.Mala.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data=Mala)
summary(model.Mala.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
## 
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.2268     0.2991   34.19   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 2.796  2.796 1.781   0.228
##
## R-sq.(adj) =  0.0226   
## Scale est. = 19.775    n = 222

model2.Mala.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Mala)
summary(model2.Mala.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.23123    0.02905    76.8   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 2.622  2.622 1.283   0.317
## 
## R-sq.(adj) =  0.0197   
## Scale est. = 0.18654   n = 222

model3.Mala.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Mala)
summary(model3.Mala.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -25.00878   78.25569   -0.32    0.750
## ANO           0.01759    0.03906    0.45    0.653
##
## R-sq.(adj) =  -0.00362   
## Scale est. = 20.241    n = 222

model4.Mala.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Mala)
summary(model4.Mala.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -3.523221   7.582644  -0.465    0.643
## ANO          0.002872   0.003785   0.759    0.449
##
## R-sq.(adj) =  -0.00192   
## Scale est. = 0.19004   n = 222

model5.Mala.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Mala)
summary(model5.Mala.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
##    Data: Macu
##
## REML criterion at convergence: 1302.9
##
## Scaled residuals: 
##   Min     1Q Median     3Q    Max 
## -1.565 -0.783 -0.169  0.558  3.989 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.00    0.000   
## Residual             20.43    4.519   
## Number of obs: 222, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -25.00878   78.25569 220.00000   -0.32    0.750
## ANO           0.01759    0.03906 220.00000    0.45    0.653
##
## Correlation of Fixed Effects:
##     (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Mala.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Mala)
summary(model6.Mala.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Mala2
##
## REML criterion at convergence: 275.9
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.45616 -0.75948  0.03627  0.69254  2.60850 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.0000   0.0000  
## Residual             0.1918   0.4379  
## Number of obs: 222, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)  -3.523221   7.582644 220.000000  -0.465    0.643
## ANO           0.002872   0.003785 220.000000   0.759    0.449
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



model.Mala.cp.gam <- gam(CP ~ s(ANO, k = 30), data = Mala, method = "REML", sp = 0.0001)
summary(model.Mala.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 30)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.2268     0.2928   34.92   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 28.95     29 1.522  0.0514 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0646   Deviance explained = 18.7%
## -REML = 746.65  Scale est. = 19.036    n = 222

gam.check(model.Mala.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.178031e-08,-2.178031e-08]
## (score 746.6544 & scale 19.03614).
## Hessian positive definite, eigenvalue range [110,110].
## Model rank =  30 / 30 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 29.0 28.9    1.05    0.76



model2.Mala.cp.gam <- gam(log(CP) ~ s(ANO, k = 30), data = Mala, method = "REML", sp = 0.0001)
summary(model2.Mala.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 30)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.23123    0.02855   78.14   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 28.95     29 1.435  0.0806 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0544   Deviance explained = 17.8%
## -REML = 234.52  Scale est. = 0.18099   n = 222

gam.check(model2.Mala.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.61998e-08,-2.61998e-08]
## (score 234.519 & scale 0.1809916).
## Hessian positive definite, eigenvalue range [110,110].
## Model rank =  30 / 30 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 29.0 28.9    1.03    0.66



## H.malabaricus_Geral2 - P -----------------------------------------------------##

model.Mala.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data=Mala)
summary(model.Mala.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   41.570      4.089   10.17   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO)   1      1 0.02   0.889
##
## R-sq.(adj) =  -0.00446   
## Scale est. = 3694.6    n = 222

model2.Mala.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Mala)
summary(model2.Mala.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.88142    0.09251   31.15   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.548    0.46
##
## R-sq.(adj) =  -0.00206   
## Scale est. = 1.8913    n = 222

model3.Mala.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Mala)
summary(model3.Mala.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -106.10923 1057.26400   -0.10    0.920
## ANO            0.07371    0.52773    0.14    0.889
##
## R-sq.(adj) =  -0.00446   
## Scale est. = 3694.6    n = 222

model4.Mala.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Mala)
summary(model4.Mala.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -14.790784  23.920672  -0.618    0.537
## ANO           0.008821   0.011940   0.739    0.461
##
## R-sq.(adj) =  -0.00206   
## Scale est. = 1.8913    n = 222

model5.Mala.p.lmer<-lmer(P~ANO + (1|Amb), data = Mala)
summary(model5.Mala.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 2448.4
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.6812 -0.5745 -0.3887  0.1269  6.7686 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)    6.833  2.614  
## Residual             3724.926 61.032  
## Number of obs: 222, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)  -77.85583 1058.72533  210.74739  -0.074    0.941
## ANO            0.05966    0.52846  210.93612   0.113    0.910
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Mala.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Mala)
summary(model6.Mala.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Mala2
##
## REML criterion at convergence: 781.4
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.13437 -0.72292  0.00618  0.73114  2.34540 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.000    0.000   
## Residual             1.908    1.381   
## Number of obs: 222, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept) -14.790785  23.920672 220.000000  -0.618    0.537
## ANO           0.008821   0.011940 220.000000   0.739    0.461
## 
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



model.Mala.p.gam <- gam(P ~ s(ANO, k = 30), data = Mala, method = "REML", sp = 0.001)
summary(model.Mala.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 30)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  41.570      3.936   10.56   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 28.49  28.97 1.56  0.0426 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0733   Deviance explained = 19.3%
## -REML =   1287  Scale est. = 3439.5    n = 222

gam.check(model.Mala.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.056056e-08,-2.056056e-08]
## (score 1286.957 & scale 3439.453).
## Hessian positive definite, eigenvalue range [110,110].
## Model rank =  30 / 30 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 29.0 28.5     1.1    0.94



model2.Mala.p.gam <- gam(log(P) ~ s(ANO, k = 30), data = Mala, method = "REML", sp = 0.001)
summary(model2.Mala.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 30)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.88142    0.09099   31.67   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 28.49  28.97 1.25   0.196
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0349   Deviance explained = 15.9%
## -REML = 457.98  Scale est. = 1.838     n = 222

gam.check(model2.Mala.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.981592e-08,-3.981592e-08]
## (score 457.9814 & scale 1.838027).
## Hessian positive definite, eigenvalue range [110,110].
## Model rank =  30 / 30 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 29.0 28.5    1.02    0.59



##----------------------------------------------------------------------------##


Mala2 <- filter(Mala, Amb == "Grande")
Mala3 <- filter(Mala, Amb == "Pequeno")

str(Mala)
Mala2$CP
Mala2$P
Mala2$ANO
Mala2$Longitude
Mala2$Latitude
Mala2$Amb
Mala2$state

Mala2

summary(Mala2)
##       CP              P                ANO          state               Amb              Longitude     
## Min.   : 3.20   Min.   :  1.000   Min.   :1980   Length:126         Length:126         Min.   :-53.82  
## 1st Qu.: 6.90   1st Qu.:  6.125   1st Qu.:2003   Class :character   Class :character   1st Qu.:-50.42  
## Median : 9.45   Median : 17.000   Median :2004   Mode  :character   Mode  :character   Median :-49.55  
## Mean   :10.03   Mean   : 37.629   Mean   :2002                                         Mean   :-49.77  
## 3rd Qu.:12.50   3rd Qu.: 47.750   3rd Qu.:2006                                         3rd Qu.:-49.35  
## Max.   :26.00   Max.   :454.000   Max.   :2013                                         Max.   :-45.74  
## 
## Latitude     
## Min.   :-24.61  
## 1st Qu.:-21.11  
## Median :-20.71  
## Mean   :-20.78  
## 3rd Qu.:-20.39  
## Max.   :-16.62



## H.malabaricus_AmbGran - teste de normalidade ------------------------------##
shapiro.test(Mala2$CP) ## W = 0.95067, p-value = 0.0001646
shapiro.test(Mala2$P)  ## W = 0.59345, p-value < 2.2e-16
shapiro.test(Mala2$ANO) ## W = 0.78565, p-value = 2.766e-12



## H.malabaricus_AmbGran - cor e cov -----------------------------------------##
cor(Mala2$ANO, Mala2$CP) ## 0.02969961
cor(Mala2$ANO, Mala2$P)  ## -0.002306962
cor(Mala2$ANO, Mala2$CP, method="spearman") ## -0.03300317
cor(Mala2$ANO, Mala2$P, method="spearman")  ## -0.03963552

cov(Mala2$ANO, Mala2$CP) ## 0.9145333
cov(Mala2$ANO, Mala2$P)  ## -0.9979111



## H.malabaricus_AmbGran- CP -------------------------------------------------##

model.Mala2.cp.gamm<-gamm(CP~s(ANO), data=Mala2)
summary(model.Mala2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
## 
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.0262     0.3644   27.52   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
## s(ANO)   1      1 0.11    0.74
##
## R-sq.(adj) =  -0.00718   
## Scale est. = 16.598    n = 126

model2.Mala2.cp.gamm<-gamm(log(CP)~s(ANO), data=Mala2)
summary(model2.Mala2.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.22371    0.03653   60.88   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.242   0.624
##
## R-sq.(adj) =  -0.00612   
## Scale est. = 0.16679   n = 126

model3.Mala2.cp.lm<-lm(CP~ANO, data = Mala2)
summary(model3.Mala2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Mala2)
##
## Residuals: 
##    Min      1Q  Median      3Q     Max 
## -6.8845 -3.1393 -0.6103  2.4357 15.9639 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -22.31461   97.74637  -0.228    0.820
## ANO           0.01615    0.04881   0.331    0.741
##
## Residual standard error: 4.107 on 124 degrees of freedom
## Multiple R-squared:  0.0008821,	Adjusted R-squared:  -0.007175 
## F-statistic: 0.1095 on 1 and 124 DF,  p-value: 0.7413

model4.Mala2.cp.lm<-lm(log(CP)~ANO, data = Mala2)
summary(model4.Mala2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Mala2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.06922 -0.29425  0.01723  0.29674  1.03292 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -2.575113   9.798567  -0.263    0.793
## ANO          0.002397   0.004893   0.490    0.625
##
## Residual standard error: 0.4117 on 124 degrees of freedom
## Multiple R-squared:  0.001931,	Adjusted R-squared:  -0.006118 
## F-statistic: 0.2399 on 1 and 124 DF,  p-value: 0.6252



model.Mala2.cp.gam <- gam(CP ~ s(ANO), data = Mala2, method = "REML")
summary(model.Mala2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.0262     0.3652   27.45   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.344  1.609 0.328   0.774
##
## R-sq.(adj) =  -0.00343   Deviance explained = 0.736%
## -REML = 355.92  Scale est. = 16.803    n = 126

gam.check(model.Mala2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-0.0003446661,0.0002139474]
## (score 355.9166 & scale 16.80303).
## Hessian positive definite, eigenvalue range [0.04854997,62.00026].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
##indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 9.00 1.34    0.96    0.32



model2.Mala2.cp.gam <- gam(log(CP) ~ s(ANO, k = 22), data = Mala2, method = "REML", sp = 0.0001)
summary(model2.Mala2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 22)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  22.22371    0.03687   60.32   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 20.8  20.99 0.885    0.61
##
## R-sq.(adj) =  -0.0166   Deviance explained = 15.3%
## -REML = 127.26  Scale est. = 0.17126   n = 126

gam.check(model2.Mala2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.844076e-08,-2.844076e-08]
## (score 127.264 & scale 0.1712556).
## Hessian positive definite, eigenvalue range [62,62].
## Model rank =  22 / 22 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 21.0 20.8    0.99    0.49



## H.malabaricus_AmbGran - P -------------------------------------------------##

model.Mala2.p.gamm<-gamm(P~s(ANO), data = Mala2)
summary(model.Mala2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   37.629      5.121   7.348 2.36e-11 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.001   0.979
##
## R-sq.(adj) =  -0.00806   
## Scale est. = 3278.3    n = 126

model2.Mala2.p.gamm<-gamm(log(P)~s(ANO), data=Mala2)
summary(model2.Mala2.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.835      0.117   24.23   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.174   0.677
##
## R-sq.(adj) =  -0.00666   
## Scale est. = 1.7114    n = 126

model3.Mala2.p.lm<-lm(P~ANO, data = Mala2)
summary(model3.Mala2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Mala2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -36.95 -31.44 -20.60  10.01 416.38 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)   72.91867 1373.70576   0.053    0.958
## ANO           -0.01762    0.68603  -0.026    0.980
##
## Residual standard error: 57.72 on 124 degrees of freedom
## Multiple R-squared:  5.322e-06,	Adjusted R-squared:  -0.008059 
## F-statistic: 0.0006599 on 1 and 124 DF,  p-value: 0.9795

model4.Mala2.p.lm<-lm(log(P)~ANO, data = Mala2)
summary(model4.Mala2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Mala2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.8915 -0.9563 -0.0127  1.0226  3.2787 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -10.21922   31.38642  -0.326    0.745
## ANO           0.00652    0.01567   0.416    0.678
##
## Residual standard error: 1.319 on 124 degrees of freedom
## Multiple R-squared:  0.001393,	Adjusted R-squared:  -0.00666 
## F-statistic: 0.173 on 1 and 124 DF,  p-value: 0.6782



model.Mala2.p.gam <- gam(P ~ s(ANO), data = Mala2, method = "REML")
summary(model.Mala2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  37.629      5.142   7.318 2.75e-11 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.001  1.003 0.001   0.991
## 
## R-sq.(adj) =  -0.00806   Deviance explained = 0.00151%
## -REML = 683.67  Scale est. = 3331.1    n = 126

gam.check(model.Mala2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-0.0003444446,0.0003007677]
## (score 683.6712 & scale 3331.133).
## Hessian positive definite, eigenvalue range [0.0003443885,61.9997].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##       k' edf k-index p-value
## s(ANO)  9   1    1.05    0.64



model2.Mala2.p.gam <- gam(log(P) ~ s(ANO), data = Mala2, method = "REML")
summary(model2.Mala2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.8354     0.1169   24.25   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.674  2.081 0.533   0.555
## 
## R-sq.(adj) =  0.00321   Deviance explained = 1.66%
## -REML = 214.92  Scale est. = 1.7219    n = 126

gam.check(model2.Mala2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.540433e-07,1.405771e-09]
## (score 214.9163 & scale 1.7219).
## Hessian positive definite, eigenvalue range [0.146506,62.00184].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 9.00 1.67    0.93    0.26



##----------------------------------------------------------------------------##

Mala3$CP
Mala3$P
Mala3$ANO
Mala3$Longitude
Mala3$Latitude
Mala3$Amb

summary(Mala3)
##       CP              P               ANO          state               Amb              Longitude     
## Min.   : 3.70   Min.   :  1.00   Min.   :1980   Length:96          Length:96          Min.   :-53.78  
## 1st Qu.: 6.50   1st Qu.:  6.75   1st Qu.:2004   Class :character   Class :character   1st Qu.:-50.81  
## Median : 9.40   Median : 21.05   Median :2006   Mode  :character   Mode  :character   Median :-49.46  
## Mean   :10.49   Mean   : 46.74   Mean   :2005                                         Mean   :-49.47  
## 3rd Qu.:13.10   3rd Qu.: 56.00   3rd Qu.:2010                                         3rd Qu.:-48.69  
## Max.   :28.00   Max.   :320.00   Max.   :2017                                         Max.   :-42.77  
## NA's   :9       
##
## Latitude     
## Min.   :-24.69  
## 1st Qu.:-21.93  
## Median :-20.90  
## Mean   :-21.20  
## 3rd Qu.:-20.40  
## Max.   :-17.54  
## NA's   :9   



## H.malabaricus_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Mala3$CP) ##  W = 0.92864, p-value = 5.902e-05
shapiro.test(Mala3$P)  ## W = 0.69115, p-value = 6.553e-13
shapiro.test(Mala3$ANO) ## W = 0.81691, p-value = 1.462e-09



## H.malabaricus_AmbPeque - cor e cov ----------------------------------------##
cor(Mala3$ANO, Mala3$CP) ## 0.01653931
cor(Mala3$ANO, Mala3$P)  ## -0.0007087565
cor(Mala3$ANO, Mala3$CP, method="spearman") ## 0.1562987
cor(Mala3$ANO, Mala3$P, method="spearman")  ## 0.138023

cov(Mala3$ANO, Mala3$CP) ## 0.6606634
cov(Mala3$ANO, Mala3$P)  ## -0.3672807



## H.malabaricus_AmbPeque - CP -----------------------------------------------##

model.Mala3.cp.gamm<-gamm(CP~s(ANO), data=Mala3)
summary(model.Mala3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.4901     0.4942   21.23   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 2.641  2.641 3.112   0.145
##
## R-sq.(adj) =  0.0663   
## Scale est. = 23.198    n = 96

model2.Mala3.cp.gamm<-gamm(log(CP)~s(ANO), data=Mala3)
summary(model2.Mala3.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.24110    0.04817   46.52   <2e-16 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.278   0.599
##
## R-sq.(adj) =  -0.00769   
## Scale est. = 0.22046   n = 96

model3.Mala3.cp.lm<-lm(CP~ANO, data = Mala3)
summary(model3.Mala3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Mala3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -6.804 -3.975 -1.028  2.547 17.674 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -10.41649  130.36047   -0.08    0.936
## ANO           0.01043    0.06503    0.16    0.873
##
## Residual standard error: 5.045 on 94 degrees of freedom
## Multiple R-squared:  0.0002735,	Adjusted R-squared:  -0.01036 
## F-statistic: 0.02572 on 1 and 94 DF,  p-value: 0.8729

model4.Mala3.cp.lm<-lm(log(CP)~ANO, data = Mala3)
summary(model4.Mala3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Mala3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.93694 -0.36465  0.01558  0.32319  1.14145 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -4.188335  12.261902  -0.342    0.733
## ANO          0.003207   0.006117   0.524    0.601
##
## Residual standard error: 0.4745 on 94 degrees of freedom
## Multiple R-squared:  0.002916,	Adjusted R-squared:  -0.007691 
## F-statistic: 0.2749 on 1 and 94 DF,  p-value: 0.6013



model.Mala3.cp.gam <- gam(CP ~ s(ANO, k = 25), data = Mala3, method = "REML", sp = 0.001)
summary(model.Mala3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 25)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.490      0.458    22.9   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 22.96  23.85 1.743  0.0275 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =    0.2   Deviance explained = 39.4%
## -REML = 328.28  Scale est. = 20.14     n = 96

gam.check(model.Mala3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-8.556295e-05,-8.556295e-05]
## (score 328.2815 & scale 20.14024).
## Hessian positive definite, eigenvalue range [47.00009,47.00009].
## Model rank =  25 / 25 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##       k' edf k-index p-value
## s(ANO) 24  23    1.03    0.59



model2.Mala3.cp.gam <- gam(log(CP) ~ s(ANO, k = 25), data = Mala3, method = "REML", sp = 0.0001)
summary(model2.Mala3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 25)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.24110    0.04627   48.43   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 23.88     24 1.324   0.177
##
## R-sq.(adj) =   0.08   Deviance explained = 31.1%
## -REML = 136.78  Scale est. = 0.20557   n = 96

gam.check(model2.Mala3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.755396e-10,-5.755396e-10]
## (score 136.7842 & scale 0.2055691).
## Hessian positive definite, eigenvalue range [47,47].
## Model rank =  25 / 25 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 24.0 23.9    1.04    0.62



## H.malabaricus_AmbPeque - P ------------------------------------------------##

model.Mala3.p.gamm<-gamm(P~s(ANO), data = Mala3)
summary(model.Mala3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   46.742      6.437   7.261 1.18e-10 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 2.545  2.545 2.783   0.197
##
## R-sq.(adj) =  0.0586   
## Scale est. = 3936.9    n = 96

model2.Mala3.p.gamm<-gamm(log(P)~s(ANO), data=Mala3)
summary(model2.Mala3.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.9418     0.1495   19.68   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.266   0.608
##
## R-sq.(adj) =  -0.00782   
## Scale est. = 2.1228    n = 96

model3.Mala3.p.lm<-lm(P~ANO, data = Mala3)
summary(model3.Mala3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Mala3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -45.763 -39.993 -25.754   9.277 273.167 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  5.836e+01  1.691e+03   0.035    0.973
## ANO         -5.798e-03  8.437e-01  -0.007    0.995
##
## Residual standard error: 65.45 on 94 degrees of freedom
## Multiple R-squared:  5.023e-07,	Adjusted R-squared:  -0.01064 
## F-statistic: 4.722e-05 on 1 and 94 DF,  p-value: 0.9945

model4.Mala3.p.lm<-lm(log(P)~ANO, data = Mala3)
summary(model4.Mala3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Mala3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.9545 -1.0587  0.1647  1.0509  2.9792 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -16.560358  38.048694  -0.435    0.664
## ANO           0.009728   0.018980   0.513    0.609
##
## Residual standard error: 1.472 on 94 degrees of freedom
## Multiple R-squared:  0.002787,	Adjusted R-squared:  -0.007822 
## F-statistic: 0.2627 on 1 and 94 DF,  p-value: 0.6095



model.Mala3.p.gam <- gam(P ~ s(ANO, k = 25), data = Mala3, method = "REML", sp = 0.001)
summary(model.Mala3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 25)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   46.742      5.186   9.014 1.96e-13 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 22.96  23.85 3.199 5.13e-05 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.391   Deviance explained = 53.8%
## -REML = 557.29  Scale est. = 2581.5    n = 96

gam.check(model.Mala3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.136111e-06,-4.136111e-06]
## (score 557.2873 & scale 2581.54).
## Hessian positive definite, eigenvalue range [47,47].
## Model rank =  25 / 25 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##       k' edf k-index p-value
## s(ANO) 24  23    1.04    0.62



model2.Mala3.p.gam <- gam(log(P) ~ s(ANO, k = 25), data = Mala3, method = "REML", sp = 0.001)
summary(model2.Mala3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 25)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.9418     0.1453   20.25   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 22.96  23.85 1.134    0.31
##
## R-sq.(adj) =  0.0578   Deviance explained = 28.5%
## -REML = 219.44  Scale est. = 2.0268    n = 96

gam.check(model2.Mala3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.411621e-09,-1.411621e-09]
## (score 219.4435 & scale 2.026786).
## Hessian positive definite, eigenvalue range [47,47].
## Model rank =  25 / 25 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##       k' edf k-index p-value
## s(ANO) 24  23    1.05    0.62



##----------------------------------------------------------------------------##
                                ## L.friderici ## 

dir()

Frid <- read.table("L.friderici_Geral2.txt", header=T)
str(Frid)
Frid$CP
Frid$P
Frid$ANO
Frid$Longitude
Frid$Latitude
Frid$Amb

Frid

summary(Frid)
##       CP              P                ANO          state               Amb              Longitude     
## Min.   : 3.60   Min.   :  1.000   Min.   :1981   Length:43          Length:43          Min.   :-53.73  
## 1st Qu.: 5.85   1st Qu.:  4.775   1st Qu.:2004   Class :character   Class :character   1st Qu.:-50.31  
## Median : 9.70   Median : 21.000   Median :2006   Mode  :character   Mode  :character   Median :-49.42  
## Mean   :10.51   Mean   : 50.973   Mean   :2005                                         Mean   :-49.61  
## 3rd Qu.:14.46   3rd Qu.: 82.000   3rd Qu.:2011                                         3rd Qu.:-49.27  
## Max.   :21.80   Max.   :264.000   Max.   :2018                                         Max.   :-47.59  
## 
## Latitude     
## Min.   :-23.43  
## 1st Qu.:-21.27  
## Median :-20.61  
## Mean   :-20.80  
## 3rd Qu.:-20.31  
## Max.   :-17.21



## L.friderici_Geral - teste de normalidade ----------------------------------##
shapiro.test(Frid$CP) ## W = 0.93226, p-value = 0.01391
shapiro.test(Frid$P)  ## W = 0.77573, p-value = 1.122e-06
shapiro.test(Frid$ANO) ## W = 0.85478, p-value = 6.903e-05



## L.friderici_Geral - cor e cov ---------------------------------------------##
cor(Frid$ANO, Frid$CP) ## -0.02775236
cor(Frid$ANO, Frid$P)  ## 0.09455566
cor(Frid$ANO, Frid$CP, method="spearman") ## 0.03469844
cor(Frid$ANO, Frid$P, method="spearman")  ## 0.03398898

cov(Frid$ANO, Frid$CP) ## -1.330233
cov(Frid$ANO, Frid$P)  ## 57.07558



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
## (Intercept)  10.4473     0.8646   12.08  4.3e-15 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.093   0.762
##
## R-sq.(adj) =  -0.024     
## Scale est. = 24.762    n = 43

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
## (Intercept)   2.22133    0.09227   24.07   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.516   0.476
##
## R-sq.(adj) =  -0.0185    
## Scale est. = 0.24771   n = 43

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
## (Intercept)  61.60154  169.53447   0.363    0.718
## ANO          -0.02552    0.08458  -0.302    0.764
##
## R-sq.(adj) =  -0.024   
## Scale est. = 24.762    n = 43

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
## (Intercept) 14.366622  17.102959    0.84    0.406
## ANO         -0.006058   0.008533   -0.71    0.482
## 
## R-sq.(adj) =  -0.0185     
## Scale est. = 0.24771   n = 43

model5.Frid.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Frid)
summary(model5.Frid.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Frid
##
## REML criterion at convergence: 261.5
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.4805 -0.9229 -0.1315  0.7992  2.1243 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  2.904   1.704   
## Residual             25.094   5.009    
## Number of obs: 43, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 126.13120  175.16995  38.17329   0.720    0.476
## ANO          -0.05780    0.08741  38.13944  -0.661    0.512
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
## REML criterion at convergence: 72.9
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.93703 -0.90330  0.09658  0.92349  1.56477 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.03251  0.1803    
## Residual             0.25144  0.5014 
## Number of obs: 43, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 20.295639  17.575813 38.774290   1.155    0.255
## ANO         -0.009025   0.008770 38.747215  -1.029    0.310
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Frid.cp.gam <- gam(CP ~ s(ANO, k = 20), data = Frid, method = "REML", sp = 0.001)
summary(model.Frid.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 20)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.5093     0.7576   13.87 6.25e-13 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 18.09  18.86 0.998   0.515
##
## R-sq.(adj) =  0.038   Deviance explained = 45.2%
## -REML = 163.12  Scale est. = 24.68     n = 43

gam.check(model.Frid.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.070393e-05,-2.070393e-05]
## (score 163.1234 & scale 24.68015).
## Hessian positive definite, eigenvalue range [20.50002,20.50002].
## Model rank =  20 / 20 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 19.0 18.1    0.84    0.12



model2.Frid.cp.gam <- gam(log(CP) ~ s(ANO, k = 20), data = Frid, method = "REML", sp = 0.0001)
summary(model2.Frid.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 20)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.23099    0.07516   29.68   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 18.89     19 1.142   0.379
##
## R-sq.(adj) =  0.0655   Deviance explained = 48.6%
## -REML = 87.042  Scale est. = 0.2429    n = 43

gam.check(model2.Frid.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.79151e-06,-5.79151e-06]
## (score 87.04228 & scale 0.2428979).
## Hessian positive definite, eigenvalue range [20.50001,20.50001].
## Model rank =  20 / 20 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value  
## s(ANO) 19.0 18.9    0.82    0.06 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



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
## (Intercept)   50.977      9.683   5.264 4.77e-06 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 0.379   0.542
##
## R-sq.(adj) =  -0.0152   
## Scale est. = 3938.2    n = 43

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
## (Intercept)   2.935      0.276   10.63 2.33e-13 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO)   1      1 0.381   0.541
##
## R-sq.(adj) =  -0.0197   
## Scale est. = 2.3847    n = 43

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
## (Intercept) -1226.6054  2100.7010  -0.584    0.562
## ANO             0.6373     1.0479   0.608    0.546
##
## R-sq.(adj) =  -0.0152   
## Scale est. = 3938.2    n = 43

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
## (Intercept) 35.14032   52.81345   0.665    0.510
## ANO         -0.01606    0.02635  -0.610    0.545
##
## R-sq.(adj) =  -0.0197   
## Scale est. = 2.3847    n = 43

model5.Frid.p.lmer<-lmer(P~ANO + (1|Amb), data = Frid)
summary(model5.Frid.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Frid
##
## REML criterion at convergence: 469.2
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.9132 -0.6679 -0.4175  0.5642  3.2484 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  354.5   18.83   
## Residual             3988.1   63.15   
## Number of obs: 43, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -323.9236  2195.1215   36.4064  -0.148    0.884
## ANO            0.1857     1.0953   36.3543   0.170    0.866
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000

model6.Frid.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Frid)
summary(model6.Frid.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Frid
##
## REML criterion at convergence: 165.6
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.04708 -0.98394  0.09397  0.90043  1.55497 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.2934   0.5417    
## Residual             2.4184   1.5551  
## Number of obs: 43, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) 54.43458   54.43581 38.44080   1.000    0.324
## ANO         -0.02572    0.02716 38.40993  -0.947    0.350
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Frid.p.gam <- gam(P ~ s(ANO, k = 20), data = Frid, method = "REML", sp = 0.001)
summary(model.Frid.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 20)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  50.98      10.03   5.081 3.42e-05 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 18.09  18.86 0.789   0.717
##
## R-sq.(adj) =  -0.0638   Deviance explained = 39.4%
## -REML = 268.82  Scale est. = 4327.8    n = 43

gam.check(model.Frid.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.433724e-05,-5.433724e-05]
## (score 268.8238 & scale 4327.822).
## Hessian positive definite, eigenvalue range [20.50005,20.50005].
## Model rank =  20 / 20 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 19.0 18.1    0.95     0.3



model2.Frid.p.gam <- gam(log(P) ~ s(ANO, k = 20), data = Frid, method = "REML", sp = 0.0001)
summary(model2.Frid.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 20)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.9592     0.2354   12.57 8.13e-12 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 18.89     19 1.086   0.423
##
## R-sq.(adj) =  0.0427   Deviance explained = 47.3%
## -REML = 133.85  Scale est. = 2.3822    n = 43

gam.check(model2.Frid.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-7.756159e-06,-7.756159e-06]
## (score 133.8454 & scale 2.382225).
## Hessian positive definite, eigenvalue range [20.50001,20.50001].
## Model rank =  20 / 20 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value  
## s(ANO) 19.0 18.9    0.82   0.075 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



##----------------------------------------------------------------------------##

Frid2 <- filter(Frid, Amb == "Grande")
Frid3 <- filter(Frid, Amb == "Pequeno")

Frid2$CP
Frid2$P
Frid2$ANO
Frid2$Longitude
Frid2$Latitude
Frid2$Amb

Frid2

summary(Frid2)
##        CP               P               ANO          state               Amb              Longitude     
## Min.   : 3.600   Min.   :  1.20   Min.   :1981   Length:16          Length:16          Min.   :-53.73  
## 1st Qu.: 5.575   1st Qu.:  3.90   1st Qu.:1988   Class :character   Class :character   1st Qu.:-50.93  
## Median : 8.500   Median : 14.65   Median :2005   Mode  :character   Mode  :character   Median :-49.42  
## Mean   : 9.056   Mean   : 29.92   Mean   :2000                                         Mean   :-50.04  
## 3rd Qu.:11.325   3rd Qu.: 34.50   3rd Qu.:2009                                         3rd Qu.:-49.39  
## Max.   :16.200   Max.   :109.60   Max.   :2013                                         Max.   :-47.70  
## Latitude     
## Min.   :-22.48  
## 1st Qu.:-21.21  
## Median :-20.61  
## Mean   :-20.96  
## 3rd Qu.:-20.53  
## Max.   :-20.21



## L.friderici_AmbGran - teste de normalidade --------------------------------##
shapiro.test(Frid2$CP) ## W = 0.92448, p-value = 0.1991
shapiro.test(Frid2$P)  ## W = 0.75675, p-value = 0.0007669
shapiro.test(Frid2$ANO) ## W = 0.84018, p-value = 0.009802



## L.friderici_AmbGran - cor e cov -------------------------------------------##
cor(Frid2$ANO, Frid2$CP) ## -0.5140923
cor(Frid2$ANO, Frid2$P)  ## -0.382363
cor(Frid2$ANO, Frid2$CP, method="spearman") ## -0.4235044
cor(Frid2$ANO, Frid2$P, method="spearman")  ## -0.4094615

cov(Frid2$ANO, Frid2$CP) ## -25.79667
cov(Frid2$ANO, Frid2$P)  ## -166.6233



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
## (Intercept)    9.056      0.903   10.03 9.01e-08 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 5.388  0.0359 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.212   
## Scale est. = 12.23     n = 16

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
## (Intercept)   2.0948     0.1053   19.89 1.16e-11 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 5.521   0.034 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.217   
## Scale est. = 0.16639   n = 16

model3.Frid2.cp.lm<-lm(CP~ANO, data = Frid2)
summary(model3.Frid2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Frid2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -3.9121 -2.8708 -0.7971  3.0862  6.5062 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 372.29875  161.97645   2.298   0.0375 *
##   ANO          -0.18167    0.08101  -2.243   0.0416 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 3.739 on 14 degrees of freedom
## Multiple R-squared:  0.2643,	Adjusted R-squared:  0.2117 
## F-statistic: 5.029 on 1 and 14 DF,  p-value: 0.04163

model4.Frid2.cp.lm<-lm(log(CP)~ANO, data = Frid2)
summary(model4.Frid2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Frid2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.63157 -0.33042 -0.01281  0.34165  0.73206 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 44.981741  18.892871   2.381   0.0320 *
##   ANO         -0.021449   0.009449  -2.270   0.0395 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4361 on 14 degrees of freedom
## Multiple R-squared:  0.269,	Adjusted R-squared:  0.2168 
## F-statistic: 5.153 on 1 and 14 DF,  p-value: 0.03953



model.Frid2.cp.gam <- gam(CP ~ s(ANO, k = 12), data = Frid2, method = "REML", sp = 0.0001)
summary(model.Frid2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.056      1.031    8.78 0.000623 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.64  10.94 0.991   0.547
##
## R-sq.(adj) =  0.0401   Deviance explained = 72.1%
## -REML =  62.72  Scale est. = 17.021    n = 16


gam.check(model.Frid2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-8.680354e-10,-8.680354e-10]
## (score 62.71981 & scale 17.0211).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.6    1.37    0.86



model2.Frid2.cp.gam <- gam(log(CP) ~ s(ANO, k = 12), data = Frid2, method = "REML", sp = 0.0001)
summary(model2.Frid2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.0948     0.1297   16.16 4.69e-05 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.64  10.94 0.809   0.642
##
## R-sq.(adj) =  -0.108   Deviance explained = 67.8%
## -REML = 33.668  Scale est. = 0.26902   n = 16

gam.check(model2.Frid2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.198231e-08,-1.198231e-08]
## (score 33.66785 & scale 0.2690212).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.6    1.36    0.88



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
## (Intercept)   29.919      8.448   3.542  0.00325 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 2.569   0.131
##
## R-sq.(adj) =  0.0852   
## Scale est. = 1070.4    n = 16

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
## (Intercept)   2.5386     0.3209    7.91 1.56e-06 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##  edf Ref.df     F p-value  
## s(ANO)   1      1 5.188   0.039 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.204   
## Scale est. = 1.545     n = 16

model3.Frid2.p.lm<-lm(P~ANO, data = Frid2)
summary(model3.Frid2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Frid2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -32.07 -20.84 -14.53  10.05  88.48 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 2376.1396  1515.3534   1.568    0.139
## ANO           -1.1734     0.7579  -1.548    0.144
##
## Residual standard error: 34.98 on 14 degrees of freedom
## Multiple R-squared:  0.1462,	Adjusted R-squared:  0.08522 
## F-statistic: 2.397 on 1 and 14 DF,  p-value: 0.1438

model4.Frid2.p.lm<-lm(log(P)~ANO, data = Frid2)
summary(model4.Frid2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Frid2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.8178 -0.9867 -0.1324  0.9625  2.6334 
##
## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 129.21506   57.57017   2.244   0.0415 *
##  ANO          -0.06335    0.02879  -2.200   0.0451 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.329 on 14 degrees of freedom
## Multiple R-squared:  0.257,	Adjusted R-squared:  0.2039 
## F-statistic: 4.842 on 1 and 14 DF,  p-value: 0.04506



model.Frid2.p.gam <- gam(P ~ s(ANO, k = 12), data = Frid2, method = "REML", sp = 0.0001)
summary(model.Frid2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  29.919      8.949   3.343   0.0252 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.64  10.94 0.98   0.549
##
## R-sq.(adj) =  0.0419   Deviance explained = 72.2%
## -REML = 93.056  Scale est. = 1281.2    n = 16


gam.check(model.Frid2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.0757e-09,-1.0757e-09]
## (score 93.05611 & scale 1281.243).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.6    1.65    0.99



model2.Frid2.p.gam <- gam(log(P) ~ s(ANO, k = 12), data = Frid2, method = "REML", sp = 0.0001)
summary(model2.Frid2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.5386     0.3958   6.414  0.00225 **
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.64  10.94 0.784   0.655
##
## R-sq.(adj) =  -0.13   Deviance explained = 67.2%
## -REML = 49.302  Scale est. = 2.5066    n = 16

gam.check(model2.Frid2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.729236e-08,-1.729236e-08]
## (score 49.30224 & scale 2.506596).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.6    1.44    0.92



##----------------------------------------------------------------------------##

dir()

Frid3$CP
Frid3$P
Frid3$ANO
Frid3$Longitude
Frid3$Latitude
Frid3$Amb

Frid3

summary(Frid3)
##        CP              P               ANO          state               Amb              Longitude     
## Min.   : 3.80   Min.   :  1.00   Min.   :1990   Length:27          Length:27          Min.   :-51.34  
## 1st Qu.: 6.05   1st Qu.:  4.90   1st Qu.:2005   Class :character   Class :character   1st Qu.:-50.06  
## Median :10.50   Median : 29.00   Median :2007   Mode  :character   Mode  :character   Median :-49.28  
## Mean   :11.37   Mean   : 63.46   Mean   :2008                                         Mean   :-49.36  
## 3rd Qu.:15.85   3rd Qu.:118.95   3rd Qu.:2012                                         3rd Qu.:-48.44  
## Max.   :21.80   Max.   :264.00   Max.   :2018                                         Max.   :-47.59  
## Latitude     
## Min.   :-23.43  
## 1st Qu.:-21.41  
## Median :-20.42  
## Mean   :-20.70  
## 3rd Qu.:-20.15  
## Max.   :-17.21  


## L.friderici_AmbPeque - teste de normalidade -------------------------------##
shapiro.test(Frid3$CP) ## W = 0.91875, p-value = 0.03683
shapiro.test(Frid3$P)  ## W = 0.81283, p-value = 0.0002302
shapiro.test(Frid3$ANO) ## W = 0.93495, p-value = 0.09144



## L.friderici_AmbPeque - cor e cov ------------------------------------------##
cor(Frid3$ANO, Frid3$CP) ## 0.1644878
cor(Frid3$ANO, Frid3$P)  ## 0.1842263
cor(Frid3$ANO, Frid3$CP, method="spearman") ## 0.1830492
cor(Frid3$ANO, Frid3$P, method="spearman")  ## 0.1737406

cov(Frid3$ANO, Frid3$CP) ## 5.3651
cov(Frid3$ANO, Frid3$P)  ## 81.53803



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
## (Intercept)   11.370      1.025    11.1 3.78e-11 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##  edf Ref.df     F p-value
## s(ANO)   1      1 0.723   0.403
##
## R-sq.(adj) =  -0.0119   
## Scale est. = 27.303    n = 27

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
## (Intercept)  2.31169    0.09747   23.72   <2e-16 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##  edf Ref.df     F p-value
## s(ANO)   1      1 0.515    0.48
##
## R-sq.(adj) =  -0.0198   
## Scale est. = 0.24703   n = 27

model3.Frid3.cp.lm<-lm(CP~ANO, data = Frid3)
summary(model3.Frid3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Frid3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -7.1676 -5.3994 -0.1615  4.4415 10.6854 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -283.6879   353.8749  -0.802    0.430
## ANO            0.1470     0.1763   0.834    0.412
##
## Residual standard error: 5.43 on 25 degrees of freedom
## Multiple R-squared:  0.02706,	Adjusted R-squared:  -0.01186 
## F-statistic: 0.6952 on 1 and 25 DF,  p-value: 0.4123

model4.Frid3.cp.lm<-lm(log(CP)~ANO, data = Frid3)
summary(model4.Frid3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Frid3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.9444 -0.5088  0.1040  0.4470  0.7908 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -21.36926   33.66022  -0.635    0.531
## ANO           0.01179    0.01677   0.704    0.488
##
## Residual standard error: 0.5165 on 25 degrees of freedom
## Multiple R-squared:  0.01941,	Adjusted R-squared:  -0.01981 
## F-statistic: 0.495 on 1 and 25 DF,  p-value: 0.4882



model.Frid3.cp.gam <- gam(CP ~ s(ANO, k = 12), data = Frid3, method = "REML", sp = 0.0001)
summary(model.Frid3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  11.370      1.121   10.14    4e-08 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.95     11 0.647    0.76
##
## R-sq.(adj) =  -0.164   Deviance explained = 32.6%
## -REML = 113.79  Scale est. = 33.925    n = 27


gam.check(model.Frid3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-8.925343e-05,-8.925343e-05]
## (score 113.787 & scale 33.92486).
## Hessian positive definite, eigenvalue range [12.50009,12.50009].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.9    0.97     0.4



model2.Frid3.cp.gam <- gam(log(CP) ~ s(ANO, k = 12), data = Frid3, method = "REML", sp = 0.0001)
summary(model2.Frid3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.3117     0.1062   21.77  8.6e-13 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.95     11 0.647   0.759
##
## R-sq.(adj) =  -0.164   Deviance explained = 32.6%
## -REML = 54.864  Scale est. = 0.30452   n = 27

gam.check(model2.Frid3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.142411e-10,-3.142411e-10]
## (score 54.86434 & scale 0.3045157).
## Hessian positive definite, eigenvalue range [12.5,12.5].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.9       1    0.38




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
## (Intercept)    63.46      13.86    4.58 0.000111 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.913   0.348
##
## R-sq.(adj) =  -0.0047   
## Scale est. = 4991.8    n = 27

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
## (Intercept)   3.2084     0.3054   10.51 1.18e-10 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.498   0.487
##
## R-sq.(adj) =  -0.0204   
## Scale est. = 2.4255    n = 27

model3.Frid3.p.lm<-lm(P~ANO, data = Frid3)
summary(model3.Frid3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Frid3)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -68.97 -57.07 -23.60  61.08 204.43 
##
## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)
## (Intercept) -4420.799   4784.899  -0.924    0.364
## ANO             2.233      2.383   0.937    0.358
##
## Residual standard error: 73.42 on 25 degrees of freedom
## Multiple R-squared:  0.03394,	Adjusted R-squared:  -0.004703 
## F-statistic: 0.8783 on 1 and 25 DF,  p-value: 0.3576

model4.Frid3.p.lm<-lm(log(P)~ANO, data = Frid3)
summary(model4.Frid3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Frid3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.1088 -1.5558  0.4613  1.4900  2.4308 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -69.81178  105.47401  -0.662    0.514
## ANO           0.03637    0.05253   0.692    0.495
##
## Residual standard error: 1.618 on 25 degrees of freedom
## Multiple R-squared:  0.01881,	Adjusted R-squared:  -0.02044 
## F-statistic: 0.4793 on 1 and 25 DF,  p-value: 0.4951



model.Frid3.p.gam <- gam(P ~ s(ANO, k = 12), data = Frid3, method = "REML", sp = 0.001)
summary(model.Frid3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  63.46      15.40   4.121 0.000856 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.55  10.92 0.507   0.862
##
## R-sq.(adj) =  -0.193   Deviance explained = 29.1%
## -REML = 168.67  Scale est. = 6401.6    n = 27


gam.check(model.Frid3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.334746e-09,-1.334746e-09]
## (score 168.6668 & scale 6401.631).
## Hessian positive definite, eigenvalue range [12.5,12.5].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.5    1.06    0.49



model2.Frid3.p.gam <- gam(log(P) ~ s(ANO, k = 12), data = Frid3, method = "REML", sp = 0.001)
summary(model2.Frid3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.2084     0.3339   9.609  6.5e-08 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.55  10.92 0.522   0.832
##
## R-sq.(adj) =  -0.172   Deviance explained = 30.3%
## -REML = 72.861  Scale est. = 3.0099    n = 27

gam.check(model2.Frid3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-9.425438e-10,-9.425438e-10]
## (score 72.8606 & scale 3.009864).
## Hessian positive definite, eigenvalue range [12.5,12.5].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.5    1.06    0.54




##----------------------------------------------------------------------------##
                             ## P.lineatus ## 

dir()

Line <- read.table("P.lineatus_Geral2.txt", header=T)

Line$CP
Line$P
Line$ANO
Line$Longitude
Line$Latitude
Line$Amb

Line

summary(Line)
##        CP              P               ANO          state               Amb              Longitude     
## Min.   : 7.60   Min.   : 12.00   Min.   :1992   Length:19          Length:19          Min.   :-50.11  
## 1st Qu.: 9.55   1st Qu.: 26.20   1st Qu.:2005   Class :character   Class :character   1st Qu.:-49.31  
## Median :11.50   Median : 60.00   Median :2006   Mode  :character   Mode  :character   Median :-49.27  
## Mean   :12.59   Mean   : 68.52   Mean   :2004                                         Mean   :-49.11  
## 3rd Qu.:16.30   3rd Qu.:110.00   3rd Qu.:2007                                         3rd Qu.:-49.27  
## Max.   :18.20   Max.   :155.00   Max.   :2017                                         Max.   :-47.77  
##                                                                                       NA's   :4       
##     Latitude     
## Min.   :-23.33  
## 1st Qu.:-20.42  
## Median :-20.42  
## Mean   :-20.43  
## 3rd Qu.:-20.37  
## Max.   :-17.21  
## NA's   :4     



## P.lineatus_Geral - teste de normalidade -----------------------------------##
shapiro.test(Line$CP) ## W = 0.91695, p-value = 0.0994
shapiro.test(Line$P)  ## W = 0.90978, p-value = 0.07333
shapiro.test(Line$ANO) ## W = 0.83427, p-value = 0.003761



## P.lineatus_Geral - cor e cov ----------------------------------------------##
cor(Line$ANO, Line$CP) ## -0.5048187
cor(Line$ANO, Line$P)  ## -0.4245797
cor(Line$ANO, Line$CP, method="spearman") ## -0.4504815
cor(Line$ANO, Line$P, method="spearman")  ## -0.4050747

cov(Line$ANO, Line$CP) ## -13.48684
cov(Line$ANO, Line$P)  ## -145.2673



## P.lineatus_Geral - CP -----------------------------------------------------##

model.Line.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Line)
summary(model.Line.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Line.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Line)
summary(model2.Line.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Line.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Line)
summary(model3.Line.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 520.2910   210.5577   2.471   0.0243 *
## ANO          -0.2533     0.1050  -2.411   0.0275 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.211   
## Scale est. = 9.4631    n = 19

model4.Line.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Line)
summary(model4.Line.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 41.81575   17.33851   2.412   0.0275 *
## ANO         -0.01962    0.00865  -2.268   0.0366 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.187   
## Scale est. = 0.064167  n = 19

model5.Line.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Line)
summary(model5.Line.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Line
##
## REML criterion at convergence: 98.2
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.4088 -0.5703 -0.1317  0.3748  2.0720  
##
## Random effects:
##   Groups   Name        Variance  Std.Dev. 
## Amb      (Intercept) 7.269e-14 2.696e-07
## Residual             1.058e+01 3.252e+00
## Number of obs: 19, groups:  Amb, 2
##
## Fixed effects:
##            Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept) 520.2910   210.5577  17.0000   2.471   0.0243 *
##   ANO        -0.2533     0.1050  17.0000  -2.411   0.0275 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Line.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Line)
summary(model6.Line.cp.lmer)
## boundary (singular) fit: see ?isSingular
## 
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Line
##
## REML criterion at convergence: 13.3
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.61308 -0.50510  0.00685  0.42973  1.91954 
##
## Random effects:
##   Groups   Name        Variance  Std.Dev. 
## Amb      (Intercept) 1.557e-14 1.248e-07
## Residual             7.172e-02 2.678e-01
## Number of obs: 19, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept) 41.81575   17.33851 17.00000   2.412   0.0275 *
##   ANO       -0.01962    0.00865 17.00000  -2.268   0.0366 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



model.Line.cp.gam <- gam(CP ~ s(ANO, k = 7), data = Line, method = "REML", sp = 0.0001)
summary(model.Line.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  12.5947     0.5943   21.19 1.28e-11 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.73  5.291 3.829  0.0226 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.499   Deviance explained = 63.1%
## -REML = 49.795  Scale est. = 6.7112    n = 19

gam.check(model.Line.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.191664e-07,-2.191664e-07]
## (score 49.79538 & scale 6.71125).
## Hessian positive definite, eigenvalue range [8.5,8.5].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 4.73    1.44    0.96



model2.Line.cp.gam <- gam(log(CP) ~ s(ANO, k = 7), data = Line, method = "REML", sp = 0.0001)
summary(model2.Line.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.49214    0.04742   52.55   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.73  5.291 3.884  0.0211 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.516   Deviance explained = 64.3%
## -REML = 7.0476  Scale est. = 0.042733  n = 19

gam.check(model2.Line.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.017121e-07,-2.017121e-07]
## (score 7.047636 & scale 0.04273316).
## Hessian positive definite, eigenvalue range [8.5,8.5].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 4.73    1.46    0.96



## P.lineatus_Geral - P ------------------------------------------------------##

model.Line.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Line)
summary(model.Line.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Line.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Line)
summary(model2.Line.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Line.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Line)
summary(model3.Line.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5536.930   2828.241   1.958   0.0669 .
## ANO           -2.728      1.411  -1.934   0.0700 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.132   
## Scale est. = 1707.4    n = 19

model4.Line.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Line)
summary(model4.Line.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 97.57427   51.37512   1.899   0.0746 .
## ANO         -0.04672    0.02563  -1.823   0.0860 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.114   
## Scale est. = 0.56337   n = 19

model5.Line.p.lmer<-lmer(P~ANO + (1|Amb), data = Line)
summary(model5.Line.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Line
##
## REML criterion at convergence: 186.5
##
## Scaled residuals: 
##  Min      1Q  Median      3Q     Max 
##-1.1919 -0.7453 -0.1555  0.4550  2.0192 
##
## Random effects:
##   Groups   Name        Variance  Std.Dev. 
## Amb      (Intercept) 1.932e-12 1.390e-06
## Residual             1.908e+03 4.368e+01
## Number of obs: 19, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)  
## (Intercept) 5536.930   2828.241   17.000   1.958   0.0669 .
## ANO           -2.728      1.411   17.000  -1.934   0.0700 .
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Line.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Line)
summary(model6.Line.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Line
##
## REML criterion at convergence: 50.2
##
## Scaled residuals: 
##  Min      1Q  Median      3Q     Max 
## -1.7342 -0.5466  0.2337  0.5049  1.6599 
##
## Random effects:
##   Groups   Name        Variance  Std.Dev. 
## Amb      (Intercept) 4.066e-13 6.376e-07
## Residual             6.297e-01 7.935e-01
## Number of obs: 19, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)  
## (Intercept) 97.57427   51.37512 17.00000   1.899   0.0746 .
## ANO         -0.04672    0.02563 17.00000  -1.823   0.0860 .
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Line.p.gam <- gam(P ~ s(ANO, k = 7), data = Line, method = "REML", sp = 0.0001)
summary(model.Line.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  68.516      9.053   7.569  3.6e-06 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.73  5.291 1.925   0.156
##
## R-sq.(adj) =  0.292   Deviance explained = 47.8%
## -REML = 95.891  Scale est. = 1557.1    n = 19

gam.check(model.Line.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.276008e-05,-1.276008e-05]
## (score 95.89107 & scale 1557.094).
## Hessian positive definite, eigenvalue range [8.500013,8.500013].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 4.73    1.39    0.92



model2.Line.p.gam <- gam(log(P) ~ s(ANO, k = 7), data = Line, method = "REML", sp = 0.0001)
summary(model2.Line.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(P) ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.9372     0.1505   26.16 8.29e-13 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.73  5.291 2.367  0.0922 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.395   Deviance explained = 55.4%
## -REML = 26.763  Scale est. = 0.4303    n = 19

gam.check(model2.Line.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.318894e-06,-4.318894e-06]
## (score 26.76324 & scale 0.4303045).
## Hessian positive definite, eigenvalue range [8.500004,8.500004].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 4.73    1.41    0.94



##----------------------------------------------------------------------------##

dir()

Line2 <- filter(Line, Amb == "Pequeno")

Line2$CP
Line2$P
Line2$ANO
Line2$Longitude
Line2$Latitude
Line2$Amb

Line2

summary(Line2)
##       CP               P               ANO          state               Amb              Longitude     
## Min.   : 7.600   Min.   : 12.00   Min.   :1992   Length:16          Length:16          Min.   :-50.11  
## 1st Qu.: 9.675   1st Qu.: 26.80   1st Qu.:2002   Class :character   Class :character   1st Qu.:-49.28  
## Median :13.250   Median : 68.40   Median :2006   Mode  :character   Mode  :character   Median :-49.27  
## Mean   :13.006   Mean   : 72.29   Mean   :2004                                         Mean   :-49.13  
## 3rd Qu.:16.525   3rd Qu.:110.25   3rd Qu.:2006                                         3rd Qu.:-49.27  
## Max.   :18.200   Max.   :155.00   Max.   :2017                                         Max.   :-47.77  
##                                                                                        NA's   :4       
##    Latitude     
## Min.   :-23.33  
## 1st Qu.:-20.42  
## Median :-20.41  
## Mean   :-20.30  
## 3rd Qu.:-20.31  
## Max.   :-17.21  
## NA's   :4   



## P.lineatus_AmbPeque - teste de normalidade --------------------------------##
shapiro.test(Line2$CP) ## W = 0.91074, p-value = 0.1196
shapiro.test(Line2$P)  ## W = 0.91619, p-value = 0.1464
shapiro.test(Line2$ANO) ## W = 0.83875, p-value = 0.009347



## P.lineatus_AmbPeque - cor e cov -------------------------------------------##
cor(Line2$ANO, Line2$CP) ## -0.4688927
cor(Line2$ANO, Line2$P)  ## -0.4064314
cor(Line2$ANO, Line2$CP, method="spearman") ## -0.471159
cor(Line2$ANO, Line2$P, method="spearman")  ## -0.4353521

cov(Line2$ANO, Line2$CP) ## -13.89167
cov(Line2$ANO, Line2$P)  ## -152.1417



## P.lineatus_AmbPeque - CP --------------------------------------------------##

model.Line2.cp.gamm<-gamm(CP~s(ANO), data=Line2)
summary(model.Line2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Line2.cp.gamm<-gamm(log(CP)~s(ANO), data=Line2)
summary(model2.Line2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Line2.cp.lm<-lm(CP~ANO, data = Line2)
summary(model3.Line2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Line2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -4.8859 -2.0281  0.2796  1.4263  6.3017 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 476.4151   233.3007   2.042   0.0605 .
## ANO          -0.2313     0.1164  -1.986   0.0669 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 3.495 on 14 degrees of freedom
## Multiple R-squared:  0.2199,	Adjusted R-squared:  0.1641 
## F-statistic: 3.946 on 1 and 14 DF,  p-value: 0.06693


model4.Line2.cp.lm<-lm(log(CP)~ANO, data = Line2)
summary(model4.Line2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Line2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.45358 -0.15392  0.05176  0.12673  0.48174 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 38.253859  19.230651   1.989   0.0666 .
## ANO         -0.017833   0.009597  -1.858   0.0843 .
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.2881 on 14 degrees of freedom
## Multiple R-squared:  0.1978,	Adjusted R-squared:  0.1405 
## F-statistic: 3.452 on 1 and 14 DF,  p-value: 0.08431



model.Line2.cp.gam <- gam(CP ~ s(ANO, k = 6), data = Line2, method = "REML", sp = 0.0001)
summary(model.Line2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  13.0062     0.6516   19.96 7.73e-10 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.254  4.581 4.094   0.021 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.535   Deviance explained = 66.7%
## -REML = 42.331  Scale est. = 6.7934    n = 16

gam.check(model.Line2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.887453e-08,-4.887453e-08]
## (score 42.33139 & scale 6.793363).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.25    1.67    0.99



model2.Line2.cp.gam <- gam(log(CP) ~ s(ANO, k = 6), data = Line2, method = "REML", sp = 0.0001)
summary(model2.Line2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.52185    0.05119   49.27 5.24e-14 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.254  4.581 4.389  0.0162 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.566   Deviance explained = 68.9%
## -REML = 6.9038  Scale est. = 0.041919  n = 16

gam.check(model2.Line2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.491084e-08,-2.491084e-08]
## (score 6.903834 & scale 0.04191883).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.25    1.73       1



## P.lineatus_AmbPeque - P ---------------------------------------------------##

model.Line2.p.gamm<-gamm(P~s(ANO), data = Line2)
summary(model.Line2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Line2.p.gamm<-gamm(log(P)~s(ANO), data=Line2)
summary(model2.Line2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Line2.p.lm<-lm(P~ANO, data = Line2)
summary(model3.Line2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Line2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -54.595 -31.212  -0.728  14.195  85.872 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 5147.552   3049.332   1.688    0.114
## ANO           -2.533      1.522  -1.664    0.118
##
## Residual standard error: 45.68 on 14 degrees of freedom
## Multiple R-squared:  0.1652,	Adjusted R-squared:  0.1056 
## F-statistic:  2.77 on 1 and 14 DF,  p-value: 0.1182

model4.Line2.p.lm<-lm(log(P)~ANO, data = Line2)
summary(model4.Line2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Line2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.4109 -0.4514  0.1818  0.3932  1.2579 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 89.44307   54.82958   1.631    0.125
## ANO         -0.04265    0.02736  -1.559    0.141
##
## Residual standard error: 0.8214 on 14 degrees of freedom
## Multiple R-squared:  0.1478,	Adjusted R-squared:  0.08698 
## F-statistic: 2.429 on 1 and 14 DF,  p-value: 0.1414



model.Line2.p.gam <- gam(P ~ s(ANO, k = 5), data = Line2, method = "REML", sp = 0.0001)
summary(model.Line2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 5)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  72.294      9.312   7.764  8.4e-06 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.939  3.997 3.461  0.0457 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.405   Deviance explained = 56.1%
## -REML = 79.952  Scale est. = 1387.4    n = 16

gam.check(model.Line2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-9.709368e-07,-9.709368e-07]
## (score 79.95211 & scale 1387.402).
## Hessian positive definite, eigenvalue range [7.000001,7.000001].
## Model rank =  5 / 5 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 4.00 3.94    1.63    0.99



model2.Line2.p.gam <- gam(log(P) ~ s(ANO, k = 6), data = Line2, method = "REML", sp = 0.0001)
summary(model2.Line2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.9918     0.1411   28.28 1.95e-11 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.254  4.581 4.323  0.0165 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.569   Deviance explained = 69.1%
## -REML = 21.215  Scale est. = 0.31871   n = 16

gam.check(model2.Line2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.909569e-08,-2.909569e-08]
## (score 21.21488 & scale 0.3187067).
## Hessian positive definite, eigenvalue range [7,7].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.25    1.77       1




##----------------------------------------------------------------------------##
                            ## P.maculatus ##

dir()

Macu <- read.table("P.maculatus_Geral2.txt", header=T)

Macu$P

summary(Macu)
## catalognumber         CP              P               day            month             ANO      
## Min.   : 1272   Min.   : 5.20   Min.   :  3.30   Min.   : 1.00   Min.   : 1.000   Min.   :1900  
## 1st Qu.:11178   1st Qu.:10.50   1st Qu.: 20.25   1st Qu.: 3.00   1st Qu.: 1.500   1st Qu.:2004  
## Median :15333   Median :14.10   Median : 48.00   Median :15.00   Median : 4.000   Median :2011  
## Mean   :14304   Mean   :13.57   Mean   : 60.69   Mean   :15.27   Mean   : 4.667   Mean   :1988  
## 3rd Qu.:19151   3rd Qu.:16.50   3rd Qu.: 72.00   3rd Qu.:26.50   3rd Qu.: 7.000   3rd Qu.:2013  
## Max.   :21344   Max.   :21.80   Max.   :252.00   Max.   :30.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:15          Length:15          Min.   :-53.06   Min.   :-23.87  
## Class :character   Class :character   1st Qu.:-50.23   1st Qu.:-22.18  
## Mode  :character   Mode  :character   Median :-49.24   Median :-20.79  
##                                       Mean   :-48.84   Mean   :-21.10  
##                                       3rd Qu.:-47.25   3rd Qu.:-20.23  
##                                       Max.   :-44.35   Max.   :-16.97  



## P.maculatus_Geral - teste de normalidade ----------------------------------##
shapiro.test(Macu$CP) ## W = 0.98569, p-value = 0.9944
shapiro.test(Macu$P)  ## W = 0.7712, p-value = 0.001607
shapiro.test(Macu$ANO) ## W = 0.56795, p-value = 1.313e-05



## P.maculatus_Geral - cor e cov ---------------------------------------------##
cor(Macu$ANO, Macu$CP) ## -0.1201215
cor(Macu$ANO, Macu$P)  ## -0.3465333
cor(Macu$ANO, Macu$CP, method="spearman") ## 0.1576253
cor(Macu$ANO, Macu$P, method="spearman")  ## 0.05978889

cov(Macu$ANO, Macu$CP) ## -25.23333
cov(Macu$ANO, Macu$P)  ## -988.931



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
## (Intercept) 37.43297   54.70452   0.684    0.506
## ANO         -0.01200    0.02751  -0.436    0.670
##
## R-sq.(adj) =  -0.0614   
## Scale est. = 19.303    n = 15

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
## (Intercept)  4.1604053  4.7308921   0.879    0.395
## ANO         -0.0008132  0.0023787  -0.342    0.738
##
## R-sq.(adj) =  -0.0673   
## Scale est. = 0.14437   n = 15

model5.Macu.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Macu)
summary(model5.Macu.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 89.7
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.9092 -0.6099  0.1016  0.6389  1.5936 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 16.88    4.108   
## Residual             19.88    4.458   
## Number of obs: 15, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) 30.050323  52.095507 12.298344   0.577    0.574
## ANO         -0.009276   0.026076 12.104503  -0.356    0.728
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -0.998

model6.Macu.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Macu)
summary(model6.Macu.cp.lmer)
##Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 25.7
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -2.4431 -0.4982  0.2188  0.6337  1.2227 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.1770   0.4208  
## Residual             0.1414   0.3761  
## Number of obs: 15, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  3.3518543  4.4018275 12.2898788   0.761    0.461
## ANO         -0.0005149  0.0022007 12.0813462  -0.234    0.819
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -0.997



model.Macu.cp.gam <- gam(CP ~ s(ANO, k = 7), data = Macu, method = "REML", sp = 0.0001)
summary(model.Macu.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  13.573      1.229   11.05 2.93e-07 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.094  3.608 0.497   0.748
##
## R-sq.(adj) =  -0.079   Deviance explained = 15.9%
## -REML = 43.975  Scale est. = 22.643    n = 15

gam.check(model.Macu.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.683308e-08,-2.683308e-08]
## (score 43.97454 & scale 22.64329).
## Hessian positive definite, eigenvalue range [6.5,6.5].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 3.09    1.21    0.68



model2.Macu.cp.gam <- gam(log(CP) ~ s(ANO, k = 6), data = Macu, method = "REML", sp = 0.0001)
summary(model2.Macu.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.5434     0.1027   24.77 7.78e-11 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.241  3.759 0.704   0.626
##
## R-sq.(adj) =  -0.013   Deviance explained = 22.2%
## -REML = 12.024  Scale est. = 0.1581    n = 15

gam.check(model2.Macu.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.003265e-08,-1.003265e-08]
## (score 12.02358 & scale 0.1580965).
## Hessian positive definite, eigenvalue range [6.5,6.5].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 3.24    1.26    0.77



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
## (Intercept) 995.7806   702.2094   1.418    0.180
## ANO          -0.4703     0.3531  -1.332    0.206
##
## R-sq.(adj) =  0.0524   
## Scale est. = 3180.7    n = 15

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
## (Intercept)  9.335987  13.779291   0.678    0.510
## ANO         -0.002881   0.006928  -0.416    0.684
##
## R-sq.(adj) =  -0.0628   
## Scale est. = 1.2247    n = 15

model5.Macu.p.lmer<-lmer(P~ANO + (1|Amb), data = Macu)
summary(model5.Macu.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Macu
##
## REML criterion at convergence: 156.6
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.50591 -0.62697  0.01312  0.33942  2.47226 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)    0      0.00   
## Residual             3670     60.58   
## Number of obs: 15, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)  
## (Intercept) 995.7806   702.2094  13.0000   1.418    0.180
## ANO          -0.4703     0.3531  13.0000  -1.332    0.206
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
## REML criterion at convergence: 53.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -2.1971 -0.5581  0.2192  0.5603  1.4768 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 1.152    1.073   
## Residual             1.250    1.118   
## Number of obs: 15, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  7.376213  13.066078 12.295214   0.565    0.583
## ANO         -0.002158   0.006539 12.099360  -0.330    0.747
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -0.998



model.Macu.p.gam <- gam(P ~ s(ANO, k = 7), data = Macu, method = "REML", sp = 0.0001)
summary(model.Macu.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  60.69      17.00    3.57  0.00446 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.094  3.608 0.424   0.777
##
## R-sq.(adj) =  -0.119   Deviance explained = 12.8%
## -REML = 78.004  Scale est. = 4335.7    n = 15

gam.check(model.Macu.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.459015e-08,-3.459015e-08]
## (score 78.00399 & scale 4335.678).
## Hessian positive definite, eigenvalue range [6.5,6.5].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 3.09    0.99    0.38



model2.Macu.p.gam <- gam(log(P) ~ s(ANO, k = 6), data = Macu, method = "REML", sp = 0.0001)
summary(model2.Macu.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.6077     0.3137    11.5 2.22e-07 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.241  3.759 0.364   0.833
##
## R-sq.(adj) =  -0.11   Deviance explained = 14.7%
## -REML = 26.531  Scale est. = 1.4758    n = 15

gam.check(model2.Macu.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.846753e-08,-3.846753e-08]
## (score 26.5315 & scale 1.475834).
## Hessian positive definite, eigenvalue range [6.5,6.5].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 3.24    1.21    0.73



##----------------------------------------------------------------------------##

dir()

Macu2 <- filter(Macu, Amb == "Pequeno")

Macu2$CP
Macu2$P
Macu2$ANO
Macu2$Longitude
Macu2$Latitude
Macu2$Amb

Macu2

summary(Macu2)
## catalognumber         CP              P               day            month             ANO      
## Min.   : 1272   Min.   : 5.20   Min.   :  3.30   Min.   : 1.00   Min.   : 1.000   Min.   :1900  
## 1st Qu.:13522   1st Qu.:11.30   1st Qu.: 24.25   1st Qu.: 2.00   1st Qu.: 1.250   1st Qu.:2004  
## Median :15654   Median :14.40   Median : 54.00   Median :14.50   Median : 4.000   Median :2011  
## Mean   :14710   Mean   :14.08   Mean   : 64.59   Mean   :14.64   Mean   : 4.643   Mean   :1987  
## 3rd Qu.:19240   3rd Qu.:16.85   3rd Qu.: 75.50   3rd Qu.:26.75   3rd Qu.: 7.500   3rd Qu.:2013  
## Max.   :21344   Max.   :21.80   Max.   :252.00   Max.   :30.00   Max.   :12.000   Max.   :2017  
## state               Amb              Longitude         Latitude     
## Length:14          Length:14          Min.   :-53.06   Min.   :-23.87  
## Class :character   Class :character   1st Qu.:-50.26   1st Qu.:-22.37  
## Mode  :character   Mode  :character   Median :-49.46   Median :-20.78  
##                                       Mean   :-49.16   Mean   :-21.07  
##                                       3rd Qu.:-47.82   3rd Qu.:-20.21  
##                                       Max.   :-45.90   Max.   :-16.97



## P.maculatus_AmbPeque - teste de normalidade -------------------------------##
shapiro.test(Macu2$CP) ## W = 0.9913, p-value = 0.9998
shapiro.test(Macu2$P)  ## W = 0.77392, p-value = 0.002418
shapiro.test(Macu2$ANO) ## W = 0.57821, p-value = 2.611e-05



## P.maculatus_AmbPeque - cor e cov ------------------------------------------##
cor(Macu2$ANO, Macu2$CP) ## -0.08297348
cor(Macu2$ANO, Macu2$P)  ## -0.3324333
cor(Macu2$ANO, Macu2$CP, method="spearman") ## 0.1005728
cor(Macu2$ANO, Macu2$P, method="spearman")  ## -0.01117475

cov(Macu2$ANO, Macu2$CP) ## -16.87527
cov(Macu2$ANO, Macu2$P)  ## -985.3764



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
## -8.7359 -2.6074  0.5093  2.9650  7.0651 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 29.056134  51.942805   0.559    0.586
## ANO         -0.007538   0.026134  -0.288    0.778
##
## Residual standard error: 4.458 on 12 degrees of freedom
## Multiple R-squared:  0.006885,	Adjusted R-squared:  -0.07588 
## F-statistic: 0.08319 on 1 and 12 DF,  p-value: 0.7779

model4.Macu2.cp.lm<-lm(log(CP)~ANO, data = Macu2)
summary(model4.Macu2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Macu2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -0.93545 -0.15898  0.08522  0.24218  0.45691 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.3579671  4.3816755   0.766    0.458
## ANO         -0.0003858  0.0022045  -0.175    0.864
##
## Residual standard error: 0.3761 on 12 degrees of freedom
## Multiple R-squared:  0.002545,	Adjusted R-squared:  -0.08058 
## F-statistic: 0.03062 on 1 and 12 DF,  p-value: 0.864



model.Macu2.cp.gam <- gam(CP ~ s(ANO, k = 7), data = Macu2, method = "REML", sp = 0.0001)
summary(model.Macu2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  14.079      1.256   11.21 5.87e-07 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.066  3.564 0.214   0.921
##
## R-sq.(adj) =  -0.196   Deviance explained = 8.63%
## -REML = 40.651  Scale est. = 22.091    n = 14

gam.check(model.Macu2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-8.477311e-08,-8.477311e-08]
## (score 40.65062 & scale 22.09091).
## Hessian positive definite, eigenvalue range [6,6].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 3.07    1.36    0.84



model2.Macu2.cp.gam <- gam(log(CP) ~ s(ANO, k = 7), data = Macu2, method = "REML", sp = 0.0001)
summary(model2.Macu2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.591      0.104   24.92 2.75e-10 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.066  3.564 0.301   0.877
##
## R-sq.(adj) =  -0.156   Deviance explained = 11.6%
## -REML = 10.786  Scale est. = 0.15135   n = 14

gam.check(model2.Macu2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.770425e-08,-5.770425e-08]
## (score 10.78613 & scale 0.1513525).
## Hessian positive definite, eigenvalue range [6,6].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 3.07    1.51    0.97



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
## -91.915 -33.526   1.259  19.018 149.085 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 939.1586   716.4431   1.311    0.214
## ANO          -0.4401     0.3605  -1.221    0.246
##
## Residual standard error: 61.49 on 12 degrees of freedom
## Multiple R-squared:  0.1105,	Adjusted R-squared:  0.03639 
## F-statistic: 1.491 on 1 and 12 DF,  p-value: 0.2455

model4.Macu2.p.lm<-lm(log(P)~ANO, data = Macu2)
summary(model4.Macu2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Macu2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.5107 -0.5301  0.2920  0.6152  1.6411 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.181108  13.023368   0.551    0.591
## ANO         -0.001733   0.006552  -0.264    0.796
##
## Residual standard error: 1.118 on 12 degrees of freedom
## Multiple R-squared:  0.005796,	Adjusted R-squared:  -0.07705 
## F-statistic: 0.06996 on 1 and 12 DF,  p-value: 0.7959



model.Macu2.p.gam <- gam(P ~ s(ANO, k = 7), data = Macu2, method = "REML", sp = 0.0001)
summary(model.Macu2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  64.59      17.99   3.591  0.00497 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.066  3.564 0.366   0.814
##
## R-sq.(adj) =  -0.154   Deviance explained = 11.8%
## -REML = 72.507  Scale est. = 4529.5    n = 14

gam.check(model.Macu2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.258339e-08,-4.258339e-08]
## (score 72.50747 & scale 4529.548).
## Hessian positive definite, eigenvalue range [6,6].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 3.07    1.01    0.37



model2.Macu2.p.gam <- gam(log(P) ~ s(ANO, k = 7), data = Macu2, method = "REML", sp = 0.0001)
summary(model2.Macu2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.737      0.320   11.68    4e-07 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.066  3.564 0.096   0.983
##
## R-sq.(adj) =  -0.235   Deviance explained = 5.59%
## -REML =  24.25  Scale est. = 1.4333    n = 14

gam.check(model2.Macu2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.352604e-07,-1.352604e-07]
## (score 24.24966 & scale 1.433301).
## Hessian positive definite, eigenvalue range [6,6].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 3.07    1.41    0.88




##----------------------------------------------------------------------------##
                            ## R.latirostris ## 

dir()

Lati <- read.table("R.latirostris_Geral2.txt", header=T)

Lati$CP
Lati$P
Lati$ANO
Lati$Longitude
Lati$Latitude
Lati$Amb

Lati

summary(Lati)
##        CP               P               ANO          state               Amb              Longitude     
## Min.   : 6.300   Min.   : 1.000   Min.   :1999   Length:17          Length:17          Min.   :-51.30  
## 1st Qu.: 7.200   1st Qu.: 2.700   1st Qu.:2004   Class :character   Class :character   1st Qu.:-49.49  
## Median : 8.500   Median : 5.000   Median :2009   Mode  :character   Mode  :character   Median :-49.23  
## Mean   : 9.576   Mean   : 9.941   Mean   :2007                                         Mean   :-48.82  
## 3rd Qu.:11.700   3rd Qu.:11.000   3rd Qu.:2011                                         3rd Qu.:-47.77  
## Max.   :19.600   Max.   :58.000   Max.   :2013                                         Max.   :-46.82  
## Latitude     
## Min.   :-24.84  
## 1st Qu.:-22.46  
## Median :-21.04  
## Mean   :-21.74  
## 3rd Qu.:-20.97  
## Max.   :-19.54 



## R.latirostris_Geral - teste de normalidade --------------------------------##
shapiro.test(Lati$CP) ## W = 0.81504, p-value = 0.003286
shapiro.test(Lati$P)  ## W = 0.6073, p-value = 1.226e-05
shapiro.test(Lati$ANO) ## W = 0.89792, p-value = 0.06269



## R.latirostris_Geral - cor e cov -------------------------------------------##
cor(Lati$ANO, Lati$CP) ## 0.2433723
cor(Lati$ANO, Lati$P)  ## 0.1582815
cor(Lati$ANO, Lati$CP, method="spearman") ## 0.2910043
cor(Lati$ANO, Lati$P, method="spearman")  ## 0.3725259

cov(Lati$ANO, Lati$CP) ## 3.668015
cov(Lati$ANO, Lati$P)  ## 9.741912



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
## (Intercept)   9.5765     0.7781   12.31 3.05e-09 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value   
## s(ANO)   1      1 1.007   0.331
##
## R-sq.(adj) =  -0.00349   
## Scale est. = 9.6861    n = 17

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
## (Intercept)   9.5765     0.7781   12.31 3.05e-09 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO)   1      1 1.007   0.331
##
## R-sq.(adj) =  -0.00349   
## Scale est. = 9.6861    n = 17

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
## (Intercept) -345.0364   364.9055  -0.946    0.359
## ANO            0.1766     0.1818   0.972    0.347
##
## R-sq.(adj) =  -0.00349   
## Scale est. = 9.6861    n = 17

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
## (Intercept) -36.59264   32.67561  -1.120    0.280
## ANO           0.01933    0.01628   1.188    0.253
##
## R-sq.(adj) =  0.025   
## Scale est. = 0.077667  n = 17

model5.Lati.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Lati)
summary(model5.Lati.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Lati
##
## REML criterion at convergence: 87.1
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.1771 -0.5131 -0.2606  0.5241  3.0504 
##
## Random effects:
##  Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.00    0.000   
## Residual             10.98    3.313   
## Number of obs: 17, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -345.0364   364.9055   15.0000  -0.946    0.359
## ANO            0.1766     0.1818   15.0000   0.972    0.347
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular


model6.Lati.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Lati)
summary(model6.Lati.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Lati
##
## REML criterion at convergence: 14.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.4860 -0.5147 -0.1828  0.6764  2.6002 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.00000  0.0000  
## Residual             0.08802  0.2967  
## Number of obs: 17, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -36.59264   32.67561  15.00000  -1.120    0.280
## ANO           0.01933    0.01628  15.00000   1.188    0.253
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



model.Lati.cp.gam <- gam(CP ~ s(ANO, k = 10), data = Lati, method = "REML", sp = 0.0001)
summary(model.Lati.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 10)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.5765     0.6195   15.46    1e-06 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 8.903  8.997 2.141   0.161
##
## R-sq.(adj) =  0.404   Deviance explained = 73.5%
## -REML = 55.909  Scale est. = 6.5249    n = 17

gam.check(model.Lati.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.334701e-10,-2.334701e-10]
## (score 55.90855 & scale 6.524949).
## Hessian positive definite, eigenvalue range [7.5,7.5].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##        k' edf k-index p-value
## s(ANO) 9.0 8.9    1.64    0.99



model2.Lati.cp.gam <- gam(log(CP) ~ s(ANO, k = 10), data = Lati, method = "REML", sp = 0.0001)
summary(model2.Lati.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 10)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.21320    0.06538   33.85 4.14e-09 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 8.903  8.997 1.389   0.335
##
## R-sq.(adj) =  0.195   Deviance explained = 64.3%
## -REML =  22.05  Scale est. = 0.072676  n = 17

gam.check(model2.Lati.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.556082e-08,-4.556082e-08]
## (score 22.04997 & scale 0.07267644).
## Hessian positive definite, eigenvalue range [7.5,7.5].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##        k' edf k-index p-value
## s(ANO) 9.0 8.9    1.62    0.99



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
## (Intercept)    9.941      3.235   3.073  0.00773 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 0.411   0.531
##
## R-sq.(adj) =  -0.0399   
## Scale est. = 167.4     n = 17

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
## (Intercept)    9.941      3.235   3.073  0.00773 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 0.411   0.531
##
## R-sq.(adj) =  -0.0399   
## Scale est. = 167.4     n = 17

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
## (Intercept) -931.8782  1516.9924  -0.614    0.548
## ANO            0.4692     0.7557   0.621    0.544
##
## R-sq.(adj) =  -0.0399   
## Scale est. = 167.4     n = 17

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
## (Intercept) -140.72785  110.16873  -1.277    0.221
## ANO            0.07098    0.05488   1.293    0.215
##
## R-sq.(adj) =  0.0404   
## Scale est. = 0.88289   n = 17

model5.Lati.p.lmer<-lmer(P~ANO + (1|Amb), data = Lati)
summary(model5.Lati.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Lati
##
## REML criterion at convergence: 129.9
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.7694 -0.3846 -0.3086  0.0499  3.5052 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)   0.0     0.00   
## Residual             189.7    13.77   
## Number of obs: 17, groups:  Amb, 2
##
## Fixed effects:
##  Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -931.8782  1516.9924   15.0000  -0.614    0.548
## ANO            0.4692     0.7557   15.0000   0.621    0.544
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Lati.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Lati)
summary(model6.Lati.p.lmer)
##boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Lati
##
## REML criterion at convergence: 51.2
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.0106 -0.4500 -0.1280  0.6257  2.3325 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.000    0       
## Residual             1.001    1       
## Number of obs: 17, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)   
## (Intercept) -140.72785  110.16873   15.00000  -1.277    0.221
## ANO            0.07098    0.05488   15.00000   1.293    0.215
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Lati.p.gam <- gam(P ~ s(ANO, k = 10), data = Lati, method = "REML", sp = 0.0001)
summary(model.Lati.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 10)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.941      1.656   6.005 0.000512 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 8.903  8.997 6.03  0.0133 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.745   Deviance explained = 88.7%
## -REML = 71.188  Scale est. = 46.593    n = 17

gam.check(model.Lati.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.008078e-05,-1.008078e-05]
## (score 71.18848 & scale 46.59341).
## Hessian positive definite, eigenvalue range [7.50001,7.50001].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##        k' edf k-index p-value
## s(ANO) 9.0 8.9    1.69       1



model2.Lati.p.gam <- gam(log(P) ~ s(ANO, k = 10), data = Lati, method = "REML", sp = 0.0001)
summary(model2.Lati.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 10)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.7607     0.2345   7.508 0.000127 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 8.903  8.997 1.177   0.421
##
## R-sq.(adj) =  0.103   Deviance explained = 60.2%
## -REML = 41.127  Scale est. = 0.93482   n = 17

gam.check(model2.Lati.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.00279e-07,-2.00279e-07]
## (score 41.12725 & scale 0.9348242).
## Hessian positive definite, eigenvalue range [7.5,7.5].
## Model rank =  10 / 10 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##        k' edf k-index p-value
## s(ANO) 9.0 8.9    1.59    0.98



##----------------------------------------------------------------------------##

dir()

Lati2 <- filter(Lati, Amb == "Pequeno")

Lati2$CP
Lati2$P
Lati2$ANO
Lati2$Longitude
Lati2$Latitude
Lati2$Amb

Lati2

summary(Lati2)
##       CP               P              ANO          state               Amb              Longitude     
## Min.   : 6.300   Min.   : 1.00   Min.   :1999   Length:9           Length:9           Min.   :-50.02  
## 1st Qu.: 6.900   1st Qu.: 2.00   1st Qu.:2003   Class :character   Class :character   1st Qu.:-49.49  
## Median : 8.100   Median : 4.00   Median :2004   Mode  :character   Mode  :character   Median :-49.42  
## Mean   : 9.333   Mean   :10.22   Mean   :2006                                         Mean   :-48.80  
## 3rd Qu.: 9.900   3rd Qu.: 7.00   3rd Qu.:2011                                         3rd Qu.:-47.70  
## Max.   :19.600   Max.   :58.00   Max.   :2013                                         Max.   :-46.82  
## 
## Latitude     
## Min.   :-24.05  
## 1st Qu.:-21.97  
## Median :-21.04  
## Mean   :-21.61  
## 3rd Qu.:-20.97  
## Max.   :-20.97  



## R.latirostris_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Lati2$CP) ## W = 0.70844, p-value = 0.001781
shapiro.test(Lati2$P)  ## W = 0.52585, p-value = 1.288e-05
shapiro.test(Lati2$ANO) ## W = 0.92836, p-value = 0.4659



## R.latirostris_AmbPeque - cor e cov ----------------------------------------##
cor(Lati2$ANO, Lati2$CP) ## 0.2010028
cor(Lati2$ANO, Lati2$P)  ## 0.1427503
cor(Lati2$ANO, Lati2$CP, method="spearman") ## 0.369761
cor(Lati2$ANO, Lati2$P, method="spearman")  ## 0.3797502

cov(Lati2$ANO, Lati2$CP) ## 4.158333
cov(Lati2$ANO, Lati2$P)  ## 12.98056



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
## -3.8953 -1.6749 -1.3147 -0.6254 10.0649 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -321.719    609.806  -0.528    0.614
## ANO            0.165      0.304   0.543    0.604
##
## Residual standard error: 4.316 on 7 degrees of freedom
## Multiple R-squared:  0.0404,	Adjusted R-squared:  -0.09668 
## F-statistic: 0.2947 on 1 and 7 DF,  p-value: 0.6041

model4.Lati2.cp.lm<-lm(log(CP)~ANO, data = Lati2)
summary(model4.Lati2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Lati2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.41787 -0.14945 -0.11835  0.00039  0.78455 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -31.64921   51.26165  -0.617    0.557
## ANO           0.01686    0.02556   0.660    0.531
##
## Residual standard error: 0.3628 on 7 degrees of freedom
## Multiple R-squared:  0.05854,	Adjusted R-squared:  -0.07595 
## F-statistic: 0.4353 on 1 and 7 DF,  p-value: 0.5305



model.Lati2.cp.gam <- gam(CP ~ s(ANO, k = 6), data = Lati2, method = "REML", sp = 0.0001)
summary(model.Lati2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.3333     0.4659   20.03 0.000238 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.936  4.998 13.08  0.0296 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.885   Deviance explained = 95.6%
## -REML = 22.666  Scale est. = 1.9537    n = 9

gam.check(model.Lati2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.099425e-05,-2.099425e-05]
## (score 22.66559 & scale 1.95368).
## Hessian positive definite, eigenvalue range [3.500021,3.500021].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.94    1.74    0.94



model2.Lati2.cp.gam <- gam(log(CP) ~ s(ANO, k = 5), data = Lati2, method = "REML", sp = 0.0001)
summary(model2.Lati2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 5)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.17037    0.05428   39.98 2.15e-06 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 3.968  3.999 8.142  0.0336 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.783   Deviance explained = 89.1%
## -REML = 5.9254  Scale est. = 0.026518  n = 9

gam.check(model2.Lati2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.067257e-10,-1.067257e-10]
## (score 5.92538 & scale 0.02651757).
## Hessian positive definite, eigenvalue range [3.5,3.5].
## Model rank =  5 / 5 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 4.00 3.97    1.62    0.96



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
## -11.913  -6.613  -5.306  -4.730  47.148 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -1023.1846  2708.1699  -0.378    0.717
## ANO             0.5152     1.3502   0.382    0.714
##
## Residual standard error: 19.17 on 7 degrees of freedom
## Multiple R-squared:  0.02038,	Adjusted R-squared:  -0.1196 
## F-statistic: 0.1456 on 1 and 7 DF,  p-value: 0.7141

model4.Lati2.p.lm<-lm(log(P)~ANO, data = Lati2)
summary(model4.Lati2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Lati2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.81415 -0.47826 -0.05293  0.02640  2.46054 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -105.89922  173.77127  -0.609    0.561
## ANO            0.05356    0.08664   0.618    0.556
##
## Residual standard error: 1.23 on 7 degrees of freedom
## Multiple R-squared:  0.05178,	Adjusted R-squared:  -0.08368 
## F-statistic: 0.3822 on 1 and 7 DF,  p-value: 0.556



model.Lati2.p.gam <- gam(P ~ s(ANO, k = 7), data = Lati2, method = "REML", sp = 0.0001)
summary(model.Lati2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.222      1.181   8.652  0.00769 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 5.639  5.936 33.89  0.0288 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.962   Deviance explained = 98.9%
## -REML = 30.534  Scale est. = 12.563    n = 9

gam.check(model.Lati2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.088198e-09,-2.088198e-09]
## (score 30.53398 & scale 12.56335).
## Hessian positive definite, eigenvalue range [3.5,3.5].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 6.00 5.64     1.7    0.95



model2.Lati2.p.gam <- gam(log(P) ~ s(ANO, k = 6), data = Lati2, method = "REML", sp = 0.0001)
summary(model2.Lati2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.534      0.271   5.663   0.0103 *
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.936  4.998 2.731   0.218
##
## R-sq.(adj) =  0.527   Deviance explained = 81.9%
## -REML = 18.362  Scale est. = 0.66082   n = 9

gam.check(model2.Lati2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.111764e-06,-1.111764e-06]
## (score 18.36238 & scale 0.6608218).
## Hessian positive definite, eigenvalue range [3.500001,3.500001].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.94    1.68    0.94




##----------------------------------------------------------------------------##
                                 ## R.quelen ## 

dir()

Quel <- read.table("R.quelen_Geral2.txt", header=T)

Quel$CP
Quel$P
Quel$ANO
Quel$Longitude
Quel$Latitude
Quel$Amb

Quel

summary(Quel)
##       CP               P               ANO          state               Amb              Longitude     
## Min.   : 4.000   Min.   :  1.00   Min.   :1985   Length:163         Length:163         Min.   :-56.02  
## 1st Qu.: 6.300   1st Qu.:  4.30   1st Qu.:2004   Class :character   Class :character   1st Qu.:-50.28  
## Median : 8.400   Median : 11.00   Median :2005   Mode  :character   Mode  :character   Median :-49.46  
## Mean   : 9.125   Mean   : 20.79   Mean   :2006                                         Mean   :-49.50  
## 3rd Qu.:11.100   3rd Qu.: 24.75   3rd Qu.:2009                                         3rd Qu.:-48.62  
## Max.   :20.700   Max.   :153.00   Max.   :2017                                         Max.   :-43.78  
## NA's   :1       
##    Latitude     
## Min.   :-24.60  
## 1st Qu.:-21.26  
## Median :-20.84  
## Mean   :-20.84  
## 3rd Qu.:-20.41  
## Max.   :-15.70  
## NA's   :1     



## R.quelen_Geral - teste de normalidade -------------------------------------##
shapiro.test(Quel$CP) ## W = 0.92152, p-value = 1.006e-07
shapiro.test(Quel$P)  ## W = 0.69804, p-value < 2.2e-16
shapiro.test(Quel$ANO) ## W = 0.89936, p-value = 4.092e-09



## R.quelen_Geral - cor e cov ------------------------------------------------##
cor(Quel$ANO, Quel$CP) ## 0.07659072
cor(Quel$ANO, Quel$P)  ## 0.08003511
cor(Quel$ANO, Quel$CP, method="spearman") ## 0.05934191
cor(Quel$ANO, Quel$P, method="spearman")  ## 0.02422791

cov(Quel$ANO, Quel$CP) ## 1.203208
cov(Quel$ANO, Quel$P)  ## 9.149345



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
## (Intercept)   9.1253     0.2823   32.32   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.955    0.33
##
## R-sq.(adj) =  -0.000307   
## Scale est. = 12.903    n = 163

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
## (Intercept)  2.13833    0.02963   72.17   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.495   0.483
##
## R-sq.(adj) =  -0.00315   
## Scale est. = 0.14222   n = 163

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
## (Intercept) -105.91507  132.16714  -0.801    0.424
## ANO            0.05736    0.06587   0.871    0.385
##
## R-sq.(adj) =  -0.000367   
## Scale est. = 12.865    n = 163

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
## (Intercept) -7.509965  13.758686  -0.546    0.586
## ANO          0.004809   0.006858   0.701    0.484
##
## R-sq.(adj) =  -0.00315   
## Scale est. = 0.14222   n = 163

model5.Quel.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Quel)
summary(model5.Quel.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Quel
##
## REML criterion at convergence: 882.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.5818 -0.7596 -0.2484  0.6096  3.2745 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.4532  0.6732  
## Residual             12.9005  3.5917  
## Number of obs: 163, groups:  Amb, 2
##
## Fixed effects:
##  Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -64.13080  135.73013 136.96055  -0.472    0.637
## ANO           0.03660    0.06764 137.26840   0.541    0.589
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
## REML criterion at convergence: 157.5
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.10856 -0.79872 -0.06184  0.75156  2.40035 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.003382 0.05816 
## Residual             0.142775 0.37786 
## Number of obs: 163, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  -3.139323  14.200913 122.057731  -0.221    0.825
## ANO           0.002638   0.007077 122.476939   0.373    0.710
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Quel.cp.gam <- gam(CP ~ s(ANO, k = 19), data = Quel, method = "REML", sp = 0.0001)
summary(model.Quel.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 19)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.1252     0.2765   33.01   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 17.85     18 1.42    0.13
##
## R-sq.(adj) =  0.0461   Deviance explained = 15.1%
## -REML = 486.04  Scale est. = 12.457    n = 163

gam.check(model.Quel.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.522577e-08,-3.522577e-08]
## (score 486.0402 & scale 12.45735).
## Hessian positive definite, eigenvalue range [80.5,80.5].
## Model rank =  19 / 19 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 18.0 17.9    1.03    0.6



model2.Quel.cp.gam <- gam(log(CP) ~ s(ANO, k = 19), data = Quel, method = "REML", sp = 0.001)
summary(model2.Quel.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 19)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.13833    0.02921   73.21   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.81  17.75 1.188    0.28
##
## R-sq.(adj) =  0.0313   Deviance explained = 13.2%
## -REML = 106.23  Scale est. = 0.13904   n = 163

gam.check(model2.Quel.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.593317e-08,-5.593317e-08]
## (score 106.2281 & scale 0.1390445).
## Hessian positive definite, eigenvalue range [80.5,80.5].
## Model rank =  19 / 19 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 18.0 16.8    1.02    0.61



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
## (Intercept)   20.786      2.053   10.12   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 1.044   0.308
##
## R-sq.(adj) =  0.000234   
## Scale est. = 682.87    n = 163

model2.Quel.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Quel)
summary(model2.Quel.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
## 
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.3996     0.0894   26.84   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 0.194    0.66
##
## R-sq.(adj) =  -0.00501   
## Scale est. = 1.2947    n = 163

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
## (Intercept) -950.5072   953.3720  -0.997     0.32
## ANO            0.4841     0.4752   1.019     0.31
##
## R-sq.(adj) =  0.000234   
# Scale est. = 682.87    n = 163

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
## (Intercept) -15.839864  41.512244  -0.382    0.703
## ANO           0.009091   0.020692   0.439    0.661
##
## R-sq.(adj) =  -0.00501   
## Scale est. = 1.2947    n = 163

model5.Quel.p.lmer<-lmer(P~ANO + (1|Amb), data = Quel)
summary(model5.Quel.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Quel
##
## REML criterion at convergence: 1522.7
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.8472 -0.6025 -0.4102  0.2043  5.0191 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)   0.8925  0.9447 
## Residual             691.0331 26.2875 
## Number of obs: 163, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)  
## (Intercept) -925.8973   957.4004   52.8019  -0.967    0.338
## ANO            0.4719     0.4772   53.2836   0.989    0.327
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
## REML criterion at convergence: 513.5
##
## Scaled residuals: 
##  Min       1Q   Median       3Q      Max 
## -2.08615 -0.80777  0.00057  0.73182  2.31292 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.008728 0.09343 
## Residual             1.307642 1.14352 
## Number of obs: 163, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept) -10.967354  42.198641  75.627559  -0.260    0.796
## ANO           0.006671   0.021032  76.169621   0.317    0.752
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000



model.Quel.p.gam <- gam(P ~ s(ANO, k = 19), data = Quel, method = "REML", sp = 0.001)
summary(model.Quel.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 19)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  20.786      2.014   10.32   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.81  17.75 1.343    0.17
##
## R-sq.(adj) =  0.0444   Deviance explained = 14.4%
## -REML =  787.5  Scale est. = 660.83    n = 163

gam.check(model.Quel.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.236453e-08,-4.236453e-08]
## (score 787.5005 & scale 660.8321).
## Hessian positive definite, eigenvalue range [80.5,80.5].
## Model rank =  19 / 19 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 18.0 16.8    1.12    0.94



model2.Quel.p.gam <- gam(log(P) ~ s(ANO, k = 19), data = Quel, method = "REML", sp = 0.001)
summary(model2.Quel.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 19)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.39957    0.08848   27.12   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 16.81  17.75 1.11   0.349
##
## R-sq.(adj) =  0.0215   Deviance explained = 12.3%
## -REML = 284.53  Scale est. = 1.2762    n = 163

gam.check(model2.Quel.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-6.375538e-08,-6.375538e-08]
## (score 284.527 & scale 1.276182).
## Hessian positive definite, eigenvalue range [80.5,80.5].
## Model rank =  19 / 19 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 18.0 16.8    1.04     0.64



##----------------------------------------------------------------------------##

dir()

Quel2 <- filter(Quel, Amb == "Grande")
Quel3 <- filter(Quel, Amb == "Pequeno")

Quel2$CP
Quel2$P
Quel2$ANO
Quel2$Longitude
Quel2$Latitude
Quel2$Amb

Quel2

summary(Quel2)
##       CP               P                ANO          state               Amb              Longitude     
## Min.   : 4.100   Min.   :  1.000   Min.   :1985   Length:118         Length:118         Min.   :-56.02  
## 1st Qu.: 6.300   1st Qu.:  4.625   1st Qu.:2003   Class :character   Class :character   1st Qu.:-50.59  
## Median : 7.900   Median : 10.250   Median :2005   Mode  :character   Mode  :character   Median :-49.67  
## Mean   : 8.783   Mean   : 19.108   Mean   :2005                                         Mean   :-49.89  
## 3rd Qu.:10.200   3rd Qu.: 21.500   3rd Qu.:2007                                         3rd Qu.:-49.32  
## Max.   :20.700   Max.   :153.000   Max.   :2013                                         Max.   :-46.77  
##                                                                                         NA's   :1       
##    Latitude     
## Min.   :-23.18  
## 1st Qu.:-21.17  
## Median :-20.79  
## Mean   :-20.65  
## 3rd Qu.:-20.37  
## Max.   :-15.95  
## NA's   :1      



## R.quelen_AmbGran - teste de normalidade -----------------------------------##
shapiro.test(Quel2$CP) ## W = 0.90024, p-value = 2.442e-07
shapiro.test(Quel2$P)  ## W = 0.62537, p-value = 6.605e-16
shapiro.test(Quel2$ANO) ## W = 0.86153, p-value = 4.09e-09



## R.quelen_AmbGran - cor e cov ----------------------------------------------##
cor(Quel2$ANO, Quel2$CP) ## -0.1163727
cor(Quel2$ANO, Quel2$P)  ## -0.07092467
cor(Quel2$ANO, Quel2$CP, method="spearman") ## -0.1063919
cor(Quel2$ANO, Quel2$P, method="spearman")  ## -0.1063919

cov(Quel2$ANO, Quel2$CP) ## -1.662161
cov(Quel2$ANO, Quel2$P)  ## -7.886441



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
## (Intercept)   8.7831     0.3098   28.35   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.606   0.208
##
## R-sq.(adj) =  0.00504   
## Scale est. = 11.227    n = 118

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
## (Intercept)  2.10751    0.03249   64.87   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 1.823    0.18
##
## R-sq.(adj) =  0.00685   
## Scale est. = 0.12348   n = 118

model3.Quel2.cp.lm<-lm(CP~ANO, data = Quel2)
summary(model3.Quel2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Quel2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -4.6189 -2.4374 -0.6721  1.4367 12.0747 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 196.32726  148.61531   1.321    0.189
## ANO          -0.09352    0.07411  -1.262    0.209
##
## Residual standard error: 3.379 on 116 degrees of freedom
## Multiple R-squared:  0.01354,	Adjusted R-squared:  0.005039 
## F-statistic: 1.593 on 1 and 116 DF,  p-value: 0.2095

model4.Quel2.cp.lm<-lm(log(CP)~ANO, data = Quel2)
summary(model4.Quel2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Quel2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.68936 -0.25900 -0.01642  0.21988  0.94024 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 23.059154  15.585411   1.480    0.142
## ANO         -0.010448   0.007772  -1.344    0.181
##
## Residual standard error: 0.3544 on 116 degrees of freedom
## Multiple R-squared:  0.01534,	Adjusted R-squared:  0.006852 
## F-statistic: 1.807 on 1 and 116 DF,  p-value: 0.1815



model.Quel2.cp.gam <- gam(CP ~ s(ANO, k = 17), data = Quel2, method = "REML", sp = 0.0001)
summary(model.Quel2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 17)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  8.783      0.312   28.15   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 15.66  15.98 0.963   0.505
##
## R-sq.(adj) =  -0.000842   Deviance explained = 13.3%
## -REML = 346.46  Scale est. = 11.488    n = 118

gam.check(model.Quel2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.95605e-08,-3.95605e-08]
## (score 346.4567 & scale 11.48834).
## Hessian positive definite, eigenvalue range [58,58].
## Model rank =  17 / 17 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## #s(ANO) 16.0 15.7    1.11    0.87



model2.Quel2.cp.gam <- gam(log(CP) ~ s(ANO, k = 17), data = Quel2, method = "REML", sp = 0.0001)
summary(model2.Quel2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 17)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.10751    0.03299   63.89   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 15.66  15.98 0.859    0.62
##
## R-sq.(adj) =  -0.0152   Deviance explained = 12.1%
## -REML = 85.811  Scale est. = 0.12839   n = 118

gam.check(model2.Quel2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.984781e-08,-4.984781e-08]
## (score 85.81128 & scale 0.1283941).
## Hessian positive definite, eigenvalue range [58,58].
## Model rank =  17 / 17 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 16.0 15.7    1.07    0.78



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
## (Intercept)   19.108      2.422    7.89 1.86e-12 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.592   0.443
##
## R-sq.(adj) =  -0.00355   
## Scale est. = 686.32    n = 118

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
## (Intercept)   2.3301     0.1001   23.29   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value
##s(ANO)   1      1 1.72   0.192
##
##R-sq.(adj) =  0.00599   
## Scale est. = 1.1712    n = 118

model3.Quel2.p.lm<-lm(P~ANO, data = Quel2)
summary(model3.Quel2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Quel2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -20.798 -14.179  -7.960   0.406 134.640 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  908.9476  1161.9596   0.782    0.436
## ANO           -0.4437     0.5794  -0.766    0.445
##
## Residual standard error: 26.42 on 116 degrees of freedom
## Multiple R-squared:  0.00503,	Adjusted R-squared:  -0.003547 
## F-statistic: 0.5865 on 1 and 116 DF,  p-value: 0.4453

model4.Quel2.p.lm<-lm(log(P)~ANO, data = Quel2)
summary(model4.Quel2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Quel2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.30861 -0.83290  0.05071  0.63390  2.75309 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 65.01582   48.00107   1.354    0.178
## ANO         -0.03126    0.02394  -1.306    0.194
##
## Residual standard error: 1.092 on 116 degrees of freedom
## Multiple R-squared:  0.01449,	Adjusted R-squared:  0.005993 
## F-statistic: 1.705 on 1 and 116 DF,  p-value: 0.1942



model.Quel2.p.gam <- gam(P ~ s(ANO, k = 17), data = Quel2, method = "REML", sp = 0.0001)
summary(model.Quel2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 17)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  19.108      2.437   7.842 4.64e-12 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 15.66  15.98 0.92   0.548
##
## R-sq.(adj) =  -0.00707   Deviance explained = 12.8%
## -REML = 584.82  Scale est. = 700.61    n = 118

gam.check(model.Quel2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.310245e-08,-4.310245e-08]
## (score 584.82 & scale 700.6094).
## Hessian positive definite, eigenvalue range [58,58].
## Model rank =  17 / 17 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 16.0 15.7    1.21    0.99



model2.Quel2.p.gam <- gam(log(P) ~ s(ANO, k = 17), data = Quel2, method = "REML", sp = 0.0001)
summary(model2.Quel2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 17)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.3301     0.1019   22.86   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 15.66  15.98 0.805   0.677
##
## R-sq.(adj) =  -0.0231   Deviance explained = 11.4%
## -REML = 216.66  Scale est. = 1.2263    n = 118

gam.check(model2.Quel2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.583599e-08,-5.583599e-08]
## (score 216.66 & scale 1.226282).
## Hessian positive definite, eigenvalue range [58,58].
## Model rank =  17 / 17 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 16.0 15.7    1.09    0.78



##----------------------------------------------------------------------------##


Quel3$CP
Quel3$P
Quel3$ANO
Quel3$Longitude
Quel3$Latitude
Quel3$Amb

Quel3

summary(Quel3)
##       CP              P              ANO          state               Amb              Longitude     
## Min.   : 4.00   Min.   : 1.50   Min.   :2003   Length:45          Length:45          Min.   :-55.26  
## 1st Qu.: 6.30   1st Qu.: 3.30   1st Qu.:2005   Class :character   Class :character   1st Qu.:-49.49  
## Median : 9.00   Median :14.30   Median :2008   Mode  :character   Mode  :character   Median :-48.49  
## Mean   :10.02   Mean   :25.18   Mean   :2009                                         Mean   :-48.50  
## 3rd Qu.:13.40   3rd Qu.:41.00   3rd Qu.:2012                                         3rd Qu.:-47.67  
## Max.   :18.30   Max.   :94.00   Max.   :2017                                         Max.   :-43.78  
## 
## Latitude     
## Min.   :-24.60  
## 1st Qu.:-22.25  
## Median :-21.05  
## Mean   :-21.33  
## 3rd Qu.:-20.58  
## Max.   :-15.70  



## R.quelen_AmbPeque - teste de normalidade ----------------------------------##
shapiro.test(Quel3$CP) ##  W = 0.93631, p-value = 0.01577
shapiro.test(Quel3$P)  ## W = 0.83428, p-value = 1.478e-05
shapiro.test(Quel3$ANO) ## W = 0.93607, p-value = 0.01546



## R.quelen_AmbPeque - cor e cov ---------------------------------------------##
cor(Quel3$ANO, Quel3$CP) ## 0.3803416
cor(Quel3$ANO, Quel3$P)  ## 0.4068781
cor(Quel3$ANO, Quel3$CP, method="spearman") ## 0.3235306
cor(Quel3$ANO, Quel3$P, method="spearman")  ## 0.2692169

cov(Quel3$ANO, Quel3$CP) ## 5.855051
cov(Quel3$ANO, Quel3$P)  ## 39.97283



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
## (Intercept)  10.0222     0.5587   17.94   <2e-16 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO)   1      1 7.442 0.00919 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.125   
## Scale est. = 13.734    n = 45

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
## (Intercept)  2.21915    0.05988   37.06   <2e-16 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 5.897  0.0194 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0977   
## Scale est. = 0.15774   n = 45

model3.Quel3.cp.lm<-lm(CP~ANO, data = Quel3)
summary(model3.Quel3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Quel3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -6.9107 -3.2764 -0.3707  3.3950  5.7293 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -804.8949   302.1869  -2.664  0.01084 * 
##   ANO            0.4057     0.1504   2.697  0.00996 **
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 3.791 on 43 degrees of freedom
## Multiple R-squared:  0.1447,	Adjusted R-squared:  0.1248 
## F-statistic: 7.272 on 1 and 43 DF,  p-value: 0.009958

model4.Quel3.cp.lm<-lm(log(CP)~ANO, data = Quel3)
summary(model4.Quel3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Quel3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.84754 -0.33554  0.02347  0.37383  0.57960 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -75.52685   32.38616  -2.332   0.0244 *
##   ANO           0.03871    0.01612   2.401   0.0208 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4063 on 43 degrees of freedom
## Multiple R-squared:  0.1182,	Adjusted R-squared:  0.09767 
## F-statistic: 5.763 on 1 and 43 DF,  p-value: 0.02077



model.Quel3.cp.gam <- gam(CP ~ s(ANO, k = 12), data = Quel3, method = "REML", sp = 0.0001)
summary(model.Quel3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  10.0222     0.4722   21.22   <2e-16 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.92     11 3.502 0.00245 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.389   Deviance explained = 54.1%
## -REML = 142.05  Scale est. = 10.035    n = 45

gam.check(model.Quel3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.328396e-06,-1.328396e-06]
## (score 142.0509 & scale 10.03471).
## Hessian positive definite, eigenvalue range [21.5,21.5].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.9    1.18    0.87



model2.Quel3.cp.gam <- gam(log(CP) ~ s(ANO, k = 12), data = Quel3, method = "REML", sp = 0.0001)
summary(model2.Quel3.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.21915    0.05257   42.22   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.92     11 2.846 0.00954 **
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =   0.32   Deviance explained = 48.9%
## -REML = 47.633  Scale est. = 0.12434   n = 45

gam.check(model2.Quel3.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-5.285838e-06,-5.285838e-06]
## (score 47.63266 & scale 0.1243412).
## Hessian positive definite, eigenvalue range [21.50001,21.50001].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.9    1.21    0.89



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
## (Intercept)    25.18       2.93   8.596 1.65e-10 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F  p-value    
## s(ANO) 5.3    5.3 5.51 0.000402 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.413   
## Scale est. = 377.71    n = 45

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
## (Intercept)   2.5818     0.1771   14.58   <2e-16 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO)   1      1 4.869  0.0327 *
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0787   
## Scale est. = 1.3802    n = 45

model3.Quel3.p.lm<-lm(P~ANO, data = Quel3)
summary(model3.Quel3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Quel3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -37.203 -17.514  -6.514  14.176  61.606 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -5538.3103  1904.8034  -2.908  0.00574 **
##   ANO             2.7699     0.9483   2.921  0.00554 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 23.9 on 43 degrees of freedom
## Multiple R-squared:  0.1655,	Adjusted R-squared:  0.1461 
## F-statistic: 8.531 on 1 and 43 DF,  p-value: 0.005542


model4.Quel3.p.lm<-lm(log(P)~ANO, data = Quel3)
summary(model4.Quel3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Quel3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.53240 -1.11095  0.09302  1.03837  1.74744 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -206.39868   95.79814  -2.155   0.0368 *
##   ANO            0.10404    0.04769   2.181   0.0347 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.202 on 43 degrees of freedom
## Multiple R-squared:  0.09964,	Adjusted R-squared:  0.0787 
## F-statistic: 4.759 on 1 and 43 DF,  p-value: 0.03466



model.Quel3.p.gam <- gam(P ~ s(ANO, k = 12), data = Quel3, method = "REML", sp = 0.0001)
summary(model.Quel3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  25.184      2.902   8.679 4.86e-10 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.92     11 4.04 0.000873 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.434   Deviance explained = 57.4%
## -REML = 220.05  Scale est. = 378.87    n = 45

gam.check(model.Quel3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.303186e-07,-4.303186e-07]
## (score 220.0517 & scale 378.87).
## Hessian positive definite, eigenvalue range [21.5,21.5].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.9    1.12     0.7



model2.Quel3.p.gam <- gam(log(P) ~ s(ANO, k = 12), data = Quel3, method = "REML", sp = 0.001)
summary(model2.Quel3.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 12)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.5818     0.1535   16.82   <2e-16 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 10.36   10.9 2.694  0.0114 *
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.324   Deviance explained = 48.3%
## -REML = 83.405  Scale est. = 1.0606    n = 45

gam.check(model2.Quel3.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-8.270262e-06,-8.270262e-06]
## (score 83.40496 & scale 1.060573).
## Hessian positive definite, eigenvalue range [21.50001,21.50001].
## Model rank =  12 / 12 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 11.0 10.4    1.19    0.89



##----------------------------------------------------------------------------##
                               ## S.hilarii ##

dir()

Hila <- read.table("S.hilarii_Geral2.txt", header=T)

Hila$CP
Hila$P
Hila$ANO
Hila$Longitude
Hila$Latitude
Hila$Amb

Hila

summary(Hila)
## catalognumber         CP              P               day            month            ANO          state          
## Min.   :  531   Min.   : 6.40   Min.   :  5.00   Min.   : 1.00   Min.   :1.000   Min.   :1980   Length:12         
## 1st Qu.: 1678   1st Qu.:10.72   1st Qu.: 21.25   1st Qu.: 9.25   1st Qu.:2.750   1st Qu.:1988   Class :character  
## Median : 9415   Median :13.10   Median : 34.00   Median :14.50   Median :3.500   Median :1999   Mode  :character  
## Mean   :10394   Mean   :14.63   Mean   : 79.81   Mean   :16.00   Mean   :3.833   Mean   :1999                     
## 3rd Qu.:18807   3rd Qu.:20.82   3rd Qu.:160.25   3rd Qu.:24.50   3rd Qu.:4.500   3rd Qu.:2009                     
## Max.   :21436   Max.   :23.40   Max.   :253.50   Max.   :31.00   Max.   :7.000   Max.   :2017                     
## Amb              Longitude         Latitude     
## Length:12          Min.   :-50.23   Min.   :-23.40  
## Class :character   1st Qu.:-49.42   1st Qu.:-21.19  
## Mode  :character   Median :-49.38   Median :-20.56  
## Mean   :-49.08   Mean   :-21.05  
## 3rd Qu.:-48.91   3rd Qu.:-20.41  
## Max.   :-47.59   Max.   :-19.99 



## S.hilarii_Geral - teste de normalidade ------------------------------------##
shapiro.test(Hila$CP) ## W = 0.89891, p-value = 0.1536
shapiro.test(Hila$P)  ## W = 0.78155, p-value = 0.005824
shapiro.test(Hila$ANO) ## W = 0.89803, p-value = 0.1496



## S.hilarii_Geral - cor e cov -----------------------------------------------##
cor(Hila$ANO, Hila$CP) ## 0.2695756
cor(Hila$ANO, Hila$P)  ## 0.2131989
cor(Hila$ANO, Hila$CP, method="spearman") ## 0.3719321
cor(Hila$ANO, Hila$P, method="spearman")  ## 0.2280716

cov(Hila$ANO, Hila$CP) ## 20.5697
cov(Hila$ANO, Hila$P)  ## 246.7811



## S.hilarii_Geral - CP ------------------------------------------------------##

model.Hila.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Hila)
summary(model.Hila.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   13.600      2.523   5.392 0.000305 ***
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.111   0.746
##
## R-sq.(adj) =  -0.0532   
## Scale est. = 22.676    n = 12

model2.Hila.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Hila)
summary(model2.Hila.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.4797     0.2354   10.54 1.72e-06 ***
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 1.661  1.661 0.867    0.41
##
## R-sq.(adj) =  -0.0151   
## Scale est. = 0.07368   n = 12

model3.Hila.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Hila)
summary(model3.Hila.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -67.20214  254.63152  -0.264    0.797
## ANO           0.04042    0.12751   0.317    0.758
##
## R-sq.(adj) =  -0.0532   
## Scale est. = 22.676    n = 12

model4.Hila.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Hila)
summary(model4.Hila.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 2.178e+00  1.696e+01   0.128    0.900
## ANO         1.583e-04  8.497e-03   0.019    0.986
##
## R-sq.(adj) =  -0.0967   
## Scale est. = 0.094462  n = 12

model5.Hila.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Hila)
summary(model5.Hila.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Hila
##
## REML criterion at convergence: 71.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.2909 -0.6682 -0.3508  0.9323  1.4585 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 26.51    5.149   
## Residual             24.12    4.911   
## Number of obs: 12, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  1.365e+01  2.502e+02  9.858e+00   0.055    0.958
## ANO         -3.124e-04  1.253e-01  9.861e+00  -0.002    0.998
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Hila.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Hila)
summary(model6.Hila.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Hila
##
## REML criterion at convergence: 17.6
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.4022 -0.6090 -0.1802  0.8269  1.3349 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.1869   0.4324  
## Residual             0.1028   0.3206  
## Number of obs: 12, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  5.620681  16.507994  9.635212   0.340    0.741
## ANO         -0.001576   0.008271  9.635922  -0.191    0.853
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Hila.cp.gam <- gam(CP ~ s(ANO, k = 9), data = Hila, method = "REML", sp = 0.0001)
summary(model.Hila.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  14.633      1.734   8.441  0.00311 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.908  7.997 0.837    0.63
##
## R-sq.(adj) =  -0.123   Deviance explained = 68.4%
## -REML = 50.411  Scale est. = 36.068    n = 12

gam.check(model.Hila.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-8.522575e-09,-8.522575e-09]
## (score 50.41104 & scale 36.06792).
## Hessian positive definite, eigenvalue range [5,5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.91    1.66    0.98



model2.Hila.cp.gam <- gam(log(CP) ~ s(ANO, k = 9), data = Hila, method = "REML", sp = 0.0001)
summary(model2.Hila.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.6116     0.1087   24.02 0.000129 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.908  7.997 1.179   0.496
##
## R-sq.(adj) =  0.122   Deviance explained = 75.3%
## -REML = 22.734  Scale est. = 0.14189   n = 12

gam.check(model2.Hila.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-6.795098e-11,-6.795098e-11]
## (score 22.73423 & scale 0.1418864).
## Hessian positive definite, eigenvalue range [5,5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.91     1.7    0.99



## S.hilarii_Geral - P -------------------------------------------------------##

model.Hila.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data=Hila)
summary(model.Hila.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    79.81      24.24   3.292  0.00812 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value
## s(ANO)   1      1 0.524   0.486
##
## R-sq.(adj) =  -0.05   
## Scale est. = 6464.4    n = 12

model2.Hila.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Hila)
summary(model.Hila.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
## 
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    79.81      24.24   3.292  0.00812 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##       edf Ref.df     F p-value
## s(ANO)   1      1 0.524   0.486
##
## R-sq.(adj) =  -0.05   
## Scale est. = 6464.4    n = 12

model3.Hila.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Hila)
summary(model3.Hila.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -2640.453   3942.151   -0.67    0.518
## ANO             1.361      1.972    0.69    0.506
##
## R-sq.(adj) =  -0.05   
## Scale est. = 6464.4    n = 12

model4.Hila.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Hila)
summary(model4.Hila.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  4.6351319 53.1568437   0.087    0.932
## ANO         -0.0006125  0.0266273  -0.023    0.982
##
## R-sq.(adj) =  -0.104   
## Scale est. = 0.93485   n = 12

model5.Hila.p.lmer<-lmer(P~ANO + (1|Amb), data = Hila)
summary(model5.Hila.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Hila
## 
## REML criterion at convergence: 127.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.9278 -0.7008 -0.3777  0.7637  1.8183 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 2229     47.21   
## Residual             7032     83.86   
## Number of obs: 12, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -971.8879  4097.3124    9.6630  -0.237    0.817
## ANO            0.5201     2.0518    9.6424   0.253    0.805
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000

model6.Hila.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Hila)
summary(model6.Hila.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Hila
##
## REML criterion at convergence: 40.5
##
## Scaled residuals: 
##  Min      1Q  Median      3Q     Max 
## -1.2598 -0.6709 -0.2595  0.9204  1.4635 
##
## Random effects:
##  Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 1.698    1.303   
## Residual             1.015    1.007   
## Number of obs: 12, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) 16.128063  51.791309  9.672172   0.311    0.762
## ANO         -0.006403   0.025948  9.673559  -0.247    0.810
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



model.Hila.p.gam <- gam(P ~ s(ANO, k = 9), data = Hila, method = "REML", sp = 0.0001)
summary(model.Hila.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  79.81      32.44    2.46   0.0883 .
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.908  7.997 0.418   0.858
##
## R-sq.(adj) =  -0.709   Deviance explained =   52%
## -REML = 79.686  Scale est. = 12628     n = 12

gam.check(model.Hila.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-3.146371e-06,-3.146371e-06]
## (score 79.68625 & scale 12627.97).
## Hessian positive definite, eigenvalue range [5.000003,5.000003].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.91    1.56    0.93



model2.Hila.p.gam <- gam(log(P) ~ s(ANO, k = 9), data = Hila, method = "REML", sp = 0.0001)
summary(model2.Hila.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 9)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.7522     0.3856   9.732  0.00203 **
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 7.908  7.997 0.789   0.653
##
## R-sq.(adj) =  -0.17   Deviance explained = 67.1%
## -REML =  35.38  Scale est. = 1.784     n = 12

gam.check(model2.Hila.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.69578e-08,-1.69578e-08]
## (score 35.37993 & scale 1.783986).
## Hessian positive definite, eigenvalue range [5,5].
## Model rank =  9 / 9 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 8.00 7.91    1.69    0.97



##----------------------------------------------------------------------------##

dir()

Hila2 <- filter(Hila, Amb == "Pequeno")

Hila2$CP
Hila2$P
Hila2$ANO
Hila2$Longitude
Hila2$Latitude
Hila2$Amb

Hila2

summary(Hila2)
## catalognumber         CP              P              day            month            ANO          state          
## Min.   :  531   Min.   : 9.90   Min.   : 17.5   Min.   : 1.00   Min.   :1.000   Min.   :1984   Length:9          
## 1st Qu.: 9360   1st Qu.:13.00   1st Qu.: 32.0   1st Qu.:10.00   1st Qu.:3.000   1st Qu.:1989   Class :character  
## Median :17809   Median :13.50   Median : 40.7   Median :16.00   Median :4.000   Median :2006   Mode  :character  
## Mean   :13512   Mean   :16.57   Mean   :102.1   Mean   :15.67   Mean   :4.333   Mean   :2003                     
## 3rd Qu.:20620   3rd Qu.:21.50   3rd Qu.:176.0   3rd Qu.:23.00   3rd Qu.:6.000   3rd Qu.:2014                     
## Max.   :21436   Max.   :23.40   Max.   :253.5   Max.   :31.00   Max.   :7.000   Max.   :2017                     
## Amb              Longitude         Latitude     
## Length:9           Min.   :-50.23   Min.   :-23.40  
## Class :character   1st Qu.:-49.43   1st Qu.:-20.82  
## Mode  :character   Median :-49.28   Median :-20.42  
##                    Mean   :-48.97   Mean   :-21.07  
##                    3rd Qu.:-47.85   3rd Qu.:-20.39  
##                    Max.   :-47.59   Max.   :-19.99   



## S.hilarii_AmbPeque - teste de normalidade ---------------------------------##
shapiro.test(Hila2$CP) ## W = 0.85322, p-value = 0.08085
shapiro.test(Hila2$P)  ## W = 0.82697, p-value = 0.04129
shapiro.test(Hila2$ANO) ## W = 0.86021, p-value = 0.09638



## S.hilarii_AmbPeque - cor e cov --------------------------------------------##
cor(Hila2$ANO, Hila2$CP) ## -0.03723647
cor(Hila2$ANO, Hila2$P)  ## -0.03389677
cor(Hila2$ANO, Hila2$CP, method="spearman") ## 0.3277427
cor(Hila2$ANO, Hila2$P, method="spearman")  ## 0.05042195

cov(Hila2$ANO, Hila2$CP) ## -2.475
cov(Hila2$ANO, Hila2$P)  ## -39.3



## S.hilarii_AmbPeque - CP ---------------------------------------------------##

model.Hila2.cp.gamm<-gamm(CP~s(ANO), data=Hila2)
summary(model.Hila2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Hila2.cp.gamm<-gamm(log(CP)~s(ANO), data=Hila2)
summary(model2.Hila2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Hila2.cp.lm<-lm(CP~ANO, data = Hila2)
summary(model3.Hila2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Hila2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -6.623 -3.508 -2.863  4.657  7.037 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  45.72799  295.79893   0.155    0.882
## ANO          -0.01456    0.14768  -0.099    0.924
##
## Residual standard error: 5.446 on 7 degrees of freedom
## Multiple R-squared:  0.001387,	Adjusted R-squared:  -0.1413 
## F-statistic: 0.009719 on 1 and 7 DF,  p-value: 0.9242

model4.Hila2.cp.lm<-lm(log(CP)~ANO, data = Hila2)
summary(model4.Hila2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Hila2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.4679 -0.1942 -0.1442  0.2805  0.4059 
##
## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)
## (Intercept)  5.233361  18.216931   0.287    0.782
## ANO         -0.001233   0.009095  -0.136    0.896
##
## Residual standard error: 0.3354 on 7 degrees of freedom
## Multiple R-squared:  0.002618,	Adjusted R-squared:  -0.1399 
## F-statistic: 0.01837 on 1 and 7 DF,  p-value: 0.896



model.Hila2.cp.gam <- gam(CP ~ s(ANO, k = 6), data = Hila2, method = "REML", sp = 0.0001)
summary(model.Hila2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  16.567      1.416    11.7  0.00103 **
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.832  4.979 1.529   0.372
## 
## R-sq.(adj) =  0.306   Deviance explained = 72.5%
## -REML = 29.734  Scale est. = 18.035    n = 9

gam.check(model.Hila2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-9.467086e-09,-9.467086e-09]
## (score 29.73422 & scale 18.0346).
## Hessian positive definite, eigenvalue range [3.5,3.5].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.83    1.44    0.81



model2.Hila2.cp.gam <- gam(log(CP) ~ s(ANO, k = 6), data = Hila2, method = "REML", sp = 0.0001)
summary(model2.Hila2.cp.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.76410    0.08553   32.32 4.24e-05 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.832  4.979 1.617   0.354
##
## R-sq.(adj) =  0.333   Deviance explained = 73.6%
## -REML = 10.094  Scale est. = 0.065831  n = 9

gam.check(model2.Hila2.cp.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-4.925586e-09,-4.925586e-09]
## (score 10.09407 & scale 0.06583111).
## Hessian positive definite, eigenvalue range [3.5,3.5].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.83    1.44     0.8



## S.hilarii_AmbPeque - P ----------------------------------------------------##

model.Hila2.p.gamm<-gamm(P~s(ANO), data = Hila2)
summary(model.Hila2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Hila2.p.gamm<-gamm(log(P)~s(ANO), data=Hila2)
summary(model2.Hila2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
## A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Hila2.p.lm<-lm(P~ANO, data = Hila2)
summary(model3.Hila2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Hila2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -83.88 -66.84 -60.68  70.69 154.66 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  565.1242  5160.3097    0.11    0.916
## ANO           -0.2312     2.5762   -0.09    0.931
##
## Residual standard error: 95.01 on 7 degrees of freedom
## Multiple R-squared:  0.001149,	Adjusted R-squared:  -0.1415 
## F-statistic: 0.008052 on 1 and 7 DF,  p-value: 0.931

model4.Hila2.p.lm<-lm(log(P)~ANO, data = Hila2)
summary(model4.Hila2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Hila2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.3262 -0.6550 -0.4821  0.8776  1.4146 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 16.518153  58.613547   0.282    0.786
## ANO         -0.006146   0.029262  -0.210    0.840
##
## Residual standard error: 1.079 on 7 degrees of freedom
## Multiple R-squared:  0.006263,	Adjusted R-squared:  -0.1357 
## F-statistic: 0.04412 on 1 and 7 DF,  p-value: 0.8396



model.Hila2.p.gam <- gam(P ~ s(ANO, k = 6), data = Hila2, method = "REML", sp = 0.0001)
summary(model.Hila2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  102.08      29.82   3.423   0.0384 *
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.832  4.979 0.871   0.575
##
## R-sq.(adj) =  -0.0124   Deviance explained = 59.9%
## -REML = 50.948  Scale est. = 8005.2    n = 9

gam.check(model.Hila2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.274839e-06,-1.274839e-06]
## (score 50.94814 & scale 8005.241).
## Hessian positive definite, eigenvalue range [3.500001,3.500001].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.83    1.41    0.78



model2.Hila2.p.gam <- gam(log(P) ~ s(ANO, k = 6), data = Hila2, method = "REML", sp = 0.0001)
summary(model2.Hila2.p.gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  log(CP) ~ s(ANO, k = 6)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.207      0.305   13.79 0.000618 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO) 4.832  4.979 1.193   0.456
##
## R-sq.(adj) =  0.184   Deviance explained = 67.7%
## -REML = 18.991  Scale est. = 0.83715   n = 9

gam.check(model2.Hila2.p.gam)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.1897e-07,-1.1897e-07]
## (score 18.99087 & scale 0.8371457).
## Hessian positive definite, eigenvalue range [3.5,3.5].
## Model rank =  6 / 6 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 5.00 4.83    1.44    0.82


