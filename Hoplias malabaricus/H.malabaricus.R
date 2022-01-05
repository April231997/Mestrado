dir()

Mala <- read.table("H.malabaricus.txt", header=T)

str(Mala)
Mala$CP
Mala$P
Mala$ANO
Mala$Longitude
Mala$Latitude
Mala$Amb

Mala

summary(Mala)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  592   Min.   : 3.000   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 6062   1st Qu.: 5.700   1st Qu.:  3.00   1st Qu.:12.00   1st Qu.: 5.000   1st Qu.:2003  
## Median : 8826   Median : 8.500   Median : 12.00   Median :19.00   Median : 7.000   Median :2005  
## Mean   : 8988   Mean   : 9.787   Mean   : 38.09   Mean   :18.09   Mean   : 6.916   Mean   :2003  
## 3rd Qu.:10913   3rd Qu.:12.900   3rd Qu.: 44.00   3rd Qu.:26.00   3rd Qu.: 9.000   3rd Qu.:2007  
## Max.   :21330   Max.   :28.000   Max.   :543.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:453         Length:453         Min.   :-53.82   Min.   :-24.69  
## Class :character   Class :character   1st Qu.:-50.62   1st Qu.:-21.19  
## Mode  :character   Mode  :character   Median :-49.55   Median :-20.65  
##                                       Mean   :-49.65   Mean   :-20.88  
##                                       3rd Qu.:-49.27   3rd Qu.:-20.33  
##                                       Max.   :-42.77   Max.   :-16.62



## H.malabaricus_Geral - teste de normalidade --------------------------------##
shapiro.test(Mala$CP) ## W = 0.92324, p-value = 1.898e-14
shapiro.test(Mala$P)  ## W = 0.59142, p-value < 2.2e-16
shapiro.test(Mala$ANO) ## W = 0.80655, p-value < 2.2e-16



## H.malabaricus_Geral - cor e cov -------------------------------------------##
cor(Mala$ANO, Mala$CP) ## -0.006001329
cor(Mala$ANO, Mala$P)  ## -0.0105746
cor(Mala$ANO, Mala$CP, method="spearman") ## -0.03484835
cor(Mala$ANO, Mala$P, method="spearman")  ## -0.04315964

cov(Mala$ANO, Mala$CP) ## -0.2267699
cov(Mala$ANO, Mala$P)  ## -5.208702



## H.malabaricus_Geral - CP --------------------------------------------------##

model.Mala.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Mala)
summary(model.Mala.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   9.7868     0.2294   42.66   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##         edf Ref.df     F p-value  
## s(ANO) 3.588  3.588 3.187  0.0535 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0217   
## Scale est. = 23.791    n = 453

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
## (Intercept)  2.15703    0.03291   65.55   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 3.826  3.826 3.411  0.0321 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.025   
## Scale est. = 0.24408   n = 453

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
## (Intercept) 17.547599  60.893196   0.288    0.773
## ANO         -0.003874   0.030396  -0.127    0.899
##
##
## R-sq.(adj) =  -0.00218   
## Scale est. = 24.337    n = 453

model4.Mala.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Mala)
summary(model4.Mala.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) 0.4269362  6.2221395   0.069    0.945
## ANO         0.0008636  0.0031059   0.278    0.781
##
##
## R-sq.(adj) =  -0.00223   
## Scale est. = 0.25069   n = 453

model5.Mala.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Mala)
summary(model5.Mala.cp.lmer)
## boundary (singular) fit: see ?isSingular
## 
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Mala
##
## REML criterion at convergence: 2737.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.3714 -0.8237 -0.2535  0.6388  3.6725 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)  0.00    0.000   
## Residual             24.45    4.944   
## Number of obs: 453, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)  17.547602  60.893196 451.000000   0.288    0.773
## ANO          -0.003874   0.030396 451.000000  -0.127    0.899
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Mala.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Mala)
summary(model6.Mala.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Mala
##
## REML criterion at convergence: 674.5
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -2.05617 -0.84704 -0.02172  0.77497  2.44401 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.003079 0.05549 
## Residual             0.251230 0.50123 
## Number of obs: 453, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)  -0.146930   6.240849 444.177918  -0.024    0.981
## ANO           0.001150   0.003115 444.289007   0.369    0.712
##
## Correlation of Fixed Effects:
##  (Intr)
## ANO -1.000



## H.malabaricus_Geral - P ---------------------------------------------------##

model.Mala.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Mala)
summary(model.Mala.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   38.095      3.025    12.6   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.051   0.822
##
## R-sq.(adj) =  -0.00211   
## Scale est. = 4135.2    n = 453

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
## (Intercept)   2.5405     0.1157   21.96   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##         edf Ref.df     F p-value  
## s(ANO) 3.651  3.651 3.162  0.0543 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0214   
## Scale est. = 2.3669    n = 453

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
## (Intercept) 216.35467  793.74289   0.273    0.785
## ANO          -0.08898    0.39621  -0.225    0.822
##
##
## R-sq.(adj) =  -0.00211   
## Scale est. = 4135.2    n = 453

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
## (Intercept) -0.885898  19.376601  -0.046    0.964
## ANO          0.001710   0.009672   0.177    0.860
##
##
## R-sq.(adj) =  -0.00231   
## Scale est. = 2.423     n = 453

model5.Mala.p.lmer<-lmer(P~ANO + (1|Amb), data = Mala)
summary(model5.Mala.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Mala
##
## REML criterion at convergence: 5053.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.6023 -0.5367 -0.3992  0.0705  7.8435 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)    0      0.00   
## Residual             4154     64.45   
## Number of obs: 453, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) 216.35464  793.74289 451.00000   0.273    0.785
## ANO          -0.08898    0.39621 451.00000  -0.225    0.822
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Mala.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Mala)
summary(model6.Mala.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Mala
##
## REML criterion at convergence: 1697.8
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.72334 -0.84939 -0.01561  0.75382  2.48021 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.0401   0.2003  
## Residual             2.4283   1.5583  
## Number of obs: 453, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)  -2.468874  19.418284 447.402685  -0.127    0.899
## ANO           0.002501   0.009693 447.508704   0.258    0.797
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



##----------------------------------------------------------------------------##

dir()

Mala2 <- read.table("H.malabaricus_AmbGran.txt", header=T)
str(Mala2)
Mala2$CP
Mala2$P
Mala2$ANO
Mala2$Longitude
Mala2$Latitude
Mala2$Amb

Mala2

summary(Mala2)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  592   Min.   : 3.200   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 5923   1st Qu.: 6.600   1st Qu.:  6.00   1st Qu.:10.50   1st Qu.: 4.000   1st Qu.:2003  
## Median : 7264   Median : 9.100   Median : 16.00   Median :16.00   Median : 7.000   Median :2004  
## Mean   : 8238   Mean   : 9.967   Mean   : 36.01   Mean   :16.77   Mean   : 6.466   Mean   :2002  
## 3rd Qu.:10322   3rd Qu.:12.550   3rd Qu.: 41.50   3rd Qu.:24.00   3rd Qu.: 8.000   3rd Qu.:2006  
## Max.   :19286   Max.   :26.000   Max.   :454.00   Max.   :31.00   Max.   :12.000   Max.   :2013  
## state               Amb              Longitude         Latitude     
## Length:223         Length:223         Min.   :-53.82   Min.   :-24.61  
## Class :character   Class :character   1st Qu.:-50.41   1st Qu.:-21.18  
## Mode  :character   Mode  :character   Median :-49.56   Median :-20.69  
##                                       Mean   :-49.73   Mean   :-20.78  
##                                       3rd Qu.:-49.35   3rd Qu.:-20.44  
##                                       Max.   :-45.74   Max.   :-16.62



## H.malabaricus_AmbGran - teste de normalidade ------------------------------##
shapiro.test(Mala2$CP) ## W = 0.94134, p-value = 8.104e-08
shapiro.test(Mala2$P)  ## W = 0.59897, p-value < 2.2e-16
shapiro.test(Mala2$ANO) ## W = 0.77991, p-value < 2.2e-16



## H.malabaricus_AmbGran - cor e cov -----------------------------------------##
cor(Mala2$ANO, Mala2$CP) ## 0.04571224
cor(Mala2$ANO, Mala2$P)  ## -0.008482954
cor(Mala2$ANO, Mala2$CP, method="spearman") ## 0.003618576
cor(Mala2$ANO, Mala2$P, method="spearman")  ## 0.01169663

cov(Mala2$ANO, Mala2$CP) ## 1.569066
cov(Mala2$ANO, Mala2$P)  ## -3.72979



## H.malabaricus_AmbGran - CP ------------------------------------------------##

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
## (Intercept)   9.9673     0.2984   33.41   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.465   0.496
##
## R-sq.(adj) =  -0.00243   
## Scale est. = 19.764    n = 223

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
## (Intercept)  2.20143    0.02991   73.61   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.952    0.33
##
## R-sq.(adj) =  -0.000237   
## Scale est. = 0.19854   n = 223

model3.Mala2.cp.lm<-lm(CP~ANO, data = Mala2)
summary(model3.Mala2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Mala2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -6.8731 -3.3907 -0.7936  2.8551 16.0064 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -43.07631   77.97460  -0.552    0.581
## ANO           0.02650    0.03895   0.680    0.497
##
## Residual standard error: 4.466 on 221 degrees of freedom
## Multiple R-squared:  0.00209,	Adjusted R-squared:  -0.002426 
## F-statistic: 0.4628 on 1 and 221 DF,  p-value: 0.497

model4.Mala2.cp.lm<-lm(log(CP)~ANO, data = Mala2)
summary(model4.Mala2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Mala2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.05346 -0.31817  0.01399  0.35009  1.05289 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -5.405353   7.815195  -0.692    0.490
## ANO          0.003800   0.003904   0.973    0.331
##
## Residual standard error: 0.4476 on 221 degrees of freedom
## Multiple R-squared:  0.004269,	Adjusted R-squared:  -0.000237 
## F-statistic: 0.9474 on 1 and 221 DF,  p-value: 0.3314



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
## (Intercept)   36.013      3.826   9.413   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.016     0.9
##
## R-sq.(adj) =  -0.00445   
## Scale est. = 3249.5    n = 223

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
## (Intercept)  2.69702    0.09226   29.23   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value
## s(ANO)   1      1 0.852   0.357
##
## R-sq.(adj) =  -0.000686   
## Scale est. = 1.8896    n = 223

model3.Mala2.p.lm<-lm(P~ANO, data = Mala2)
summary(model3.Mala2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Mala2)
##
## Residuals:
##   Min     1Q Median     3Q    Max 
## -36.15 -29.95 -19.95   5.58 418.05 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 162.10208  999.81636   0.162    0.871
## ANO          -0.06298    0.49940  -0.126    0.900
##
## Residual standard error: 57.26 on 221 degrees of freedom
## Multiple R-squared:  7.196e-05,	Adjusted R-squared:  -0.004453 
## F-statistic: 0.0159 on 1 and 221 DF,  p-value: 0.8998

model4.Mala2.p.lm<-lm(log(P)~ANO, data = Mala2)
summary(model4.Mala2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Mala2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.7968 -0.9496  0.0645  1.0307  3.4100 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -19.50402   24.11012  -0.809    0.419
## ANO           0.01109    0.01204   0.921    0.358
##
## Residual standard error: 1.381 on 221 degrees of freedom
## Multiple R-squared:  0.003822,	Adjusted R-squared:  -0.0006855 
## F-statistic: 0.8479 on 1 and 221 DF,  p-value: 0.3581



##----------------------------------------------------------------------------##

dir()

Mala3 <- read.table("H.malabaricus_AmbPeque.txt", header=T)
str(Mala3)
Mala3$CP
Mala3$P
Mala3$ANO
Mala3$Longitude
Mala3$Latitude
Mala3$Amb

Mala3

summary(Mala3)
## catalognumber         CP               P               day            month             ANO      
## Min.   :  949   Min.   : 3.000   Min.   :  1.00   Min.   : 1.00   Min.   : 1.000   Min.   :1980  
## 1st Qu.: 7984   1st Qu.: 5.200   1st Qu.:  2.00   1st Qu.:14.00   1st Qu.: 6.000   1st Qu.:2005  
## Median : 9006   Median : 7.850   Median :  9.00   Median :23.00   Median : 8.000   Median :2006  
## Mean   : 9715   Mean   : 9.612   Mean   : 40.11   Mean   :19.38   Mean   : 7.352   Mean   :2005  
## 3rd Qu.:12803   3rd Qu.:13.000   3rd Qu.: 44.75   3rd Qu.:26.00   3rd Qu.: 9.000   3rd Qu.:2010  
## Max.   :21330   Max.   :28.000   Max.   :543.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:230         Length:230         Min.   :-53.78   Min.   :-24.69  
## Class :character   Class :character   1st Qu.:-51.06   1st Qu.:-21.29  
## Mode  :character   Mode  :character   Median :-49.52   Median :-20.60  
##                                       Mean   :-49.56   Mean   :-20.98  
##                                       3rd Qu.:-49.22   3rd Qu.:-20.28  
##                                       Max.   :-42.77   Max.   :-16.62



## H.malabaricus_AmbPeque - teste de normalidade -----------------------------##
shapiro.test(Mala3$CP) ##  W = 0.89518, p-value = 1.427e-11
shapiro.test(Mala3$P)  ## W = 0.58732, p-value < 2.2e-16
shapiro.test(Mala3$ANO) ## W = 0.78161, p-value < 2.2e-16



## H.malabaricus_AmbPeque - cor e cov ----------------------------------------##
cor(Mala3$ANO, Mala3$CP) ## -0.03799798
cor(Mala3$ANO, Mala3$P)  ## -0.02286134
cor(Mala3$ANO, Mala3$CP, method="spearman") ## 0.01605942
cor(Mala3$ANO, Mala3$P, method="spearman")  ## 0.00228451

cov(Mala3$ANO, Mala3$CP) ## -1.508641
cov(Mala3$ANO, Mala3$P)  ## -11.97015



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
## (Intercept)   9.6117     0.3419   28.11   <2e-16 ***
##   ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##  edf Ref.df     F p-value   
## s(ANO) 3.696  3.696 4.456  0.0061 **
##   ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0647   
## Scale est. = 26.772    n = 230

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
## (Intercept)  2.11328    0.03512   60.18   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df    F p-value  
## s(ANO) 3.587  3.587 3.38  0.0216 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0522   
## Scale est. = 0.28239   n = 230

model3.Mala3.cp.lm<-lm(CP~ANO, data = Mala3)
summary(model3.Mala3.cp.lm)


model4.Mala3.cp.lm<-lm(log(CP)~ANO, data = Mala3)
summary(model4.Mala3.cp.lm)
 


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
## (Intercept)   40.113      4.615   8.692 7.21e-16 ***
##   ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##       edf Ref.df     F p-value
## s(ANO) 2.134  2.134 2.264   0.164
##
## R-sq.(adj) =  0.0211   
## Scale est. = 4876.8    n = 230

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
## (Intercept)   2.3858     0.1097   21.74   <2e-16 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 3.697  3.697 3.126  0.0279 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0516   
## Scale est. = 2.7581    n = 230

model3.Mala3.p.lm<-lm(P~ANO, data = Mala3)
summary(model3.Mala3.p.lm)
## 

model4.Mala3.p.lm<-lm(log(P)~ANO, data = Mala3)
summary(model4.Mala3.p.lm)
## 