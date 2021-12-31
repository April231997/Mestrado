dir()

Hila <- read.table("S.hilarii.txt", header=T)
str(Hila)
Hila$CP
Hila$P
Hila$ANO
Hila$Longitude
Hila$Latitude
Hila$Amb

Hila

summary(Hila)
## catalognumber         CP              P              day            month           ANO      
## Min.   :  531   Min.   : 6.40   Min.   :  5.0   Min.   : 1.00   Min.   :1.00   Min.   :1980  
## 1st Qu.: 1678   1st Qu.:10.65   1st Qu.: 19.5   1st Qu.: 9.25   1st Qu.:2.75   1st Qu.:1989  
## Median : 9415   Median :13.20   Median : 32.0   Median :16.00   Median :3.00   Median :2006  
## Mean   :10748   Mean   :14.46   Mean   : 77.6   Mean   :16.90   Mean   :3.60   Mean   :2000  
## 3rd Qu.:18807   3rd Qu.:20.30   3rd Qu.:154.2   3rd Qu.:29.00   3rd Qu.:4.00   3rd Qu.:2009  
## Max.   :21436   Max.   :24.30   Max.   :289.0   Max.   :31.00   Max.   :7.00   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:20          Length:20          Min.   :-50.23   Min.   :-23.40  
## Class :character   Class :character   1st Qu.:-49.42   1st Qu.:-21.19  
## Mode  :character   Mode  :character   Median :-49.32   Median :-20.46  
##                                       Mean   :-49.06   Mean   :-20.97  
##                                       3rd Qu.:-48.91   3rd Qu.:-20.39  
##                                       Max.   :-47.59   Max.   :-19.99  



## S.hilarii_Geral - teste de normalidade ------------------------------------##
shapiro.test(Hila$CP) ## W = 0.8986, p-value = 0.03882
shapiro.test(Hila$P)  ## W = 0.76286, p-value = 0.0002538
shapiro.test(Hila$ANO) ## W = 0.88312, p-value = 0.02014



## S.hilarii_Geral - cor e cov -----------------------------------------------##
cor(Hila$ANO, Hila$CP) ## 0.2751821
cor(Hila$ANO, Hila$P)  ## 0.2206498
cor(Hila$ANO, Hila$CP, method="spearman") ## 0.3332311
cor(Hila$ANO, Hila$P, method="spearman")  ## 0.2938945

cov(Hila$ANO, Hila$CP) ## 19.5
cov(Hila$ANO, Hila$P)  ## 251.6842



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
## (Intercept)   12.619      3.062   4.121 0.000736 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 2.279  2.279 2.191  0.0912 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  -0.00327   
## Scale est. = 14.093    n = 20

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
## (Intercept)   2.4547     0.2404   10.21 1.39e-08 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 2.342  2.342 3.181  0.0419 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  -0.0832   
## Scale est. = 0.055254  n = 20

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
## (Intercept)  -4.13191  196.18092  -0.021    0.983
## ANO           0.00867    0.09825   0.088    0.931
##
## R-sq.(adj) =  -0.0436   
## Scale est. = 20.741    n = 20

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
## (Intercept)  5.311743  13.014295   0.408    0.688
## ANO         -0.001411   0.006519  -0.216    0.831
##
## R-sq.(adj) =  -0.089   
## Scale est. = 0.086752  n = 20

model5.Hila.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Hila)
summary(model5.Hila.cp.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Hila
##
## REML criterion at convergence: 119.3
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.3702 -0.6557 -0.3084  0.8549  1.8476 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 25.04    5.004   
## Residual             21.55    4.643   
## Number of obs: 20, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  56.32369  197.33316  17.99014   0.285    0.779
## ANO          -0.02174    0.09885  17.98963  -0.220    0.828
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
## REML criterion at convergence: 21.2
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.5327 -0.6040 -0.2223  0.7893  1.6526 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.16112  0.4014  
## Residual             0.09086  0.3014  
## Number of obs: 20, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept)  8.185628  12.931989 17.934051   0.633    0.535
## ANO         -0.002856   0.006478 17.932261  -0.441    0.665
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



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
## (Intercept)    77.60      18.84   4.118 0.000645 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##        edf Ref.df     F p-value
## s(ANO)   1      1 0.972   0.337
##
## R-sq.(adj) =  -0.00416   
## Scale est. = 6745.5    n = 20

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
## (Intercept)    77.60      18.84   4.118 0.000645 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##       edf Ref.df     F p-value
## s(ANO)   1      1 0.972   0.337
##
## R-sq.(adj) =  -0.00416   
## Scale est. = 6745.5    n = 20

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
## (Intercept) -2810.081   3008.709  -0.934    0.363
## ANO             1.444      1.504   0.960    0.350
##
## R-sq.(adj) =  -0.00416   
## Scale est. = 6745.5    n = 20

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
## (Intercept) 17.521626  41.097117   0.426    0.675
## ANO         -0.007074   0.020585  -0.344    0.735
##
## R-sq.(adj) =  -0.107   
## Scale est. = 0.8661    n = 20

model5.Hila.p.lmer<-lmer(P~ANO + (1|Amb), data = Hila)
summary(model5.Hila.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Hila
## 
## REML criterion at convergence: 222
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.9363 -0.7682 -0.2757  0.7348  2.3458 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 2806     52.97   
## Residual             6761     82.23   
## Number of obs: 20, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -323.4117  3366.3102   15.7051  -0.096    0.925
## ANO            0.1929     1.6859   15.6548   0.114    0.910
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
## REML criterion at convergence: 62.6
##
## Scaled residuals: 
##  Min      1Q  Median      3Q     Max 
## -1.3181 -0.6662 -0.1861  0.8145  1.7918 
##
## Random effects:
##  Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 1.588    1.2602  
## Residual             0.907    0.9523  
## Number of obs: 20, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) 26.67900   40.84803 17.93853   0.653    0.522
## ANO         -0.01168    0.02046 17.93690  -0.571    0.575
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



##----------------------------------------------------------------------------##

dir()

Hila2 <- read.table("S.hilarii_AmbPeque.txt", header=T)
str(Hila2)
Hila2$CP
Hila2$P
Hila2$ANO
Hila2$Longitude
Hila2$Latitude
Hila2$Amb

Hila2

summary(Hila2)
## catalognumber         CP              P              day           month        ANO      
## Min.   :  531   Min.   : 9.60   Min.   : 17.0   Min.   : 1.0   Min.   :1   Min.   :1984  
## 1st Qu.: 9360   1st Qu.:12.70   1st Qu.: 29.0   1st Qu.:10.0   1st Qu.:3   1st Qu.:1998  
## Median :17809   Median :14.20   Median : 48.0   Median :16.0   Median :4   Median :2006  
## Mean   :14052   Mean   :16.18   Mean   : 98.6   Mean   :16.6   Mean   :4   Mean   :2005  
## 3rd Qu.:20980   3rd Qu.:21.35   3rd Qu.:168.0   3rd Qu.:26.0   3rd Qu.:5   3rd Qu.:2016  
## Max.   :21436   Max.   :24.30   Max.   :289.0   Max.   :31.0   Max.   :7   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:15          Length:15          Min.   :-50.23   Min.   :-23.40  
## Class :character   Class :character   1st Qu.:-49.40   1st Qu.:-20.66  
## Mode  :character   Mode  :character   Median :-49.28   Median :-20.41  
##                                       Mean   :-48.95   Mean   :-20.97  
##                                       3rd Qu.:-47.85   3rd Qu.:-20.38  
##                                       Max.   :-47.59   Max.   :-19.99  



## S.hilarii_AmbPeque - teste de normalidade ---------------------------------##
shapiro.test(Hila2$CP) ## W = 0.88169, p-value = 0.05028
shapiro.test(Hila2$P)  ## W = 0.81974, p-value = 0.006664
shapiro.test(Hila2$ANO) ## W = 0.8383, p-value = 0.01191



## S.hilarii_AmbPeque - cor e cov --------------------------------------------##
cor(Hila2$ANO, Hila2$CP) ## -0.06803891
cor(Hila2$ANO, Hila2$P)  ## -0.05038113
cor(Hila2$ANO, Hila2$CP, method="spearman") ## 0.2002648
cor(Hila2$ANO, Hila2$P, method="spearman")  ## 0.119424

cov(Hila2$ANO, Hila2$CP) ## -4.038571
cov(Hila2$ANO, Hila2$P)  ## -53.77143



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
## -6.537 -3.700 -1.937  4.644  8.483 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  74.60881  237.62791   0.314    0.759
## ANO          -0.02915    0.11854  -0.246    0.810
##
## Residual standard error: 5.221 on 13 degrees of freedom
## Multiple R-squared:  0.004629,	Adjusted R-squared:  -0.07194 
## F-statistic: 0.06046 on 1 and 13 DF,  p-value: 0.8096

model4.Hila2.cp.lm<-lm(log(CP)~ANO, data = Hila2)
summary(model4.Hila2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Hila2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.47305 -0.21332 -0.08157  0.28325  0.47991 
##
## Coefficients:
##  Estimate Std. Error t value Pr(>|t|)
## (Intercept)  7.155007  14.770849   0.484    0.636
## ANO         -0.002203   0.007369  -0.299    0.770
##
## Residual standard error: 0.3245 on 13 degrees of freedom
## Multiple R-squared:  0.006832,	Adjusted R-squared:  -0.06957 
## F-statistic: 0.08942 on 1 and 13 DF,  p-value: 0.7696



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
## -81.03 -69.40 -50.03  67.25 195.24 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  876.5485  4277.2672   0.205    0.841
## ANO           -0.3881     2.1338  -0.182    0.858
##
## Residual standard error: 93.98 on 13 degrees of freedom
## Multiple R-squared:  0.002538,	Adjusted R-squared:  -0.07419 
## F-statistic: 0.03308 on 1 and 13 DF,  p-value: 0.8585

model4.Hila2.p.lm<-lm(log(P)~ANO, data = Hila2)
summary(model4.Hila2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Hila2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.2895 -0.7986 -0.2515  0.9220  1.6558 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) 24.55925   47.48367   0.517    0.614
## ANO         -0.01019    0.02369  -0.430    0.674
##
## Residual standard error: 1.043 on 13 degrees of freedom
## Multiple R-squared:  0.01403,	Adjusted R-squared:  -0.06181 
## F-statistic: 0.185 on 1 and 13 DF,  p-value: 0.6742



