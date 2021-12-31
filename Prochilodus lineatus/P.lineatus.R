dir()

Line <- read.table("P.lineatus.txt", header=T)
str(Line)
Line$CP
Line$P
Line$ANO
Line$Longitude
Line$Latitude
Line$Amb

Line

summary(Line)
## catalognumber         CP              P               day            month             ANO      
## Min.   : 7686   Min.   : 3.60   Min.   :  1.00   Min.   : 3.00   Min.   : 1.000   Min.   :2005  
## 1st Qu.: 9426   1st Qu.: 9.30   1st Qu.: 24.75   1st Qu.:21.00   1st Qu.: 2.000   1st Qu.:2005  
## Median : 9696   Median : 9.90   Median : 29.00   Median :21.00   Median : 3.000   Median :2005  
## Mean   :10956   Mean   :10.44   Mean   : 38.79   Mean   :20.85   Mean   : 4.787   Mean   :2006  
## 3rd Qu.:10056   3rd Qu.:10.80   3rd Qu.: 37.00   3rd Qu.:23.00   3rd Qu.: 9.000   3rd Qu.:2006  
## Max.   :21385   Max.   :21.50   Max.   :291.00   Max.   :31.00   Max.   :12.000   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:136         Length:136         Min.   :-50.11   Min.   :-23.33  
## Class :character   Class :character   1st Qu.:-49.27   1st Qu.:-20.42  
## Mode  :character   Mode  :character   Median :-49.27   Median :-20.42  
##                                       Mean   :-49.18   Mean   :-20.54  
##                                       3rd Qu.:-49.27   3rd Qu.:-20.42  
##                                       Max.   :-47.77   Max.   :-17.21



## P.lineatus_Geral - teste de normalidade -----------------------------------##
shapiro.test(Line$CP) ## W = 0.76139, p-value = 1.361e-13
shapiro.test(Line$P)  ## W = 0.54058, p-value < 2.2e-16
shapiro.test(Line$ANO) ## W = 0.43596, p-value < 2.2e-16



## P.lineatus_Geral - cor e cov ----------------------------------------------##
cor(Line$ANO, Line$CP) ## 0.4021166
cor(Line$ANO, Line$P)  ## 0.4128143
cor(Line$ANO, Line$CP, method="spearman") ## 0.0004002544
cor(Line$ANO, Line$P, method="spearman")  ## -0.02275495

cov(Line$ANO, Line$CP) ## 2.556209
cov(Line$ANO, Line$P)  ## 38.89673



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
## (Intercept) -723.46986  144.35763  -5.012 1.67e-06 ***
##   ANO            0.36584    0.07196   5.084 1.22e-06 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##
## R-sq.(adj) =  0.155   
## Scale est. = 4.8127    n = 136

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
## (Intercept) -48.787738  12.986979  -3.757 0.000256 ***
##   ANO           0.025478   0.006474   3.936 0.000133 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0969   
## Scale est. = 0.038951  n = 136

model5.Line.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Line)
summary(model5.Line.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Line
##
## REML criterion at convergence: 604.6
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -3.2488 -0.3839 -0.1124  0.2496  4.8504 
##
## Random effects:
##   Groups   Name        Variance  Std.Dev. 
## Amb      (Intercept) 1.428e-11 3.779e-06
## Residual             4.884e+00 2.210e+00
## Number of obs: 136, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept) -723.46986  144.35763  134.00000  -5.012 1.67e-06 ***
##   ANO            0.36584    0.07196  134.00000   5.084 1.22e-06 ***
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
## REML criterion at convergence: -40.9
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -5.3607 -0.3854 -0.0676  0.3301  3.6276 
##
## Random effects:
##   Groups   Name        Variance  Std.Dev. 
## Amb      (Intercept) 7.886e-15 8.880e-08
## Residual             3.953e-02 1.988e-01
## Number of obs: 136, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept) -48.787738  12.986979 134.000000  -3.757 0.000256 ***
##   ANO           0.025478   0.006474 134.000000   3.936 0.000133 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



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
## (Intercept) -11128.813   2128.549  -5.228 6.41e-07 ***
##   ANO              5.567      1.061   5.247 5.90e-07 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.164   
## Scale est. = 1046.3    n = 136

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
## (Intercept) -123.19282   39.42154  -3.125  0.00218 **
##   ANO            0.06313    0.01965   3.212  0.00165 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.0646   
## Scale est. = 0.3589    n = 136

model5.Line.p.lmer<-lmer(P~ANO + (1|Amb), data = Line)
summary(model5.Line.p.lmer)
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Line
##
## REML criterion at convergence: 1325.7
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -2.5315 -0.3166 -0.1460  0.0689  7.5593 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept)    4.652  2.157  
## Residual             1061.259 32.577  
## Number of obs: 136, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept) -11113.338   2128.491    133.991  -5.221 6.62e-07 ***
##   ANO              5.559      1.061    133.981   5.240 6.09e-07 ***
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
## REML criterion at convergence: 256
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -5.5365 -0.3566 -0.0410  0.3708  3.9259 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.0319   0.1786  
## Residual             0.3595   0.5996  
## Number of obs: 136, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)   
## (Intercept) -126.79906   39.25533  133.58529  -3.230  0.00156 **
##   ANO            0.06488    0.01957  133.56098   3.316  0.00118 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000



##----------------------------------------------------------------------------##

dir()

Line2 <- read.table("P.lineatus_AmbPeque.txt", header=T)
str(Line2)
Line2$CP
Line2$P
Line2$ANO
Line2$Longitude
Line2$Latitude
Line2$Amb

Line2

summary(Line2)
## catalognumber         CP              P               day            month            ANO      
## Min.   : 7686   Min.   : 6.40   Min.   :  8.00   Min.   : 3.00   Min.   : 1.00   Min.   :2005  
## 1st Qu.: 9426   1st Qu.: 9.30   1st Qu.: 25.00   1st Qu.:21.00   1st Qu.: 2.00   1st Qu.:2005  
## Median : 9503   Median : 9.90   Median : 29.00   Median :21.00   Median : 3.00   Median :2005  
## Mean   :10715   Mean   :10.41   Mean   : 37.57   Mean   :20.94   Mean   : 4.48   Mean   :2006  
## 3rd Qu.:10056   3rd Qu.:10.70   3rd Qu.: 37.00   3rd Qu.:23.00   3rd Qu.: 9.00   3rd Qu.:2006  
## Max.   :21385   Max.   :18.50   Max.   :186.00   Max.   :27.00   Max.   :12.00   Max.   :2017  
## 
## state               Amb              Longitude         Latitude     
## Length:125         Length:125         Min.   :-50.11   Min.   :-23.33  
## Class :character   Class :character   1st Qu.:-49.27   1st Qu.:-20.42  
## Mode  :character   Mode  :character   Median :-49.27   Median :-20.42  
## Mean   :-49.20   Mean   :-20.51  
## 3rd Qu.:-49.27   3rd Qu.:-20.42  
## Max.   :-47.77   Max.   :-17.21



## P.lineatus_AmbPeque - teste de normalidade --------------------------------##
shapiro.test(Line2$CP) ## W = 0.73075, p-value = 7.612e-14
shapiro.test(Line2$P)  ## W = 0.58839, p-value < 2.2e-16
shapiro.test(Line2$ANO) ## W = 0.39646, p-value < 2.2e-16



## P.lineatus_AmbPeque - cor e cov -------------------------------------------##
cor(Line2$ANO, Line2$CP) ## 0.5068479
cor(Line2$ANO, Line2$P)  ## 0.5482054
cor(Line2$ANO, Line2$CP, method="spearman") ## 0.0537058
cor(Line2$ANO, Line2$P, method="spearman")  ## 0.02074117

cov(Line2$ANO, Line2$CP) ## 2.925735
cov(Line2$ANO, Line2$P)  ## 42.15671



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
## -6.3148 -0.7956 -0.1956  0.4044  8.2044 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -811.92863  126.10879  -6.438 2.45e-09 ***
##   ANO            0.40994    0.06287   6.521 1.63e-09 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.87 on 123 degrees of freedom
## Multiple R-squared:  0.2569,	Adjusted R-squared:  0.2509 
## F-statistic: 42.52 on 1 and 123 DF,  p-value: 1.627e-09

model4.Line2.cp.lm<-lm(log(CP)~ANO, data = Line2)
summary(model4.Line2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Line2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.50291 -0.07589 -0.01271  0.05449  0.60633 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -57.784511  10.810041  -5.345 4.22e-07 ***
##   ANO           0.029965   0.005389   5.561 1.59e-07 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.1603 on 123 degrees of freedom
## Multiple R-squared:  0.2009,	Adjusted R-squared:  0.1944 
## F-statistic: 30.92 on 1 and 123 DF,  p-value: 1.593e-07



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
## -85.495  -9.521  -3.614   3.386 123.386 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.181e+04  1.630e+03  -7.247 4.12e-11 ***
##   ANO          5.907e+00  8.125e-01   7.270 3.65e-11 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 24.17 on 123 degrees of freedom
## Multiple R-squared:  0.3005,	Adjusted R-squared:  0.2948 
## F-statistic: 52.85 on 1 and 123 DF,  p-value: 3.654e-11

model4.Line2.p.lm<-lm(log(P)~ANO, data = Line2)
summary(model4.Line2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Line2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -1.47298 -0.21130 -0.02206  0.17183  1.65407 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -149.79862   31.45919  -4.762 5.30e-06 ***
##   ANO            0.07640    0.01568   4.872 3.33e-06 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.4665 on 123 degrees of freedom
## Multiple R-squared:  0.1618,	Adjusted R-squared:  0.1549 
## F-statistic: 23.74 on 1 and 123 DF,  p-value: 3.331e-06


