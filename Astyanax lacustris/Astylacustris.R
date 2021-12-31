dir()

Asty <- read.table("Astylacustris.txt", header=T)
str(Asty)
Asty$CP
Asty$P
Asty$ANO
Asty$Longitude
Asty$Latitude
Asty$Amb

Asty

summary(Asty)
## catalognumber         CP              P               day            month             ANO      
## Min.   :21017   Min.   :2.700   Min.   : 1.000   Min.   : 5.00   Min.   : 1.000   Min.   :2012  
## 1st Qu.:21321   1st Qu.:3.550   1st Qu.: 1.000   1st Qu.:10.00   1st Qu.: 2.000   1st Qu.:2016  
## Median :22755   Median :4.500   Median : 2.000   Median :11.00   Median : 3.000   Median :2017  
## Mean   :22338   Mean   :4.587   Mean   : 3.522   Mean   :14.73   Mean   : 4.881   Mean   :2016  
### rd Qu.:22821   3rd Qu.:5.300   3rd Qu.: 4.500   3rd Qu.:19.50   3rd Qu.: 7.000   3rd Qu.:2017  
## Max.   :22865   Max.   :8.300   Max.   :15.000   Max.   :28.00   Max.   :12.000   Max.   :2019  
## 
## state               Amb              Longitude         Latitude     
## Length:67          Length:67          Min.   :-51.48   Min.   :-23.49  
## Class :character   Class :character   1st Qu.:-48.58   1st Qu.:-23.46  
## Mode  :character   Mode  :character   Median :-48.41   Median :-23.34  
##                                       Mean   :-48.84   Mean   :-22.23  
##                                       3rd Qu.:-48.16   3rd Qu.:-20.42  
##                                       Max.   :-47.77   Max.   :-18.92 

## Astyanax lacustris_Geral - teste de normalidade ---------------------------##
shapiro.test(Asty$CP) ## W = 0.94443, p-value = 0.004788  
shapiro.test(Asty$P)  ## W = 0.80069, p-value = 4.134e-08  
shapiro.test(Asty$ANO) ## W = 0.71855, p-value = 4.99e-10  



## Astyanax lacustris_Geral - cor e cov --------------------------------------##
cor(Asty$ANO, Asty$CP) ## 0.0860064
cor(Asty$ANO, Asty$P)  ## 0.08535767
cor(Asty$ANO, Asty$CP, method="spearman") ## 0.120057
cor(Asty$ANO, Asty$P, method="spearman")  ## 0.09106156

cov(Asty$ANO, Asty$CP) ## 0.2296246
cov(Asty$ANO, Asty$P)  ## 0.5162822



## Astyanax lacustris_Geral - CP ---------------------------------------------##

model.Asty.cp.gamm<-gamm(CP~s(ANO), random=list(Amb=~1), data = Asty)
summary(model.Asty.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Asty.cp.gamm<-gamm(log(CP)~s(ANO), random=list(Amb=~1), data=Asty)
summary(model2.Asty.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Asty.cp.gamm<-gamm(CP~ANO, random=list(Amb=~1), data=Asty)
summary(model3.Asty.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -107.91669  161.64625  -0.668    0.507
## ANO            0.05580    0.08017   0.696    0.489
##
##
## R-sq.(adj) =  -0.00787   
## Scale est. = 1.6936    n = 67

model4.Asty.cp.gamm<-gamm(log(CP)~ANO, random=list(Amb=~1), data=Asty)
summary(model4.Asty.cp.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(CP) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -16.25805   34.19506  -0.475    0.636
## ANO           0.00880    0.01696   0.519    0.606
##
##
## R-sq.(adj) =  -0.0112   
## Scale est. = 0.07579   n = 67

model5.Asty.cp.lmer<-lmer(CP~ANO + (1|Amb), data = Asty)
summary(model5.Asty.cp.lmer)
## boundary (singular) fit: see ?isSingular
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: CP ~ ANO + (1 | Amb)
## Data: Asty
##
## REML criterion at convergence: 230.5
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.45811 -0.78133 -0.09577  0.52644  2.78027 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.000    0.000   
## Residual             1.746    1.321   
## Number of obs: 67, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error         df t value Pr(>|t|)
## (Intercept) -107.91669  161.64625   65.00000  -0.668    0.507
## ANO            0.05580    0.08017   65.00000   0.696    0.489
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Asty.cp.lmer<-lmer(log(CP)~ANO + (1|Amb), data = Asty)
summary(model6.Asty.cp.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(CP) ~ ANO + (1 | Amb)
## Data: Asty
##
## REML criterion at convergence: 28.6
##
## Scaled residuals: 
##   Min       1Q   Median       3Q      Max 
## -1.78011 -0.77439  0.04751  0.64869  2.23775 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.00000  0.0000  
## Residual             0.07812  0.2795  
## Number of obs: 67, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -16.25805   34.19506  65.00000  -0.475    0.636
## ANO           0.00880    0.01696  65.00000   0.519    0.606
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



## Astyanax lacustris_Geral - P ---------------------------------------------##

model.Asty.p.gamm<-gamm(P~s(ANO), random=list(Amb=~1), data = Asty)
summary(model.Asty.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Asty.p.gamm<-gamm(log(P)~s(ANO), random=list(Amb=~1), data=Asty)
summary(model2.Asty.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom 

model3.Asty.p.gamm<-gamm(P~ANO, random=list(Amb=~1), data=Asty)
summary(model3.Asty.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -249.4271   366.2241  -0.681    0.498
## ANO            0.1255     0.1816   0.691    0.492
##
##
## R-sq.(adj) =  -0.00799   
## Scale est. = 8.6932    n = 67

model4.Asty.p.gamm<-gamm(log(P)~ANO, random=list(Amb=~1), data=Asty)
summary(model4.Asty.p.gamm$gam)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   log(P) ~ ANO
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -39.48203   95.85594  -0.412    0.682
## ANO           0.02005    0.04754   0.422    0.675
##
##
## R-sq.(adj) =  -0.0126   
## Scale est. = 0.59555   n = 67

model5.Asty.p.lmer<-lmer(P~ANO + (1|Amb), data = Asty)
summary(model5.Asty.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: P ~ ANO + (1 | Amb)
## Data: Asty
##
## REML criterion at convergence: 336.8
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -0.9565 -0.7469 -0.3291  0.2966  3.8042 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.000    0.000   
## Residual             8.961    2.993   
## Number of obs: 67, groups:  Amb, 2
## 
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -249.4271   366.2241   65.0000  -0.681    0.498
## ANO            0.1255     0.1816   65.0000   0.691    0.492
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular

model6.Asty.p.lmer<-lmer(log(P)~ANO + (1|Amb), data = Asty)
summary(model6.Asty.p.lmer)
## boundary (singular) fit: see ?isSingular
##
## Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
## Formula: log(P) ~ ANO + (1 | Amb)
## Data: Asty
##
## REML criterion at convergence: 162.6
##
## Scaled residuals: 
##   Min      1Q  Median      3Q     Max 
## -1.2875 -1.1595 -0.2236  0.6755  2.2201 
##
## Random effects:
##   Groups   Name        Variance Std.Dev.
## Amb      (Intercept) 0.0000   0.0000  
## Residual             0.6139   0.7835  
## Number of obs: 67, groups:  Amb, 2
##
## Fixed effects:
##   Estimate Std. Error        df t value Pr(>|t|)
## (Intercept) -39.48203   95.85594  65.00000  -0.412    0.682
## ANO           0.02005    0.04754  65.00000   0.422    0.675
##
## Correlation of Fixed Effects:
##   (Intr)
## ANO -1.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular



##----------------------------------------------------------------------------##

dir()

Asty2 <- read.table("Astylacustris_AmbGran.txt", header=T)
str(Asty2)
Asty2$CP
Asty2$P
Asty2$ANO
Asty2$Longitude
Asty2$Latitude
Asty2$Amb

Asty2

summary(Asty2)
## catalognumber         CP              P              day            month            ANO      
## Min.   :21017   Min.   :3.100   Min.   :1.000   Min.   : 8.00   Min.   :2.000   Min.   :2016  
## 1st Qu.:21023   1st Qu.:3.700   1st Qu.:2.000   1st Qu.:11.00   1st Qu.:2.000   1st Qu.:2016  
## Median :22753   Median :4.600   Median :3.000   Median :19.00   Median :5.000   Median :2017  
## Mean   :22164   Mean   :4.622   Mean   :3.348   Mean   :16.57   Mean   :4.565   Mean   :2017  
## 3rd Qu.:22791   3rd Qu.:5.450   3rd Qu.:4.500   3rd Qu.:21.00   3rd Qu.:7.000   3rd Qu.:2018  
## Max.   :22829   Max.   :6.700   Max.   :8.000   Max.   :22.00   Max.   :7.000   Max.   :2018  
## 
## state               Amb              Longitude         Latitude     
## Length:23          Length:23          Min.   :-48.58   Min.   :-23.49  
## Class :character   Class :character   1st Qu.:-48.50   1st Qu.:-23.49  
## Mode  :character   Mode  :character   Median :-48.41   Median :-23.46  
##                                       Mean   :-48.40   Mean   :-21.89  
##                                       3rd Qu.:-48.28   3rd Qu.:-18.92  
##                                       Max.   :-48.22   Max.   :-18.92 

## Astyanax lacustris_AmbGran - teste de normalidade -------------------------##
shapiro.test(Asty2$CP) ## W = 0.94894, p-value = 0.2782  
shapiro.test(Asty2$P)  ## W = 0.88222, p-value = 0.01107  
shapiro.test(Asty2$ANO) ## W = 0.80716, p-value = 0.0004999  


## Astyanax lacustris_AmbGran - cor e cov ------------------------------------##
cor(Asty2$ANO, Asty2$CP) ## 0.3483976
cor(Asty2$ANO, Asty2$P)  ## 0.149331
cor(Asty2$ANO, Asty2$CP, method="spearman") ## 0.3415192
cor(Asty2$ANO, Asty2$P, method="spearman")  ## 0.2783456

cov(Asty2$ANO, Asty2$CP) ## 0.3019763
cov(Asty2$ANO, Asty2$P)  ## 0.2588933



## Astyanax lacustris_AmbGran - CP -------------------------------------------##

model.Asty2.cp.gamm<-gamm(CP~s(ANO), data=Asty2)
summary(model.Asty2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Asty2.cp.gamm<-gamm(log(CP)~s(ANO), data=Asty2)
summary(model2.Asty2.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Asty2.cp.gamm<-gamm(CP~ANO, data=Asty2)
summary(model2.Asty2.cp.gamm$gam)
## Error in gamm(CP ~ ANO, data = Asty2) : 
## gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Asty2.cp.gamm.exp<-gamm(log(CP)~ANO, data=Asty2)
summary(model4.Asty2.cp.gamm.exp$gam)
## Error in gamm(log(CP) ~ ANO, data = Asty2) : 
## gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Asty2.cp.lm<-lm(CP~ANO, data = Asty2)
summary(model5.Asty2.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Asty2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.0830 -0.8635 -0.4830  0.5962  2.1170 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -964.5113   568.9825  -1.695    0.105
## ANO            0.4805     0.2821   1.703    0.103
##
## Residual standard error: 1.049 on 21 degrees of freedom
## Multiple R-squared:  0.1214,	Adjusted R-squared:  0.07954 
## F-statistic: 2.901 on 1 and 21 DF,  p-value: 0.1033

model6.Asty2.cp.lm<-lm(log(CP)~ANO, data = Asty2)
summary(model6.Asty2.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Asty2)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.26047 -0.17929 -0.08354  0.12562  0.44868 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)  
## (Intercept) -244.70803  122.74800  -1.994   0.0593 .
## ANO            0.12207    0.06086   2.006   0.0579 .
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.2263 on 21 degrees of freedom
## Multiple R-squared:  0.1608,	Adjusted R-squared:  0.1208 
## F-statistic: 4.023 on 1 and 21 DF,  p-value: 0.05793



## Astyanax lacustris_AmbGran - P -------------------------------------------##

model.Asty2.p.gamm<-gamm(P~s(ANO), data = Asty2)
summary(model.Asty2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Asty2.p.gamm<-gamm(log(P)~s(ANO), data=Asty2)
summary(model2.Asty2.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Asty2.p.gamm<-gamm(P~ANO, data=Asty2)
summary(model3.Asty2.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Asty2) : 
##   gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Asty2.p.gamm<-gamm(log(P)~ANO, data=Asty2)
summary(model4.Asty2.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Asty2) : 
##   gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Asty2.p.lm<-lm(P~ANO, data = Asty2)
summary(model5.Asty2.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Asty2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.3836 -1.5896 -0.7956  1.3223  4.6164 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -827.5189  1200.5342  -0.689    0.498
## ANO            0.4119     0.5952   0.692    0.496
##
## Residual standard error: 2.213 on 21 degrees of freedom
## Multiple R-squared:  0.0223,	Adjusted R-squared:  -0.02426 
## F-statistic: 0.479 on 1 and 21 DF,  p-value: 0.4965

model6.Asty2.p.lm<-lm(log(P)~ANO, data = Asty2)
summary(model6.Asty2.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Asty2)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.0127 -0.4479 -0.1708  0.5251  1.1900 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -516.7716   368.8087  -1.401    0.176
## ANO            0.2567     0.1829   1.404    0.175
##
## Residual standard error: 0.6799 on 21 degrees of freedom
## Multiple R-squared:  0.0858,	Adjusted R-squared:  0.04227 
## F-statistic: 1.971 on 1 and 21 DF,  p-value: 0.175



##----------------------------------------------------------------------------##

dir()

Asty3 <- read.table("Astylacustris_AmbPeque.txt", header=T)
str(Asty3)
Asty3$CP
Asty3$P
Asty3$ANO
Asty3$Longitude
Asty3$Latitude
Asty3$Amb

Asty3

summary(Asty3)
## catalognumber         CP              P               day            month             ANO      
## Min.   :21204   Min.   :2.700   Min.   : 1.000   Min.   : 5.00   Min.   : 1.000   Min.   :2012  
## 1st Qu.:22375   1st Qu.:3.400   1st Qu.: 1.000   1st Qu.:10.00   1st Qu.: 2.000   1st Qu.:2015  
## Median :22755   Median :4.300   Median : 2.000   Median :10.50   Median : 2.000   Median :2017  
## Mean   :22429   Mean   :4.568   Mean   : 3.614   Mean   :13.77   Mean   : 5.045   Mean   :2016  
## 3rd Qu.:22836   3rd Qu.:5.225   3rd Qu.: 4.250   3rd Qu.:17.00   3rd Qu.:11.250   3rd Qu.:2017  
## Max.   :22865   Max.   :8.300   Max.   :15.000   Max.   :28.00   Max.   :12.000   Max.   :2019  
## 
## state               Amb              Longitude         Latitude     
## Length:44          Length:44          Min.   :-51.48   Min.   :-23.47  
## Class :character   Class :character   1st Qu.:-49.95   1st Qu.:-23.38  
## Mode  :character   Mode  :character   Median :-48.42   Median :-23.33  
##                                       Mean   :-49.06   Mean   :-22.41  
##                                       3rd Qu.:-48.16   3rd Qu.:-20.71  
##                                       Max.   :-47.77   Max.   :-20.42 

## Astyanax lacustris_AmbPeque - teste de normalidade ------------------------##
shapiro.test(Asty3$CP) ## W = 0.92044, p-value = 0.004924  
shapiro.test(Asty3$P)  ## W = 0.77048, p-value = 7.055e-07
shapiro.test(Asty3$ANO) ## W = 0.70678, p-value = 4.599e-08


## Astyanax lacustris_AmbPeque - cor e cov -----------------------------------##
cor(Asty3$ANO, Asty3$CP) ## 0.05281316
cor(Asty3$ANO, Asty3$P)  ## 0.09414422
cor(Asty3$ANO, Asty3$CP, method="spearman") ## -0.01588296
cor(Asty3$ANO, Asty3$P, method="spearman")  ## -0.01393925

cov(Asty3$ANO, Asty3$CP) ## 0.1799154
cov(Asty3$ANO, Asty3$P)  ## 0.7494715


## Astyanax lacustris_AmbPeque - CP ------------------------------------------##

model.Asty3.cp.gamm<-gamm(CP~s(ANO), data=Asty3)
summary(model.Asty3.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Asty3.cp.gamm<-gamm(log(CP)~s(ANO), data=Asty3)
summary(model2.Asty3.cp.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Asty3.cp.gamm<-gamm(CP~ANO, data=Asty3)
summary(model3.Asty3.cp.gamm$gam)
## Error in gamm(P ~ ANO, data = Asty3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Asty3.cp.gamm.exp<-gamm(log(CP)~ANO, data=Asty3)
summary(model4.Asty3.cp.gamm$gam)
## Error in gamm(P ~ ANO, data = Asty3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Asty3.cp.lm<-lm(CP~ANO, data = Asty3)
summary(model5.Asty3.cp.lm)
## Call:
## lm(formula = CP ~ ANO, data = Asty3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -1.9013 -1.1117 -0.2720  0.6237  3.6987 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -59.37432  186.55918  -0.318    0.752
## ANO           0.03172    0.09254   0.343    0.733
##
## Residual standard error: 1.445 on 42 degrees of freedom
## Multiple R-squared:  0.002789,	Adjusted R-squared:  -0.02095 
## F-statistic: 0.1175 on 1 and 42 DF,  p-value: 0.7335

model6.Asty3.cp.lm<-lm(log(CP)~ANO, data = Asty3)
summary(model6.Asty3.cp.lm)
## Call:
## lm(formula = log(CP) ~ ANO, data = Asty3)
##
## Residuals:
##   Min       1Q   Median       3Q      Max 
## -0.48262 -0.24769 -0.01753  0.17755  0.64038 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.897254  38.851223  -0.023    0.982
## ANO          0.001177   0.019272   0.061    0.952
##
## Residual standard error: 0.301 on 42 degrees of freedom
## Multiple R-squared:  8.874e-05,	Adjusted R-squared:  -0.02372 
## F-statistic: 0.003727 on 1 and 42 DF,  p-value: 0.9516



## Astyanax lacustris_AmbPeque - P ------------------------------------------##

model.Asty3.p.gamm<-gamm(P~s(ANO), data = Asty3)
summary(model.Asty3.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model2.Asty3.p.gamm<-gamm(log(P)~s(ANO), data=Asty3)
summary(model2.Asty3.p.gamm$gam)
## Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
##  A term has fewer unique covariate combinations than specified maximum degrees of freedom

model3.Asty3.p.gamm<-gamm(P~ANO, data=Asty3)
summary(model3.Asty3.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Asty3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model4.Asty3.p.gamm<-gamm(log(P)~ANO, data=Asty3)
summary(model4.Asty3.p.gamm$gam)
## Error in gamm(P ~ ANO, data = Asty3) : 
##  gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect

model5.Asty3.p.lm<-lm(P~ANO, data = Asty3)
summary(model5.Asty3.p.lm)
## Call:
## lm(formula = P ~ ANO, data = Asty3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -3.0160 -2.7518 -1.0911  0.4982 11.2482 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept) -262.7508   434.6354  -0.605    0.549
## ANO            0.1321     0.2156   0.613    0.543
##
## Residual standard error: 3.367 on 42 degrees of freedom
## Multiple R-squared:  0.008863,	Adjusted R-squared:  -0.01474 
## F-statistic: 0.3756 on 1 and 42 DF,  p-value: 0.5433

model6.Asty3.p.lm<-lm(log(P)~ANO, data = Asty3)
summary(model6.Asty3.p.lm)
## Call:
## lm(formula = log(P) ~ ANO, data = Asty3)
##
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -0.9492 -0.9401 -0.2244  0.5020  1.7679 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)
## (Intercept)  -8.17699  107.89028  -0.076    0.940
## ANO           0.00452    0.05352   0.084    0.933
##
## Residual standard error: 0.8358 on 42 degrees of freedom
## Multiple R-squared:  0.0001698,	Adjusted R-squared:  -0.02364 
## F-statistic: 0.007133 on 1 and 42 DF,  p-value: 0.9331

