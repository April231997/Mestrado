## Acestrorhynchus lacustris - Ammbiente Pequeno -----------------------------##
ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()
Aces <- read.table("Aces.lacustris_amb.peque.txt", header=T)
str(Aces)
Aces$CP
Aces$P
Aces$ANO
summary(Aces)
##       CP              P               ANO          Estado            Ambiente        
## Min.   : 3.30   Min.   :  1.00   Min.   :1983   Length:96          Length:96         
## 1st Qu.:12.10   1st Qu.: 23.00   1st Qu.:2005   Class :character   Class :character  
## Median :13.30   Median : 33.50   Median :2005   Mode  :character   Mode  :character  
## Mean   :13.73   Mean   : 44.90   Mean   :2003                                        
## 3rd Qu.:16.00   3rd Qu.: 61.25   3rd Qu.:2006                                        
## Max.   :22.50   Max.   :180.00   Max.   :2017  

## Acestrorhynchus lacustris_Amb.peque - teste de normalidade ----------------##
shapiro.test(Aces$CP) ## W = 0.95678, p-value = 0.00307     <-- c/ DIST. NORMAL
shapiro.test(Aces$P)  ## W = 0.84471, p-value = 1.264e-08  <-- c/ DIST. NORMAL
shapiro.test(Aces$ANO)  ## W = 0.78418, p-value = 1.489e-10  <-- c/ DIST. NORMAL

## Acestrorhynchus lacustris_Amb.peque - cor e cov ---------------------------##
cor(Aces$ANO, Aces$CP) ## 0.02714659
cor(Aces$ANO, Aces$P)  ## -0.09631089
cor(Aces$ANO, Aces$CP, method="spearman") ## -0.003566854
cor(Aces$ANO, Aces$P, method="spearman")  ## -0.05217839

cov(Aces$ANO, Aces$CP) ## 0.7663706
cov(Aces$ANO, Aces$P)  ## -26.88443

## Acestrorhynchus lacustris_Amb.peque - CP - Gaussiano ----------------------##

Aces4 <- filter(Aces, CP >= 10.7)
Aces4

gam_mod1 <- gam(CP ~ s(ANO, k = 18), data = Aces4, method = "REML", sp = 0.001)
summary(gam_mod1)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   CP ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   14.593      0.253   57.67   <2e-16 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 16.37  16.92 2.291 0.00894 **
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## R-sq.(adj) =  0.216   Deviance explained = 36.9%
## -REML = 228.84  Scale est. = 5.4418    n = 85

gam.check(gam_mod1)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-9.816449e-05,-9.816449e-05]
## (score 228.8449 & scale 5.441836).
## Hessian positive definite, eigenvalue range [41.5001,41.5001].
## Model rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'  edf k-index p-value
## s(ANO) 17.0 16.4    0.91    0.18

plot(gam_mod1, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure",
     xlab = "Ano", ylab = "Comprimento Padrão",
     shift = coef(gam_mod1)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)





## Acestrorhynchus lacustris_Amb.peque - Peso - Gaussiano --------------------##

gam_mod3 <- gam(P ~ s(ANO, k = 18), data = Aces4, method = "REML", sp = 0.001)
summary(gam_mod3)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##   P ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   49.965      3.092   16.16   <2e-16 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F  p-value    
## s(ANO) 16.37  16.92 3.123 0.000481 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## R-sq.(adj) =   0.31   Deviance explained = 44.5%
## -REML = 436.99  Scale est. = 812.4     n = 85

gam.check(gam_mod3)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-2.668307e-05,-2.668307e-05]
## (score 436.9876 & scale 812.3977).
## Hessian positive definite, eigenvalue range [41.50003,41.50003].
## Model rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'  edf k-index p-value
## s(ANO) 17.0 16.4    0.98    0.35

plot(gam_mod3, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure",
     xlab = "Ano", ylab = "Peso",
     shift = coef(gam_mod3)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)

## Acestrorhynchus lacustris_Amb.peque - Peso - Poisson ----------------------##

gam_mod4 <- gam(P ~ s(ANO, k = 18), data = Aces4, method = "REML", sp = 0.001, family = "poisson")
summary(gam_mod4)
## Family: poisson 
## Link function: log 
##
## Formula:
##  P ~ s(ANO, k = 18)
##
## Parametric coefficients:
##   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  3.82776    0.01668   229.5   <2e-16 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## Approximate significance of smooth terms:
##  edf Ref.df Chi.sq p-value    
## s(ANO) 16.98     17  793.2  <2e-16 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##
## R-sq.(adj) =  0.359   Deviance explained = 45.5%
## -REML =  759.7  Scale est. = 1         n = 85

gam.check(gam_mod4)
## Method: REML   Optimizer: outer newton
## Model required no smoothing parameter selectionModel rank =  18 / 18 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##        k' edf k-index p-value
## s(ANO) 17  17     0.9    0.12

plot(gam_mod4, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure",
     xlab = "Ano", ylab = "Peso",
     shift = coef(gam_mod4)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)







## Acestrorhynchus lacustris - Ammbiente Grande ------------------------------##
ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()
Aces2 <- read.table("Aces.lacustris_amb.gran.txt", header=T)
str(Aces2)
Aces2$CP
Aces2$P
Aces2$ANO
summary(Aces2)
##       CP              P               ANO          Estado            Ambiente        
## Min.   : 4.00   Min.   :  1.00   Min.   :1980   Length:29          Length:29         
## 1st Qu.: 9.60   1st Qu.: 10.00   1st Qu.:1982   Class :character   Class :character  
## Median :12.90   Median : 27.00   Median :1985   Mode  :character   Mode  :character  
## Mean   :11.52   Mean   : 29.14   Mean   :1992                                        
## 3rd Qu.:13.90   3rd Qu.: 40.00   3rd Qu.:2005                                        
## Max.   :19.80   Max.   :131.00   Max.   :2013

## Acestrorhynchus lacustris_Amb.gran - teste de normalidade -----------------##
shapiro.test(Aces2$CP) ## W = 0.91834, p-value = 0.02768     
shapiro.test(Aces2$P)  ## W = 0.80523, p-value = 0.0001038  
shapiro.test(Aces2$ANO)  ## W = 0.77176, p-value = 2.71e-05  

## Acestrorhynchus lacustris_Amb.grab - cor e cov ----------------------------##
cor(Aces2$ANO, Aces2$CP) ## -0.276793
cor(Aces2$ANO, Aces2$P)  ## -0.2293384
cor(Aces2$ANO, Aces2$CP, method="spearman") ## -0.3332164
cor(Aces2$ANO, Aces2$P, method="spearman")  ## -0.3952822

cov(Aces2$ANO, Aces2$CP) ## -13.9564
cov(Aces2$ANO, Aces2$P)  ## -76.7968




## Acestrorhynchus lacustris_Amb.gran - CP - Gaussiano -----------------------##

Aces3 <- filter(Aces2, CP >= 10.7)
Aces3

gam_mod5 <- gam(CP ~ s(ANO, k = 8), data = Aces3, method = "REML", sp = 0.001)
summary(gam_mod5)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO, k = 8)
##
## Parametric coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  14.0105     0.2688   52.12 2.13e-15 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value   
## s(ANO) 6.117   6.72 4.368 0.00882 **
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## R-sq.(adj) =  0.671   Deviance explained = 78.2%
## -REML = 40.368  Scale est. = 1.3732    n = 19

gam.check(gam_mod5)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-6.136331e-09,-6.136331e-09]
## (score 40.36781 & scale 1.373167).
## Hessian positive definite, eigenvalue range [8.5,8.5].
## Model rank =  8 / 8 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##         k'  edf k-index p-value
## s(ANO) 7.00 6.12    1.45    0.95

plot(gam_mod5, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure",
     xlab = "Ano", ylab = "Comprimento Padrão",
     shift = coef(gam_mod5)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)

## Acestrorhynchus lacustris_Amb.gran - Peso - Gaussiano -----------------------##

gam_mod6 <- gam(P ~ s(ANO, k = 8), data = Aces3, method = "REML", sp = 0.001)
summary(gam_mod6)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  P ~ s(ANO, k = 8)
##
##  Parametric coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   41.632      3.545   11.74 6.79e-08 ***
##  ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##   edf Ref.df     F p-value  
## s(ANO) 6.117   6.72 3.127  0.0253 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.644   Deviance explained = 76.5%
## -REML = 85.154  Scale est. = 238.8     n = 19

gam.check(gam_mod6)
## Method: REML   Optimizer: outer newton
## full convergence after 5 iterations.
## Gradient range [-1.382298e-07,-1.382298e-07]
## (score 85.15385 & scale 238.8046).
## Hessian positive definite, eigenvalue range [8.5,8.5].
## Model rank =  8 / 8 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##          k'  edf k-index p-value
## s(ANO) 7.00 6.12    1.47    0.96

plot(gam_mod6, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure",
     xlab = "Ano", ylab = "Peso",
     shift = coef(gam_mod6)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)
