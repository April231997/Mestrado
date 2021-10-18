ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()
a <- read.table("Salminus.txt", header=T)
str(a)
a$CP
a$ANO
summary(a)
##       CP              P              ANO         Ambiente        
## Min.   : 6.40   Min.   :  5.0   Min.   :1980   Length:20         
## 1st Qu.:10.65   1st Qu.: 19.5   1st Qu.:1989   Class :character  
## Median :13.20   Median : 32.0   Median :2006   Mode  :character  
## Mean   :14.46   Mean   : 77.6   Mean   :2000                     
## 3rd Qu.:20.30   3rd Qu.:154.2   3rd Qu.:2009                     
## Max.   :24.30   Max.   :289.0   Max.   :2017

## Salminus geral - teste de normalidade--------------------------------------##
shapiro.test(a$CP) ## W = 0.8986, p-value = 0.03882     <-- S/ DIST. NORMAL
shapiro.test(a$P)  ## W = 0.76286, p-value = 0.0002538  <-- S/ DIST. NORMAL
shapiro.test(a$ANO)  ## W = 0.88312, p-value = 0.02014  <-- S/ DIST. NORMAL

## Salminus geral - correlação -----------------------------------------------##
cor(a$ANO, a$CP) ## 0.2751821
cor(a$ANO, a$P)  ## 0.2206498
cor(a$ANO, a$CP, method="spearman") ## 0.3332311
cor(a$ANO, a$P, method="spearman")  ## 0.2938945

cov(a$ANO, a$CP) ## 19.5
cov(a$ANO, a$P)  ## 251.6842

## Salminus geral CP - Gaussiano ---------------------------------------------##

gam_mod <- gam(CP ~ s(ANO), data = a, method = "REML", sp = 0.1)
summary(gam_mod)
plot(gam_mod, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure2",
     xlab = "Ano", ylab = "Comprimento Padrão",
     shift = coef(gam_mod)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)
## Resultado:
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  CP ~ s(ANO)
##
## Parametric coefficients:
## Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    14.46       1.11   13.03 2.82e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##          edf Ref.df     F p-value
## s(ANO) 2.001   2.46 1.388   0.228
##
## R-sq.(adj) =  0.145   Deviance explained = 23.5%
## -REML = 58.127  Scale est. = 24.636    n = 20

coef(gam_mod) ##9 basis function

gam.check(gam_mod)
## Method: REML   Optimizer: outer newton
## full convergence after 6 iterations.
## Gradient range [-1.295278e-08,-1.295278e-08]
## (score 58.1547 & scale 25.05101).
## Hessian positive definite, eigenvalue range [9,9].
## Model rank =  10 / 10 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value
## s(ANO) 9.00 1.77    0.83    0.16    <--- não deu significativo. Não precisa ajustar k.

## k está okays, mas o hist não deu formato de sino.


## Salminus apenas de ambientes pequenos -------------------------------------##
dir()
b <- read.table("Salminus_pequeno.txt", header=T)
str(b)
b$CP
b$ANO
summary(b)
##        CP              P              ANO          Estado            Ambiente        
## Min.   : 9.60   Min.   : 17.0   Min.   :1984   Length:15          Length:15         
## 1st Qu.:12.70   1st Qu.: 29.0   1st Qu.:1998   Class :character   Class :character  
## Median :14.20   Median : 48.0   Median :2006   Mode  :character   Mode  :character  
## Mean   :16.18   Mean   : 98.6   Mean   :2005                                        
## 3rd Qu.:21.35   3rd Qu.:168.0   3rd Qu.:2016                                        
## Max.   :24.30   Max.   :289.0   Max.   :2017  

## Salminus Amb.Peque. - teste de normalidade --------------------------------##
shapiro.test(b$CP) ## W = 0.88169, p-value = 0.05028     <-- c/ DIST. NORMAL
shapiro.test(b$P)  ## W = 0.81974, p-value = 0.006664    <-- c/ DIST. NORMAL
shapiro.test(b$ANO)  ## W = 0.8383, p-value = 0.01191    <-- c/ DIST. NORMAL

## Salminus Amb.Peque. - correlação ------------------------------------------##
cor(b$ANO, b$CP) ## -0.06803891
cor(b$ANO, b$P)  ## -0.05038113
cor(b$ANO, b$CP, method="spearman") ## 0.2002648
cor(b$ANO, b$P, method="spearman")  ## 0.119424

## Salminus Amb.Peque. CP - Gaussiano ----------------------------------------##

gam_b <- gam(CP ~ s(ANO, k = 7), data = b, method = "REML", sp = 0.1)
summary(gam_b)
plot(gam_b, residuals = TRUE, 
     pch = 20, cex = 1, shade = TRUE, shade.col = "azure2",
     xlab = "Ano", ylab = "Comprimento Padrão",
     shift = coef(gam_b)[1], seWithMean = TRUE,
     rug = TRUE, se = TRUE)
## Resultado:
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## CP ~ s(ANO, k = 7)
##
## Parametric coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   16.180      1.149   14.08 5.05e-09 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Approximate significance of smooth terms:
##          edf Ref.df     F p-value
## s(ANO) 1.534  1.834 1.503    0.23
##
## R-sq.(adj) =  0.221   Deviance explained = 30.6%
## -REML =  41.55  Scale est. = 19.813    n = 15

gam.check(gam_b)
## Method: REML   Optimizer: outer newton
## full convergence after 6 iterations.
## Gradient range [-8.370824e-09,-8.370824e-09]
## (score 41.5498 & scale 19.81299).
## Hessian positive definite, eigenvalue range [6.5,6.5].
## Model rank =  7 / 7 
##
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##         k'  edf k-index p-value
## s(ANO) 6.00 1.53    1.06    0.54

## k está okays, mas o hist não deu formato de sino.

