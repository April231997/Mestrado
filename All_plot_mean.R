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
library(hrbrthemes)

ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()



##----------------------------------------------------------------------------##
                          ## Aces. lacustris ##



Aces <- read.table("Aceslacustris_Geral2.txt", header=T)
Aces3 <- filter(Aces, Amb == "Grande")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Aces.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Aces.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkgoldenrod3", lwd = 3)



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
##
## R-sq.(adj) =  0.979   Deviance explained = 99.5%
## -REML = 46.292  Scale est. = 0.30496   n = 15

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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Aces3.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Aces3.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkgoldenrod3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Aces3.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model2.Aces3.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkgoldenrod3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Aces3.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model2.Aces3.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkgoldenrod3", lwd = 3)



## ---------------------------------------------------------------------------##
                             ## C.callichthys ##

Calli <- read.table("C.callichthys_Geral2.txt", header=T)
Calli2 <- filter(Calli, Amb == "Grande")
Calli3 <- filter(Calli, Amb == "Pequeno")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Calli.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Calli.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Calli.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model2.Calli.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Calli.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Calli.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Calli2.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Comprimento Padrão (cm)", col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Calli2.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Calli2.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Calli2.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model2.Calli2.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Calli2.p.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Peso (g)", col = "black", lwd = 3)




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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Calli2.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Calli2.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Calli2.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model2.Calli2.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "black", lwd = 3)
 


## ---------------------------------------------------------------------------##
                               ## H.malabaricus ##

    
Mala <- read.table("H.malabaricus_Geral2.txt", header=T)
Mala2 <- filter(Mala, Amb == "Grande")
Mala3 <- filter(Mala, Amb == "Pequeno")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Mala.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Mala.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deepskyblue", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Mala3.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Mala3.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deepskyblue", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Mala3.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Mala3.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deepskyblue", lwd = 3)



##----------------------------------------------------------------------------##
                              ## L.friderici ## 

dir()  

Frid <- read.table("L.friderici_Geral2.txt", header=T)
Frid2 <- filter(Frid, Amb == "Grande")
Frid3 <- filter(Frid, Amb == "Pequeno")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Frid2.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Comprimento Padrão (cm)", col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Frid2.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Comprimento Padrão (cm)", col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(CP ~ ANO, data = Frid2, xlab = "Ano", ylab = "Comprimento Padrão (cm)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(372.29875, -0.18167, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(log(CP) ~ ANO, data = Frid2, xlab = "Ano", ylab = "log CP (cm)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(44.981741, -0.021449, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Frid2.p.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Peso (g)", col = "darkslategray3", lwd = 3)



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
##              Estimate Std. Error t value Pr(>|t|)  
## (Intercept) 129.21506   57.57017   2.244   0.0415 *
##  ANO          -0.06335    0.02879  -2.200   0.0451 *
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 1.329 on 14 degrees of freedom
## Multiple R-squared:  0.257,	Adjusted R-squared:  0.2039 
## F-statistic: 4.842 on 1 and 14 DF,  p-value: 0.04506



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(log(P) ~ ANO, data = Frid2, xlab = "Ano", ylab = "log Peso (g)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(129.21506, -0.06335, lty = 2, col = "red", lwd = 3)



##----------------------------------------------------------------------------##
                              ## P.lineatus ## 



Line <- read.table("P.lineatus_Geral2.txt", header=T)
Line2 <- filter(Line, Amb == "Pequeno")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model3.Line.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Comprimento Padrão (cm)", col = "deeppink4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model4.Line.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "log CP (cm)", col = "deeppink4", lwd = 3)



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
## Groups   Name        Variance  Std.Dev. 
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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(CP ~ ANO, data = Line, xlab = "Ano", ylab = "Comprimento Padrão (cm)", pch = 20, cex = 1, col = "deeppink4", lwd = 3)
abline(520.2910+7.269e-14,-0.2533, lty = 2, col = "red", lwd = 3)
abline(520.2910-7.269e-14,-0.2533, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(log(CP) ~ ANO, data = Line, xlab = "Ano", ylab = "log CP (cm)", pch = 20, cex = 1, col = "deeppink4", lwd = 3)
abline(41.81575+1.557e-14,-0.01962, lty = 2, col = "red", lwd = 3)
abline(41.81575-1.557e-14,-0.01962, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Line.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Line.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deeppink4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Line.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "log CP (cm)",
     shift = coef(model2.Line.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deeppink4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Line2.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comrprimento Padrão (cm)",
     shift = coef(model.Line2.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deeppink4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Line2.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "log CP (cm)",
     shift = coef(model2.Line2.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deeppink4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Line2.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Line2.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deeppink4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Line2.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "log Peso (g)",
     shift = coef(model2.Line2.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "deeppink4", lwd = 3)



##----------------------------------------------------------------------------##
                              ## R.latirostris ## 



Lati <- read.table("R.latirostris_Geral2.txt", header=T)
Lati2 <- filter(Lati, Amb == "Pequeno")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Lati.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Lati.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "coral4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Lati2.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Lati2.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "coral4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Lati2.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "log CP (cm)",
     shift = coef(model2.Lati2.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "coral4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Lati2.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Lati2.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "coral4", lwd = 3)



##----------------------------------------------------------------------------##
                               ## R.quelen ## 



Quel <- read.table("R.quelen_Geral2.txt", header=T)
Quel2 <- filter(Quel, Amb == "Grande")
Quel3 <- filter(Quel, Amb == "Pequeno")



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Quel3.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Comprimento Padrão (cm)", col = "blue4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Quel3.cp.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Comprimento Padrão (cm)", col = "blue4", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(CP ~ ANO, data = Quel3, xlab = "Ano", ylab = "Comprimento Padrão (cm)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(-804.8949, 0.4057, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(log(CP) ~ ANO, data = Quel3, xlab = "Ano", ylab = "log CP (cm)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(-75.52685, 0.03871, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Quel3.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model.Quel3.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Quel3.cp.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Comprimento Padrão (cm)",
     shift = coef(model2.Quel3.cp.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Quel3.p.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "Peso (g)", col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Quel3.p.gamm$gam, rug = FALSE, shade = TRUE, shade.col = "lightgray", xlab = "Ano", ylab = "log Peso (g)", col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(P ~ ANO, data = Quel3, xlab = "Ano", ylab = "Peso (g)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(-5538.3103, 2.7699, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(log(P) ~ ANO, data = Quel3, xlab = "Ano", ylab = "log Peso (g)", pch = 20, cex = 1, col = "darkslategray3", lwd = 3)
abline(-206.39868, 0.10404, lty = 2, col = "red", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model.Quel3.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "Peso (g)",
     shift = coef(model.Quel3.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkslategray3", lwd = 3)



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



par(mar = c (5, 5, 2, 4), las = 1, cex.axis = 1.3, cex.lab = 1.5, tcl = 0.3, bty = "o", family = "serif", mgp = c(3, 0.3, 0))
plot(model2.Quel3.p.gam, residuals = FALSE, 
     shade = TRUE, shade.col = "lightgray",
     xlab = "Ano", ylab = "log Peso (g)",
     shift = coef(model2.Quel3.p.gam)[1], seWithMean = TRUE,
     rug = FALSE, se = TRUE, col = "darkslategray3", lwd = 3)


