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

mcycle <- MASS::mcycle

head(mcycle)
plot(mcycle)

lm_mod <- lm(accel ~ times, data = mcycle)
termplot(lm_mod, partial.resid = TRUE, se = TRUE)

gam_mod <- gam(accel ~ s(times), data = mcycle)
plot(gam_mod, residuals = TRUE, pch = 1)

coef(gam_mod)

## mgcv o the work of selecting a smoothing parameter. 
## However, we can fix the smoothing parameter 
## to a value of our choosing via the sp argument.

gam(y ~s(x), data = dat, sp = 0.1)
gam(y ~ s(x, sp = 0.1), data = dat)

## most GAM experts, strongly recommend that you fit models with the REML, 
## or "Restricted Maximum Likelihood" method. 
## While different methods have their advantages, 
## REML is most likely to give you reliable, stable results.

gam(y ~s(x), data = dat, method = "REML")

## a smooth with a small number of basis functions is limited 
## in its wiggliness, while one with many basis functions 
## is capable of capturing finer patterns.

## To set the number of basis functions in a smooth, 
## we use the k argument in the smooth function in a GAM formula. 
## Setting this value too low will prevent the model from being 
## sufficiently wiggly. If it's high, though, the automatic smoothing 
## parameter selection will prevent it from being too wiggly. 
## We just don't want to set it very high, which can result in a model 
## with more parameters than data, or one that is slow to fit.

## Setting number of basis functions
gam(y ~ s(x, k = 3), data = dat, method = "REML")
gam(y ~ s(x, k = 10), data = dat, method = "REML")
## Use the defaults
gam(y ~ s(x), data = dat, method = "REML") 

## Exercício
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

gam_mod_sk <- gam(accel ~ s(times, k = 50), data = mcycle, sp = 0.0001)
plot(gam_mod_sk, residuals = TRUE, pch = 1)

## when you use categorical variables this way, it's important 
## that the variables are stored as factors. 

data("mpg", package="gamair")
head(mpg)
str(mpg)
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price), 
                data = mpg, method = "REML")
plot(mod_city, pages = 1)

## Categories are inherently linear, 
## so you'll model them as linear terms.

mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style,
                 data = mpg, method = "REML")
plot(mod_city2, all.terms = TRUE, pages = 1)

## different smooths for different levels of categorical terms
mod_city3 <- gam(city.mpg ~ s(weight, by = drive) + s(length, by = drive) + s(price, by = drive) + drive,
                 data = mpg, method = "REML")
plot(mod_city3, pages = 1)

## Interpreting GAM outputs ---------- ##
mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                data = mpg, method = "REML")
summary(mod_city4)
plot(mod_city4, pages = 1)
## Family: gaussian 
## Link function: identity 
##
## Formula:
##  city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width)
##
##Parametric coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)   25.201      0.188     134   <2e-16 ***
##  ---
##  Signif. codes:  
##  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##Approximate significance of smooth terms:
##  edf Ref.df      F  p-value    
##s(weight) 5.620  6.799 17.524  < 2e-16 ***
##s(length) 2.943  3.759  0.904    0.421    
##s(price)  1.000  1.000 16.647 6.79e-05 ***
##s(rpm)    7.751  8.499 16.486  < 2e-16 ***
##s(width)  1.003  1.005  0.006    0.954    
##---
##Signif. codes:  
##0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##R-sq.(adj) =  0.831   Deviance explained = 84.7%
##-REML = 496.47  Scale est. = 7.0365    n = 199 ##

##The column edf stands for effective degrees of freedom. 
##This value represents the complexity of the smooth. An edf of 1 is equivalent 
##to a straight line. An edf of 2 is equivalent to a quadratic curve, and so on, 
##with higher edfs describing more wiggly curves.
## An edf over 6, is complex and wiggly. 
## But the compression ratio smooth, with an edf of 1, is linear.

## The terms to the right of the EDF column have to do with significance testing 
## for smooths. The Ref.df and F columns are test statistics used in an ANOVA test 
## to test overall significance of the smooth. 
## The result of this test is the p-value to the right. 

## A good way to interpret significance for smooth terms in GAMs is this: 
## a significant smooth term is one where you can not draw a horizontal line 
## through the 95% confidence interval.

## Note that high EDF doesn't mean significance or vice-versa. 
## A smooth may be linear and significant, non-linear and non-significant, 
## or one of each.

## No exemplo, price é significante, mas tem edf 1, então é linear.
## E lenght não é significante e não linear, pois o edf é 2

## Visualizing GAMs ---------------------------------- ##
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
plot(mod, residuals = TRUE)                         ## add partial residuals to the partial effect plot of a GAM so as to compare the model to the data.
plot(mod, residuals = TRUE, pch = 1, cex = 1)       ## making partial residuals more visible by changing the shape using the pch argument, and size of the residuals using the cex argument.

mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod, select = 3)                   ## to view only the partial effect of price
plot(mod, pages = 1, all.terms = TRUE)  ## show all terms on a single page

mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink") ## It's often preferable to use shading rather than lines to show these intervals, and you can do so with the shade argument.

## It's often useful to plot the standard errors of a partial effect term 
##combined with the standard errors of the model intercept. 
## This is because confidence intervals at the mean value of a variable can 
## be very tiny, and don't reflect overall uncertainty in our model. 
## Using the seWithMean argument adds in this uncertainty.

## To make the plots even more interpretable, it's useful to shift the scale 
## so that the intercept is included. Using the shift argument, 
## we can shift the scale by value of the intercept, which is the first 
## coefficient of the model. Note how the y-axis has changed. Now, 
## the partial effect plot has a more natural interpretation - 
## it shows us the prediction of the output, assuming other variables are at 
## their average value.
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)

plot(mod, rug = TRUE) ## show data alongside model predictions
plot(mod, se = TRUE)  ## By default, plot will put standard errors on your plots. These show the 95% confidence interval for the mean shape of the effect.


## Model checking with gam.check() ----------------------------- ##
## We've learned that the number of basis functions determines how wiggly 
## a smooth can be. If there are not enough basis functions, 
## it may not be wiggly enough to capture the relationships in data.

library(mgcv)
set.seed(0)
dat <- gamSim(1,n=200)
library(mgcv)
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")
gam.check(mod)

## Result:
## Method: REML   Optimizer: outer newton
## full convergence after 10 iterations.
## Gradient range [-0.0001190691,0.0001259251]
## (score 460.9549 & scale 5.229925).
## Hessian positive definite, eigenvalue range [0.0001190726,97.53256].
## Model rank =  17 / 17 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
##
##         k'  edf k-index p-value   
## s(x0) 4.00 2.54    0.99    0.47   
## s(x1) 4.00 2.25    0.95    0.18   
## s(x2) 4.00 3.94    0.84    0.01 **
## s(x3) 4.00 1.00    0.99    0.40   
## ---
## Signif. codes:  
## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## First, gam.check() reports on model convergence. 
## Here, it reports full convergence. R has found a best solution. 
## If the model has not converged, results are likely not correct. 
## This can happen when there are too many parameters in the model 
## for not enough data.
## Below, we see a table of basis checking results. 
## This shows a statistical test for patterns in model residuals, which should 
## be random. Each line reports the test results for one smooth. 
## It shows the k value or number of basis functions, the effective degrees 
## of freedom, a test statistic, and p-value.
## Here, small p-values indicate that residuals are not randomly distributed. 
## This often means there are not enough basis functions.

set.seed(0)
dat <- mgcv::gamSim(1,n=600, scale=0.6, verbose=FALSE)
mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")
gam.check(mod)
## p-value x2 deu significativo. Mudar o k.
b <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k = 3),
            data = dat, method = "REML")
plot(b, pages = 1)
gam.check(b, pch=1, cex=1)


## Checking concurvity ----------------------------------##
## When two variables or covariates in a model are strongly correlated, 
## it's difficult to fit the model, because the outcome variable could be 
## responding to either one. We call this phenomenon collinearity, 
## and it can result in poorly fit models with large confidence intervals. 
## In general, we avoid putting multiple collinear variables into the same model.

library(gamair)
set.seed(0)
data("mpg", package="gamair", verbose=FALSE)
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")
concurvity(mod, full = TRUE)

mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")
concurvity(mod, full = FALSE)

## SPATIAL GAMs AND INTERACTIONS --------------------------------------------##
library(sp)
data(meuse, package="sp")
head(meuse)
str(meuse)
mod2d <- gam(cadmium ~ s(x, y), data = meuse, method = "REML")
summary(mod2d)
coef(mod2d)
mod2da <- gam(cadmium ~ s(x, y) + s(dist) + s(elev), 
              data = meuse, method = "REML")
summary(mod2da)
