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
library('gganimate')

## hadcrut-temp-example, echo = FALSE
library('readr')
library('dplyr')
URL <-  "https://bit.ly/hadcrutv4"
gtemp <- read_delim(URL, delim = ' ', col_types = 'nnnnnnnnnnnn', col_names = FALSE) %>%
    select(num_range('X', 1:2)) %>% setNames(nm = c('Year', 'Temperature'))

## Plot
gtemp_plt <- ggplot(gtemp, aes(x = Year, y = Temperature)) +
    geom_line() + 
    geom_point() +
    labs(x = 'Year', y = expression(Temeprature ~ degree*C))
gtemp_plt

## Random effectts ##
## gam() + bam()
m_gam <- gam(CP ~ s(ANO, bs = "re"), data = Salminus, method = "REML")
## ou ##
## gamm() + gmm4::gamm4()
m_nlme <- lme(CP ~ 1, data = Samlminus, ~ 1 | Salminus, method = "REML") 
## as duas funções são iguais matematicamente, mas o gamm é melhor se houver afeitos aleatórios complexos, como modleos não gaussianos ##
## para menos de 10 observações, gam não é o melhor modelo ##



## Checking basis size -------------------------------------------------------------------##
## simulated data ##
set.seed(2)
n <- 400
x1 <- rnorm(n)
x2 <- rnorm(n)
y_val <- 1 + 2*cos(pi*x1) + 2/(1+exp(-5*(x2)))
_norm <- y_val + rnorm(n, 0, 0.5)
y_negbinom <- rnbinom(n, mu = exp(y_val),size=10)
y_binom <- rbinom(n,1,prob = exp(y_val)/(1+exp(y_val))) 

## plot
p1 <- ggplot(data.frame(x = x1, y = y_norm),
             aes(x = x, y = y)) +
    geom_point()

p2 <- ggplot(data.frame(x = x2, y = y_norm),
             aes(x = x, y = y)) +
    geom_point()

p3 <- ggplot(data.frame(x = x1, y = y_negbinom),
             aes(x = x, y = y)) +
    geom_point()

p4 <- ggplot(data.frame(x = x2, y = y_negbinom),
             aes(x = x, y = y)) +
    geom_point()

p5 <- ggplot(data.frame(x = x1, y = y_binom),
             aes(x = x, y = y)) +
    geom_point()

p6 <- ggplot(data.frame(x = x2, y = y_binom),
             aes(x = x, y = y)) +
    geom_point()

plot_grid(p1, p3, p5, p2, p4, p6, ncol = 3, align = 'hv', axis = 'lrtb')

## how well does the model fit? many choices: k(df), family, type of smoother,..
## k precisa ser grande o suficiente para caber todo "wiggliness" (curvas). 
Mas a compuação fica mais devagar com k grande
## check se o k é grande o suficiente 
norm_model_1 <- gam(y_norm~s(x1, k = 4) + s(x2, k = 4), method = 'REML')
gam.check(norm_model_1)
## essa função pega os resíduos do modelo e randomiza para ver se há algum tipo de relação com a cováriavel
## é uma avaliação se as associações são maiores nos resíduos do que nos resíduos randomizados, sugerindo se tem uma variação não modelada
## k-index tem q ser proximo de 1 e o p-value não pode ter * ##

p1 <- draw(norm_model_1)
## gam.check() creactes 4 plots: 1. quantile-quantile plots of residuals. if the model is right, should follow 1-1 line
## 2. histogram of residual
## 3. residuals vs linear predictor
## 4. observed vs fitted values

## gam_check_plots1, include=TRUE, echo=TRUE, results="hide"
norm_model <- gam(y_norm ~ s(x1, k=12) + s(x2, k=12), method = 'REML')
gam.check(norm_model, rep = 500)
## rep function simula dados da resposta, criando intervalos de confiaça no plot
## se mudar a family, se encaixar o modelo errado, fica visivel no gráfico
pois_model <- gam(y_negbinom ~ s(x1, k=12) + s(x2, k=12), family=poisson, method= 'REML')
gam.check(pois_model, rep = 500)
negbin_model <- gam(y_negbinom ~ s(x1, k=12) + s(x2, k=12), family = nb, method = 'REML')
gam.check(negbin_model, rep = 500)


## Model selection - shrinkage -------------------------------------------------------------------##
## setup-shrinkage-example
## an example of automatic model selection via null space penalization
set.seed(3)
n <- 200
dat <- gamSim(1, n=n, scale=.15, dist='poisson', verbose = FALSE) ## simulate data
dat <- transform(dat, x4 = runif(n, 0, 1), x5 = runif(n, 0, 1),
                 f4 = rep(0, n), f5 = rep(0, n))   ## spurious

## shrinkage-example-model-fit, echo = TRUE
b <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3) +
             s(x4) + s(x5),
         data = dat, family = poisson, method = 'REML',
         select = TRUE)


## shrinkage-example-truth, echo = FALSE
p1 <- ggplot(dat, aes(x = x0, y = f0)) + geom_line()
p2 <- ggplot(dat, aes(x = x1, y = f1)) + geom_line()
p3 <- ggplot(dat, aes(x = x2, y = f2)) + geom_line()
p4 <- ggplot(dat, aes(x = x3, y = f3)) + geom_line()
p5 <- ggplot(dat, aes(x = x4, y = f4)) + geom_line()
p6 <- ggplot(dat, aes(x = x5, y = f5)) + geom_line()
plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3, align = 'vh', labels = paste0('x', 1:6))


## shrinkage-example-summary
summary(b)
## shrinkage-example-plot
library('gratia'); draw(b, scales = 'fixed')

## AIC ---------------------------------------------------------------------------------##
data(co2s)
head(co2s)
ggplot(co2s, aes(x = c.month, y = co2)) + geom_line()
b <- gam(co2 ~ s(c.month, k=300, bs="cr"), data = co2s, method = 'REML')
summary(b)

## previsão: com base nos dados que tem até agora, o que esse modelo acha que vai acontecer nos próximos 3 anos? ##
pd <- with(co2s, data.frame(c.month = 1:(nrow(co2s)+36))) ## do mês 1 até o final dos dados + 36 após ## 
pd <- cbind(pd, predict(b, pd, se = TRUE))
pd <- transform(pd, upr = fit + (2*se.fit), lwr = fit - (2 * se.fit)) ## cria os intervalos de confiança para os valores preditivos ##
ggplot(pd, aes(x = c.month, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    geom_line(data = co2s, aes(c.month, co2), col = 'red') +
    geom_line(alpha = 0.5)
## esse é um dos problemas do gam, ele não extrapola muito bem, ele vai apenas extrapolar linearmente, mesmo quando os dados não são ##
## se usar o "cc" (suavizador ciclico) para o mês, então tem uma tendencia gradual para a previsão
b2 <- gam(co2 ~ s(month, bs = "cc") + s(c.month, bs = "cr", k = 300),
          data = co2s, method = 'REML',
          knots = list(month = c(0.5, 12.5))) ## defini o nó = pontos finais para o cc ##
summary(b2) ## p-value dos smooths deu significante ##
## após, volta para a função de predisão
nr <- nrow(co2s)
pd2 <- with(co2s, data.frame(c.month = 1:(nr+36),
                             month = rep(1:12, length.out=nr+36)))
pd2 <- cbind(pd2, predict(b2, pd2, se = TRUE))
pd2 <- transform(pd2, upr = fit + (2*se.fit), lwr = fit - (2 * se.fit))
ggplot(pd2, aes(x = c.month, y = fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    geom_line(data = co2s, aes(c.month, co2), col = 'red') +
    geom_line(alpha = 0.5)
## agora a previsão se encaixa melhor ##


## Galveston full model -----------------------------------------------------------------##
## 2:30:00 do vídeo 
galveston <- read_csv(here('data', 'gbtemp.csv')) %>%
    mutate(datetime = as.POSIXct(paste(DATE, TIME), format = '%m/%d/%y %H:%M', tz = "CDT"),
           STATION_ID = factor(STATION_ID), DoY = as.numeric(format(datetime, format = '%j')),
           ToD = as.numeric(format(datetime, format = '%H')) +
               (as.numeric(format(datetime, format = '%M')) / 60))
galveston

knots <- list(DoY = c(0.5, 366.5)) ## organiza os anos ##
##full model##
m <- bam(MEASUREMENT ~
             s(ToD, k = 10) +
             s(DoY, k = 12, bs = 'cc') +
             s(YEAR, k = 30) +
             s(LONGITUDE, LATITUDE, k = 100, bs = 'ds', m = c(1, 0.5)) +
             ti(DoY, YEAR, bs = c('cc', 'tp'), k = c(12, 15)) +
             ti(LONGITUDE, LATITUDE, ToD, d = c(2,1), bs = c('ds','tp'),
                m = list(c(1, 0.5), NA), k = c(20, 10)) +
             ti(LONGITUDE, LATITUDE, DoY, d = c(2,1), bs = c('ds','cc'),
                m = list(c(1, 0.5), NA), k = c(25, 12)) +
             ti(LONGITUDE, LATITUDE, YEAR, d = c(2,1), bs = c('ds','tp'),
                m = list(c(1, 0.5), NA), k = c(25, 15)),
         data = galveston, method = 'fREML', knots = knots,
         nthreads = c(4, 1), discrete = TRUE)
         
## simple model ##
m.sub <- bam(MEASUREMENT ~
             s(ToD, k = 10) +
             s(DoY, k = 12, bs = 'cc') +
             s(YEAR, k = 30) +
             s(LONGITUDE, LATITUDE, k = 100, bs = 'ds', m = c(1, 0.5)) +
             ti(DoY, YEAR, bs = c('cc', 'tp'), k = c(12, 15)),
         data = galveston, method = 'fREML', knots = knots,
         nthreads = c(4, 1), discrete = TRUE)

AIC(m, m.sub)
anova(m, m.sub, test = 'F') ## removendo as interações + complexas, o modelo fica significativamente pior ##
summary(m) ## p-value da significante, o que quer dizer que as funções estão longe de ser funções planas ##
plot(m, pages = 1, scheme = 2, shade = TRUE)
draw(m, scales = 'free')

## predições ##
pdata <- with(galveston,
              expand.grid(ToD = 12,
                          DoY = 180,
                          YEAR = seq(min(YEAR), max(YEAR), by = 1),
                          LONGITUDE = seq(min(LONGITUDE), max(LONGITUDE), length = 100),
                          LATITUDE  = seq(min(LATITUDE), max(LATITUDE), length = 100)))
fit <- predict(m, pdata)
ind <- exclude.too.far(pdata$LONGITUDE, pdata$LATITUDE,
                       galveston$LONGITUDE, galveston$LATITUDE, dist = 0.1)
fit[ind] <- NA
pred <- cbind(pdata, Fitted = fit)

plt <- ggplot(pred, aes(x = LONGITUDE, y = LATITUDE)) +
    geom_raster(aes(fill = Fitted)) + facet_wrap(~ YEAR, ncol = 12) +
    scale_fill_viridis(name = expression(degree*C), option = 'plasma', na.value = 'transparent') +
    coord_quickmap() +
    theme(legend.position = 'right')
plt

## tendencias ##
pdata <- with(galveston,
              expand.grid(ToD = 12,
                          DoY = c(1, 90, 180, 270),
                          YEAR = seq(min(YEAR), max(YEAR), length = 500),
                          LONGITUDE = -94.8751,
                          LATITUDE  = 29.50866))

fit <- data.frame(predict(m, newdata = pdata, se.fit = TRUE))
fit <- transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit))
pred <- cbind(pdata, fit)

plt2 <- ggplot(pred, aes(x = YEAR, y = fit, group = factor(DoY))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
    geom_line() + facet_wrap(~ DoY, scales = 'free_y') +
    labs(x = NULL, y = expression(Temperature ~ (degree * C)))
plt2


