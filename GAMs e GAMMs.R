## pacotes

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
library('hrbrthemes')
library("EnvStats")
library('tinytex')
library('outliers')
library('rmarkdown')
library('ghibli')
library('mgcViz')
library("tidyverse")
library('forcats')
library('geofacet')
library('ggh4x')
library("patchwork")
library('mgcv')
library('gamair')
library('RColorBrewer')



## Filtrar o conjunto de dados

data <- read.table("espécie", header = T) 
data1 <- filter(data, CP > n)  ## filtra os exemplares com tamanho maiores que n, o tamanho mínimo determinado para cada espécie, mas mantém ambos os ambientes
data2 <- filter(data, Amb == "Grande" & CP > n) ## filtra o tipo de ambiente e os exemplares com tamanho maiores que n, o tamanho mínimo determinado para cada espécie
data3 <- filter(data, Amb == "Pequeno" & CP > n)



## Modelos Mistos Aditivos Generalizados (Generalized Additive Mixed Models - GAMMs)

model.data.cp.gamm <- gamm(CP ~ s(year), random = list(Amb=~1), data = data) ## modelo gamms para o comprimento padrão (CP)
summary(model.data.cp.gamm$gam)

model.data.p.gamm <- gamm(P ~ s(year), random = list(Amb=~1), data = data) ## modelo gamms para o peso (P)
summary(model.data.p.gamm$gam)



## Modelos Aditivos Generalizados (Generalized Additive Models - GAMs)

model.data2.cp.gam <- gam(CP ~ s(year, k = n), data = data2, method = "REML", sp = n) ## k e sp variam de espécie para espécie. 
summary(model.data2.cp.gam)

model.data2.p.gam <- gam(P ~ s(year, k = n), data = data2, method = "REML", sp = n)
summary(model.data2.p.gam)



## gráficos

par(mfrow=c(2,2)) ## defini a quantidade de colunas e linhas
par(mar = c(4, 5, 2, 2)) ## defini as margens
nf <- layout( matrix(c(1,2,3,4,5,5), nrow=3, byrow=TRUE)) ## defini o layout quando tiver número impar de gráficos

title <- expression(paste(italic("nome da espécie"))) ## adiciona itálico no nome da espécie

p1 <- plot(model.data.cp.gamm$gam, rug = FALSE, residuals = TRUE, pch = 1, cex = 0.8, seWithMean = TRUE, se = TRUE, ylab = "Comprimento Padrão (cm)", xlab="Ano", 
      cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, shift = coef(model.data.cp.gamm$gam)[1], main = (title))
p1

p2 <- plot(model.data.p.gamm$gam, rug = FALSE, residuals = TRUE, pch = 1, cex = 0.8, seWithMean = TRUE, se = TRUE, ylab = "Peso (g)", xlab="Ano", 
      cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, shift = coef(model.data.p.gamm$gam)[1], main = (title))
p2

p3 <- plot(model.data2.cp.gam, rug = FALSE, residuals = TRUE, pch = 1, cex = 0.8, seWithMean = TRUE, se = TRUE, ylab = "Comprimento Padrão (cm)", xlab="Ano", 
      cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, shift = coef(model.data2.cp.gam)[1], main = (title))
p3

p4 <- plot(model.data2.p.gam, rug = FALSE, residuals = TRUE, pch = 1, cex = 0.8, seWithMean = TRUE, se = TRUE, ylab = "Peso (g)", xlab="Ano", 
      cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, shift = coef(model.data2.p.gam)[1], main = (title))
p4


## Todos esses procedimentos foram repetidos para cada espécie-alvo e para cada tipo de conjunto de dados utilizado.
