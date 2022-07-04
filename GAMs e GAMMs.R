## Filtrar o conjunto de dados

data <- read.table("espécie", header = T) 
data1 <- filter(data, CP > n)  ## filtra os exemplares com tamanho maiores que n, o tamanho mínimo determinado para cada espécie
data2 <- filter(data, Amb == "Grande" & CP > n)
data3 <- filter(data, Amb == "Pequeno" & CP > n)



## Modelos Mistos Aditivos Generalizados (Generalized Additive Mixed Models - GAMMs)

model.data.cp.gamm <- gamm(CP ~ s(year), random = list(Amb=~1), data = data) ## modelo gamms para o comprimento padrão (CP)
summary(model.data.cp.gamm$gam)

model.data.p.gamm <- gamm(P ~ s(year), random = list(Amb=~1), data = data) ## modelo gamms para o peso (P)
summary(model.data.p.gamm$gam)


title <- expression(paste(italic("nome da espécie"))) ## adiciona itálico no nome da espécie
p1 <- plot(model.data.cp.gamm$gam, rug = FALSE, residuals = TRUE, pch = 1, cex = 0.8, seWithMean = TRUE, se = TRUE, ylab = "Comprimento Padrão (cm)", xlab="Ano", 
      cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, shift = coef(model.data.cp.gamm$gam)[1], main = (title))
p1

p2 <- plot(model.data.p.gamm$gam, rug = FALSE, residuals = TRUE, pch = 1, cex = 0.8, seWithMean = TRUE, se = TRUE, ylab = "Peso (g)", xlab="Ano", 
      cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, shift = coef(model.data.p.gamm$gam)[1], main = (title))
p2



## Modelos Aditivos Generalizados (Generalized Additive Models - GAMs)

