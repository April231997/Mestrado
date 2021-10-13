ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()
Salminus <- read.table("Salminus.txt", header=T)
Salminus

library('mgcv')

model1 <- gam(CP ~ s(ANO), data = Salminus, method = 'REML')
summary(model1)
plot(model1)
