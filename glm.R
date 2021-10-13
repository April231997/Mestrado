ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()

Salminus <- read.table("Salminus.txt", header=T)
Salminus 
names(Salminus)

## Separar apenas o de ambiente grande ##
SalminusCPG <- Salminus[Salminus$Ambiente=='Grande',]
SalminusCPG

model.1 <- glm(CP ~ ANO, data = SalminusCPG, family = poisson)  ## Poisson é para contagem. Qual usar para comprimente e peso? ##
summary(model1)

## Separar apenas o de ambiente pequeno ##
SalminusCPP <- Salminus[Salminus$Ambiente=='Pequeno',]
SalminusCPP

model2 <- glm(CP ~ ANO, data = SalminusCPP, family = poisson)
summary(model2)
 
  
## sem separação por ambiente ##  
glm01 <- glm(CP ~ ANO + Ambiente + ANO:Ambiente, family = poisson, data = Salminus)
summary(glm01)

glm02 <- glm(CP ~ ANO + Ambiente, family = poisson, data = Salminus)
summary(glm02)

anova(glm01, glm02, test = "Chisq")



glm03 <- glm(CP~ANO, data=Salminus, family=quasi(link=power(0.3333)))
glm04 <- glm(CP~ANO, data=Salminus, family=quasi(link=log))
anova(model3,model4)
plot(model3)
plot(model4)
