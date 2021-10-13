ls()
getwd()
setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
dir()
Salminus <- read.table("Salminus.txt", header=T)
Salminus 
names(Salminus)
plot(ANO, CP, data = Salminus)

Salminus$Ambiente
Salminus$Ambiente <- as.factor(Salminus$Ambiente)

SalminusPG <- Salminus[Salminus$Ambiente=='Grande',]
SalminusPG

model.glm.salminusPG <- glm(P ~ ANO, data = SalminusPG, family = poisson)
summary(model.glm.salminusPG)


SalminusPP <- Salminus[Salminus$Ambiente=='Pequeno',]
SalminusPP

model.glm.salminusPP <- glm(P ~ ANO, data = SalminusPP, family = poisson)
summary(model.glm.salminusPP)
## Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  contrasts can be applied only to factors with 2 or more levels      ##
  
  
  

  ls()
  getwd()
  setwd("C:/Users/beaco/OneDrive/Documentos/Mestrado/Análises")
  dir()
  Salminus <- read.table("Salminus.txt", header=T)
  Salminus
  glm01 <- glm(CP ~ ANO + Ambiente + ANO:Ambiente, family = poisson, data = Salminus)
  summary(glm01)
  glm02 <- glm(CP ~ ANO + Ambiente, family = poisson, data = Salminus)
  anova(glm01, glm02, test = "Chisq")

model1 <- glm(CP~ANO, data=Salminus, family=quasi(link=power(0.3333)))
model2 <- glm(CP~ANO, data=Salminus, family=quasi(link=log))
anova(model1,model2)
plot(model1)
plot(model2)

