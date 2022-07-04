## Teste de Rosner's para a identificação e retirada de outliers

## pacote necessário

library("EnvStats")


rosnerTest(data$CP, k = 3) ## identifica os outliers no comprimento padrão

rosnerTest(data$P, k = 3) ## identifica os outliers no peso

data <- data[-c(out1, out2, out3,...), ] ##remove os outliers
