# Mestrado

OBJETIVO:
O objetivo geral deste projeto é testar empiricamente se houve um decréscimo no tamanho médio e peso corpóreo em espécies de peixes nativos da bacia do rio Paraná, mais especificamente da porção do Alto Paraná, entre os anos de 1900 e 2018.

VARIÁVEIS:
O comprimento padrão (CP) e o peso (P) são as variáveis dependentes desse estudo. A principal variável explanatória é o ano da amostragem. Contudo, informações complementares de 
cada registro nas coleções, como o tipo de ambiente, época do ano, sub-bacia hidrográfica, apetrecho de coleta, etc, são importantes para explicar as diferenças em comprimento e 
peso observadas. 
Sabemos que existem diferenças de tamanho entre peixes de diferentes ambientes e aqui escolhemos separar em Ambiente Grande (corrego e riacho) e Pequeno (reservatórios, lagoas, 
rios). O apetrecho de coleta pode exercer um efeito seletivo importante na seleção de exemplares de maior ou menor tamanho, contudo, nem sempre essa informação está disponível nos 
registros de coleções científicas, mas quando estiver, separamos por Apetrecho seletivo e não-seletivo. A época do ano e sub-bacia hidrográfica foram recuperados usando a data de 
coleta e local de coleta como base.

ANÁLISE PLANEJADA:
Para verificar as associações entre as variáveis dependentes e explanatórias serão desenvolvidos Modelos Mistos Aditivos Generalizados (Generalized Additive Mixed Models -
GAMMs) simples e múltiplos. Embora a obtenção das amostras não envolva efeitos de agrupamento, esse tipo de modelagem será adotado porque permite incorporar estruturas de autocorrelação espacial (no caso, influência das sub-bacias) e, também, evidenciar relações não  lineares entre as variáveis envolvidas. Desse modo, o comprimento e o peso serão 
modelados às explanatórias, considerando cinco estruturas de correlação espacial: exponencial, gaussiana, linear, racional e esférica. Para a escolha dos modelos, serão usados os 
menores valores de AIC (Akaike Information Criterion), o maior valor do coeficiente de determinação ajustado (R2 ajustado) e os valores de significância entre as variáveis 
independentes e dependente (P < 0.05 como critério para o nível de significância). Todos os procedimentos das modelagens serão realizados com as funções disponíveis nos 
pacotes “mgcv” versão 1.8–16 e “nlme” para o programa R.
