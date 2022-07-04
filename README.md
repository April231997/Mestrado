# Mestrado

OBJETIVO:
O objetivo geral desta pesquisa é avaliar se espécies de peixes nativas da bacia do Alto Rio Paraná apresentaram um decréscimo no comprimento padrão e peso corpóreo entre os anos de 1950 e 2019. Para isso, foram obtidas variáveis de exemplares importantes para a pesca da região depositados em coleções científicas. 
A ausência de alteração no comprimento padrão e peso ao longo do tempo foi a hipótese nula testada no presente trabalho. Em contrapartida, os resultados esperados era uma tendência de redução nas dimensões corporais das espécies-alvo, fundamentando os relatos de SBS e, também, fornecendo resultados de extrema importância para identificar linhas de base apropriadas, delimitar impactos sobre o ecossistema e propor métodos de manejo ambiental adequados a fim de auxiliar tomadores de decisão.


VARIÁVEIS:
O comprimento padrão e o peso foram as variáveis resposta desse estudo. As tomadas de dados corporais das espécies selecionadas foram realizadas com o auxílio de um  paquímetro digital (mm) e régua (cm). Seguindo Oliveira et al. [1], as medições do comprimento padrão (CP) devem ser obtidas medindo a distância da projeção anterior do focinho ou do lábio superior até a base da cauda, sempre em linha reta, nunca ao longo da curva do corpo do animal. Já para a tomada do peso (g) do animal, foi utilizada uma balança digital. 
A principal variável explanatória foi o ano da amostragem. Contudo, foram também obtidas informações sobre os tipos de ambiente nos registros da coleção, com o objetivo de avaliar se esta variável também poderia contribuir para explicar as diferenças nos comprimentos e pesos observados. Dessa forma, os peixes foram divididos em dois tipos de ambientes de acordo com Castro & Polaz [2]: ambientes pequenos (córregos e riachos) e ambientes grandes (reservatórios, lagoas e rios).
Sabemos que o apetrecho de coleta pode exercer um efeito seletivo importante nos exemplares de maior ou menor tamanho, contudo, esse tipo de informação começou a ser  registrada apenas muito recentemente (em 2003) no DZSJRP, portanto, não foi possível incluir essa variável nas análises.


ANÁLISE PLANEJADA:
Para verificar as associações entre as variáveis dependentes e explanatórias foram desenvolvidos Modelos Aditivos Generalizados (Generalized Additive Models - GAMs) e  Modelos Mistos Aditivos Generalizados (Generalized Additive Mixed Models - GAMMs). Esse tipo de modelagem foi adotada porque permite incorporar estruturas de autocorrelação espacial (no caso, o tipo de ambiente) e, também, evidenciar relações não lineares entre as variáveis envolvidas.
Assim, dois tipos de dados foram utilizados na realização das modelagens para cada espécie. Um primeiro conjunto de dados, que incluiu os dois tipos de ambientes juntos  (ambientes grandes e pequenos), para o qual realizou-se GAMMs, com o tipo de ambiente representando o efeito randômico. E um segundo tipo de dados, que incluiu apenas um tipo de ambiente de cada vez. Nesse caso, por não precisar incorporar efeitos randômicos nos modelos, realizou-se GAMs.
Antes da realização de cada modelo, foi feito o teste de Rosner’s em cada conjunto de dados para a identificação e remoção de outliers. Para a escolha dos modelos, foram usados o maior valor do coeficiente de determinação ajustado (R2 ajustado) e os valores de significância entre as variáveis independentes e dependentes (adotou se p < 0.05 como critério para o nível de significância). Todos os procedimentos das modelagens foram realizados com as funções disponíveis nos pacotes “mgcv” versão 1.8–16 [3], “nlme4” [4] e “EnvStats” [5] no ambiente R 4.2.0 [6].


REFERÊNCIAS:

[1] OLIVEIRA, Edson Fontes et al. Ecomorphological patterns of the fish assemblage in a tropical floodplain: effects of trophic, spatial and phylogenetic structures. Neotropical Ichthyology, v. 8, n. 3, p. 569-586, 2010

[2] CASTRO, Ricardo; POLAZ, Carla N. M. Peixes de pequeno porte: a porção maior e mais ameaçada da fauna megadiversa de peixes de água doce neotropicais. Biota Neotropica, v. 20, n. 1, 2020.

[3] WOOD, Simon. mgcv: Mixed GAM Computation Vehicle with GCV/AIC/REML smoothness estimation. 2012.

[4] BATES, Douglas et al. Package ‘lme4’. Version, v. 1, n. 17, p. 437, 2018.

[5] MILLARD, Steven P.; KOWARIK, Alexander; KOWARIK, Maintainer Alexander. Package ‘EnvStats’. Packag. Environ. Stat. Version, v. 2, p. 31-32, 2018.

[6] TEAM, R. Core et al. R: A language and environment for statistical computing. 2013.


