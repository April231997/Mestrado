## Pacotes

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



## Cores

install.packages('ghibli')
library('ghibli')

par(mfrow=c(9,3))
for(i in names(ghibli_palettes)) print(ghibli_palette(i))

ghibli_palettes$SpiritedLight



## Gráficos individuais

title <- expression(paste(italic("nome da espécie"))) ## adiciona itálico

b1 <- ggplot(data, aes(x = as.factor(group_year), y = CP)) +
  geom_boxplot(fill="#B7D9F2FF", alpha=0.2, outlier.colour = "#B7D9F2FF") +
  geom_jitter(color="#1F262EFF", size=0.4, alpha=0.9) +
  ggtitle(title) +
  labs(x = "", y = " \nComprimento Padrão (cm)\n ") +
  scale_y_continuous(breaks = seq(10, 25, 3)) +
  theme_light() +
  theme(axis.text = element_text(color = "grey40"), 
        plot.title = element_text(face = "bold", color = "grey20", size = 12),
        panel.grid = element_line(colour = "white"))
b1

b2 <- ggplot(data, aes(x = as.factor(group_year), y = P)) +
  geom_boxplot(fill="#B7D9F2FF", alpha=0.2, outlier.colour = "#B7D9F2FF") +
  geom_jitter(color="#1F262EFF", size=0.4, alpha=0.9) +
  ggtitle(title) +
  labs(x = "", y = " \nPeso (g)\n ") +
  scale_y_continuous(breaks = seq(10, 25, 3)) +
  theme_light() +
  theme(axis.text = element_text(color = "grey40"), 
        plot.title = element_text(face = "bold", color = "grey20", size = 12),
        panel.grid = element_line(colour = "white"))
b2



## gráfico final 

pf <- h1 + h2 + h3 + h4 + h5 + h6 +
  plot_layout(ncol = 3) &
  plot_annotation(title = " \n   Título", 
                  subtitle = " \n   subtítulo\n \n",
                  caption = "") &
  theme_light() &
  theme(plot.subtitle = element_text(face = "bold", color = "grey20", size = 11),
        axis.text = element_text(color = "grey40"), 
        plot.title = element_text(face = "bold", color = "grey20", size = 12),
        axis.title.y = element_text(color = "grey40"),
        axis.title.x = element_text(color = "grey40"),
        panel.grid = element_line(colour = "white"))
pf

