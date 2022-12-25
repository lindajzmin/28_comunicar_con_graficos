
# CLUB DE LECTURA: R PARA CIENCIA DE DATOS
# #Capítulo 28: Comunicar con gráficos
# https://www.kaggle.com/
# Autora: Linda Cabrera (@lindajzmin)


## Cargar las librerías
library(tidyverse)
library(ggrepel)
library(RColorBrewer)

## Cargar los datos
fifa21 <- read_rds('./data/fifa21a.RDS')

## Son demasiados nombres y no estético, se pierde el gráfico, porque las etiquetas de superponen entre sí
## Filtro para visualizar los jugadores que ganan más de cada equipo
## Creo un data frame con los jugadores mejores pagados de cada Club
mejor_pagado <- fifa21 %>%
    group_by(Club) %>%                      # agrupamos por Club
    filter(row_number(desc(Sueldo)) == 1)   # ordenamos de forma descendente por sueldo y filtramos el primer jugador de cada grupo 

##  Gráfico completo
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo/1000)) +     #bajar la escala del sueldo a miles
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2, color = "white") +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, segment.color = "white", segment.size = 0.7, box.padding = 2) +
    scale_y_continuous(breaks = seq(10, 50, by = 10), limits = c(15, 45)) +     #eje y
    scale_x_continuous(breaks = seq(55, 95, by = 5), limits = c(55, 95)) +      #eje x
    scale_size(                                      #modifica la escala de la variable sueldo
        breaks = floor(seq(1, 600, length.out = 5)), #los límites del 1 al 600, lo divide en 5 grupos
        limits = c(1, 600),                          #los límites de los valores
        range = c(2, 25),                            #radio de los puntos
        labels = function(x) {                       #para la redacción en la leyenda
            paste0("€", x, "K")                 
        }) +
    labs(title = "EDAD vs RENDIMIENTO DE LOS FUTBOLISTAS",
         subtitle = "La influencia de la edad en el rendimiento de los futbolistas según FIFA 21",
         caption = "Club de Lectura | R para Ciencia de Datos | @RLadiesBquilla, @rladiesgps, @RLadiesGye, @Rladies_Milagro",
         x = "R E N D I M I E N T O",
         y = "E D A D",
         size = "SALARIO",
         colour = "EQUIPOS") +
    theme(text = element_text(family = "Arial",
                              face = "italic",
                              colour = "white",
                              size = 13),                        #elementos del texto de todo el gráfico
          axis.text = element_text(color = "white"),             #texto de los ejes
          axis.ticks = element_line(color = "white"),            #las piquitos en cada axis
          plot.title = element_text(family = "Arial",
                                    face = "bold.italic",
                                    size = 20),                  #formato del título del gráfico
          plot.subtitle = element_text(size = 18),               #formato del subtítulo del gráfico
          plot.background = element_rect(fill = "black"),        #fondo de todo el gráfico
          legend.position = "bottom",                            #posición de las leyendas en la parte inferior
          legend.box = "vertical",                               #se acomodaron la leyenda de equipos y salarios de forma vertical
          legend.background = element_blank(),                   #fondo de las leyendas
          legend.key = element_rect(fill = "black",
                                    color = "black"),            #key es la base de las geometrías de las leyendas
          panel.background = element_rect(fill = "black"),       #fondo del plano del gráfico
          panel.grid.major = element_line(colour = "dimgrey"),   #formato de red principal
          panel.grid.minor = element_blank(),                    #formato de red secundaria
          plot.caption = element_text(size = 10)) +              #formato del caption
    scale_color_manual(values = c(
        "#e6b5c6",
        "#7842ed",
        "#203b9c",
        "#9b253d",
        "#e31e75",
        "#704e8f",
        "#af4beb",
        "#3ee5d6")) +
    guides(color = guide_legend(override.aes = list(size = 8)))
    

### GUARDAR EL GRÁFICO
ggsave("grafico1.JPEG", width = 18, height = 12)
ggsave("grafico1.PNG", width = 18, height = 12)
