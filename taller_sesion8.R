
# CLUB DE LECTURA: R PARA CIENCIA DE DATOS
# #Capítulo 28: Comunicar con gráficos
# https://www.kaggle.com/
# Autora: Linda Cabrera (@lindajzmin)


## Cargar las librerías
library(tidyverse)
library(datos)
library(ggplot2)
library(ggrepel)
library(viridis)

## Cargar librería de las fuentes
library(extrafont)   #para cargar tipos de letras
loadfonts(device = "win", quiet = TRUE) 
fonts()

#si no cargan las fuentes, probar con esto:
library(remotes)
library(showtext)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()

## Cargar los datos

fifa21 <- read_rds('./data/fifa21a.RDS')
names(fifa21)


## GRAFICO 1: EDAD vs RENDIMIENTO DE LOS JUGADORES
## Construiremos el gráfico capa a capa.

### PASO 1: DATA Y GEOMS
##  Creo una gráfica con ggplot, determinamos el dataframe y asignaciones estéticas 
ggplot(fifa21, aes(y = Edad, x = overall))
    
##  No hay información, únicamente se han dibujado los ejes, por lo que debemos agregar una capa que me indique la geometría del gráfico que deseo.
ggplot(fifa21, aes(y = Edad, x = overall)) +
    geom_point()

## Agrego una nueva variable que es el sueldo y se reflejará en el tamaño de los puntos
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point()

## Si quiero visualizar el equipo al que pertenecen, voy a manipular el color de los puntos.
## Agrego un alpha, para la transparencia de los puntos, unos se trasponen con otros
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.6)


### PASO 2: ANOTACIONES
## Me interesa agregar nombres a los puntos para saber de qué futbolista trata, lo hago con otra geometría geom_text()
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_text(aes(label = Nombre_Jugador))

## Son demasiados nombres y no estético, se pierde el gráfico, porque las etiquetas de superponen entre sí
## Filtro para visualizar los jugadores que ganan más de cada equipo
## Creo un data frame con los jugadores mejores pagados de cada Club
mejor_pagado <- fifa21 %>%
    group_by(Club) %>%                      # agrupamos por Club
    filter(row_number(desc(Sueldo)) == 1)   # ordenamos de forma descendente por sueldo y filtramos el primer jugador de cada grupo 

## Agrego en geom_text(), el dataframe de mejor_pagado
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_text(data = mejor_pagado, aes(label = Nombre_Jugador), alpha=0.7, colour = "purple", family="Nunito", fontface = "italic", size = 4)

## Existe otra forma: con la geometría geom_label(), auqnue igual subsiste el problema de que superponen las etiquetas
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_label(data = mejor_pagado, aes(label = Nombre_Jugador), alpha=0.7, colour = "purple", family="Nunito", fontface = "italic", size = 4, nudge_y = -2, nudge_x = 2)

## Existe otra forma: con la librería ggrepel tenemos la opción de agregar etiquetas
## max.overlaps = Inf para que aparezcan todas las etiquetas, cuando no aparecen todas
## box.padding = 2, segment.color = "black" otros atributos (líneas que unen al punto con la etiqueta)
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black")
  
## Puedo agregar una segunda capa para resaltar a los jugadores mejor pagados  
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black")

## Puedo resaltar un poco más, aumentando el grosor de los bordes  
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black")


### PASO 3: ETIQUETAS
##  Con labs() agrego etiquetas para el título, subtítulo, ejes y leyendas
##  Recuerda: Evita poner nombres al gráfico como "gráfico de dispersión" o "gráfico de puntos"
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
    labs(title = "EDAD vs RENDIMIENTO DE LOS FUTBOLISTAS",
     subtitle = "La influencia de la edad en el rendimiento de los futbolistas según FIFA 21",
     caption = "Club de Lectura | R para Ciencia de Datos | @RLadiesBquilla, @rladiesgps, @RLadiesGye, @Rladies_Milagro",
     x = "R E N D I M I E N T O",
     y = "E D A D",
     size = "SALARIO",
     colour = "EQUIPOS")


### PASO 4: ESCALAS
##  Puedo eliminar las escalas de los ejes
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
    scale_y_continuous(label = NULL) +     #eje y
    scale_x_continuous(label = NULL) +      #eje x
    labs(title = "EDAD vs RENDIMIENTO DE LOS FUTBOLISTAS",
         subtitle = "La influencia de la edad en el rendimiento de los futbolistas según FIFA 21",
         caption = "Club de Lectura | R para Ciencia de Datos | @RLadiesBquilla, @rladiesgps, @RLadiesGye, @Rladies_Milagro",
         x = "R E N D I M I E N T O",
         y = "E D A D",
         size = "SALARIO",
         colour = "EQUIPOS")

##  Modifico las escalas del eje x y eje y
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo)) +
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
    scale_y_continuous(breaks = seq(10, 50, by = 10), limits = c(15, 45)) +     #eje y
    scale_x_continuous(breaks = seq(55, 95, by = 5), limits = c(55, 95)) +      #eje x
    labs(title = "EDAD vs RENDIMIENTO DE LOS FUTBOLISTAS",
         subtitle = "La influencia de la edad en el rendimiento de los futbolistas según FIFA 21",
         caption = "Club de Lectura | R para Ciencia de Datos | @RLadiesBquilla, @rladiesgps, @RLadiesGye, @Rladies_Milagro",
         x = "R E N D I M I E N T O",
         y = "E D A D",
         size = "SALARIO",
         colour = "EQUIPOS")


##  También puedo modificar las escalas de las geometrías (sólo para geom_point() y geom_text())
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo/1000)) +     #bajar la escala del sueldo a miles
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
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
         colour = "EQUIPOS")


### PASO 5: TEMAS
##  Puedo usar temas predefinidos
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo/1000)) +     #bajar la escala del sueldo a miles
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
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
    theme_dark()

##  Puedo manipular el tema a mi preferencia
ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo/1000)) +     #bajar la escala del sueldo a miles
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
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
      plot.caption = element_text(size = 10))                #formato del caption


### PASO 6: PALETA DE COLORES
##  Paleta de Erich Neuwirth

library(RColorBrewer)

RColorBrewer::display.brewer.all()

ggplot(fifa21, aes(y = Edad, x = overall, size = Sueldo/1000)) +     #bajar la escala del sueldo a miles
    geom_point(aes(color = Club), alpha= 0.7) +
    geom_point(shape = 21, data = mejor_pagado, stroke = 2) +
    ggrepel::geom_label_repel(data = mejor_pagado, aes(label = Nombre_Jugador), alpha = 0.7, max.overlaps = Inf, size = 4, fill = "gray87", color = "black") +
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
    scale_colour_brewer(palette = "Set1")


##  Puedo agregar una escala de colores de forma manual con scale_color_manual()
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
        "#3ee5d6"))


##  Guías por cada escala con guides()
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
    

### PASO 7: GUARDAR EL GRÁFICO
ggsave("grafico1.JPEG", width = 18, height = 12)
ggsave("grafico1.PNG", width = 18, height = 12)
