#-----------------------------------------------------------------------------------------
# Author: Christian Badillo
# Date: 14/05/2025

#-----------------------------------------------------------------------------------------
# Librerías

library(tidyverse)
library(ggplot2)
library(sf)

# ----------------------------------------------------------------------------------------
# Datos
predictores <- read.delim("bmgrid.txt", sep = ",")

predictores <- predictores %>% select(-RowNames)

str(predictores)

target <- read.delim("conteos.txt")

summary(predictores)

datos <- cbind(predictores, target)
names(datos)
#----------------------------------------------------------------------------------------
# Visualizaciones
my_sf <- st_as_sf(datos[, c("lon", "lat")], coords = c('lon', 'lat'))

ggplot(data = my_sf) + 
    geom_sf(aes(color = datos$incendios)) + 
    scale_color_gradient2(low = "#C6E297", mid = "#E9BA31", high = "#FF0000", midpoint = 3)+
    labs(
        title = "Número de Incendios Registrados",
        x = "Longitud",
        y = "Latitud",
        color = "Conteo"
    )

ggplot(data = my_sf) + 
    geom_sf(aes(color = datos$elevation)) + 
    scale_color_gradient2(low = "#ACE6DE", mid = "#D4AF7D", high = "#654500", midpoint = 1288)+
    geom_sf(
        data = my_sf[datos$incendios > 0, ],  # Filtramos los puntos
        shape = 17,           # Código de forma para triángulo
        color = "red",        # Color rojo
        size = datos$incendios[datos$incendios > 0] / 1.35,              # Tamaño ajustable
        alpha = 0.35
    ) +
    labs(
        title = "Número de Incendios Registrados",
        x = "Longitud",
        y = "Latitud",
        color = "Elevación"
    )

ggplot(data = my_sf) + 
    geom_sf(aes(color = datos$vegetation)) + 
    scale_color_gradient2(low = "#D4AF7D", mid = "#BBD971", high = "#5CA141", midpoint = 15.00)+
    geom_sf(
        data = my_sf[datos$incendios > 0, ],  # Filtramos los puntos
        shape = 17,           # Código de forma para triángulo
        color = "red",        # Color rojo
        size = datos$incendios[datos$incendios > 0] / 1.35,              # Tamaño ajustable
        alpha = 0.35
    ) +
    labs(
        title = "Número de Incendios Registrados",
        x = "Longitud",
        y = "Latitud",
        color = "Vegetación"
    )

ggplot(data = my_sf) + 
    geom_sf(aes(color = datos$veg9)) + 
    scale_color_gradient2(low = "#D4AF7D", mid = "#BBD971", high = "#5CA141", midpoint = 5)+
    geom_sf(
        data = my_sf[datos$incendios > 0, ],  # Filtramos los puntos
        shape = 17,           # Código de forma para triángulo
        color = "red",        # Color rojo
        size = datos$incendios[datos$incendios > 0] / 1.35,              # Tamaño ajustable
        alpha = 0.35
    ) +
    labs(
        title = "Número de Incendios Registrados",
        x = "Longitud",
        y = "Latitud",
        color = "Vegetación"
    )

ggplot(data = my_sf) + 
    geom_sf(aes(color = datos$slope)) + 
    scale_color_gradient2(low = "#9C9578", mid = "#575E72", high = "#00214E", midpoint = 6)+
    geom_sf(
        data = my_sf[datos$incendios > 0, ],  # Filtramos los puntos
        shape = 17,           # Código de forma para triángulo
        color = "red",        # Color rojo
        size = datos$incendios[datos$incendios > 0] / 1.35,              # Tamaño ajustable
        alpha = 0.35
    ) +
    labs(
        title = "Número de Incendios Registrados",
        x = "Longitud",
        y = "Latitud",
        color = "Inclinación"
    )

ggplot(data = my_sf) + 
    geom_sf(aes(color = datos$exposure)) + 
    scale_color_gradient2(low = "#FFB3B5", mid = "#61D8D6", high = "#DBB9FA", midpoint = 180)+
    geom_sf(
        data = my_sf[datos$incendios > 0, ],  # Filtramos los puntos
        shape = 17,           # Código de forma para triángulo
        color = "red",        # Color rojo
        size = datos$incendios[datos$incendios > 0] / 1.35,              # Tamaño ajustable
        alpha = 0.35
    ) +
    labs(
        title = "Número de Incendios Registrados",
        x = "Longitud",
        y = "Latitud",
        color = "Inclinación"
    )
