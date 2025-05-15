# Preparación de datos

#----------------------------------------------------------------
# Cargamos las librerías necesarias
library(tidyverse)


data.conteo <- read.delim("MUESTRA_ELECCIONES_FEDERALES_2018.txt", sep ="|", skip = "1")

data <- data.conteo %>%
    select(-c(MODIFICADO, ORIGEN_CAPTURA, SEGUNDOS, MINUTOS, HORA, DIA, MES, ANIO,
              ID_AREA_RESPONSABILIDAD, ESTRATO_F, ESTRATO_L, ID_ESTRATO_F, ID_ESTRATO_L,
              ID_DIST_LOC, SECCION, TIPO_SECCION, ID_CASILLA, TIPO_CASILLA, EXT_CONTIGUA,
              ID_MUNICIPIO))

names(data) <- tolower(names(data))

names(data)

str(data)

data <- data %>%
    # Convertimos a factores los id
    mutate(across(c(id_estado, id_distrito_federal), ~as.factor(.x))) %>%
    # Convertimos a numérico las demás
    mutate(across(c(-id_estado, -id_distrito_federal), ~as.numeric(.x)))

str(data)

idx_to_change <- which(data$lista_nominal < data$total)

# Reemplazamos los indices en lista nominal lo que tienen en total
data$lista_nominal[idx_to_change] <- data$total[idx_to_change]

# Comprobamos
which(data$lista_nominal < data$total)

# Ahora creamos una variable para la participación ciudadana
data <- data %>%
    mutate(participacion = total/lista_nominal)


summary(data$participacion)


#-----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(knitr)
library(sf)

mex.states <- read_sf("conjunto_de_datos/areas_geoestadisticas_estatales.shp")

mex.states <- mex.states %>%
    arrange(CVE_ENT)

# Mapa de la participación ciudadana
data.mapa <- data %>%
    group_by(id_estado) %>%
    summarise(participacion = mean(participacion, na.rm = TRUE)) 

ggplot(mex.states) +
    geom_sf(aes(fill = data.mapa$participacion), color = "black") +
    scale_fill_gradient2(low = "#BE2A3E", mid = "#F5CA63", high = "#22763F", na.value = "white",
                         midpoint = 0.5,
                        labels = scales::percent_format(suffix = "%"),
                        limits = c(0, 1)) +
    theme_minimal() +
    labs(title = "Participación Ciudadana",
         subtitle = "Conteo rápido de las elecciones federales de 2018.",
         caption = "Promedio de participación ciudadana en el conteo rápido de las elecciones federales de 2018.",
         x = "",
         y = "",
         fill = ""
    ) + 
    theme_minimal() + 
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank())

#-----------------------------------------------------------------
# Mapa del partido ganador en el conteo rápido por estado

# Creamos los datos en proporciones.
data.votos <- data %>%
    select(-c(lista_nominal, id_distrito_federal, participacion, 
              jhrc, amlo, jamk, rac, cnr, nulos, total)) %>%
    # Agrupamos por estado
    group_by(id_estado) %>%
    # Sumamos los votos por partido
    summarise_all(sum, na.rm = TRUE)

ganador_idx <- apply(data.votos, 1, function(x) which.max(x))
ganadores <- names(data.votos)[ganador_idx]

# Hacemos el mapa
ggplot(mex.states) +
    geom_sf(aes(fill = ganadores), color = "black") +
    theme_minimal() +
    labs(title = "Partido Ganador",
         subtitle = "Conteo rápido de las elecciones federales de 2018.",
         caption = "",
         x = "",
         y = "",
         fill = ""
    ) + 
    theme_minimal() + 
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank())
