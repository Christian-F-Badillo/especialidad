# R Script for Factor Anlysis.
# Date: 11/18/2024
# Author: Christian Badillo

# Load Data.
data <- read.csv("spain_teams_player_data.csv")

# Load Libraries.
library(stats)
library(tidyverse)
library(plotly)

head(data)

# Clean data.
names.players <- data$Jugador
club.players <- data$Club

new.data <- data %>%
    select(!c(player, club, positions, country, value))

new.data <- data %>%
    select(!c(Jugador, Pais, Club, Posicion, Valor))

positions <- c()
data.pos <- data$Posicion

for(i in 1:length(data.pos)){
    pos <- unlist(strsplit(data.pos, fixed = T, split = ",")[i][1])
    positions[i] <- unlist(strsplit(pos, fixed = F, split = "'"))[2]
}

data$Posicion <- positions

# Análisis Factorial (Ortogonal)
fit <- factanal(x = new.data, factors = 5, rotation = "varimax")

print(fit, digits=2, cutoff=.3, sort=TRUE)

# Análisis Factorial (No Otorgonal)
fit2 <- factanal(x = new.data, factors = 5, rotation = "promax")

print(fit2, digits=2, cutoff=.3, sort=TRUE)

# Visualizations.

# Extraer la matriz de cargas factoriales del modelo
cargas <- as.matrix(unclass(fit$loadings))

# Convertir la matriz en un data frame largo para `plotly`
df <- reshape2::melt(cargas, value.name = "value")

p <- ggplot(df, aes(Var2, Var1)) +
    geom_raster(aes(fill=value)) +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    labs(x="Factores",
         y="Variables",
         title = "Cargas Factoriales (Varimax)",
         fill = "Carga") +
    theme(text = element_text(family = 'Fira Sans'),
          plot.title = element_text(hjust = 0.5))
p

#ggplotly(p)

#--------------------------------------------------------------------------------------------------

# Extraer la matriz de cargas factoriales del modelo
cargas2 <- as.matrix(unclass(fit2$loadings))

# Convertir la matriz en un data frame largo para `plotly`
df2 <- reshape2::melt(cargas2, value.name = "value")

p2 <- ggplot(df2, aes(Var2, Var1)) +
    geom_raster(aes(fill=value)) +
    scale_fill_distiller(palette = "Spectral", direction = -1) +
    labs(x="Factores",
         y="Variables",
         title = "Cargas Factoriales (Promax)",
         fill = "Carga") +
    theme(text = element_text(family = 'Fira Sans'),
          plot.title = element_text(hjust = 0.5))
p2
#ggplotly(p2)

#------------------------------------------------------------------
df.uniq <- reshape2::melt(as.matrix(fit$uniquenesses))

uni.p <- ggplot(df.uniq, aes(x = Var1, y = value)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
        title = "Unicidades de las Variables (Varimax)",
        x = "Variable",
        y = "Unicidad"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(uni.p)



df.uniq2 <- reshape2::melt(as.matrix(fit2$uniquenesses))

uni.p2 <- ggplot(df.uniq2, aes(x = Var1, y = value)) +
    geom_bar(stat = "identity", fill = "pink") +
    labs(
        title = "Unicidades de las Variables (Promax)",
        x = "Variable",
        y = "Unicidad"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(uni.p2)

fit$method
