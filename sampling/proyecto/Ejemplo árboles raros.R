# Paquete necesario para la visualización
library(ggplot2)

# Simulación de un bosque con 100 unidades (parcelas) en una cuadrícula de 10x10
set.seed(42)

# Número de parcelas y tamaño de la cuadrícula
n_unidades <- 100
n_filas <- 10
n_columnas <- 10

# Definir la probabilidad de que una unidad tenga un árbol raro (usamos una probabilidad baja)
prob_raro <- 0.05

# Simular si un árbol es raro (1 = raro, 0 = no raro)
bosque <- sample(c(0, 1), size = n_unidades, replace = TRUE, prob = c(1 - prob_raro, prob_raro))

# Crear una matriz 10x10 para representar el bosque
bosque_matrix <- matrix(bosque, nrow = n_filas, ncol = n_columnas)

# Mostrar la cuadrícula inicial del bosque
bosque_matrix

# Definir la función para expandir el muestreo
expandir_muestreo <- function(muestras, bosque_matrix, n_filas, n_columnas) {
  muestras_ampliadas <- muestras
  for (i in muestras) {
    fila <- (i - 1) %/% n_columnas + 1
    columna <- (i - 1) %% n_columnas + 1
    if (bosque_matrix[fila, columna] == 1) {  # Si encontramos un árbol raro
      # Agregar vecinos (adjasentes, por ejemplo)
      vecinos <- c(i - 1, i + 1, i - n_columnas, i + n_columnas)  # Arriba, abajo, izquierda, derecha
      vecinos <- vecinos[vecinos > 0 & vecinos <= n_unidades]  # Filtrar valores fuera del rango
      muestras_ampliadas <- unique(c(muestras_ampliadas, vecinos))
    }
  }
  return(muestras_ampliadas)
}

# Selección inicial de muestras aleatorias
muestras <- sample(1:n_unidades, size = 5)
muestras_ampliadas <- muestras

# Expansión del muestreo adaptativo
muestras_ampliadas <- expandir_muestreo(muestras_ampliadas, bosque_matrix, n_filas, n_columnas)

# Crear un data.frame para la visualización
bosque_df <- expand.grid(x = 1:n_columnas, y = 1:n_filas)
bosque_df$raro <- as.vector(bosque_matrix)
bosque_df$muestreado <- ifelse(1:n_unidades %in% muestras_ampliadas, 1, 0)

# Visualización del proceso de muestreo adaptativo
ggplot(bosque_df, aes(x = x, y = y, fill = factor(raro), color = factor(muestreado))) +
  geom_tile(show.legend = FALSE) +
  scale_fill_manual(values = c("white", "green")) +
  scale_color_manual(values = c("white", "red")) +
  geom_text(aes(label = ifelse(muestreado == 1, "X", "")), color = "black", size = 4) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
  ggtitle("Proceso de Muestreo Adaptativo en Bosque")
