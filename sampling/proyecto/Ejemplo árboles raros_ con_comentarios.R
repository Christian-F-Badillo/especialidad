# Paquete necesario para la visualización
library(ggplot2)

# Comenzamos simulando un bosque de 100 parcelas
# Fijamos una semilla
set.seed(42)

# Fijamos el número de parcelas y tamaño de la cuadrícula
n_unidades <- 100
n_filas <- 10
n_columnas <- 10

# Se define una probabilidad (baja) de que una unidad tenga un árbol de interés con alguna 
# característica rara o difícil de encontrar, se define una baja para que sean pocas las
# parcelas con ests características.

prob_raro <- 0.05

# Llamaremos a estos árboles como "raros"
# Por tanto simularemos si un árbol es raro con: 1 = raro, 0 = no raro
bosque <- sample(c(0, 1), size = n_unidades, replace = TRUE, prob = c(1 - prob_raro, prob_raro))

# Se representará el bosque con una matriz 10x10
bosque_matrix <- matrix(bosque, nrow = n_filas, ncol = n_columnas)
bosque_matrix

# Definir la función para expandir el muestreo
expandir_muestreo <- function(muestras, bosque_matrix, n_filas, n_columnas) {
  muestras_ampliadas <- muestras
  for (i in muestras) {
    fila <- (i - 1) %/% n_columnas + 1
    columna <- (i - 1) %% n_columnas + 1
    if (bosque_matrix[fila, columna] == 1) {  # Si encontramos un árbol raro
      # Entonces se agregarán vecinos
      vecinos <- c(i - 1, i + 1, i - n_columnas, i + n_columnas)  # En todas las direcciones: Arriba, abajo, izquierda, derecha
      vecinos <- vecinos[vecinos > 0 & vecinos <= n_unidades]  # Filtrar valores fuera del rango
      muestras_ampliadas <- unique(c(muestras_ampliadas, vecinos))
    }
  }
  return(muestras_ampliadas)
}

# Se seleccionan inicialmente muestras aleatorias
muestras <- sample(1:n_unidades, size = 5)
muestras_ampliadas <- muestras

# Expandemos el muestreo adaptativo
muestras_ampliadas <- expandir_muestreo(muestras_ampliadas, bosque_matrix, n_filas, n_columnas)
muestras_ampliadas <- expandir_muestreo(muestras_ampliadas, bosque_matrix, n_filas, n_columnas)

# Se crea un data.frame para la visualización
bosque_df <- expand.grid(x = 1:n_columnas, y = 1:n_filas)
bosque_df$raro <- as.vector(bosque_matrix)
bosque_df$muestreado <- ifelse(1:n_unidades %in% muestras_ampliadas, 1, 0)

# Visualizar el muestreo adaptativo.
ggplot(bosque_df, aes(x = x, y = y, fill = factor(raro), color = factor(muestreado))) +
  geom_tile(show.legend = FALSE) +
  scale_fill_manual(values = c("white", "green")) +
  scale_color_manual(values = c("white", "red")) +
  geom_text(aes(label = ifelse(muestreado == 1, "X", "")), color = "black", size = 4) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
  ggtitle("")

# En verde se observan las parcelas con árboles "raros", es decir, dentro de esas parcelas
# se encuentra algún árbol con las características buscadas o que sea difícil de encontrar.
# Al colocar una probabilidad del 5% (0.05) se asegura que sean pocas las parcelas que
# contengan un árbol raro.

# Por otro lado las parcelas en color rojo contienen una X, lo que indica que han sido muestreadas,
# inicialmente se seleccionan 5 parcelas de manera aleatoria, sin considerar si tienen un árbol "raro".
# Ahora, en el caso de que una de esas parcelas cuente con un árbol raro, el muestreo se expande, de tal
# forma que las parcelas que se encuentren arriba, abajo, a la izquierda o a la derecha, también son muestreadas
# y se agregan las unidades.

# Al expandir el muestreo,se pueden encontrar agrupamientos o clusters al rededor de parcelas con los árboles
# de interés, además, al incluir las parcelas vecinas, aumenta la cantidad de muestras
# y se mejora la representación de las unidades que contengan árboles "raros".

