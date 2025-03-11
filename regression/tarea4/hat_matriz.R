library(tidyverse)
library(ggplot2)
library(reshape2)

data <- read.csv(
    "car data.csv"
)

# Esto transforma primero todo a factores, luego a numerico y luego a matriz
# No me gustan las dummies como tal, entonces codifico cada categoria como un numero
# y hay n categorias, entonces el numero va de 1 a n en cada observacion en las
# variables categóricas.
# Si quieren le pueden cambiar para hacerlas dummies.
data.m <- data %>%
    select(-c("Car_Name")) %>%
    mutate(
        across(
            where(is.character),
            ~ factor(.x, levels = unique(.x), labels = 1:length(unique(.x)))
        )
    ) %>%
    mutate_all(as.numeric) %>%
    as.matrix()

apply(data.m, 2, class)

# Calculamos la matriz sombrerito.
hat_matrix <- data.m %*% solve(t(data.m) %*% data.m) %*% t(data.m)

# ----------------------------------------------------------------------
# Plot opcional, yo lo voy a poner, pero si gustan es opcional y le pueden 
# mover a su gusto
mat_to_plot <- melt(hat_matrix)

ggplot(mat_to_plot, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(
        x = "Observations",
        y = "Observations",
        title = "Hat Matrix"
    )
