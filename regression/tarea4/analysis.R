library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)

### Datos.
data <- read.csv("car data.csv")

data.m <- data %>% select(-Car_Name)

# Visualización de las relaciones.
ggplot(data.m, aes(x = as.factor(Year), y = Selling_Price, color = as.factor(Year))) + 
    geom_point() + theme_minimal() + 
    labs(title = "Año vs Precio de Venta",
         x = "Año",
         y = "Precio de Venta",
         color = "",
         fill = "") +
    theme(legend.position = "none")

ggplot(data.m, aes(x = Kms_Driven, y = Selling_Price, colour = Selling_Price)) + 
    geom_point(size = 3.5, alpha = 0.45) + 
    theme_minimal() + 
    labs(title = "Kms vs Precio de Venta",
         x = "Kilometraje",
         y = "Precio de Venta",
         color = "",
         fill = "") +
    scale_x_continuous(labels = scales::comma) +
    scale_colour_gradient( low = "yellowgreen", high = "red") +
    theme(legend.position = "none")

ggplot(data.m, aes(x = Present_Price, y = Selling_Price, colour = Selling_Price)) + 
    geom_point(size = 4, alpha = 0.8) +
    theme_minimal() + 
    labs(title = "Precio Actual vs Precio de Venta",
         x = "Precio Actual",
         y = "Precio de Venta",
         color = "",
         fill = "") +
    scale_x_continuous(labels = scales::comma) +
    scale_colour_gradient( low = "lightblue", high = "orange") +
    theme(legend.position = "none")

ggplot(data.m, aes(y = Selling_Price, x = as.factor(Owner))) +
    geom_violin(fill = "lightblue", color = "blue") +
    geom_boxplot( fill = "white", color = "blue", width = 0.1, outlier.colour = "red") +
    theme_minimal() +
    labs(title = "Dueños vs Precio de Venta",
         x = "Dueños",
         y = "Precio de Venta"
         ) +
    theme(legend.position = "none")

ggplot(data.m, aes(x = Fuel_Type, y = Selling_Price)) + 
    geom_boxplot(fill = "yellowgreen", color = "black", 
                 outlier.colour = "red", alpha = 0.5) +
    theme_minimal() + 
    labs(title = "Tipo de Combustible vs Precio de Venta",
        x = "Tipo de Combustible",
        y = "Precio de Venta"
        ) +
    theme(legend.position = "none")

ggplot(data.m, aes(x = Seller_Type, y = Selling_Price)) +
    geom_boxplot(fill = "lightblue", color = "blue", 
                 outlier.colour = "red", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Tipo de Vendedor vs Precio de Venta",
         x = "Tipo de Vendedor",
         y = "Precio de Venta"
         ) +
    theme(legend.position = "none")

ggplot(data.m, aes(x = Transmission, y = Selling_Price)) +
    geom_violin(fill = "lightblue", color = "blue") +
    geom_boxplot( fill = "white", color = "blue", 
                  width = 0.06, outlier.colour = "red") +
    theme_minimal() +
    labs(title = "Tipo de Transmisión vs Precio de Venta",
         x = "Tipo de Transmisión",
         y = "Precio de Venta"
         ) +
    theme(legend.position = "none")




modelo <- lm(Selling_Price ~ ., data = data.m)
