##################
# Script para estimar las Covarianzas

library(ggplot)
library(tidyverse)

data.raw <- read.csv("imputacion_completa.csv", header = TRUE)

View(data.raw)

columns <- names(data.raw)
columns

subset.columns <- c("Niveles.de.educacion", "Desercion.escolar", "Anios.promedio.de.escolaridad",
                    "Satisfaccion.con.tiempo.para.ocio", "Poblacion.ocupada.trabajando.mas.de.48.horas",
                    "Gini.del.ingreso.disponible.de.los.hogares.per.capita")

data <- data.raw %>%
    select(subset.columns)

head(data)

##############################################################################
# Podemos obtener la matrix de correlación como:
mat.corr.data <- matrix(cor(data, metho="pearson"), 6, 6)
mat.corr.data

# O podemos obtener la matriz centradora.
n <- nrow(data)
J <- diag(n) - 1/n * matrix(rep(1, n), n, n)
# Ahora premultiplicamos esto por los datos.
centered.data.mat <- matrix(J, n, n) %*% as.matrix(data, 33, 6)
# La varianza de cada variable se obtiene como:
var.cent.data <- t(as.matrix(centered.data.mat, 33, 6)) %*% as.matrix(centered.data.mat, 33, 6)
var.cent.data <- as.matrix(var.cent.data, 6, 6) * 1/n
var.cent.data
## Para comparar
var(data)

###############################################################
# Vector de medias, esto se puede hacer con
mean.vector <- apply(data, 2, mean)
mean.vector
