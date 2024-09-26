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

######################## Forma 2 de obtener la Matriz de Correlación ########################
# O podemos obtener la matriz centradora.
n <- nrow(data)
J <- diag(n) - 1/n * matrix(rep(1, n), n, n)
# Ahora premultiplicamos esto por los datos.
centered.data.mat <- matrix(J, n, n) %*% as.matrix(data, 33, 6)
# La varianza de cada variable se obtiene como:
cov.data <- t(as.matrix(centered.data.mat, 33, 6)) %*% as.matrix(centered.data.mat, 33, 6)
cov.data <- as.matrix(var.cent.data, 6, 6) * 1/n
cov.data
## Se puede verificar que da la misma entrada que:
# cov(data) * (n-1)/n

# Ahora obtenemos la varianza de cada variable
var.data <- diag(cov.data)
var.data
# Ahora calculamos su desviación estandar
sd.data <- sqrt(var.data)
sd.data <- as.matrix(sd.data, 1, 6)
sd.data
# Creamos una matrix cuyas diagonales sea la sd de cada var
D <- matrix(rep(0, 36), 6, 6)
for (i in 1:6) {
  for (j in 1:6) {
    if (i == j) {
      D[i, j] <- sd.data[i]
    }
  }
}
D
# Encontramos su inversa
D.inv <- solve(D)
D.inv

# Ahora obtnemos R
R <- t(D.inv)%*%cov.data%*%D.inv
R

# Veamos si R es igual a mat.corr.data con una tolerancia de 1.5e-8
all.equal(mat.corr.data, R)

###############################################################
# Vector de medias, esto se puede hacer con
mean.vector <- apply(data, 2, mean)
mean.vector

##############################################################
# Usamos el vector de medias y el de desviación estandar para estimar el CV.
sd.variables <- apply(data, 2, sd)
sd.variables
c.v <- sd.variables/mean.vector
c.v
