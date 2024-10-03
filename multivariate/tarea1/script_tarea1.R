######## Tarea 1

# Librerias
library(ggplot2)
library(mice)

# Datos
bienestar2022 <- read.csv("bienestar2022.csv")

# Número de Datos faltantes
total_na <- sum(is.na(bienestar2022))
cat(paste("El número total de datos faltantes es: ", total_na, "\n"))

# Número de Datos faltantes por columna
na_por_columna <- apply(bienestar2022, 2, function(x) sum(is.na(x)))
na_por_columna <- data.frame(names = names(na_por_columna), na_por_columna)

# Ahora vamos encontrar su fila y columna correspondiente para encontrar a qué estados pertenecen esos valores faltantes.
cat("Posiciones de los valores faltantes:\n")
na_positions <- which(is.na(bienestar2022), arr.ind = TRUE) # devuelve un arreglo, (fila columna)

# Crear el data frame donde aparecerá primero la fila del arreglo anterior (fila) seguido de la columna correspondiente (variable)
na_dataframe <- data.frame(Fila = na_positions[, 1], Columna = colnames(bienestar2022)[na_positions[, 2]])

# Imprimir las posiciones de los NA con el arreglo (Estado, variable)
print(na_dataframe)

# Ahora, antes de realizar la imputación, haremos las distribuciones de las variables para que después
# de imputar podamos realizar la comparación y así comprobar que no se está distorsionando.

# histograma
ggplot(bienestar2022, aes(x = Calidad.de.la.red.social.de.soporte)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Calidad.de.la.red.social.de.soporte (antes de imputación)")

# gráfica de barras
#ggplot(bienestar2022, aes(x = factor(Niveles.de.educacion))) +
#  geom_bar(fill = "blue", color = "black") +
#  theme_minimal() +
#  ggtitle("Distribución de Calidad de la Red Social de Soporte (antes de imputación)") +
#  xlab("Calidad de la Red Social de Soporte")


# boxplot
#ggplot(bienestar2022, aes(y = Niveles.de.educacion)) +
#  geom_boxplot(fill = "blue", color = "black") +
#  theme_minimal() +
#  ggtitle("Boxplot de Niveles de Educación (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Niveles.de.educacion)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Niveles.de.educacion (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Poblacion.ocupada.trabajando.mas.de.48.horas)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Poblacion.ocupada.trabajando.mas.de.48.horas (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Porcentaje.de.la.poblacion.en.situacion.de.pobreza.extrema)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Porcentaje.de.la.poblacion.en.situacion.de.pobreza.extrema (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Contaminacion.del.aire)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Contaminacion.del.aire (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Participacion.civica.y.politica)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Participacion.civica.y.politica (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Salud.autorreportada)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Salud.autorreportada (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos.)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos. (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Satisfaccion.con.la.vida)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Satisfaccion.con.la.vida (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Confianza.en.la.policia)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Confianza.en.la.policia (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Tasa.de.incidencia.delictiva)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Tasa.de.incidencia.delictiva (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Tasa.de.informalidad.laboral)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Tasa.de.informalidad.laboral (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Tasa.de.desocupacion)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Tasa.de.desocupacion (antes de imputación)")

# histograma
ggplot(bienestar2022, aes(x = Porcentaje.de.viviendas.con.techos.de.materiales.resistentes)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    theme_minimal() +
    ggtitle("Distribución de Porcentaje.de.viviendas.con.techos.de.materiales.resistentes (antes de imputación)")


# Patrón de faltantes
md.pattern(bienestar2022)

# Imputación múltiple
imp <- mice(bienestar2022, seed=10, print= FALSE)
print(imp)

imp$imp$Calidad.de.la.red.social.de.soporte

imp$imp$Niveles.de.educacion

complete(imp)

completados <- complete(imp)

head(completados)

# Guardar los datos completos en formato CSV
write.csv(completados, file = "imputacion_completa.csv", row.names = TRUE)



###########################################################################
###########################################################################
#                Sección 3
###########################################################################
###########################################################################

# Ya que seleccionamos el código, ahora vamos a hacer que lo lea
imputacion_completa <- read.csv("imputacion_completa.csv")

# Vamos a ver si se logró hacer la imputación
total_na <- sum(is.na(imputacion_completa))
# Vamos a imprimirlos:
cat("El total de valores faltantes es:", total_na, "\n")

# Se logró :D
head(imputacion_completa)

#----------------- Comenzamos con los ejercicios de la sección 4 --------------
#-------------------------------------- ecdf ----------------------------------
# --Tasa de Obesidad
obesidad_ecdf <- ecdf(imputacion_completa$Tasa.de.obesidad)

plot(obesidad_ecdf, 
     main = "ecdf - Tasa de Obesidad", 
     xlab = "Tasa de Obesidad", 
     ylab = "Probabilidad", 
     col = "blue", 
     lwd = 2)

# Puntos
points(imputacion_completa$Tasa.de.obesidad, obesidad_ecdf(imputacion_completa$Tasa.de.obesidad), col = "black", pch = 19)


# --Tasa de mortalidad infantil
tdmi_ecdf <- ecdf(imputacion_completa$Tasa.de.mortalidad.infantil)

plot(tdmi_ecdf, 
     main = "ecdf - Tasa de mortalidad infantil", 
     xlab = "Tasa de mortalidad infantil", 
     ylab = "Probabilidad", 
     col = "gold", 
     lwd = 2)

# Puntos
points(imputacion_completa$Tasa.de.mortalidad.infantil, tdmi_ecdf(imputacion_completa$Tasa.de.mortalidad.infantil), col = "black", pch = 19)


# --Razón de mortalidad materna.
tdmm_ecdf <- ecdf(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos.)

plot(tdmm_ecdf, 
     main = "ecdf - Razón de mortalidad materna", 
     xlab = "Razón de mortalidad materna", 
     ylab = "Probabilidad", 
     col = "deeppink", 
     lwd = 2)

# Puntos
points(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos., tdmm_ecdf(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos.), col = "black", pch = 19)



# --Satisfacción con la vida
sclv_ecdf <- ecdf(imputacion_completa$Satisfaccion.con.la.vida)

plot(sclv_ecdf, 
     main = "ecdf - Satisfacción con la vida", 
     xlab = "Satisfacción con la vida", 
     ylab = "Probabilidad", 
     col = "green", 
     lwd = 2)

# Puntos
points(imputacion_completa$Satisfaccion.con.la.vida, sclv_ecdf(imputacion_completa$Satisfaccion.con.la.vida), col = "black", pch = 19)

# --Viviendas con acceso a servicos básicos.

viviendas_ecdf <- ecdf(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos)

plot(viviendas_ecdf, 
     main = "ecdf - Vivivendas con acceso a servicios básicos", 
     xlab = "Viviendas con acceso a servicios básicos", 
     ylab = "Probabilidad", 
     col = "indianred1", 
     lwd = 2)

# Puntos
points(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos, viviendas_ecdf(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos), col = "black", pch = 19)

#---------------------------------- boxplot --------------------------------------

boxplot(imputacion_completa$Tasa.de.obesidad, 
        main = "Boxplot - Tasa de Obesidad", 
        ylab = "Tasa de Obesidad", 
        col = "blue")


boxplot(imputacion_completa$Tasa.de.mortalidad.infantil, 
        main = "Boxplot - Tasa de Motarlidad infantil", 
        ylab = "Tasa de Mortalidad Infantil", 
        col = "gold")

boxplot(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos., 
        main = "Boxplot - Razón de Mortalidad Materna", 
        ylab = "Razón de Mortalidad Materna", 
        col = "deeppink")

boxplot(imputacion_completa$Satisfaccion.con.la.vida, 
        main = "Boxplot - Satisfacción con la vida", 
        ylab = "Satisfacción con la vida", 
        col = "green")

boxplot(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos, 
        main = "Boxplot - Viviendas con acceso a servicios básicos", 
        ylab = "Viviendas con acceso a servicios básicos", 
        col = "indianred1")

#------------------------------ Normal -----------------------------------------
qqnorm(imputacion_completa$Tasa.de.obesidad, main = "QQ-Plot - Tasa de Obesidad")
qqline(imputacion_completa$Tasa.de.obesidad, col = "black")  # Línea de referencia

qqnorm(imputacion_completa$Tasa.de.obesidad, 
       main = "QQ-Plot - Tasa de Obesidad", 
       xlab = "Cuantiles teóricos", 
       ylab = "Cuantiles de Tasa de Obesidad", 
       col = "blue", 
       pch = 19)  

qqline(imputacion_completa$Tasa.de.obesidad, col = "black", lwd = 2)



qqnorm(imputacion_completa$Tasa.de.mortalidad.infantil, 
       main = "QQ-Plot - Tasa de Mortaldiad Infantil", 
       xlab = "Cuantiles teóricos", 
       ylab = "Cuantiles de Tasa de Mortalidad Infantil", 
       col = "gold", 
       pch = 19)  # Cambiar el color y tipo de punto

qqline(imputacion_completa$Tasa.de.mortalidad.infantil, col = "black", lwd = 2)


qqnorm(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos., 
       main = "QQ-Plot - Razón de Mortaldiad Materna", 
       xlab = "Cuantiles teóricos", 
       ylab = "Cuantiles de Razón de Mortalidad Materna", 
       col = "deeppink", 
       pch = 19)

qqline(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos., col = "black", lwd = 2)


qqnorm(imputacion_completa$Satisfaccion.con.la.vida, 
       main = "QQ-Plot - Satisfacción con la vida", 
       xlab = "Cuantiles teóricos", 
       ylab = "Cuantiles de Satisfacción con la vida", 
       col = "green", 
       pch = 19)

qqline(imputacion_completa$Satisfaccion.con.la.vida, col = "black", lwd = 2)

qqnorm(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos, 
       main = "QQ-Plot - Viviendas con acceso a servicios básicos", 
       xlab = "Cuantiles teóricos", 
       ylab = "Cuantiles de Viviendas con acceso a servicios básicos", 
       col = "indianred1", 
       pch = 19)

qqline(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos, col = "black", lwd = 2)

#------------------------------- Histograma -------------------------------------

hist(imputacion_completa$Tasa.de.obesidad, 
     main = "Histograma - Tasa de Obesidad", 
     xlab = "Tasa de Obesidad", 
     ylab = "Frecuencia", 
     col = "blue", 
     border = "black")

hist(imputacion_completa$Tasa.de.mortalidad.infantil, 
     main = "Histograma - Tasa de Mortalidad Infantil", 
     xlab = "Tasa de Mortalidad Infantil", 
     ylab = "Frecuencia", 
     col = "gold", 
     border = "black")

hist(imputacion_completa$Razon.de.mortalidad.materna..defunciones.por.cada.100.mil.nacidos.vivos., 
     main = "Histograma - Razón de Mortalidad Materna", 
     xlab = "Razón de Mortalidad Materna", 
     ylab = "Frecuencia", 
     col = "deeppink", 
     border = "black")

hist(imputacion_completa$Satisfaccion.con.la.vida, 
     main = "Histograma - Satisfacción con la vida", 
     xlab = "Satisfacción con la vida", 
     ylab = "Frecuencia", 
     col = "green", 
     border = "black")

hist(imputacion_completa$Viviendas.con.acceso.a.servicios.basicos, 
     main = "Histograma - Viviendas con acceso a servicios básicos", 
     xlab = "Viviendas con acceso a servicios básicos", 
     ylab = "Frecuencia", 
     col = "indianred1", 
     border = "black")


#----------------------- Ejercicio 5 ------------------------------------

#----------------------- Ejercicio 5 ------------------------------------
#----------------------- Curvas de Andrews ------------------------------

# Variables seleccionadas:
# 1.- Vivienda: Porcentaje.de.viviendas.con.techos.de.materiales.resistentes
# 2.- Ingresos: Gini.del.ingreso.disponible.de.los.hogares.per.capita
# 3.- Empleo: Tasa.de.condiciones.criticas.de.ocupacion
# 4.- Accesibilidad a servicios: Viviendas.con.acceso.a.servicios.basicos
# 5.- Seguridad: Tasa.de.homicidio
# 6.- Educación: Niveles.de.educacion
# 7.- Medio ambiente: Contaminacion.del.aire
# 8.- Compromiso cívico y gobernanza: Participacion.electoral
# 9.- Salud: Esperanza.de.vida.al.nacer
# 10.- Satisfacción con la vida: Satisfaccion.con.la.vida
# 11.- Balance vida-trabajo: Satisfaccion.con.tiempo.para.ocio
# 12.- Relaciones sociales en la comunidad: Calidad.de.la.red.social.de.soporte

# Preparamos nuestro conjunto de variables
# Vamos a agrupar las 12 variables mencionadas


variables_ejercicio5 <-imputacion_completa[,c(1,36,11,31,2,27,6,15,18,21,26,9,5)]
names(variables_ejercicio5)
rownames(variables_ejercicio5)

rownames(variables_ejercicio5) <- variables_ejercicio5[, 1]
rownames(variables_ejercicio5)
print(variables_ejercicio5)

#Abreviamos filas y columnas:

# Abreviacion de filas (Estados)
abreviaturas <- c("EEUM", "AGU", "BC", "BCS", "CAM", "COA", "COL", "Chis", "Chih", "CMX", "DUR", "Gto", "Gro", "HGO", "JAL", "MX", "Mich", "Mor", "NAY", "NL", "Oax", "Pue", "QRO", "QR", "SLP", "SIN", "Son", "Tab", "TAMPS", "TLX", "Ver", "Yuc", "Zac")

rownames(variables_ejercicio5) <- abreviaturas

#Vamos a abreviar las variables (columnas)
abreviaturas_variables <- c("Estados", "%VTR", "GIDH", "TCCO", "VASB", "TH", "NE", "CAire", "PElec", "EspVN", "SV", "STO", "CalRed")

colnames(variables_ejercicio5)<- abreviaturas_variables

print(variables_ejercicio5)


library(andrews)
library(RColorBrewer)

n <- nrow(variables_ejercicio5)  # Número de estados
colores <- colorRampPalette(brewer.pal(8, "Set1"))(33)
curvas<- andrews(variables_ejercicio5, type = 1, 
                 clr= colores,
                 main= "Curvas de Andrews Estados Unios Mexicanos")


# Añadir las abreviaturas a la gráfica
#for (i in 1:n) {
#Obtener la coordenada y correspondiente a la curva
#  y_coord <- curvas[i, 2]  # Ajusta el índice según sea necesario
#  text(x = 0.5, y = y_coord, labels = abreviaturas[i], cex = 0.5, pos = 1, col = colores[i])
#}




#------------------------------- Curvas paralelas -------------------------------
install.packages("GGally")
library(GGally)



# Vamos a generar el gráfico de curvas paralelas
ggparcoord(data = variables_ejercicio5, 
           columns = 2:(ncol(variables_ejercicio5)),  # Ajusta según la columa que selecciones, tomé la dos porque si no, incluía los nombres de los estados como variable
           groupColumn = "Estados", 
           scale = "globalminmax") +
    labs(title = "Gráfico de Curvas Paralelas de Estados de México") +
    theme_minimal() +
    theme(axis.text = element_text(size = 8),  # Para ajustar el tamaño del texto de los ejes
          plot.title = element_text(size = 10))  # Para ajustar el tamaño del título


#------------------------------------ Caritas de Chernoff ---------------------------
library(aplpack)

faces(variables_ejercicio5[,2:13], main= "Caras de Chernoff")

#Fijamos que las caritas no tengan sonrisa y volvemos a ver 
variables_sonrisafija <- cbind(variables_ejercicio5[,1:6], rep(0, length(variables_ejercicio5$Estados)), variables_ejercicio5[,7:13])

faces(variables_sonrisafija[,2:14], 
      face.type=1,
      print.info= TRUE,
      cex = 1.6,
      main= "Caras de Chernoff",
)

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

