#------------------------------------------TAREA 2--------------------------------------
# -------------------------------------- Ejercicio 1 -----------------------------------

# Se presentan los récords nacionales (athletic.csv) de 8 diferentes pruebas de atletismo en 55 países. Los récords  recolectados corresponden a las siguientes pruebas:  
# 100m (seg)  
# 200m (seg)  
# 400m (seg)  
# 800m (min)  
# 1500m (min)  
# 5000m (min)  
# 10000m (min)  
# Marathon (min)  
#Los datos se encuentran en el archivo athletic.rda  

# 1. Observa que algunos datos están en segundos y otros en minutos. Comenta los problemas que esto  puede generar en el análisis de componentes principales.
# Utilizar variables con diferentes unidades de medición podría traer problemas en el análisis de componentes principales, ya que el análisis podría ser sencible a alguna de ellas. Por lo que es conveniente
# trabajar con los datos centrados y estandarizados, es decir, con la matriz R.


# 2. Calcula la varianza de cada una de las variables y haz el cociente de la máxima entre la mínima.  Comenta (¿qué variables se deben transformar para evitar el cociente tan grande? ¿Cómo  transformarlas?).
# Vamos a cargar la base de datos
athletic <- read.csv("athletic.csv")
athletic

# Primero vamos a poner todo en minutos
athletic[,1:3] <- athletic[,1:3] / 60
athletic

# Cálculo de varianzas
varianzas <- apply (athletic, 2, var)
varianzas

var_max <- max(varianzas)
var_max

var_min <- min(varianzas)
var_min

cociente_varianzas <- var_max /  var_min
cociente_varianzas

# El cociente calculado es de 2481590, es un número demasiado grande que indica que la diferencia entre varianzas es muy grande. 
# Esto ocurre por la variable Marathon, ya que es la que posee más varianza (85.134042), incluso entre las variables que también están en 
# minutos. Esto es porque su rango de valores inicia desde el 128 hasta el 164.7
# Por ello, esa variable es la que se podría normalizar (restar a cada valor su media y divir entre su varianza)
# para poder utilizar la matriz de varianzas y covarianzas
# En R se tiene la función sclae() para realizar ese proceso

athletic_normalizada <- scale(athletic)
athletic_normalizada

# 3. Calcula la matriz de correlaciones (no imprimir). Comenta las relaciones entre las variables. 

matriz_correlaciones <- cor(athletic)
#print(matriz_correlaciones)

# De manera general podemos decir que las variables se correlacionan de manera positiva ya que no se tienen valores menores a cero
# Aquellas variables con mayor correlación (cercanas a 1) son 100m con 400m, 400m con 800m, 400m con 1500m, 800m con 1500m, 500m con 1500m (0.928114), 
# 1500m con 10000m (0.9337307), 5000m con 10000m (0.9738873) siendo estas 3 últimas las más grandes. 

#En general, la mayoría de las variables se encuentran asociadas de manera positiva y con una relación fuerte. 
#la más débil es la de 0.26 entre la prueba de 200 y 400m
#Y en general es la prueba de 200m la que tiene menor correlación con las demás

# 4. Calcule los componentes principales. (¿es recomendable usar la matriz de correlaciones?, explica). Describe brevemente los resultados.

#pca <- princomp(na.omit(athletic),cor = TRUE)
pca <- prcomp(athletic, center = TRUE, scale.= TRUE)
summary(pca)

#De la matriz S, haciendo el cociente de la varianza máxima con la mínima se obtiene 
#20990.91 que es un valor muy grande y hace que usar la matriz S no sea viable. 
#Por lo cual conviene utilizar la matriz de correlaciones.


# 5. Haz una gráfica de la varianza de las componentes (screeplot).Comenta.  
# 6. Explica tu criterio para selección de número de componentes. ¿qué proporción de la varianza total  se explica con el número de variables que seleccionaste? 
#Vamos a extraer la varianza de cada componente

varianzas_pca <- summary(pca)$importance[2,]

#Graficando
plot(varianzas_pca, type="b",
     main= "Varianzas de las componentes",
     xlab = "Número de componentes principales", 
     ylab = "Varianza", 
     col = "blue", pch=16)

##convendría quedarse con entre 2 y 3 componentes. 
#Si nos quedaramos con dos ya tendríamos el 87% de la varianza explicada y se facilitaría la interpretación
#Si nos quedaramos con tres ya tendríamos el 95% de la varianza explicada, aunque la interpretación podría
#ser menos sencilla.

# Además, conviene más interpretar a los que se encuentran lejos del eje horizontal, ya que esa primera componente por sí
#sola se lleva el 75% de la varianza. 

# 7. Haz el biplot (1ª y2a CP) comenta (identifica grupos de países, valores discrepantes y  comportamiento de las variables originales). 

biplot(pca,main = "Biplot (1ª y2a CP)",
       cex = 0.6) 

#Dadas las cargas, la segunda componente tiene mayor carga en la variable 200m, por lo que si encontramos 
#en el biplot variables por encima de la 2da componente, estas serían valores altos respecto a la prueba
#de 200m

#Como la primera componente tiene mayor carga en las variables de pruebas de 100 y 400 y más metros
#entonces las variables que se encuentren más a la derecha de la primera componente se asociarán
#a los países que se desempeñan mejor en las pruebas de 100, 400 o más metros.


##Convendría examinar a Cook_ls,  W_samoa que tienen los puntajes más altos de las pruebas que implican más de.
#400 m
#Netherlands y Mauritius tendría los puntuajes más altos en pruebas que implican menos de 400m 

#Y que probablemente República dominicana sea la que menos destace de los países.
#Revisando los valores originales:
athletic[c("Cook_Is", "W_Samoa","Netherlands", "Mauritius", "Dom_Rep"),]


# 8. Calcula la correlación de la 1ª componente con cada una de las variables originales. Comenta.

pca$rotation[,1]

#La primera componente correlaciona de manera similar con las variables de pruebas de 100, 400, 800, 1,500, 5,000,
#10,000 m y el marathon. Solo correlaciona de manera más baja con la variable de la prueba de 200m


# 9. Compara las cargas (loadings) de la 1ª y la 2ª CP. Haz los barplot correspondientes, compáralos y comenta.
cargas_PC1 <- pca$rotation[, 1]
cargas_PC2 <- pca$rotation[, 2]

cargas_PC1
cargas_PC2

#la primera componente tiene mayor carga en las variables de pruebas de 100m y 400m y más metros
#la segunda componente tiene mayor carga en las pruebas de 200m
#Por lo que, al examinar el biplot, los valores que se muevan más a la derecha podrían interpretarse como 
#los que se desempeñan de mejor manera en la mayoría de las pruebas
#Mientras que si hay valores que se mueven más hacia arriba, se interpretaría como aquellos que se 
#desempeñan mejor en las pruebas de pocos metros pero que se desempeñan peor en las otras pruebas (debido a las cargas negativas).


barplot(cargas_PC1, main = "Cargas de la 1ª Componente Principal (CP1)",
        ylab = "Carga", xlab = "Variables", col = "skyblue", las = 2)

# Graficar las cargas de la 2ª CP
barplot(cargas_PC2, main = "Cargas de la 2ª Componente Principal (CP2)",
        ylab = "Carga", xlab = "Variables", col = "salmon", las = 2)


# 10. Verifica que CP1 es ortogonal a la CP2.
#para comprobar que la primera componente es ortogonal a la segunda, basta con hacer una correlación entre los scores
#de cada componente
#Como la correlación es muy cercana a 0, podemos decir que las dos componente son ortogonales

cor(pca$x[,1], pca$x[,2])

