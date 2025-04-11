# Preparación de datos

data.conteo <- read.delim("MUESTRA_ELECCIONES_FEDERALES_2018.txt", sep ="|", skip = "1")
data <- data.conteo %>%
    select(-c(MODIFICADO, ORIGEN_CAPTURA, SEGUNDOS, MINUTOS, HORA, DIA, MES, ANIO,
              ID_AREA_RESPONSABILIDAD, ESTRATO_F, ESTRATO_L, ID_ESTRATO_F, ID_ESTRATO_L,
              ID_DIST_LOC, SECCION, TIPO_SECCION, ID_CASILLA, TIPO_CASILLA, EXT_CONTIGUA,
              ID_MUNICIPIO))

# Creamos los datos en proporciones.
data.votos <- data %>%
    # Verificamos que la lista nominal sea distinta de 0, si lo es excluimos la fila.
    filter(LISTA_NOMINAL != 0) %>%
    mutate(NO_PART_CIUD = LISTA_NOMINAL - TOTAL) %>%
    select(-c(TOTAL, LISTA_NOMINAL,NO_PART_CIUD)) 

write.csv(data.propo, "data_proporciones.csv", row.names = FALSE)
write.csv(data.votos, "data_votos.csv", row.names = FALSE)
