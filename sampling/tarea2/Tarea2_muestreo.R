# -------------------------------------------
# --------- librerias -----------------------
# -------------------------------------------
if(!require(foreign)){
    install.packages("foreign")
    library(foreign)
}

if(!require(naniar)){
    install.packages("naniar")
    library(naniar)
}

library(tidyverse)
library(mice)

# ---------- Población Datos

pop.data <- read.csv("DATOS17.csv")
loc.names.data <- read.dbf("~/Descargas/CATEMLMA17.dbf") 
loc.names.data[, 1] <- as.integer(loc.names.data[, 1])
loc.names.data[, 2] <- as.integer(loc.names.data[, 2])
loc.names.data[, 3] <- as.integer(loc.names.data[, 3])


dim(pop.data)

# Hacemos un Left Join 
data_merged = merge(pop.data, loc.names.data, by.x = c("ENT_REGIS", "MUN_REGIS", "LOC_REGIS"), 
            by.y = c("CVE_ENT", "CVE_MUN", "CVE_LOC"), sort = F, all.x =T, all.y = F)

# Ya no necesitamos esto en memoria
rm(loc.names.data)
rm(pop.data)
# Podemos guardar losd atos.
# write.csv(x = data_merged, "Datos_merged.csv")

vars.names <- colnames(data_merged)
vars.names

### ------------Dic Variables----------------------
# C indica datos de tipo string
# N indica una variable numérica.
# El número indica la longitud del dato.
# ---------------------------- Notas -----------------------------------
# Aquellas variables con terminación *con* se omite el número final en el dict
# pues refiere al primer o segundo contrayente. Pero la codificación de las variables
# son invariantes entre el primer y segundo contrayente, por tanto se omite la distintción.
dict <- list(
    "ENT_REGIS" = list("Significado" = "Entidad de registro", "tipo" = "C2", "codificación" = list("0" = "NA")),
    "Mun_regis" = list("Significado" = "Municipio o delegación de registro", "tipo" = "C3", "codificación" = list("0" = "NA")),
    "Loc_regis" = list("Significado" = "Localidad de registro.", "tipo" = "C4", "codificación" = list("0" = "NA")),
    "Tam_loc_re" = list("Tamaño de localidad de registro.", "tipo" = "N2", "codificación" = list("1" = "De 1 a 999", "2" = "De 1 000 a 1 999", 
                                                                                                 "3"= "De 2 000 a 2 499", "4" = "De 2 500 a 4 999", "5" = "De 5 000 a 9 999", 
                                                                                                 "6" = "De 10 000 a 14 999", "7" = "De 15 000 a 19 999", "8" = "De 30 000 a 39 999", 
                                                                                                 "9" = "De 30 000 a 39 999", "10" = "De 40 000 a 49 999", "11" = "De 50 000 a 74 999", 
                                                                                                 "12" = "De 75 000 a 99 999", "13" = "De 100 000 a 249 999", "14" = "De 250 000 a 499 999",
                                                                                                 "15" = "De 500 000 a 999 999", "16" = "De 1 000 000 a 1 499 999", "17" = "De 1 500 000 y más",
                                                                                                 "99" = "NA")),
    "Dia_regis" = list("Significado" = "Día de registro.", "tipo" =  "N2", "codificación" = list("0-31" = "día del mes", "99" = "NA")),
    "Mes_regis" = list("Significado" = "Mes de registro.", "tipo" = "N2", "codificación" = list("0-12" = "mes", "99" = "NA")),
    "Anio_regis" = list("Significado" = "Año de registro.", "tipo" = "N4"),
    "Regimen_ma" = list("Significado" = "Régimen matrimonial.", "tipo" = "N1", "codificación" = list("1" = "Sociedad Conyugal", "2" = "Separación de Bienes", 
                                                                                                     "3" = "Mixto", "9" = "NA")),
    "Genero" = list("Significado" = "Género de matrimonio.", "tipo" = "N1", "codificación" = list("1" = "Hombre-Mujer", 
                                                                                                  "2" = "Mismo Sexo")),
    "Sexo_con" = list("Significado" = "Sexo del contrayente.", "tipo" = "N1", "codificación" = list("1" = "Hombre", 
                                                                                                    "2" = "Mujer")),
    "Edad_con" = list("Significado" = "Edad del contrayente.", "tipo" = "N2", "codificación" = list("12-98" = "Edad en anios", 
                                                                                                  "99" = "NA")),
    "Naci_con" = list("Significado" = "Nacionalidad del contrayente.", "tipo" = "N1", "codificación" = list("1" = "Mexicana", 
                                                                                                          "2" = "Extranjera")),
    "Ocup_con" = list("Significado" = "Ocupación del contrayente.", "tipo" = "N2", "codificación" = list("1" = "Funcionarios, directores y jefes",  "2" = "Profesionistas y técnicos", 
                                                                                                         "3" = "Trabajadores auxiliares en actividades administrativas", "4" = "Comerciantes, empleados en ventas y agentes de ventas", 
                                                                                                         "5" = "Trabajadores en servicios personales y vigilancia", "6" = "Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca", 
                                                                                                         "7"="Trabajadores artesanales", "8" = "Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte", 
                                                                                                         "9" = "Trabajadores en actividades elementales y de apoyo",
                                                                                                         "10" = "Busca trabajo", "11" = "No trabaja", "98-99" = "NA")),
    "Entrh_con" = list("Significado" ="Entidad de residencia habitual del contrayente.", "tipo" = "C2", "codificación" = list("0" = "NA")),
    "Munrh_con" = list("Significado" = "Municipio o delegación de residencia habitual del contrayente.", "tipo" = "C3", "codificación" = list("0" = "NA")),
    "Locrh_con" = list("Sginificado" = "Localidad de residencia habitual del contrayente.", "tipo" = "C4", "codificación" = list("0" = "NA")),
    "Tlorh_con" = list("Significado" = "Tamaño de localidad de residencia habitual del contrayente.", "tipo" = "N2", "codificación" = list("1" = "De 1 a 999", "2" = "De 1 000 a 1 999", 
                                                                                                                                           "3"= "De 2 000 a 2 499", "4" = "De 2 500 a 4 999", "5" = "De 5 000 a 9 999", 
                                                                                                                                           "6" = "De 10 000 a 14 999", "7" = "De 15 000 a 19 999", "8" = "De 30 000 a 39 999", 
                                                                                                                                           "9" = "De 30 000 a 39 999", "10" = "De 40 000 a 49 999", "11" = "De 50 000 a 74 999", 
                                                                                                                                           "12" = "De 75 000 a 99 999", "13" = "De 100 000 a 249 999", "14" = "De 250 000 a 499 999",
                                                                                                                                           "15" = "De 500 000 a 999 999", "16" = "De 1 000 000 a 1 499 999", "17" = "De 1 500 000 y más",
                                                                                                                                           "99" = "NA")),
    "Escol_con" = list("Significado" = "Nivel de escolaridad del contrayente (escolaridad).", "tipo" = "N1", "codificación" = list("1" = "Sin escolaridad", "2" = "De 1 a 3 años de primaria", 
                                                                                                                                   "3"= "De 4 a 5 años de primaria", "4" = "Primaria completa", 
                                                                                                                                   "5" = "Secundaria o equivalente", 
                                                                                                                                   "6" = "Preparatoria o equivalente", "7" = "Profesional", "8" = "Otra", 
                                                                                                                                   "9" = "NA")),
    "Conactcon" = list("Significado" = "Condición de actividad económica del contrayente.", "tipo" = "N1", "codificación" = list("1" = "Trabaja", "2" = "No trabaja", 
                                                                                                                                 "9" = "NA")),
    "Sitlabcon" = list("Significado" = "Situación laboral del contrayente.", "tipo" = "N1", "codificación" = list("1" = "Tiene trabajo o está buscando",  "2" = "Estudiante", 
                                                                                                                  "3" = "Dedicado a quehaceres del hogar", "4" = "Jubilado o pensionado", 
                                                                                                                  "5" = "Incapacitado permanentemente para trabajar", "6" = "Otra", 
                                                                                                                  "9" = "NA")),
    "Postracon" = list("Significado" = "Posición en el trabajo del contrayente.", "tipo" = "N1", "codificación" = list("1" = "Obrero",  "2" = "Empleado", 
                                                                                                                       "3" = "Jornalero o peón", "4" = "Trabajador por cuenta propia", 
                                                                                                                       "5" = "Patrón o empresario", "6" = "Trabajador familiar no remunerado", 
                                                                                                                       "9" = "NA")),
    "Tipo_con" = list("Significado" = "Tipo de contrayente.", "tipo" = "N1", "codificación" = list("1" = "Contrayentes hombre- mujer", 
                                                                                                   "2" = "Contrayentes del mismo sexo hombres", 
                                                                                                   "3" = "Contrayentes del mismo sexo mujeres"))
    )

# Ejemplo de Uso
dict$ENT_REGIS

# --------------------------- Manejo de NA's -----------------------------------------
# Reemplazamos valores que sabemos que son NA
data_merged <- data_merged %>%
    replace_with_na(replace = list(ENT_REGIS = 0, MUN_REGIS = 0, LOC_REGIS = 0,
                                   TAM_LOC_RE = 99, DIA_REGIS = 99, MES_REGIS = 99,
                                   REGIMEN_MA = 9, EDAD_CON1 = 99, EDAD_CON2 = 99,
                                   OCUP_CON1 = c(98, 99), OCUP_CON2 = c(98, 99), 
                                   ENTRH_CON1 = 0, ENTRH_CON2 = 0, MUNRH_CON1 = 0,
                                   MUNRH_CON2 = 0, LOCRH_CON1 = 0, LOCRH_CON2 = 0,
                                   TLORH_CON1 = 99, TLORH_CON2 = 99, ESCOL_CON1 = 9,
                                   ESCOL_CON2 = 9, CONACTCON1 = 9, CONACTCON2 = 9,
                                   SITLABCON1 = 9, SITLABCON2 = 9, POSTRACON1 = 9,
                                   POSTRACON2 = 9))

### ------------------ Verificamos si hay NA's ------------------
nans <- apply(data_merged, 2, function(x) sum(is.na(x)))
nans
format(max(nans), big.mark = ",") # Existen 37191 datos faltantes.
format(dim(data_merged), big.mark = ",") 

na.desciption <- md.pairs(data_merged)
na.desciption$rm