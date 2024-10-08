#- Oct - 5 - 2024
#- Autor: Christian Badillo
#- Script para estimación de parámetros.

# -------------- ---------------------------- ---------------------------------
#- Librerias.
library(tidyverse)
library(survey)
library(ggplot2)


#- -------------------------------------------------------------------
#               Análisis de Datos
#- ------------------------------------------------------------------
#- Lectura de Datos.

data <- read.csv("imp_datos17.csv")

#- Muestramos datos.
n <- 2000
N <- nrow(data)
peso <- rep(N/n,n)
fpc <- rep(N,n)
alpha <- 0.05

set.seed(314)

sample <- data %>%
    mutate(N = n()) %>%
    slice_sample(n = n)

# Paso 1: Agrupar los datos por mes de registro y contar el número de matrimonios por mes
conteo_mensual <- table(sample$MES_REGIS)

# Paso 2: Calcular la proporción muestral por mes
total_muestra <- sum(conteo_mensual)  # Total de la muestra (2000)
proporcion_mensual <- conteo_mensual / total_muestra

# Paso 4: Calcular la varianza de la proporción con el factor de corrección finito
varianza_proporcion <- (N - total_muestra) / (N - 1) * (proporcion_mensual * (1 - proporcion_mensual) / total_muestra)
varianza_proporcion

# Paso 5: Calcular los intervalos de confianza al 95% (Z = 1.96)
z <- 1.96  # Valor crítico para un IC del 95%
limite_inferior <- proporcion_mensual - z * sqrt(varianza_proporcion) + 1/(2*n)
limite_superior <- proporcion_mensual + z * sqrt(varianza_proporcion) + 1/(2*n)

# Mostrar el resultado
limite_inferior <- as.vector(round(limite_inferior * N))
proporcion_mensual <- as.vector(round(proporcion_mensual * N))
limite_superior <- as.vector(round(limite_superior * N))
real_total_mat <- c(1247, 2809, 1762, 1429, 1601, 1426, 1498, 1502, 1335, 1539, 1623, 2229)


resultados.total.mat <- data.frame(
    mes = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"),
    real = real_total_mat,
    total.est = proporcion_mensual,
    l.ci = limite_inferior,
    u.ci = limite_superior
)

#- ---------------------------------------------------------
# Gráfico

ggplot(data = resultados.total.mat) + 
    geom_point(aes(x = 1:12, y = real, colour = "blue"), size = 2.25) + 
    geom_point(aes(x = 1:12, y = total.est, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = l.ci, ymax = u.ci, x = 1:12, colour = "yellowgreen")) + 
    labs(x = "Mes", y = "Total de matrimonios", title = "Total de matrimonios por mes con IC del 95%") +
    guides(colour = guide_legend(title = "")) + 
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) + 
    scale_x_continuous(breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", 
                                                 "Ago", "Sep", "Oct", "Nov", "Dic")) + 
    theme_minimal()


#- -------------------------------------------------------
#- Matrimonios entre mujeres

MM.mat <- table(sample$TIPO_CON == 3)
MM.mat

total_muestra <- sum(MM.mat)  # Total de la muestra (2000)
propo.MM.mat <- MM.mat / total_muestra 
propo.MM.mat <- propo.MM.mat[2]

varianza_proporcion <- (N - total_muestra) / (N - 1) * (propo.MM.mat * (1 - propo.MM.mat) / total_muestra)
varianza_proporcion

z <- 1.96  # Valor crítico para un IC del 95%
limite_inferior <- propo.MM.mat - z * sqrt(varianza_proporcion) + 1/(2*n)
limite_superior <- propo.MM.mat + z * sqrt(varianza_proporcion) + 1/(2*n)

limite_inferior <- as.vector(round(limite_inferior * N))
total.MM.mat <- as.vector(round(propo.MM.mat * N))
limite_superior <- as.vector(round(limite_superior * N))
real.total.MM <- 65

resultados.total.MM.mat <- data.frame(
    real = real.total.MM,
    total.est = total.MM.mat,
    pop.est = propo.MM.mat,
    l.ci = limite_inferior,
    u.ci = limite_superior
)

ggplot(data = resultados.total.MM.mat) + 
    geom_point(aes(x = 1, y = real, colour = "blue"), size = 2.25) + 
    geom_point(aes(x = 1, y = total.est, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = l.ci, ymax = u.ci, x = 1, colour = "yellowgreen")) + 
    labs(x = "", y = "Total de matrimonios", title = "Total de matrimonio mujer-mujer con IC del 95%") +
    guides(colour = guide_legend(title = "")) + 
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) + 
    scale_x_continuous(breaks = 1, labels = c("Matrimonios Mujer-Mujer")) + 
    theme_minimal()

#- -------------------------------------------------------
#- Matrimonios entre Mexicanos 

Nac.Mat.Mex <- table(sample$NACI_CON1 == 1 & sample$NACI_CON2 ==1)

total_muestra <- sum(Nac.Mat.Mex)  # Total de la muestra (2000)
propo.Nac.Mat.Mex <- Nac.Mat.Mex / total_muestra 
propo.Nac.Mat.Mex <- propo.Nac.Mat.Mex[2]

varianza_proporcion <- (N - total_muestra) / (N - 1) * (propo.Nac.Mat.Mex * (1 - propo.Nac.Mat.Mex) / total_muestra)

z <- 1.96  # Valor crítico para un IC del 95%
limite_inferior <- propo.Nac.Mat.Mex - z * sqrt(varianza_proporcion) + 1/(2*n)
limite_superior <- propo.Nac.Mat.Mex + z * sqrt(varianza_proporcion) + 1/(2*n)

limite_inferior <- as.vector(round(limite_inferior * N))
total.Nac.Mat.Mex <- as.vector(round(propo.Nac.Mat.Mex * N))
limite_superior <- as.vector(round(limite_superior * N))
real.total.MM <- 19096

resultados.total.Nac.Mat.Mex <- data.frame(
    real = real.total.MM,
    total.est = total.Nac.Mat.Mex,
    pop.est = propo.MM.mat,
    l.ci = limite_inferior,
    u.ci = limite_superior
)

ggplot(data = resultados.total.Nac.Mat.Mex) + 
    geom_point(aes(x = 1, y = real, colour = "blue"), size = 2.25) + 
    geom_point(aes(x = 1, y = total.est, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = l.ci, ymax = u.ci, x = 1, colour = "yellowgreen")) + 
    labs(x = "", y = "Total de matrimonios", title = "Total de Matrimonios con Ambos Contrayentes Mexicanos con IC del 95%") +
    guides(colour = guide_legend(title = "")) + 
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) + 
    scale_x_continuous(breaks = 1, labels = c("Contrayentes Mexicanos")) + 
    theme_minimal()


#- ---------------------------------------------------------------------------
#- Edad Promedio de Contrayentes Mujeres
Edad <- c(subset(sample, SEXO_CON1 == 2)$EDAD_CON1, subset(sample, SEXO_CON2 == 2)$EDAD_CON2)
data.con.MM <- cbind(Edad, N)
data.con.MM <- as.data.frame(data.con.MM)

disenio <- svydesign(id=~1, fpc=~N, data=data.con.MM)

mean.MM.Edad <- svymean(~Edad, design = disenio)
cis.MM.mean.Edad <- confint(mean.MM.Edad)

real.mean.con.MM <- 29

resultados.mean.con.MM <- data.frame(
    real = real.mean.con.MM,
    est = mean.MM.Edad[1],
    l.ci = cis.MM.mean.Edad[1],
    u.ci = cis.MM.mean.Edad[2]
)

ggplot(data = resultados.mean.con.MM) + 
    geom_point(aes(x = 1, y = real, colour = "blue"), size = 2.25) + 
    geom_point(aes(x = 1, y = est, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = l.ci, ymax = u.ci, x = 1, colour = "yellowgreen")) + 
    labs(x = "", y = "Edad", title = "Edad Promedio de Mujeres Contrayentes con IC del 95%") +
    guides(colour = guide_legend(title = "")) + 
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) + 
    scale_x_continuous(breaks = 1, labels = c("Contrayente Mujer")) + 
    theme_minimal()

#- ---------------------------------------------------------------------------
#- Edad Promedio de Contrayentes Mujeres por Entidad
Edad.p.entidad <- subset(sample, SEXO_CON1 == 2)[c("ENT_REGIS", "EDAD_CON1")]
Edad.p <- subset(sample, SEXO_CON2 == 2)[c("ENT_REGIS", "EDAD_CON2")]

colnames(Edad.p.entidad) <- c("Entidad", "Edad")
colnames(Edad.p) <- c("Entidad", "Edad")

Edad.p.entidad <- rbind(Edad.p, Edad.p.entidad)

Edad.p.entidad <- cbind(Edad.p.entidad, N)
Edad.p.entidad <- as.data.frame(Edad.p.entidad)

disenio <- svydesign(id=~1, fpc=~N, data=Edad.p.entidad)
mean.edad.p.entidad.MM <- svyby(~Edad, by= ~Entidad, design = disenio, FUN = svymean)
mean.edad.p.entidad.MM

cis <- confint(mean.edad.p.entidad.MM)
cis

mean.edad.p.entidad.MM <- cbind(mean.edad.p.entidad.MM, cis)
mean.edad.p.entidad.MM

estados <- c("Ags", "BC", "BCS", "Camp",  "Coah", "Col", "Chis", "Chih", "CDMX",  "Dgo", "Gto", "Gro", 
  "Hgo", "Jal", "Mex", "Mich", "Mor", "Nay", "NL", "Oax", "Pue", "Qro", "QR", "SLP", "Sin", 
  "Son", "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac")

val.real <- c(26.83209, 31.73776, 31.57609, 27.87647, 28.30413, 29.45669, 28.64310, 29.00789, 
          32.91158, 28.23077, 26.03475, 26.43594, 30.83019, 27.55489, 29.71291, 27.28703,
          30.35417, 31.06091, 28.58743, 27.24178, 29.69534, 29.79370, 30.13869, 28.15187,
          29.51215, 31.32750, 27.97906, 30.24859, 28.90811, 30.14092, 27.30220, 25.84688)

edad.p.entidad.MM <- mean.edad.p.entidad.MM %>%
    mutate(Entidad = factor(estados, labels = unique(estados), ordered = T)) %>%
    mutate(Val.Real = val.real)

ggplot(data = edad.p.entidad.MM) + 
    geom_point(aes(x = Entidad, y = Val.Real, colour = "blue"), size = 2.25) + 
    geom_point(aes(x = Entidad, y = Edad, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, x = Entidad, colour = "yellowgreen")) + 
    labs(x = "Entidad", y = "Edad", title = "Edad Promedio de Mujeres Contrayentes por Entidad con IC del 95%") +
    guides(colour = guide_legend(title = "")) + 
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90)) 

#- ---------------------------------------------------------------------------------------------------
#- Actividad Laboral de los Contrayentes
Act.Lab <- c(sample$OCUP_CON1, sample$OCUP_CON2)

act.lab.con <- table(Act.Lab)
act.lab.con

propo.act.lab.con <- act.lab.con / n 
propo.act.lab.con <- as.vector(propo.act.lab.con)

varianza_proporcion <- (N - total_muestra) / (N - 1) * (propo.act.lab.con * (1 - propo.act.lab.con) / n)
varianza_proporcion

z <- 1.96  # Valor crítico para un IC del 95%
limite_inferior <- propo.act.lab.con - z * sqrt(varianza_proporcion) + 1/(2*n)
limite_superior <- propo.act.lab.con + z * sqrt(varianza_proporcion) + 1/(2*n)

limite_inferior <- as.vector(round(limite_inferior * N))
total.act.lab.con <- as.vector(round(propo.act.lab.con * N))
limite_superior <- as.vector(round(limite_superior * N))
real.act.lab.con <- c(506, 5196, 663, 2013, 766, 1860, 2709, 849, 752, 10627, 12851, 1207)

res.act.lab.con <- data.frame(
    val.real = real.act.lab.con,
    act.lab = sort(unique(Act.Lab)),
    total.est = total.act.lab.con,
    pop.est = propo.act.lab.con,
    l.ci = limite_inferior,
    u.ci = limite_superior
)

res.act.lab.con <- rbind(res.act.lab.con, c(1, 10, 0, 0, 0, 0))

res.act.lab.con <- res.act.lab.con %>%
    mutate(act_laboral = case_when(
        act.lab == 1 ~ "Funcionarios, directores y jefes", 
        act.lab == 2 ~ "Profesionistas y técnicos", 
        act.lab == 3 ~ "Trabajadores auxiliares en actividades administrativas", 
        act.lab == 4 ~ "Comerciantes, empleados en ventas y agentes de ventas", 
        act.lab == 5 ~ "Trabajadores en servicios personales y vigilancia", 
        act.lab == 6 ~ "Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca", 
        act.lab == 7 ~ "Trabajadores artesanales", 
        act.lab == 8 ~ "Operadores de maquinaria industrial, ensambladores, choferes y transporte", 
        act.lab == 9 ~ "Trabajadores en actividades elementales y de apoyo", 
        act.lab == 10 ~ "Busca trabajo", 
        act.lab == 11 ~ "No trabaja", 
        act.lab == 97 ~ "No aplica a menores de 5 años",
        act.lab == 98 ~ "Insuficientemente especificada", 
        act.lab == 99 ~ "No especificada", 
        TRUE ~ NA_character_
    ))

mapa_abreviaturas <- c(
    "Funcionarios, directores y jefes" = "Func/dir/jefes", 
    "Profesionistas y técnicos" = "Prof/tecn", 
    "Trabajadores auxiliares en actividades administrativas" = "Auxadmin", 
    "Comerciantes, empleados en ventas y agentes de ventas" = "Comer/agent", 
    "Trabajadores en servicios personales y vigilancia" = "Serv/vig", 
    "Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca" = "Agric", 
    "Trabajadores artesanales" = "Artesa", 
    "Operadores de maquinaria industrial, ensambladores, choferes y transporte" = "indust/transp", 
    "Trabajadores en actividades elementales y de apoyo" = "Elemen/apoyo", 
    "Busca trabajo" = "BuscaT", 
    "No trabaja" = "NT", 
    "No aplica a menores de 5 años" = "NA<5", 
    "Insuficientemente especificada" = "InsEsp", 
    "No especificada" = "NE")

res.act.lab.con <- res.act.lab.con %>%
    mutate(abreviatura = recode(act_laboral, !!!mapa_abreviaturas))
    

ggplot(data = res.act.lab.con) + 
    geom_point(aes(x = abreviatura, y = val.real, colour = "blue"), size = 2.25) + 
    geom_point(aes(x = abreviatura, y = total.est, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = l.ci, ymax = u.ci, x = abreviatura, colour = "yellowgreen")) + 
    labs(x = "Ocupación", 
         y = "Total", 
         title = "Ocupaciones de los contrayentes con IC del 95%") +
    guides(colour = guide_legend(title = "")) + 
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45)) 

#- ---------------------------------------------------------------------------------------------------------------
#- Escolaridad Contrayentes por Entidad Federativa 
Esc.p.entidad <- sample[c("ENT_REGIS", "ESCOL_CON1")]
Esc.p <- sample[c("ENT_REGIS", "ESCOL_CON2")]

colnames(Esc.p.entidad) <- c("Entidad", "Escolaridad")
colnames(Esc.p) <- c("Entidad", "Escolaridad")

Esc.p.entidad <- rbind(Esc.p.entidad, Esc.p)

Esc.p.entidad <- cbind(Esc.p.entidad, N)
Esc.p.entidad <- as.data.frame(Esc.p.entidad)
Esc.p.entidad$Escolaridad <- factor(Esc.p.entidad$Escolaridad, levels = unique(Esc.p.entidad$Escolaridad), ordered = T)

View(Esc.p.entidad)

disenio <- svydesign(id=~1, fpc=~N, data=Esc.p.entidad)
total.esc.p.entidad <- svybys(~Escolaridad, bys = ~Entidad, design = disenio, FUN = svytotal)

df.total.escpent <- as.data.frame(total.esc.p.entidad)

estados <- c("Ags", "BC", "BCS", "Camp",  "Coah", "Col", "Chis", "Chih", "CDMX",  "Dgo", "Gto", "Gro", 
             "Hgo", "Jal", "Mex", "Mich", "Mor", "Nay", "NL", "Oax", "Pue", "Qro", "QR", "SLP", "Sin", 
             "Son", "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac")

escolaridad_abrv <- c("SinE", "1-3 Prim", "4-5 Prim", "Primaria", 
                      "Sec", "Prepa", "Prof", "Otra", "NE")

mapa_escolaridad <- c("Sin escolaridad" = "SinE", 
                      "1 a 3 años de primaria" = "1-3 Prim",
                      "4 a 5 años de primaria" = "4-5 Prim", 
                      "Primaria completa" = "Primaria", 
                      "Secundaria o equivalente" = "Sec", 
                      "Preparatoria o equivalente" = "Prepa", 
                      "Profesional" = "Prof", 
                      "Otra"= "Otra", 
                      "No especificada" = "NE")

Entidad <- rep(df.total.escpent$Entidad, 9)
Esc.code <- rep(x = 1:9, each = 32)
Escol.names <- vector(length = 9)
se.Escol.names <- vector(length = 9)
for (i in 1:9){
    Escol.names[i] <- paste("Escolaridad", i, sep = "")
    se.Escol.names[i] <- paste("se.Escolaridad", i, sep="")
}
Escolaridad <- as.numeric(unlist(df.total.escpent[Escol.names]))
SE.Escolaridad <- as.numeric(unlist(df.total.escpent[se.Escol.names]))
CI.L <- Escolaridad - z * SE.Escolaridad
CI.U <- Escolaridad + z * SE.Escolaridad

Esc.Total.by.Ent <- data.frame(
    Entidad, Escolaridad = Esc.code, Total = Escolaridad, CI.L, CI.U
)

Esc.Total.by.Ent <- Esc.Total.by.Ent %>%
    mutate(Escol.Name = case_when(
        Escolaridad == 1 ~ "Sin escolaridad",
        Escolaridad == 2 ~ "1 a 3 años de primaria",
        Escolaridad == 3 ~ "4 a 5 años de primaria",
        Escolaridad == 4  ~ "Primaria completa",
        Escolaridad == 5 ~ "Secundaria o equivalente", 
        Escolaridad == 6 ~ "Preparatoria o equivalente", 
        Escolaridad == 7 ~ "Profesional", 
        Escolaridad == 8 ~ "Otra", 
        Escolaridad == 9 ~ "No especificada", 
        TRUE ~ NA_character_ )) %>%
    mutate(Abreviacion = recode(Escol.Name, !!!mapa_escolaridad)) %>%
    mutate(Escol.Name = factor(Escol.Name, levels = c("1 a 3 años de primaria", "4 a 5 años de primaria",
                                                      "No especificada","Otra", "Preparatoria o equivalente", 
                                                      "Primaria completa", "Profesional", "Secundaria o equivalente", 
                                                       "Sin escolaridad"), ordered = T))

Reales <- c(4, 3, 1, 159, 30, 161, 177, 1, 14, 3, 375, 3, 207, 119, 245,
            286, 14, 2, 49, 6, 81, 44, 2, 21, 4, 2, 13, 81, 25, 87, 102, 
            3, 6, 143, 3, 321, 52, 337, 340, 6, 1, 2, 4, 1, 75, 22, 80, 
            59, 2, 33, 71, 85, 2, 374, 337, 306, 454, 90, 7, 11, 197, 11, 
            263, 127, 342, 300, 4, 56, 9, 19, 675, 68, 815, 621, 3, 3, 5, 
            87, 7, 130, 38, 97, 176, 3, 25, 15, 530, 12, 431, 274, 324, 742, 
            9, 24, 26, 88, 167, 272, 158, 205, 321, 17, 2, 4, 89, 152, 84, 207, 
            192, 12, 27, 21, 113, 1, 859, 298, 874, 796, 21, 26, 22, 198, 17, 
            1618, 453, 1084, 1539, 31, 12, 16, 327, 2, 367, 201, 350, 445, 18, 
            6, 2, 44, 16, 166, 42, 217, 167, 10, 58, 63, 25, 150, 62, 36, 3, 1, 
            633, 24, 326, 136, 476, 434, 3, 17, 6, 302, 247, 240, 380, 24, 22, 9, 
            373, 180, 433, 328, 27, 4, 2, 6, 156, 60, 255, 207, 8, 3, 2, 129, 280,
            47, 217, 141, 3, 8, 4, 79, 4, 233, 72, 196, 259, 1, 17, 16, 177, 316,
            63, 384, 173, 6, 12, 22, 302, 106, 341, 356, 3, 9, 12, 3, 36, 283, 45,
            173, 203, 10, 20, 248, 2, 244, 91, 196, 246, 5, 8, 2, 141, 22, 102, 93,
            2, 67, 38, 67, 9, 756, 323, 546, 506, 44, 11, 5, 104, 156, 67, 166, 212,
            7, 6, 4, 24, 1, 158, 62, 160, 208, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

Estado <- rep(estados, each = 9)

Esc.Total.by.Ent <- Esc.Total.by.Ent %>%
    arrange(Entidad, Escol.Name) %>%
    mutate(Estado = Estado) %>%
    mutate(Real.Val = Reales)

ggplot(Esc.Total.by.Ent, aes(x= factor(Estado, levels = unique(Estado)), y = Real.Val, colour = "blue"))+
    geom_point() +
    geom_point(aes(x = Estado, y = Total, colour = "yellowgreen"), size = 2.25) + 
    geom_errorbar(aes(ymin = CI.L, ymax = CI.U, colour = "yellowgreen")) +
    facet_wrap(~ Escol.Name, nrow = 9, ncol = 2, scales = "free_y") + 
    labs(x = "Estado",
         y = "Total",
         title = "Escolaridad de los Contrayentes con IC del 95%") +
    guides(colour = guide_legend(title = "")) +
    scale_color_discrete(labels=c("Valor Real", "Valor Estimado")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))
