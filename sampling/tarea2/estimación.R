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
limite_inferior <- proporcion_mensual - z * sqrt(varianza_proporcion)
limite_superior <- proporcion_mensual + z * sqrt(varianza_proporcion)

# Mostrar el resultado
limite_inferior <- as.vector(round(limite_inferior * N))
proporcion_mensual <- as.vector(round(proporcion_mensual * N))
limite_superior <- as.vector(round(limite_superior * N))
real_total_mat <- c(1247, 2809, 1762, 1429, 1601, 1426, 1498, 1502, 1335, 1539, 1623, 2229)


resultados.total.mat <- data.frame(
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
limite_inferior <- propo.MM.mat - z * sqrt(varianza_proporcion)
limite_superior <- propo.MM.mat + z * sqrt(varianza_proporcion)

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
Nac.Mat.Mex

total_muestra <- sum(Nac.Mat.Mex)  # Total de la muestra (2000)
propo.Nac.Mat.Mex <- Nac.Mat.Mex / total_muestra 
propo.Nac.Mat.Mex <- propo.Nac.Mat.Mex[2]
propo.Nac.Mat.Mex

varianza_proporcion <- (N - total_muestra) / (N - 1) * (propo.Nac.Mat.Mex * (1 - propo.Nac.Mat.Mex) / total_muestra)
varianza_proporcion

z <- 1.96  # Valor crítico para un IC del 95%
limite_inferior <- propo.Nac.Mat.Mex - z * sqrt(varianza_proporcion)
limite_superior <- propo.Nac.Mat.Mex + z * sqrt(varianza_proporcion)

limite_inferior <- as.vector(round(limite_inferior * N))
total.Nac.Mat.Mex <- as.vector(round(propo.Nac.Mat.Mex * N))
limite_superior <- as.vector(round(limite_superior * N))
real.total.MM <- 19096

resultados.total.Nac.Mat.Mex <- data.frame(
    real = real.total.MM,
    total.est = total.MM.mat,
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
Edad <- c(subset(muestra, SEXO_CON1 == 2)$EDAD_CON1, subset(muestra, SEXO_CON2 == 2)$EDAD_CON2)
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
Edad.p.entidad <- subset(muestra, SEXO_CON1 == 2)[c("ENT_REGIS", "EDAD_CON1")]
Edad.p <- subset(muestra, SEXO_CON2 == 2)[c("ENT_REGIS", "EDAD_CON2")]

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
limite_inferior <- propo.act.lab.con - z * sqrt(varianza_proporcion)
limite_superior <- propo.act.lab.con + z * sqrt(varianza_proporcion)

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

mean.edad.p.entidad.MM <- cbind(mean.edad.p.entidad.MM, cis)
mean.edad.p.entidad.MM

estados <- c("Ags", "BC", "BCS", "Camp",  "Coah", "Col", "Chis", "Chih", "CDMX",  "Dgo", "Gto", "Gro", 
             "Hgo", "Jal", "Mex", "Mich", "Mor", "Nay", "NL", "Oax", "Pue", "Qro", "QR", "SLP", "Sin", 
             "Son", "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac")