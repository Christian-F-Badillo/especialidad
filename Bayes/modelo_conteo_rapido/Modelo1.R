# Modelo para solo los votos a candidatos.

# Cargar librerías
library(tidyverse)
library(rstan)
library(bayesplot)

# Setup para rstan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Cargar datos
datos <- read.csv("data_votos.csv", header = TRUE, sep = ",")

# Seleccionar columnas relevantes
datos <- datos %>%
    select(CNR, NULOS, RAC, JAMK, AMLO, JHRC)

# -----------------------------------------------------------------------------
# Modelo de Stan.
write(
"
data {
    int<lower=0> N; // Número de observaciones
    int<lower=0> K; // Número de categorías
     int y[N, K];  // Matriz de datos
}
parameters {
    simplex[K] theta; // Esto asegura que sum(theta) == 1
}
model {
  theta ~ dirichlet(rep_vector(1.0, K)); // Priori no informativa
  for (n in 1:N)
    y[n] ~ multinomial(theta);           // Verosimilitud por casilla
}
        ",

    
    file = "Modelo1.stan"
)

# Datos para Stan

stan_data <- list(
    N = nrow(datos),
    K = ncol(datos),
    y = datos
)

# Ajustar el modelo

fit <- stan(
    file = "Modelo1.stan",
    data = stan_data,
    iter = 15000,
    warmup = 7500,
    chains = 4,
    open_progress = F,
    )

samples <- extract(fit, pars = "theta")

# Guardar los samples en un archivo RDS
saveRDS(samples, file = "samples_theta.rds")

# Resumen de los resultados
print(fit)

# Graficar los resultados
plot(fit)

traceplot(fit, inc_warmup = F, nrow = 2)

# Graficar los resultados
mcmc_dens(
    fit,
    regex_pars  = "theta",
    facet_args = list(ncol = 2),
    facet = "chain",
    ncol = 2,
    nrow = 2,
    fill = "lightblue",
    alpha = 0.5
)

# Graficar los resultados
mcmc_areas(
    fit,
    pars  = "theta[1]",
    facet_args = list(ncol = 2),
    facet = "chain",
    ncol = 2,
    nrow = 2,
    fill = "lightblue",
    alpha = 0.5,
    prob_outer = 0.99,
    prob = 0.95,
)
