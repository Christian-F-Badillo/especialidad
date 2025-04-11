# Modelo para solo los votos a candidatos.

# Cargar librerías
library(tidyverse)
library(rstan)
library(bayesplot)

# ------------------------------------------------------------------------------
# Fuciones.
### Funciones
plot_posterior_hdi <- function(samples, param_matrix, param_name, param_label = NULL, prob = 0.95) {
    hdi_vals <- quantile(param_matrix, probs = c((1 - prob) / 2, 1 - (1 - prob) / 2))
    media <- mean(param_matrix)
    
    if (is.null(param_label)) {
        param_label <- parse(text = param_name)
    }
    
    hdi_df <- data.frame(
        x = hdi_vals,
        y = 0.5,
        label = sprintf("%.5f", hdi_vals)
    )
    
    media_df <- data.frame(
        x = media,
        y = 0.8,
        label = paste0("Media: ", sprintf("%.5f", media))
    )
    
    bayesplot::mcmc_areas(
        samples,
        pars = param_name,
        facet = "chain",
        fill = "lightblue",
        alpha = 0.5,
        prob_outer = 1,
        prob = prob
    ) +
        geom_vline(
            xintercept = hdi_vals,
            linetype = "dashed",
            color = "red",
            linewidth = 1
        ) +
        geom_text(
            data = hdi_df,
            aes(x = x, y = y, label = label),
            color = "red",
            vjust = -1,
            inherit.aes = FALSE
        ) +
        geom_text(
            data = media_df,
            aes(x = x, y = y, label = label),
            color = "blue",
            vjust = -0.5,
            inherit.aes = FALSE
        ) +
        labs(
            title = "Distribución Posterior",
            subtitle = paste0("con ", prob * 100, "% HDI"),
            x = param_label,
            y = "Densidad"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_text(size = 18),
        )
}

# -----------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Guardamos los muestreos

samples <- extract(fit, pars = "theta")

# Guardar los samples en un archivo RDS
saveRDS(samples, file = "samples_theta.rds")

# -------------------------------------------------------------------------------
# Resultados

print(fit, pars = "theta", probs = c(0.025, 0.5, 0.975), digits_summary = 4)


# Trace plot
traceplot(fit, inc_warmup = F, nrow = 2)
# ggsave("img/modelo1/cadenas.png", width = 15, height = 6, dpi = 300)


# Posterior Plot
plot_posterior_hdi(
    samples = fit,              
    param_matrix = samples$theta[, 6],  
    param_name = "theta[6]"
)

# ggsave("img/modelo1/posterior_theta6.png", width = 8, height = 6, dpi = 300)
