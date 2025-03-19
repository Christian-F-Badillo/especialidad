### Muestreo Gibbs con prior conjunta: p(mu, sigma2, tau2) = IG(sigma2|alpha0,beta0) * IG(tau2|a0,b0) ###

library(LaplacesDemon)
library(MASS)

# Datos
y1 <- c(62,60,63,59)
y2 <- c(63,67,71,64,65,66)
y3 <- c(68,66,71,67,68,68)
y4 <- c(56,62,60,61,63,64,63,59)

nCad <- 10
n2 <- 100   # Longitud de cadena + calentamiento

# Hiperparámetros del prior
alpha0 <- 2  # Para sigma2
beta0  <- 2
a0     <- 2  # Para tau2
b0     <- 2

# Puntos iniciales
theta1 <- sample(y1, size = nCad, replace = TRUE)
theta2 <- sample(y2, size = nCad, replace = TRUE)
theta3 <- sample(y3, size = nCad, replace = TRUE)
theta4 <- sample(y4, size = nCad, replace = TRUE)
PInic <- cbind(theta1, theta2, theta3, theta4)
mu <- rowMeans(PInic)
PInic <- cbind(PInic, mu)

# Inicialización de matrices para almacenar muestras
theta1_mat <- matrix(0, nrow = nCad, ncol = n2)
theta2_mat <- matrix(0, nrow = nCad, ncol = n2)
theta3_mat <- matrix(0, nrow = nCad, ncol = n2)
theta4_mat <- matrix(0, nrow = nCad, ncol = n2)
mu_mat     <- matrix(0, nrow = nCad, ncol = n2)
sigma2_mat <- matrix(0, nrow = nCad, ncol = n2)
tau2_mat   <- matrix(0, nrow = nCad, ncol = n2)

for (i in 1:nCad) {
    
    # Valores iniciales
    theta <- PInic[i,1:4]
    mu <- PInic[i,5]
    
    theta1_mat[i,1] <- theta[1]
    theta2_mat[i,1] <- theta[2]
    theta3_mat[i,1] <- theta[3]
    theta4_mat[i,1] <- theta[4]
    mu_mat[i,1]     <- mu
    
    for (j in 2:n2) {
        
        # Actualización de tau^2
        tau2_new <- rigamma(1, shape = a0 + 4/2, rate = b0 + 0.5 * sum((theta - mu)^2))
        tau2_mat[i, j] <- tau2_new
        
        # Actualización de sigma^2
        n_total <- length(y1) + length(y2) + length(y3) + length(y4)
        sse <- sum((y1 - theta[1])^2) + sum((y2 - theta[2])^2) + 
            sum((y3 - theta[3])^2) + sum((y4 - theta[4])^2)
        sigma2_new <- rigamma(1, shape = alpha0 + n_total / 2, rate = beta0 + 0.5 * sse)
        sigma2_mat[i, j] <- sigma2_new
        
        # Actualización de theta_j para cada grupo
        theta_new <- numeric(4)
        for (g in 1:4) {
            y <- list(y1, y2, y3, y4)[[g]]
            theta_hat <- ((1 / tau2_new) * mu_mat[i, j-1] + (length(y) / sigma2_new) * mean(y)) / 
                ((1 / tau2_new) + (length(y) / sigma2_new))
            s_theta_sq <- 1 / ((1 / tau2_new) + (length(y) / sigma2_new))
            theta_new[g] <- rnorm(1, mean = theta_hat, sd = sqrt(s_theta_sq))
        }
        
        theta1_mat[i, j] <- theta_new[1]
        theta2_mat[i, j] <- theta_new[2]
        theta3_mat[i, j] <- theta_new[3]
        theta4_mat[i, j] <- theta_new[4]
        
        # Actualización de mu
        mu_hat <- mean(theta_new)
        mu_new <- rnorm(1, mean = mu_hat, sd = sqrt(tau2_new / 4))
        mu_mat[i, j] <- mu_new
        
        # Actualizar valores para la siguiente iteración
        theta <- theta_new
        mu <- mu_new
    }
}

# Ahora cada matriz contiene la evolución de la cadena para cada parámetro


# Posteriormente puedes calcular diagnósticos como Rhat y resumir las muestras.
# Por ejemplo, para el parámetro theta1:

Rhat <- function(Cadenas){
    
    m <- nrow(Cadenas)
    n <- ncol(Cadenas)
    
    medias <- rowMeans(Cadenas)
    
    mediaG <- mean(medias)
    
    s2j <- apply(Cadenas, MARGIN = 1, function(x){(1/(length(x)-1))*sum((x-mean(x))^2)})
    
    W <- mean(s2j)
    
    B <- (n/(m-1))*sum((medias-mediaG)^2)
    
    res <- sqrt(((n-1)*(W/n) + B/n)/W)
    
    return(res)
    
}

plot(0:10, theta1, type = "l")
lines(0:99, theta1[2,], col = "blue")
lines(0:99,theta1[3,], col = "red")

