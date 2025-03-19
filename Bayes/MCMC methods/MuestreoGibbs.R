### Muestreo Gibbs ###
# Modelo jerarquico #
library(LaplacesDemon)
library(MASS)

# Datos

y1 <- c(62,60,63,59)
y2 <- c(63,67,71,64,65,66)
y3 <- c(68,66,71,67,68,68)
y4 <- c(56,62,60,61,63,64,63,59)

nCad <- 10

# Puntos iniciales #

theta1 <- sample(y1, size = nCad, replace = TRUE)

theta2 <- sample(y2, size = nCad, replace = TRUE)

theta3 <- sample(y3, size = nCad, replace = TRUE)

theta4 <- sample(y4, size = nCad, replace = TRUE)

PInic <- cbind(theta1,theta2,theta3,theta4)

mu <- rowMeans(PInic)

PInic <- cbind(PInic,mu)

n2 <- 2000 # Longitud cadena + calentamiento 

n <- n2/2 # Longitud de cadena

# Hiperparametros del prior

alpha0 <- 0.01
beta0 <- 0.01
a0 <- 1
b0 <- 0.01


tau2_hat <- function(theta,mu){
  J <- length(theta)
  res <- (1/(J-1))*sum((theta-mu)^2)
  return(res)
}

sigma2_hat <- function(y1,y2,y3,y4,theta){
  
  n1 <- length(y1)
  n2 <- length(y2)
  n3 <- length(y3)
  n4 <- length(y4)
  
  n <- n1+n2+n3+n4
  
  s12 <- sum((y1-theta[1])^2)
  s22 <- sum((y2-theta[2])^2)
  s32 <- sum((y3-theta[3])^2)
  s42 <- sum((y4-theta[4])^2)
  
  res <- (1/n)*(s12+s22+s32+s42)
  
  return(res)
  
}


theta1 <- vector(mode = "numeric", length = 0)
theta2 <- vector(mode = "numeric", length = 0)
theta3 <- vector(mode = "numeric", length = 0)
theta4 <- vector(mode = "numeric", length = 0)
muc <- vector(mode = "numeric", length = 0)
sigma2c <- vector(mode = "numeric", length = 0)
tau2c <- vector(mode = "numeric", length = 0)


for (i in 1:nCad) {
  
  theta <- PInic[i,1:4]
  mu <- PInic[i,5]
  
  tau2cTemp <- rep(0,n2)
  sigma2cTemp <- rep(0,n2)
  mucTemp <- rep(0,n2)
  theta1cTemp <- rep(0,n2)
  theta2cTemp <- rep(0,n2)
  theta3cTemp <- rep(0,n2)
  theta4cTemp <- rep(0,n2)
  
  mucTemp[1] <- mu
  theta1cTemp[1] <- theta[1]
  theta2cTemp[1] <- theta[2]
  theta3cTemp[1] <- theta[3]
  theta4cTemp[1] <- theta[4]
  
  for (j in 2:n2) {
    
    tau2hat <- b0 + 0.5 * sum((theta - mu)^2)
    
    tau2t1 <- rinvgamma(1, shape = a0 + 4/2, scale = tau2hat)
    
    tau2cTemp[j] <- tau2t1
    
    n_total <- length(y1) + length(y2) + length(y3) + length(y4)
    sse <- sum((y1 - theta[1])^2) + sum((y2 - theta[2])^2) + 
      sum((y3 - theta[3])^2) + sum((y4 - theta[4])^2)
    
    sigma2t1 <- rinvgamma(1, shape = alpha0 + n_total / 2, scale = beta0 + 0.5 * sse)
    
    sigma2cTemp[j] <- sigma2t1
    
    muhat <- (1/4)*(theta1cTemp[j-1] + theta2cTemp[j-1] +
                      theta3cTemp[j-1] + theta4cTemp[j-1])
    
    mut1 <- rnorm(1, mean = muhat, sd = sqrt(tau2t1/4))
    
    mucTemp[j] <- mut1
    
    theta1hat <- ((1/tau2t1)*mut1 + (4/sigma2t1)*mean(y1))/((1/tau2t1) + (4/sigma2t1))
    
    theta2hat <- ((1/tau2t1)*mut1 + (6/sigma2t1)*mean(y2))/((1/tau2t1) + (6/sigma2t1))
    
    theta3hat <- ((1/tau2t1)*mut1 + (6/sigma2t1)*mean(y3))/((1/tau2t1) + (6/sigma2t1))
    
    theta4hat <- ((1/tau2t1)*mut1 + (8/sigma2t1)*mean(y4))/((1/tau2t1) + (8/sigma2t1))
    
    
    stheta12 <- 1/((1/tau2t1) + (4/sigma2t1))
    stheta22 <- 1/((1/tau2t1) + (6/sigma2t1))
    stheta32 <- 1/((1/tau2t1) + (6/sigma2t1))
    stheta42 <- 1/((1/tau2t1) + (8/sigma2t1))
    
    theta1t1 <- rnorm(1, mean = theta1hat, sd = sqrt(stheta12))
    theta2t1 <- rnorm(1, mean = theta2hat, sd = sqrt(stheta22))
    theta3t1 <- rnorm(1, mean = theta3hat, sd = sqrt(stheta32))
    theta4t1 <- rnorm(1, mean = theta4hat, sd = sqrt(stheta42))
    
    theta1cTemp[j] <- theta1t1
    theta2cTemp[j] <- theta2t1
    theta3cTemp[j] <- theta3t1
    theta4cTemp[j] <- theta4t1
    
    #theta <- c(theta1t1,theta2t1,theta3t1,theta2t1)
    theta <- c(theta1t1,theta2t1,theta3t1,theta4t1)
    
  }
  
  theta1 <- rbind(theta1,theta1cTemp)
  theta2 <- rbind(theta2,theta2cTemp)
  theta3 <- rbind(theta3,theta3cTemp)
  theta4 <- rbind(theta4,theta4cTemp)
  
  muc <- rbind(muc,mucTemp)
  sigma2c <- rbind(sigma2c,sigma2cTemp)
  tau2c <- rbind(tau2c,tau2cTemp)
  
}

plot(0:1999, theta1[1,], type = "l")
lines(0:1999, theta1[2,], col = "blue")
lines(0:1999,theta1[3,], col = "red")


plot(0:1999, theta2[1,], type = "l")
lines(0:1999, theta2[2,], col = "blue")
lines(0:1999,theta2[3,], col = "red")

plot(0:1999, muc[1,], type = "l")
lines(0:1999, muc[2,], col = "blue")
lines(0:1999, muc[3,], col = "red")

plot(1:1999, tau2c[1,2:2000], type = "l")
lines(1:1999, tau2c[2,2:2000], col = "blue")
lines(1:1999, tau2c[3,2:2000], col = "red")

plot(1:1999, sigma2c[1,2:2000], type = "l")
lines(1:1999, sigma2c[2,2:2000], col = "blue")
lines(1:1999, sigma2c[3,2:2000], col = "red")


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

Rhat(theta1)

Rhat(muc)

Rhat(tau2c)

Rhat(sigma2c)

## Inferencia para los parametros ##

theta1_gorro <- mean(c(theta1[1,1001:2000],theta1[2, 1001:2000],theta1[3,1001:2000],theta1[4,1001:2000],
                  theta1[5,1001:2000], theta1[6,1001:2000], theta1[7,1001:2000],theta1[8,1001:2000],
                  theta1[9,1001:2000], theta1[10,1001:2000]))

theta1_gorro

truehist(theta1[1,1001:2000])
truehist(theta1[2,1001:2000])
truehist(c(theta1[1,1001:2000],theta1[2, 1001:2000],theta1[3,1001:2000],theta1[4,1001:2000],
           theta1[5,1001:2000], theta1[6,1001:2000], theta1[7,1001:2000],theta1[8,1001:2000],
           theta1[9,1001:2000], theta1[10,1001:2000]))


theta1_gorro <- mean(theta1[1,1001:2000])
quantile(theta1[1,1001:2000], probs = c(0.1,0.9))
quantile(theta1[1,1001:2000], probs = c(0.025,0.25,0.5,0.75,0.975))


quantile(c(theta1[1,1001:2000],theta1[2, 1001:2000],theta1[3,1001:2000],theta1[4,1001:2000],
           theta1[5,1001:2000], theta1[6,1001:2000], theta1[7,1001:2000],theta1[8,1001:2000],
           theta1[9,1001:2000], theta1[10,1001:2000]), probs = c(0.025,0.25,0.5,0.75,0.975))


quantile(sqrt(c(sigma2c[1,1001:2000],sigma2c[2,1001:2000],sigma2c[3,1001:2000],sigma2c[4,1001:2000],
           sigma2c[5,1001:2000], sigma2c[6,1001:2000], sigma2c[7,1001:2000],sigma2c[8,1001:2000],
           sigma2c[9,1001:2000], sigma2c[10,1001:2000])), probs = c(0.025,0.25,0.5,0.75,0.975))

