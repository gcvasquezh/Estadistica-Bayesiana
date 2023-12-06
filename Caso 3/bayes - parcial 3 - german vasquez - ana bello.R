if (!require(dplyr)){install.packages("dplyr");library(dplyr)}
if (!require(readxl)){install.packages("readxl");library(readxl)}
if (!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if (!require(ggExtra)){install.packages("ggExtra");library(ggExtra)}
if (!require(gamlss)){install.packages("gamlss");library(gamlss)}
if (!require(readr)){install.packages("readr");library(readr)}
if (!require(stringr)){install.packages("stringr");library(stringr)}
if (!require(tidyr)){install.packages("tidyr");library(tidyr)}
if (!require(sf)){install.packages("sf");library(sf)}
if (!require(xtable)){install.packages("xtable");library(xtable)}
if (!require(mvtnorm)){install.packages("mvtnorm");library(mvtnorm)}
if (!require(LaplacesDemon)){install.packages("LaplacesDemon");library(LaplacesDemon)}
if (!require(doParallel)){install.packages("doParallel");library(doParallel)} ##Librería núcleos

setwd("C:\\Users\\soffy\\OneDrive\\Escritorio\\Universidad\\Bayes\\Cas 3")

# Elecciones Alcaldía Bogotá 2023 #####
nv <- c(493,257,227,48,41,38,28,11,3,54)
delta <- 0.75
a <- 1
b <- 1
# algoritmo modelo
ELE_BOG <- function(B, nv, delta, a, b) {
  # tamaños
  k <- length(nv)
  # valores iniciales
  delta <- sqrt(delta)
  alpha <- rgamma(n = 1, shape = a,rate = b)
  gamma <- log(alpha)
  ac <- 0
  cont <- 1
  # almacenamiento
  THETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = k+1)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (s in 1:B) {
    # actualizar theta
    theta  <- c(rdirichlet(n = 1, alpha = nv+alpha))
    # actualizar alpha
    # 1. Propuesta
    gamma_star <- rnorm(n = 1, mean = gamma, sd = delta)
    # 2. Tasa de aceptación
    egam_star <- exp(gamma_star)
    egam <- exp(gamma)
    log_r <- lgamma(k*egam_star)-lgamma(k*egam)+k*(lgamma(egam)-lgamma(egam_star))+(egam_star-egam)*sum(log(theta))+a*(gamma_star-gamma)+b*(egam-egam_star)
    # 3. Actualizar
    if(runif(1) < exp(log_r)){
      alpha <- exp(gamma_star)
      gamma <- gamma_star
      ac <- ac+1
    }
    if (s>1000 && s%%10==0) {
      # almacenar
      THETA[cont,] <- c(theta, alpha)
      # log-verosimilitud
      LL[cont] <- dmultinom(x = nv, prob = theta, log = T)
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA) <- c(paste0("theta", 1:k), "alpha")
  colnames(LL) <- c("ll")
  THETA <- as.data.frame(THETA)
  LL    <- as.data.frame(LL)
  return(list(THETA = THETA, LL = LL, TA=ac/B))
}
# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain <- ELE_BOG(B = 101000, nv, delta, a, b)
tictoc::toc()

chain$TA
plot(1:10000,unlist(chain$LL), type = "p", pch = ".", col="blue", xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo Elecciones Bogotá")
plot(chain$THETA[,1],type = "p", pch = ".", col="blue", xlab = "Iteración", ylab = expression(theta))
plot(chain$THETA[,11],type = "p", pch = ".", col="blue", xlab = "Iteración", ylab = expression(alpha))

#Estimación puntual
estim <- round(100*colMeans(chain$THETA[,1:3]),3)
estim <- sprintf("%.3f%%", estim)
names(estim) <- c("Galán","Bolivar","Oviedo")
estim
xtable(data.frame(t(estim)), digits = 3)
#Intervalos de credibilidad
galan <- round(100*quantile(chain$THETA$theta1, c(0.025, 0.975)), 3)
bolivar <- round(100*quantile(chain$THETA$theta2, c(0.025, 0.975)), 3)
oviedo <- round(100*quantile(chain$THETA$theta3, c(0.025, 0.975)), 3)
IC <- rbind(galan,bolivar,oviedo)
IC_porcentaje <- sapply(IC, function(x) sprintf("%.3f%%", x))
IC <- matrix(IC_porcentaje, nrow = nrow(IC), ncol = ncol(IC))
rm(IC_porcentaje)
colnames(IC) <- c("2.5%","97.5%")
rownames(IC) <- c("Galán","Bolivar","Oviedo")
IC
xtable(IC)

# graficamente

obs_graf<-c(49.02,20.11,18.71)
IC_graf<-rbind(galan,bolivar,oviedo)
estim_graf<- round(100*colMeans(chain$THETA[,1:3]),3)
par(mfrow = c(1,1), mar = c(4, 4, 8, 1), mgp = c(2, 0.5, 0))
plot(x = c(15,50), y = c(1,3), type = "n", xlab = "Proporción votos", ylab = "", main = "Prop. votos candidatos Bogotá", yaxt = "n")
abline(h = 1:3, col = "lightgray", lwd = 1)
for (l in 1:3) {
  j <- order(estim_graf)[l]
  #points(x = Y[[j]], y = rep(l, nj[j]), pch = 16, cex = 0.3)
  points(x = IC_graf[j,], y = c(l,l), type = "p", col =c("blue","blue"), pch = 16, cex = 1)
  segments(x0 = IC_graf[j, 1], y0 = l, x1 = IC_graf[j, 2], y1 = l, col = "blue",lwd=2)
  points(x = obs_graf[j], y = l, col ="red", pch = 16, cex = 1.1)
  points(x = estim_graf[j], y = l, col ="black", pch = 16, cex = 1.1)
}
for (l in 1:3) {
  j <- order(estim_graf)[l]
  segments(x0 = IC_graf[j, 1], y0 = l, x1 = IC_graf[j, 2], y1 = l, col = "blue",lwd=2)
}
axis(side = 2, at = 1:3, labels = c("Oviedo","Bolívar","Galán"), las = 2)


# tamaños efectivos de muestra
neff_theta <- coda::effectiveSize(chain$THETA)
round(neff_theta, 0)

# cv de monte carlo
cvmc<-((apply(X = chain$THETA, MARGIN = 2, FUN = sd)/sqrt(neff_theta)))/colMeans(chain$THETA)*100
cvmc
cvmc<-as.data.frame(cvmc)
xtable(cvmc)


# Diabetes #####
## Bases de datos #####
data_test<-readRDS("data_test.RDS")
data_train<-readRDS("data_train.RDS")
y_test<-as.vector(data_test[,1])
y_train<-as.vector(data_train[,1])
data_test<-as.matrix(data_test[,-1])
data_train<-as.matrix(data_train[,-1])

# dimensiones
n <- dim(data_train)[1]
p <- dim(data_train)[2]
beta_ols <- as.vector(solve(t(data_train)%*%data_train)%*%t(data_train)%*%y_train)
round(beta_ols, 3)
sig2_ols <- sum((y_train-data_train%*%beta_ols)^2)/(n-p)
round(sig2_ols, 3)

## Regresión Clásica #####

### Previa Unitaria #####
# hiperparámetros
beta0<-beta_ols
sig20<-sig2_ols
SIGMA<-n*sig2_ols*solve(t(data_train)%*%data_train)
nu0<-1

REG_UN <- function(B, y, X, beta0, SIGMA, sig20, nu0) {
  # tamaños
  n <- dim(X)[1]
  p <- dim(X)[2]
  # valores iniciales
  xt_x<-t(X)%*%X
  xt_y<-t(X)%*%y
  SIGMA_inv<-solve(SIGMA)
  SIG_bet<-SIGMA_inv%*%beta0
  par1_sig2<-(nu0+n)/2
  par2_sig2<-nu0*sig20
  sig2<-var(y)
  cont<-1
  # almacenamiento
  BETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = 65)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar beta
    varbeta<-solve(SIGMA_inv+(1/sig2)*xt_x)
    beta<-c(mvtnorm::rmvnorm(1, mean=varbeta%*%(SIG_bet+(1/sig2)*xt_y),
                             sigma=varbeta))
    # actualizar sigma^2
    sig2 <- 1/rgamma(n = 1, shape = par1_sig2, rate = (par2_sig2+sum((y-X%*%beta)^2))/2)
    if (b>1000 && b%%10==0) {
      # almacenar
      BETA[cont,] <- c(beta, sig2)
      # log-verosimilitud
      LL[cont] <- mvtnorm::dmvnorm(y, mean = X%*%beta, sigma = diag(sig2,n), log = T)
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(BETA) <- c(paste0("beta", 1:p), "sig2")
  colnames(LL) <- c("ll")
  BETA <- as.data.frame(BETA)
  LL    <- as.data.frame(LL)
  return(list(BETA = BETA, LL = LL))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M1 <- REG_UN(B = 101000, y_train, data_train, beta0, SIGMA, sig20, nu0)
tictoc::toc()

par(mar=c(5, 5, 2, 2))
plot(1:10000,unlist(chain_M1$LL), type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 1")
plot(chain_M1$BETA[,1],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(beta))
plot(chain_M1$BETA[,65],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(sigma^2))

# tamaños efectivos de muestra
neff_theta_m1 <- coda::effectiveSize(chain_M1$BETA)
round(neff_theta_m1, 0)

# cv de monte carlo
cvmc_m1<-((apply(X = chain_M1$BETA, MARGIN = 2, FUN = sd)/sqrt(neff_theta_m1)))/abs(colMeans(chain_M1$BETA))*100
cvmc_m1

summary(cvmc_m1[1:64]) # de los betas
cvmc_m1[65] # de sigma2

### Previa g #####
# hiperparámetros
sig20<-sig2_ols
nu0<-1

REG_G <- function(B, y, X, sig20, nu0) {
  # tamaños
  n <- dim(X)[1]
  p <- dim(X)[2]
  g <- n
  # valores iniciales
  media_beta<-(g/(g+1))*solve(t(X)%*%X)%*%t(X)%*%y
  var_beta<-(g/(g+1))*solve(t(X)%*%X)
  SSE_G <-t(y)%*%(diag(1,n)-(g/(g+1))*X%*%solve(t(X)%*%X)%*%t(X))%*%y
  sig2 <- 1/rgamma(n = B, shape = (nu0+n)/2, rate = (nu0*sig20+SSE_G)/2)
  cont<-1
  # almacenamiento
  BETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = 65)
  LL  <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar beta
    beta<-c(mvtnorm::rmvnorm(1, mean=media_beta, sigma=sig2[b]*var_beta))
    if (b>1000 && b%%10==0) {
      # almacenar
      BETA[cont,] <- c(beta, sig2[b])
      # log-verosimilitud
      LL[cont] <- mvtnorm::dmvnorm(y, mean = X%*%beta, sigma = diag(sig2[b],n), log = T)
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(BETA) <- c(paste0("beta", 1:p), "sig2")
  colnames(LL) <- c("ll")
  BETA <- as.data.frame(BETA)
  LL    <- as.data.frame(LL)
  return(list(BETA = BETA, LL = LL))
}
# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M2 <- REG_G(B = 101000, y_train, data_train, sig20, nu0)
tictoc::toc()

par(mar=c(5, 5, 2, 2))
plot(1:10000,unlist(chain_M2$LL), type = "p", pch = ".", col="red", xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 2")
plot(chain_M2$BETA[,1],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(beta))
plot(chain_M2$BETA[,65],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(sigma^2))

# tamaños efectivos de muestra
neff_theta_m2 <- coda::effectiveSize(chain_M2$BETA)
round(neff_theta_m2, 0)

# cv de monte carlo
cvmc_m2<-((apply(X = chain_M2$BETA, MARGIN = 2, FUN = sd)/sqrt(neff_theta_m2)))/abs(colMeans(chain_M2$BETA))*100
cvmc_m2

summary(cvmc_m2[1:64]) # de los betas
cvmc_m2[65] # de sigma2

## Regresión Rígida #####
# hiperparámetros
sig20<-sig2_ols
nu0<-1
alam<-1
blam<-2

REG_RIDGE <- function(B, y, X, sig20, nu0, alam, blam) {
  # tamaños
  n <- dim(X)[1]
  p <- dim(X)[2]
  # valores iniciales
  xt_x<-t(X)%*%X
  xt_y<-t(X)%*%y
  par1_sig2<-(nu0+n+p)/2
  par2_sig2<-nu0*sig20
  par1_lam<-(p/2)+alam
  sig2<-var(y)
  lam<- rgamma(n = 1, shape = alam, rate = blam)
  cont<-1
  # almacenamiento
  BETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = 66)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar beta
    varbeta<-solve((xt_x+diag(lam,p))/sig2)
    beta<-c(mvtnorm::rmvnorm(1, mean=varbeta%*%(xt_y/sig2),
                             sigma=varbeta))
    # actualizar sigma^2
    sig2 <- 1/rgamma(n = 1, shape = par1_sig2, rate = (par2_sig2+lam*t(beta)%*%beta+sum((y-X%*%beta)^2))/2)
    # actualizar lambda
    lam <- rgamma(n = 1, shape = par1_lam, rate = (t(beta)%*%beta/2*sig2)+blam)
    if (b>1000 && b%%10==0) {
      # almacenar
      BETA[cont,] <- c(beta, sig2, lam)
      # log-verosimilitud
      LL[cont] <- mvtnorm::dmvnorm(y, mean = X%*%beta, sigma = diag(sig2,n), log = T)
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(BETA) <- c(paste0("beta", 1:p), "sig2", "lam")
  colnames(LL) <- c("ll")
  BETA <- as.data.frame(BETA)
  LL    <- as.data.frame(LL)
  return(list(BETA = BETA, LL = LL))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M3 <- REG_RIDGE(B = 101000, y_train, data_train, sig20, nu0, alam, blam)
tictoc::toc()

par(mar=c(5, 5, 2, 2))
plot(1:10000,unlist(chain_M3$LL), type = "p", pch = ".", col="darkgreen", xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 3")
plot(chain_M3$BETA[,1],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(beta))
plot(chain_M3$BETA[,65],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(sigma^2))
plot(chain_M3$BETA[,66],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(lambda))

# tamaños efectivos de muestra
neff_theta_m3 <- coda::effectiveSize(chain_M3$BETA)
round(neff_theta_m3, 0)

# cv de monte carlo
cvmc_m3<-((apply(X = chain_M3$BETA, MARGIN = 2, FUN = sd)/sqrt(neff_theta_m3)))/abs(colMeans(chain_M3$BETA))*100
cvmc_m3

summary(cvmc_m3[1:64]) # de los betas
cvmc_m3[65] # de sigma2
cvmc_m3[66] # de lambda

## Regresión con errores correlacionados #####
# hiperparámetros
SIGMA<-diag(50,p)
sig20<-sig2_ols
delta<-0.3
nu0<-1
arho<-0
brho<-1

REG_EC <- function(B, y, X, SIGMA, sig20, delta, nu0, arho, brho) {
  # tamaños
  n <- dim(X)[1]
  p <- dim(X)[2]
  # valores iniciales
  par1_sig2<-(nu0+n)/2
  par2_sig2<-nu0*sig20
  sig2<-var(y)
  SIGMA_inv<-solve(SIGMA)
  DY <-abs(outer((1:n),(1:n),"-"))#para construir la matriz de correlacion
  rho<-runif(1,arho,brho)
  Cor<-rho^DY
  iCor<-solve(Cor)
  ac<-0
  cont<-1
  # almacenamiento
  BETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = 66)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar beta
    varbeta<-solve(SIGMA_inv+(t(X)%*%iCor%*%X)/sig2)
    beta<-c(mvtnorm::rmvnorm(1, mean=varbeta%*%(t(X)%*%iCor%*%y/sig2),
                             sigma=varbeta))
    # actualizar sigma^2
    sig2<-1/rgamma(n = 1, shape = par1_sig2, rate = (par2_sig2+t(y-X%*%beta)%*%iCor%*%(y-X%*%beta))/2)
    # simular rho (metropolis)
    # 1. Propuesta
    rho_star <- abs(runif(1,rho-delta, rho+delta))
    rho_star <- min(rho_star, 2-rho_star)
    # 2. Tasa de aceptacion
    Cor_star<-rho_star^DY
    iCor_star<-solve(Cor_star)
    log_r <- -0.5*(determinant(Cor_star,log=TRUE)$mod-determinant(Cor,log=TRUE)$mod+tr((y-X%*%beta)%*%t(y-X%*%beta)%*%(iCor_star-iCor))/sig2)
    # 3. Actualizar valor
    if(runif(1) < exp(log_r)) {
      rho<-rho_star
      Cor<-rho^DY
      iCor<-solve(Cor)
      ac<-ac+1
    }
    if (b>1000 && b%%10==0) {
      # almacenar
      BETA[cont,] <- c(beta, sig2, rho)
      # log-verosimilitud
      LL[cont] <- mvtnorm::dmvnorm(y, mean = X%*%beta, sigma = sig2*Cor, log = T)
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(BETA) <- c(paste0("beta", 1:p), "sig2", "rho")
  colnames(LL) <- c("ll")
  BETA <- as.data.frame(BETA)
  LL    <- as.data.frame(LL)
  return(list(BETA = BETA, LL = LL, TA=ac/B))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M4 <- REG_EC(B = 101000, y_train, data_train, SIGMA, sig20, delta, nu0, arho, brho)
tictoc::toc()

chain_M4$TA
par(mar=c(5, 5, 2, 2))
plot(1:10000,unlist(chain_M4$LL), type = "p", pch = ".", col="purple", xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 4")
plot(chain_M4$BETA[,1],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(beta))
plot(chain_M4$BETA[,65],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(sigma^2))
plot(chain_M4$BETA[,66],type = "p", pch = ".", col="darkblue", xlab = "Iteración", ylab = expression(rho))

# tamaños efectivos de muestra
neff_theta_m4 <- coda::effectiveSize(chain_M4$BETA)
round(neff_theta_m4, 0)

# cv de monte carlo
cvmc_m4<-((apply(X = chain_M4$BETA, MARGIN = 2, FUN = sd)/sqrt(neff_theta_m4)))/abs(colMeans(chain_M4$BETA))*100
cvmc_m4

summary(cvmc_m4[1:64]) # de los betas
cvmc_m4[65] # de sigma2
cvmc_m4[66] # de rho

## Punto 1: Estimaciones#####
#y test estimados
y_test_estim_M1<-data_test%*%colMeans(chain_M1$BETA[,1:p])
y_test_estim_M2<-data_test%*%colMeans(chain_M2$BETA[,1:p])
y_test_estim_M3<-data_test%*%colMeans(chain_M3$BETA[,1:p])
y_test_estim_M4<-data_test%*%colMeans(chain_M4$BETA[,1:p])
#Error absoluto medio
n_test<-length(y_test)
EAM_M1<-(1/n_test)*sum(abs(y_test-y_test_estim_M1));EAM_M1
EAM_M2<-(1/n_test)*sum(abs(y_test-y_test_estim_M2));EAM_M2
EAM_M3<-(1/n_test)*sum(abs(y_test-y_test_estim_M3));EAM_M3
EAM_M4<-(1/n_test)*sum(abs(y_test-y_test_estim_M4));EAM_M4
#Graficos
datos<-rbind(cbind(y_test_estim_M1,c(rep(paste0("Modelo 1: Valor EAM = ", round(EAM_M1,3)),100))),
             cbind(y_test_estim_M2,c(rep(paste0("Modelo 2: Valor EAM = ", round(EAM_M2,3)),100))),
             cbind(y_test_estim_M3,c(rep(paste0("Modelo 3: Valor EAM = ", round(EAM_M3,3)),100))),
             cbind(y_test_estim_M4,c(rep(paste0("Modelo 4: Valor EAM = ", round(EAM_M4,3)),100))))

datos<-cbind(datos,rep(y_test,4))
colnames(datos)<-c("y_hat_test","Modelo","y_test")
datos<-data.frame(datos)
datos$y_hat_test<-as.numeric(datos$y_hat_test)
datos$y_test<-as.numeric(datos$y_test)

mi_tema <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                 axis.title.x = element_text(face = "italic", size = 10),
                 axis.title.y = element_text(face = "italic", size = 10),
                 strip.background=element_rect(color="black",fill="gray80",size=1),
                 strip.text=element_text(face="bold",size=9,vjust=0.5,hjust=0.5,color="darkblue",angle=0),
                 plot.margin = margin(5, 5, 2, 2),
                 legend.position = "none")

ggplot(datos, aes(x = y_test, y = y_hat_test, color = Modelo)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  labs(title = "Gráfico de Dispersión y Linea de Tendencia", x = expression(y[test]), y = expression(hat(y)[test])) +
  scale_color_manual(values = c("darkcyan", "darkgreen", "darkred", "purple")) +
  facet_wrap(~Modelo, ncol = 2, nrow = 2, strip.position = "top", dir = "h", scales = "fixed") +
  mi_tema


## Punto 2: Bondad de Ajuste#####
### Simulación #####
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
DY <- abs(outer((1:n),(1:n),"-"))
est_prueba <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  M1<-c(mvtnorm::rmvnorm(1, mean=data_train%*%as.numeric(chain_M1$BETA[i,(1:p)]), sigma=diag(chain_M1$BETA$sig2[i],n)))
  M2<-c(mvtnorm::rmvnorm(1, mean=data_train%*%as.numeric(chain_M2$BETA[i,(1:p)]), sigma=diag(chain_M2$BETA$sig2[i],n)))
  M3<-c(mvtnorm::rmvnorm(1, mean=data_train%*%as.numeric(chain_M3$BETA[i,(1:p)]), sigma=diag(chain_M3$BETA$sig2[i],n)))
  Cor<-chain_M4$BETA$rho[i]^DY
  M4<-c(mvtnorm::rmvnorm(1, mean=data_train%*%as.numeric(chain_M4$BETA[i,(1:p)]), sigma=chain_M4$BETA$sig2[i]*Cor))
  est_prueba <- c(mean(M1),mean(M2),mean(M3),mean(M4))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()

colnames(est_prueba)<-c("Media_M1","Media_M2","Media_M3","Media_M4")
res_final <- colMeans(est_prueba)
names(res_final)<-c("Media_M1","Media_M2","Media_M3","Media_M4")

###ppp valores#####
est_obs<-mean(y_train)
ppp <- c(mean(est_prueba[,1]<est_obs),mean(est_prueba[,2]<est_obs),
         mean(est_prueba[,3]<est_obs),mean(est_prueba[,4]<est_obs))
names(ppp) <- c("Media_M1","Media_M2","Media_M3","Media_M4")
ppp <- round(ppp, digits = 3)
ppp

### Gráficos predictiva posterior #####
est_prueba<-data.frame(est_prueba)
mitema <- theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
                axis.title.x = element_text(face = "italic", size = 9),
                axis.title.y = element_text(face = "italic", size = 9),
                strip.background=element_rect(color="black",fill="gray80",size=1),
                strip.text=element_text(face="bold",size=9,vjust=0.5,hjust=0.5,color="darkblue",angle=0),
                plot.margin = margin(5, 5, 2, 2))

# Convertir a formato largo (tidy)
colnames(est_prueba) <- c(paste0("Modelo 1: Valor ppp = ", ppp[1]),
                          paste0("Modelo 2: Valor ppp = ", ppp[2]),
                          paste0("Modelo 3: Valor ppp = ", ppp[3]),
                          paste0("Modelo 4: Valor ppp = ", ppp[4]))

est_prueba$Columna <- seq_len(nrow(est_prueba))
est_prueba_largo <- est_prueba %>%
  pivot_longer(cols = -Columna, names_to = "Columna_original", values_to = "Valor")

# Histogramas
gg_histogramas <- ggplot(est_prueba_largo, aes(x = Valor)) +
  geom_density(fill = "lightblue", color = "black", alpha = 1) +
  facet_wrap(~Columna_original, scales = "fixed") +
  labs(title="Distribución Predictiva Posterior",
       x = "Valores Medias Bonda de Ajuste",
       y = "Frecuencia")+mitema

for (i in unique(est_prueba_largo$Columna_original)) {
  gg_histogramas <- gg_histogramas +
    geom_vline(data = est_prueba_largo[est_prueba_largo$Columna_original == i,],
               aes(xintercept = mean(Valor)),
               linetype = "dashed", size = 1, color = "red")
}

gg_histogramas


## Punto 3: DIC#####
### DIC Modelo 1#####
LP1        <- as.numeric(chain_M1$LL$ll)
beta_hat   <- colMeans(chain_M1$BETA[,1:p])
sigma2_hat <- mean(chain_M1$BETA$sig2)
lpyth_m1   <- mvtnorm::dmvnorm(y_train, mean = data_train%*%beta_hat, sigma = diag(sigma2_hat,n), log = T)
pDIC_m1    <- 2*(lpyth_m1 - mean(LP1))
dic_m1     <- -2*lpyth_m1 + 2*pDIC_m1;dic_m1
### DIC Modelo 2#####
LP2        <- as.numeric(chain_M2$LL$ll)
beta_hat   <- colMeans(chain_M2$BETA[,1:p])
sigma2_hat <- mean(chain_M2$BETA$sig2)
lpyth_m2   <- mvtnorm::dmvnorm(y_train, mean = data_train%*%beta_hat, sigma = diag(sigma2_hat,n), log = T)
pDIC_m2    <- 2*(lpyth_m2 - mean(LP2))
dic_m2     <- -2*lpyth_m2 + 2*pDIC_m2;dic_m2
### DIC Modelo 3#####
LP3        <- as.numeric(chain_M3$LL$ll)
beta_hat   <- colMeans(chain_M3$BETA[,1:p])
sigma2_hat <- mean(chain_M3$BETA$sig2)
lpyth_m3   <- mvtnorm::dmvnorm(y_train, mean = data_train%*%beta_hat, sigma = diag(sigma2_hat,n), log = T)
pDIC_m3    <- 2*(lpyth_m3 - mean(LP3))
dic_m3     <- -2*lpyth_m3 + 2*pDIC_m3;dic_m3
### DIC Modelo 4#####
LP4        <- as.numeric(chain_M4$LL$ll)
beta_hat   <- colMeans(chain_M4$BETA[,1:p])
sigma2_hat <- mean(chain_M4$BETA$sig2)
DY         <- abs(outer((1:n),(1:n),"-"))
Cor        <- mean(chain_M4$BETA$rho)^DY
lpyth_m4   <- mvtnorm::dmvnorm(y_train, mean = data_train%*%beta_hat, sigma = sigma2_hat*Cor, log = T)
pDIC_m4    <- 2*(lpyth_m4 - mean(LP4))
dic_m4     <- -2*lpyth_m4 + 2*pDIC_m4;dic_m4
#Tabla
DIC <- data.frame(t(c(dic_m1,dic_m2,dic_m3,dic_m4)))
colnames(DIC) <- c("Modelo 1","Modelo 2","Modelo 3", "Modelo 4")
rownames(DIC) <- c("DIC")
DIC
xtable(DIC, digits = 3)
