if (!require(dplyr)){install.packages("dplyr");library(dplyr)}
if (!require(readxl)){install.packages("readxl");library(readxl)}
if (!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if (!require(gamlss)){install.packages("gamlss");library(gamlss)}
if (!require(readr)){install.packages("readr");library(readr)}
if (!require(stringr)){install.packages("stringr");library(stringr)}
if (!require(tidyr)){install.packages("tidyr");library(tidyr)}
if (!require(sf)){install.packages("sf");library(sf)}
if (!require(reshape2)){install.packages("reshape2");library(reshape2)} 
if (!require(doParallel)){install.packages("doParallel");library(doParallel)} ##Librería núcleos

setwd("C:/Users/gcvh2/Documents/UNAL/7. 2023 - 2S/Estadística Bayesiana/Caso de estudio 2")

# Caso de estudio 2

# Bases de datos ####

## SB11_20222 ####
saber22<-read.delim("SB11_20222.txt", header = TRUE, sep = ";", dec = ".")
str(saber22)

datos<-saber22%>%
  filter(ESTU_NACIONALIDAD=="COLOMBIA" &
           ESTU_PAIS_RESIDE=="COLOMBIA" &
           ESTU_ESTADOINVESTIGACION=="PUBLICAR" &
           COLE_COD_DEPTO_UBICACION!=88)%>%
  select(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION,
         COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION,
         PUNT_GLOBAL) %>%
  as.data.frame()
str(datos) # ya hay 525061 registros
rm(saber22)

# cambiamos COLE_COD_DEPTO_UBICACION a char
datos$COLE_COD_DEPTO_UBICACION<-as.character(datos$COLE_COD_DEPTO_UBICACION)
datos$COLE_COD_DEPTO_UBICACION<-ifelse(datos$COLE_COD_DEPTO_UBICACION=="5","05",datos$COLE_COD_DEPTO_UBICACION)
datos$COLE_COD_DEPTO_UBICACION<-ifelse(datos$COLE_COD_DEPTO_UBICACION=="8","08",datos$COLE_COD_DEPTO_UBICACION)

# cambiamos COLE_COD_MCPIO_UBICACION a char
datos$COLE_COD_MCPIO_UBICACION<-as.character(datos$COLE_COD_MCPIO_UBICACION)
datos$COLE_COD_MCPIO_UBICACION<-ifelse(datos$COLE_COD_DEPTO_UBICACION=="05",paste("0",datos$COLE_COD_MCPIO_UBICACION,sep=""),datos$COLE_COD_MCPIO_UBICACION)
datos$COLE_COD_MCPIO_UBICACION<-ifelse(datos$COLE_COD_DEPTO_UBICACION=="08",paste("0",datos$COLE_COD_MCPIO_UBICACION,sep=""),datos$COLE_COD_MCPIO_UBICACION)


# Revisamos que no hayan datos faltantes
sum(is.na(datos$COLE_COD_MCPIO_UBICACION))
sum(is.na(datos$COLE_COD_DEPTO_UBICACION))
sum(is.na(datos$PUNT_GLOBAL))

# Ordenamos por código para que no haya problemas después
datos<-datos%>%arrange(COLE_COD_DEPTO_UBICACION,COLE_COD_MCPIO_UBICACION)

## estadisticas educacion.csv ####
estadisticaseducacion<-read.csv("estadísticas educación.csv", header = TRUE)
# cambiamos estadisticaseducacion$CÓDIGO_DEPARTAMENTOa char
estadisticaseducacion$CÓDIGO_DEPARTAMENTO<-as.character(estadisticaseducacion$CÓDIGO_DEPARTAMENTO)
estadisticaseducacion$CÓDIGO_DEPARTAMENTO<-ifelse(estadisticaseducacion$CÓDIGO_DEPARTAMENTO=="5","05",estadisticaseducacion$CÓDIGO_DEPARTAMENTO)
estadisticaseducacion$CÓDIGO_DEPARTAMENTO<-ifelse(estadisticaseducacion$CÓDIGO_DEPARTAMENTO=="8","08",estadisticaseducacion$CÓDIGO_DEPARTAMENTO)
# cambiamos estadisticaseducacion$CÓDIGO_MUNICIPIO a char
estadisticaseducacion$CÓDIGO_MUNICIPIO<-as.character(estadisticaseducacion$CÓDIGO_MUNICIPIO)
estadisticaseducacion$CÓDIGO_MUNICIPIO<-ifelse(estadisticaseducacion$CÓDIGO_DEPARTAMENTO=="05",paste("0",estadisticaseducacion$CÓDIGO_MUNICIPIO,sep=""),estadisticaseducacion$CÓDIGO_MUNICIPIO)
estadisticaseducacion$CÓDIGO_MUNICIPIO<-ifelse(estadisticaseducacion$CÓDIGO_DEPARTAMENTO=="08",paste("0",estadisticaseducacion$CÓDIGO_MUNICIPIO,sep=""),estadisticaseducacion$CÓDIGO_MUNICIPIO)

str(estadisticaseducacion)

# cobertura neta secundaria en 2022
cobertura <- estadisticaseducacion %>%
  filter(AÑO==2022)%>%
  select(CÓDIGO_MUNICIPIO,MUNICIPIO,COBERTURA_NETA_SECUNDARIA)%>%
  as.data.frame()
str(cobertura)
rm(estadisticaseducacion)

## pobreza monetaria.xls ####

pobrezamonetaria <- read_excel("pobreza monetaria.xls",sheet=2,col_names=TRUE,range="A16:P41")
colnames(pobrezamonetaria)[1] <- "Dep"
coddep <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "27",
            "23", "25", "41", "44", "47", "50", "52", "54", "63", "66",
            "68", "70", "73", "76", "00")
pobrezamonetaria <- cbind(coddep,pobrezamonetaria)
str(pobrezamonetaria)
rm(coddep)


# incidencia de la pobreza monetaria en 2018
pobreza <- pobrezamonetaria %>%
  select(coddep,Dep,'2018') %>%
  as.data.frame()
str(pobreza)
rm(pobrezamonetaria)

# Modelos ####

## M1 ####
# Modelo Normal
y <- datos$PUNT_GLOBAL
n <- as.numeric(length(y))

# hiperparámetros
mu0 <- 250
t20 <- 50^2
nu0 <- 1
s20 <- 50^2

# algoritmo
MCMC_1 <- function(B, n, y, mu0, t20, nu0, s20) {
  # valores iniciales
  isig2 <- rgamma(n = 1, shape = nu0/2, rate = nu0*s20/2)
  mean_y <- mean(y)
  sum_y  <- as.numeric(sum(y))
  nun   <- (nu0 + n)/2
  par1_s2n <- nu0*s20 + (n-1)*var(y)
  cont <- 1
  # almacenamiento
  THETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = 2)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar theta
    t2n   <- 1/(1/t20 + n*isig2)
    mun   <- t2n*(mu0/t20 + isig2*sum_y)
    theta <- rnorm(n = 1, mean = mun, sd = sqrt(t2n))
    # actualizar sigma^2
    s2n   <- par1_s2n + n*(mean_y-theta)^2
    isig2 <- rgamma(n = 1, shape = nun, rate = s2n/2)
    if (b>1000 && b%%10==0) {
      # almacenar
      THETA[cont,] <- c(theta, 1/isig2)
      # log-verosimilitud
      LL[cont] <- sum(dnorm(x = y, mean = theta, sd = sqrt(1/isig2), log = T))
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA) <- c("theta", "sig2")
  colnames(LL) <- c("ll")
  THETA <- as.data.frame(THETA)
  LL    <- as.data.frame(LL)
  return(list(THETA = THETA, LL = LL))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M1 <- MCMC_1(B = 101000, n, y, mu0, t20, nu0, s20)
tictoc::toc()

plot(1:10000,unlist(chain_M1$LL), type = "p", pch = ".", col="purple", xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 1")
plot(chain_M1$THETA[,1],type = "p", pch = ".", xlab = "Iteración", ylab = expression(theta))
plot(chain_M1$THETA[,2],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma^2))

# tamaños efectivos de muestra
neff_ST   <- coda::effectiveSize(chain_M1$THETA[,c(1,2)])
round(neff_ST, 0)

# errores estándar de MC
round(apply(X = chain_M1$THETA[,c(1,2)], MARGIN = 2, FUN = sd)/sqrt(neff_ST), 3)

# cv de monte carlo
cv_ST<-((round(apply(X = chain_M1$THETA[,c(1,2)], MARGIN = 2, FUN = sd)/sqrt(neff_ST), 3))/colMeans(chain_M1$THETA[,c(1,2)]))*100
cv_ST

## M2 ####
# Modelo normal con medias específicas por departamento
# m : número de grupos (departamentos)
# n : número de individuos (estudiantes)
m <- as.numeric(length(table(datos$COLE_COD_DEPTO_UBICACION)))
# tabla
estadisticos <- datos %>%
  group_by(COLE_COD_DEPTO_UBICACION) %>%
  summarise(codigo = unique(COLE_COD_DEPTO_UBICACION),
            nombre = unique(COLE_DEPTO_UBICACION),
            nj = n(),
            yb = mean(PUNT_GLOBAL),
            s2 = var(PUNT_GLOBAL)) %>%
  select(codigo,nombre,nj,yb,s2)

head(estadisticos, n = 5)
# y  : puntaje de los estudiantes (c)
# nj : número de estudiantes por departamento (c)
# yb : promedios por departamento (c)
# s2 : varianzas por departamento (c)
y <- datos$PUNT_GLOBAL
nj <- estadisticos$nj
yb <- estadisticos$yb
s2 <- estadisticos$s2
rm(estadisticos)
#Hiperparámetros
mu0  <- 250
g20  <- 50^2
eta0 <- 1
t20  <- 50^2
nu0  <- 1
s20  <- 50^2

# algoritmo
MCMC_2 <- function(B, nj, yb, s2, mu0, g20, eta0, t20, nu0, s20) {
  # tamaños
  n <- sum(nj)
  m <- length(nj)
  # valores iniciales
  theta <- yb
  sig2  <- mean(s2)
  mu    <- mean(theta)
  tau2  <- var(theta)
  par1_sig2 <- (nu0+n)/2
  par2_sig2 <- nu0*s20
  par1_mu <- mu0/g20
  par2_mu <- 1/g20
  par1_tau2 <- (eta0+m)/2
  par2_tau2 <- eta0*t20
  cont <- 1
  # almacenamiento
  THETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = m+3)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/tau2 + nj/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nj*yb/sig2), sd = sqrt(vtheta))
    # actualizar sigma^2
    sig2 <- 1/rgamma(n = 1, shape = par1_sig2, rate = 0.5*(par2_sig2 + sum((nj-1)*s2 + nj*(yb - theta)^2)))
    # actualizar mu
    vmu <- 1/(par2_mu + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(par1_mu + m*mean(theta)/tau2), sd = sqrt(vmu))
    # actualizar tau^2
    tau2 <- 1/rgamma(n = 1, shape = par1_tau2, rate = 0.5*(par2_tau2 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    if (b>1000 && b%%10==0) {
      # almacenar
      THETA[cont,] <- c(theta, sig2, mu, tau2)
      # log-verosimilitud
      LL[cont] <- sum(dnorm(x = y, mean = rep(theta, nj), sd = sqrt(sig2), log = T))
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA) <- c(paste0("theta",1:m), "sig2", "mu", "tau2")
  colnames(LL) <- c("ll")
  THETA <- as.data.frame(THETA)
  LL    <- as.data.frame(LL)
  return(list(THETA = THETA, LL = LL))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M2 <- MCMC_2(B = 101000, nj, yb, s2, mu0, g20, eta0, t20, nu0, s20)
tictoc::toc()

plot(1:10000,unlist(chain_M2$LL), type = "p", pch = ".", col="cyan", ylim=c(-2797970,-2797070), xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 2")
plot(chain_M2$THETA[,1],type = "p", pch = ".", xlab = "Iteración", ylab = expression(theta))
plot(chain_M2$THETA[,33],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma^2))
plot(chain_M2$THETA[,34],type = "p", pch = ".", xlab = "Iteración", ylab = expression(mu))
plot(chain_M2$THETA[,35],type = "p", pch = ".", xlab = "Iteración", ylab = expression(tau^2))

# tamaños efectivos de muestra
neff_SMT   <- coda::effectiveSize(chain_M2$THETA[,m+(1:3)])
round(neff_SMT, 0)

neff_THETA <- coda::effectiveSize(chain_M2$THETA[,1:m])
summary(neff_THETA)

# errores estándar de MC
round(apply(X = chain_M2$THETA[,m+(1:3)], MARGIN = 2, FUN = sd)/sqrt(neff_SMT), 3)
round(summary(apply(X = chain_M2$THETA[,1:m], MARGIN = 2, FUN = sd)/sqrt(neff_THETA)), 3)

# cv de monte carlo
cv_SMT<-((round(apply(X = chain_M2$THETA[,m+(1:3)], MARGIN = 2, FUN = sd)/sqrt(neff_SMT), 3))/colMeans(chain_M2$THETA[,m+(1:3)]))*100
cv_SMT

cv_THETA<-((round(apply(X = chain_M2$THETA[,1:m], MARGIN = 2, FUN = sd)/sqrt(neff_THETA), 3))/colMeans(chain_M2$THETA[,1:m]))*100
summary(cv_THETA)

## M3 ####
# Modelo normal con medias y varianzas específicas por departamento
# tabla
estadisticos <- datos %>%
  group_by(COLE_COD_DEPTO_UBICACION) %>%
  summarise(codigo = unique(COLE_COD_DEPTO_UBICACION),
            nombre = unique(COLE_DEPTO_UBICACION),
            nj = n(),
            yb = mean(PUNT_GLOBAL),
            s2 = var(PUNT_GLOBAL)) %>%
  select(codigo,nombre,nj,yb,s2)

head(estadisticos, n = 5)
nj <- estadisticos$nj
yb <- estadisticos$yb
s2 <- estadisticos$s2

# hiperparámetros
mu0  <- 250
g20  <- 50^2
eta0 <- 1
t20  <- 50^2
lam0 <- 1
be0  <- 1/50^2

# algoritmo modelo 3
MCMC_3 <- function(B, nj, yb, s2, mu0, g20, eta0, t20, lam0, be0) {
  # tamaños
  n <- sum(nj)
  m <- length(nj)
  # valores iniciales
  theta <- yb
  sig2  <- s2  # sigma_j^2
  mu    <- mean(theta)
  tau2  <- var(theta)
  ups2  <- 100  # sigma^2
  par1_sig2 <- (1+nj)/2 #(nu+nj)/2 pero nu=1 constante
  par1_mu <- mu0/g20
  par2_mu <- 1/g20
  par1_tau2 <- (eta0+m)/2
  par2_tau2 <- eta0*t20
  par1_ups2 <- (lam0+m)/2
  cont <- 1
  # almacenamiento
  THETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = 2*m+3)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/tau2 + nj/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nj*yb/sig2), sd = sqrt(vtheta))
    # actualizar sigma_j^2
    sig2 <- 1/rgamma(n = m, shape = par1_sig2, rate = 0.5*(ups2 + (nj-1)*s2 + nj*(yb - theta)^2))
    # actualizar mu
    vmu <- 1/(par2_mu + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(par1_mu + m*mean(theta)/tau2), sd = sqrt(vmu))
    # actualizar tau2
    tau2 <- 1/rgamma(n = 1, shape = par1_tau2, rate = 0.5*(par2_tau2 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    # actualizar sigma^2
    ups2 <- rgamma(n = 1, shape = par1_ups2, rate = 0.5*(be0 + sum(1/sig2)))
    if (b>1000 && b%%10==0) {
      # almacenar
      THETA[cont,] <- c(theta, sig2, mu, tau2, ups2)
      # log-verosimilitud
      LL[cont] <- sum(dnorm(x = y, mean = rep(theta, nj), sd = sqrt(rep(sig2, nj)), log = T))
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA) <- c(paste0("theta", 1:m), paste0("sig2", 1:m), "mu", "tau2", "ups2")
  colnames(LL) <- c("ll")
  THETA <- as.data.frame(THETA)
  LL    <- as.data.frame(LL)
  return(list(THETA = THETA, LL = LL))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M3 <- MCMC_3(B = 101000, nj, yb, s2, mu0, g20, eta0, t20, lam0, be0)
tictoc::toc()

plot(1:10000,unlist(chain_M3$LL), type = "p", pch = ".", col="blue", ylim=c(-2797970,-2797070), xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 3")
plot(chain_M3$THETA[,1],type = "p", pch = ".", xlab = "Iteración", ylab = expression(theta))
plot(chain_M3$THETA[,33],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma_j))
plot(chain_M3$THETA[,65],type = "p", pch = ".", xlab = "Iteración", ylab = expression(mu))
plot(chain_M3$THETA[,66],type = "p", pch = ".", xlab = "Iteración", ylab = expression(tau^2))
plot(chain_M3$THETA[,67],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma^2))

# tamaños efectivos de muestra
neff_SMT   <- coda::effectiveSize(chain_M3$THETA[,2*m+(1:3)])
round(neff_SMT, 0)

neff_THETA <- coda::effectiveSize(chain_M3$THETA[,1:m])
summary(neff_THETA)

neff_SIGMA <- coda::effectiveSize(chain_M3$THETA[,(m+1):(m*2)])
summary(neff_SIGMA)

# errores estándar de MC
round(apply(X = chain_M3$THETA[,2*m+(1:3)], MARGIN = 2, FUN = sd)/sqrt(neff_SMT), 3)

round(summary(apply(X = chain_M3$THETA[,1:m], MARGIN = 2, FUN = sd)/sqrt(neff_THETA)), 3)

round(summary(apply(X = chain_M3$THETA[,(m+1):(m*2)], MARGIN = 2, FUN = sd)/sqrt(neff_SIGMA)), 3)

# cv de monte carlo
cv_SMT<-((round(apply(X = chain_M3$THETA[,2*m+(1:3)], MARGIN = 2, FUN = sd)/sqrt(neff_SMT), 3))/colMeans(chain_M3$THETA[,2*m+(1:3)]))*100
cv_SMT

cv_THETA<-((round(apply(X = chain_M3$THETA[,1:m], MARGIN = 2, FUN = sd)/sqrt(neff_THETA), 3))/colMeans(chain_M3$THETA[,1:m]))*100
summary(cv_THETA)

cv_SIGMA<-((round(apply(X = chain_M3$THETA[,(m+1):(m*2)], MARGIN = 2, FUN = sd)/sqrt(neff_SIGMA), 3))/colMeans(chain_M3$THETA[,(m+1):(m*2)]))*100
summary(cv_SIGMA)


## M4 ####
# Modelo Normal con medias específicas por municipio y departamento
estadisticos_mup <- datos %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(codigo = unique(COLE_COD_MCPIO_UBICACION),
            nombre = unique(COLE_MCPIO_UBICACION),
            nj = n(),
            yb = mean(PUNT_GLOBAL),
            s2 = var(PUNT_GLOBAL)) %>%
  select(codigo,nombre,nj,yb,s2)

munxdep <- datos %>%
  select(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION,
         COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION) %>%
  distinct()

munxdep <- munxdep %>%
  group_by(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION) %>%
  summarise(nmun = n())

head(estadisticos_mup, n = 5)
njk <- estadisticos_mup$nj # numero de individuos por municipio
ybjk <- estadisticos_mup$yb
s2jk <- estadisticos_mup$s2
nk <- munxdep$nmun # numero de municipios por depto
y <- datos$PUNT_GLOBAL
rm(estadisticos_mup)

# hiperparámetros
eps0  <- 1
ka20  <- 50^2
mu0 <- 250
g20  <- 50^2
eta0 <- 1
t20  <- 50^2
sig20 <- 50^2

# algoritmo modelo 4
MCMC_4 <- function(B, njk, ybjk, s2jk, nk, eps0, ka20, mu0, g20, eta0, t20, sig20) {
  # tamaños
  n <- sum(njk)
  m <- length(nk)
  f <- length(njk)
  # valores iniciales
  kap2 <- 100
  inicio <- 1
  final <- 0
  theta <- numeric(m)
  for (r in 1:m) {
    final <- final+nk[r]
    theta[r] <- mean(ybjk[inicio:final])
    inicio <- final+1
  }
  inicio <- 1
  final <- 0
  sig2 <- mean(s2jk)
  mu    <- mean(theta)
  tau2  <- var(theta)
  nu <- 1
  cont <- 1
  # almacenamiento
  THETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = f+m+4)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar zeta
    vzeta <- 1/(1/sig2 + njk/kap2)
    zeta <- rnorm(n = f, mean = vzeta*(rep(theta,nk)/sig2 + njk*ybjk/kap2), sd = sqrt(vzeta))
    zetak <- numeric(m)
    vzetak <- numeric(m)
    inicio <- 1
    final <- 0
    for (r in 1:m) {
      final <- final+nk[r]
      zetak[r] <- mean(zeta[inicio:final])
      vzetak[r] <- var(zeta[inicio:final])
      inicio <- final+1
    }
    vzetak[3] <- (zeta[149]-theta[3])^2
    # actualizar theta
    vtheta <- 1/(1/tau2 + nk/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nk*zetak/sig2), sd = sqrt(vtheta))
    # actualizar sigma_^2
    sig2 <- 1/rgamma(n = 1, shape = (nu+f)/2, rate = 0.5*(nu*sig20 + sum((nk-1)*vzetak + nk*(zetak - theta)^2)))
    # actualizar kap2
    kap2 <- 1/rgamma(n = 1, shape = (eps0+n)/2, rate = 0.5*(eps0*ka20 + sum((njk-1)*s2jk + njk*(ybjk-zeta)^2) ))
    # actualizar mu
    vmu <- 1/(1/g20 + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(mu0/g20 + m*mean(theta)/tau2), sd = sqrt(vmu))
    # actualizar tau2
    tau2 <- 1/rgamma(n = 1, shape = (eta0+m)/2, rate = 0.5*(eta0*t20 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    if (b>1000 && b%%10==0) {
      # almacenar
      THETA[cont,] <- c(zeta,theta, sig2, kap2, mu, tau2)
      # log-verosimilitud
      LL[cont] <- sum(dnorm(x = y, mean = rep(zeta, njk), sd = sqrt(kap2), log = T))
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA) <- c(paste0("zeta", 1:f), paste0("theta", 1:m), "sig2", "kap2", "mu", "tau2")
  colnames(LL) <- c("ll")
  THETA <- as.data.frame(THETA)
  LL    <- as.data.frame(LL)
  return(list(THETA = THETA, LL = LL))
}

# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M4 <- MCMC_4(B = 101000, njk, ybjk, s2jk, nk, eps0, ka20, mu0, g20, eta0, t20, sig20)
tictoc::toc()

par(mar=c(5, 5, 2, 2))
plot(1:10000,unlist(chain_M4$LL), type = "p", pch = ".", col="red", ylim=c(-2775500,-2775350), xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 4")
plot(chain_M4$THETA[,1],type = "p", pch = ".", xlab = "Iteración", ylab = expression(zeta))
plot(chain_M4$THETA[,1113],type = "p", pch = ".", xlab = "Iteración", ylab = expression(theta))
plot(chain_M4$THETA[,1145],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma^2))
plot(chain_M4$THETA[,1146],type = "p", pch = ".", xlab = "Iteración", ylab = expression(kappa))
plot(chain_M4$THETA[,1147],type = "p", pch = ".", xlab = "Iteración", ylab = expression(mu))
plot(chain_M4$THETA[,1148],type = "p", pch = ".", xlab = "Iteración", ylab = expression(tau^2))

# tamaños efectivos de muestra
neff_ZETA   <- coda::effectiveSize(chain_M4$THETA[,1:1112])
summary(neff_ZETA, 0)

neff_THETA <- coda::effectiveSize(chain_M4$THETA[,1113:1144])
summary(neff_THETA)

neff_SKMT <- coda::effectiveSize(chain_M4$THETA[,1144+(1:4)])
round(neff_SKMT, 0)

# errores estándar de MC
round(apply(X = chain_M4$THETA[,1144+(1:4)], MARGIN = 2, FUN = sd)/sqrt(neff_SKMT), 3)

round(summary(apply(X = chain_M4$THETA[,1113:1144], MARGIN = 2, FUN = sd)/sqrt(neff_THETA)), 3)

round(summary(apply(X = chain_M4$THETA[,1:1112], MARGIN = 2, FUN = sd)/sqrt(neff_ZETA)), 3)

# cv de monte carlo
cv_SKMT <-((round(apply(X = chain_M4$THETA[,1144+(1:4)], MARGIN = 2, FUN = sd)/sqrt(neff_SKMT), 3))/colMeans(chain_M4$THETA[,1144+(1:4)]))*100
cv_SKMT

cv_THETA<-((round(apply(X = chain_M4$THETA[,1113:1144], MARGIN = 2, FUN = sd)/sqrt(neff_THETA), 3))/colMeans(chain_M4$THETA[,1113:1144]))*100
summary(cv_THETA)

cv_ZETA<-((round(apply(X = chain_M4$THETA[,1:1112], MARGIN = 2, FUN = sd)/sqrt(neff_ZETA), 3))/colMeans(chain_M4$THETA[,1:1112]))*100
summary(cv_ZETA)

## M5 ####

estadisticos_mup <- datos %>%
  group_by(COLE_COD_MCPIO_UBICACION) %>%
  summarise(codigo = unique(COLE_COD_MCPIO_UBICACION),
            nombre = unique(COLE_MCPIO_UBICACION),
            nj = n(),
            yb = mean(PUNT_GLOBAL),
            s2 = var(PUNT_GLOBAL)) %>%
  select(codigo,nombre,nj,yb,s2)

munxdep <- datos %>%
  select(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION,
         COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION) %>%
  distinct()

munxdep <- munxdep %>%
  group_by(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION) %>%
  summarise(nmun = n())

head(estadisticos_mup, n = 5)
njk <- estadisticos_mup$nj
ybjk <- estadisticos_mup$yb
s2jk <- estadisticos_mup$s2
nk <- munxdep$nmun
y <- datos$PUNT_GLOBAL

# hiperparámetros
eps0  <- 1
ka20  <- 50^2
mu0 <- 250
g20  <- 50^2
eta0 <- 1
t20  <- 50^2
lam0 <- 1
be0  <- 1/50^2

# algoritmo modelo 5
MCMC_5 <- function(B, njk, ybjk, s2jk, nk, eps0, ka20, mu0, g20, eta0, t20, lam0, be0) {
  # tamaños
  n <- sum(njk)
  m <- length(nk)
  f <- length(njk)
  # valores iniciales
  kap2 <- 100
  inicio <- 1
  final <- 0
  theta <- numeric(m)
  for (r in 1:m) {
    final <- final+nk[r]
    theta[r] <- mean(ybjk[inicio:final])
    inicio <- final+1
  }
  inicio <- 1
  final <- 0
  sig2 <- numeric(m)
  for (r in 1:m) {
    final <- final+nk[r]
    sig2[r] <- mean(s2jk[inicio:final])
    inicio <- final+1
  }# sigma_k^2
  mu    <- mean(theta)
  tau2  <- var(theta)
  ups2  <- 100  # sigma^2
  nu <- 1
  cont <- 1
  # almacenamiento
  THETA <- matrix(data = NA, nrow = (B-1000)/10, ncol = f+2*m+4)
  LL    <- matrix(data = NA, nrow = (B-1000)/10, ncol = 1)
  # cadena
  for (b in 1:B) {
    # actualizar zeta
    vzeta <- 1/(1/rep(sig2,nk) + njk/kap2)
    zeta <- rnorm(n = f, mean = vzeta*(rep(theta,nk)/rep(sig2,nk) + njk*ybjk/kap2), sd = sqrt(vzeta))
    zetak <- numeric(m)
    vzetak <- numeric(m)
    inicio <- 1
    final <- 0
    for (r in 1:m) {
      final <- final+nk[r]
      zetak[r] <- mean(zeta[inicio:final])
      vzetak[r] <- var(zeta[inicio:final])
      inicio <- final+1
    }
    vzetak[3] <- (zeta[149]-theta[3])^2
    # actualizar theta
    vtheta <- 1/(1/tau2 + nk/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nk*zetak/sig2), sd = sqrt(vtheta))
    # actualizar sigma_k^2
    sig2 <- 1/rgamma(n = m, shape = (nu+nk)/2, rate = 0.5*(nu*ups2 + (nk-1)*vzetak + nk*(zetak - theta)^2))
    # actualizar kap2
    kap2 <- 1/rgamma(n = 1, shape = (eps0+n)/2, rate = 0.5*(eps0*ka20 + sum((njk-1)*s2jk + njk*(ybjk-zeta)^2) ))
    # actualizar mu
    vmu <- 1/(1/g20 + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(mu0/g20 + m*mean(theta)/tau2), sd = sqrt(vmu))
    # actualizar tau2
    tau2 <- 1/rgamma(n = 1, shape = (eta0+m)/2, rate = 0.5*(eta0*t20 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    # actualizar sigma^2
    ups2 <- rgamma(n = 1, shape = (m*nu+lam0)/2, rate = 0.5*(be0 + nu*sum(1/sig2)))
    if (b>1000 && b%%10==0) {
      # almacenar
      THETA[cont,] <- c(zeta,theta, sig2, kap2, mu, tau2, ups2)
      # log-verosimilitud
      LL[cont] <- sum(dnorm(x = y, mean = rep(zeta, njk), sd = sqrt(kap2), log = T))
      cont <- cont+1
    }
  }
  # fin de la cadena
  # salida
  colnames(THETA) <- c(paste0("zeta", 1:f), paste0("theta", 1:m), paste0("sig2", 1:m), "kap2", "mu", "tau2", "ups2")
  colnames(LL) <- c("ll")
  THETA <- as.data.frame(THETA)
  LL    <- as.data.frame(LL)
  return(list(THETA = THETA, LL = LL))
}


# ajuste del modelo
tictoc::tic()
set.seed(17112000)
chain_M5 <- MCMC_5(B = 101000, njk, ybjk, s2jk, nk, eps0, ka20, mu0, g20, eta0, t20, lam0, be0)
tictoc::toc()

par(mar=c(5, 5, 2, 2))
plot(1:10000,unlist(chain_M5$LL), type = "p", pch = ".", col="darkgreen", ylim=c(-2775500,-2775350), xlab = "Iteración", ylab = "Log-verosimilitud", main="Modelo 5")
plot(chain_M5$THETA[,1],type = "p", pch = ".", xlab = "Iteración", ylab = expression(zeta))
plot(chain_M5$THETA[,1113],type = "p", pch = ".", xlab = "Iteración", ylab = expression(theta))
plot(chain_M5$THETA[,1145],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma_j))
plot(chain_M5$THETA[,1177],type = "p", pch = ".", xlab = "Iteración", ylab = expression(kappa))
plot(chain_M5$THETA[,1178],type = "p", pch = ".", xlab = "Iteración", ylab = expression(mu))
plot(chain_M5$THETA[,1179],type = "p", pch = ".", xlab = "Iteración", ylab = expression(tau^2))
plot(chain_M5$THETA[,1180],type = "p", pch = ".", xlab = "Iteración", ylab = expression(sigma^2))

# tamaños efectivos de muestra
neff_ZETA   <- coda::effectiveSize(chain_M5$THETA[,1:1112])
summary(neff_ZETA, 0)

neff_THETA <- coda::effectiveSize(chain_M5$THETA[,1113:1144])
summary(neff_THETA)

neff_SIGMA <- coda::effectiveSize(chain_M5$THETA[,1145:1176])
summary(neff_THETA)

neff_KMTS <- coda::effectiveSize(chain_M5$THETA[,1176+(1:4)])
round(neff_KMTS, 0)

# errores estándar de MC
round(apply(X = chain_M5$THETA[,1176+(1:4)], MARGIN = 2, FUN = sd)/sqrt(neff_KMTS), 3)

round(summary(apply(X = chain_M5$THETA[,1113:1144], MARGIN = 2, FUN = sd)/sqrt(neff_THETA)), 3)

round(summary(apply(X = chain_M5$THETA[,1145:1176], MARGIN = 2, FUN = sd)/sqrt(neff_SIGMA)), 3)

round(summary(apply(X = chain_M5$THETA[,1:1112], MARGIN = 2, FUN = sd)/sqrt(neff_ZETA)), 3)

# cv de monte carlo
cv_KMTS <-((round(apply(X = chain_M5$THETA[,1176+(1:4)], MARGIN = 2, FUN = sd)/sqrt(neff_KMTS), 3))/colMeans(chain_M5$THETA[,1176+(1:4)]))*100
cv_KMTS

cv_THETA<-((round(apply(X = chain_M5$THETA[,1113:1144], MARGIN = 2, FUN = sd)/sqrt(neff_THETA), 3))/colMeans(chain_M5$THETA[,1113:1144]))*100
summary(cv_THETA)

cv_SIGMA<-((round(apply(X = chain_M5$THETA[,1145:1176], MARGIN = 2, FUN = sd)/sqrt(neff_SIGMA), 3))/colMeans(chain_M5$THETA[,1145:1176]))*100
summary(cv_SIGMA)

cv_ZETA<-((round(apply(X = chain_M5$THETA[,1:1112], MARGIN = 2, FUN = sd)/sqrt(neff_ZETA), 3))/colMeans(chain_M5$THETA[,1:1112]))*100
summary(cv_ZETA)

# Punto 1 ####
punto1 <- datos %>%
  group_by(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION) %>%
  summarise(media_pun=mean(PUNT_GLOBAL)) %>%
  select(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION, media_pun)%>%
  as.data.frame()

punto1 <- punto1 %>%
  left_join(pobreza,by=c("COLE_COD_DEPTO_UBICACION"="coddep")) %>%
  select(CodDep=COLE_COD_DEPTO_UBICACION,Departamento=COLE_DEPTO_UBICACION,
         media_pun,IPM='2018') %>%
  as.data.frame()

setwd("C:/Users/gcvh2/Documents/UNAL/7. 2023 - 2S/Estadística Bayesiana/Caso de estudio 2/Mapas Colombia y Mundo")

#Aquí guardo área y poligonos para que R me logre graficar los departamentos
deptoshp <- st_read("MGN_DPTO_POLITICO.shp",quiet=TRUE)

#Usamos Left join para que haga el cruce por código de departamento y deje todas las columnas
#de deptoshp
mapdeptos <- deptoshp %>%
  left_join(punto1,by=c("DPTO_CCDGO"="CodDep")) %>%
  select(!(Departamento))

#Ahora mundoshp es un mapa de todo el mundo y estamos filtrando los que limitan con locombia
mundoshp <- st_read("admin00.shp",quiet=TRUE)
mundocol <- mundoshp %>%
  filter(CNTRY_NAME %in% c("Peru","Brazil","Venezuela","Ecuador","Panama"))

#Box calcula el rectangulo que envuelve a todo el conjunto de media_pun_tabla mapdeptos (locombia)
#box deja un poquito a la izquierda porque tiene en cuenta a SAN ANDRES
box <- st_bbox(mapdeptos)
box

mitema <- theme(legend.title=element_text(hjust = 0.5,color = "black"),
                legend.text=element_text(hjust = 0.5),
                plot.title=element_text(face="bold",size=15,vjust=0.5,hjust=0.5,color="black"),
                axis.title.x = element_text(hjust = 0.5, color = "black"),
                axis.title.y = element_text(hjust = 0.5, color = "black"))

# Gráfica de media puntaje global
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=media_pun),col="darkgray",linetype="solid") + #aes fill=media_pun, el color de relleno dependera de media_pun
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  geom_sf_text(data=mapdeptos,aes(label=ifelse(media_pun > quantile(media_pun, probs = 0.75, na.rm = TRUE),DPTO_CNMBR,"")),col="black",
               fontface="bold",size=2.5,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Media del/nPuntaje/nGlobal") + #Nombre Ejes
  scale_fill_gradient(low="lightgreen",high="red",n.breaks=5) + #Le damos colores de blanco a azul
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema

# Gráfica de Incidencia de la pobreza monetaria
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=IPM),col="darkgray",linetype="solid") + #aes fill='IPM', el color de relleno dependera de 'IPM'
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  geom_sf_text(data=mapdeptos,aes(label=ifelse(IPM < quantile(IPM, probs = 0.25, na.rm = TRUE),DPTO_CNMBR,"")),col="black",
               fontface="bold",size=2.5,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Incidencia de/nla Pobreza/nMonetaria/nen 2018") + #Nombre Ejes
  scale_fill_gradient(low="lightgreen",high="red",n.breaks=5) + #Le damos colores de blanco a azul
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema

# Punto 2 ####

punto2 <- datos %>%
  group_by(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION,
           COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION) %>%
  summarise(media_pun=mean(PUNT_GLOBAL)) %>%
  select(COLE_COD_DEPTO_UBICACION,COLE_DEPTO_UBICACION,
         COLE_COD_MCPIO_UBICACION,COLE_MCPIO_UBICACION,media_pun)%>%
  as.data.frame()

punto2 <- punto2 %>%
  left_join(cobertura,by=c("COLE_COD_MCPIO_UBICACION"="CÓDIGO_MUNICIPIO")) %>%
  select(CodDepto=COLE_COD_DEPTO_UBICACION,Departamento=COLE_DEPTO_UBICACION,
         CodMup=COLE_COD_MCPIO_UBICACION,Municipio=COLE_MCPIO_UBICACION,media_pun,
         CNS=COBERTURA_NETA_SECUNDARIA) %>%
  as.data.frame()

setwd("C:/Users/gcvh2/Documents/UNAL/7. 2023 - 2S/Estadística Bayesiana/Caso de estudio 2/Mapas Colombia y Mundo")

#Aquí guardo área y poligonos para que R me logre graficar los departamentos
mupshp <- st_read("MGN_MPIO_POLITICO.shp",quiet=TRUE)
mupshp <- mupshp %>%
  mutate(CodMup = paste(DPTO_CCDGO, MPIO_CCDGO, sep = ""))


#Usamos Left join para que haga el cruce por código de departamento y deje todas las columnas
#de deptoshp
mapmup <- mupshp %>%
  left_join(punto2,by=c("CodMup"="CodMup")) %>%
  select(!c(CodDepto,Departamento,Municipio))

#Box calcula el rectangulo que envuelve a todo el conjunto de media_pun_tabla mapdeptos (locombia)
#box deja un poquito a la izquierda porque tiene en cuenta a SAN ANDRES
box <- st_bbox(mapmup)
box

# Gráfica de media puntaje global
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapmup,aes(fill=media_pun),col="darkgray",linetype="solid") + #aes fill=media_pun, el color de relleno dependera de media_pun
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Media del/nPuntaje/nGlobal") + #Nombre Ejes
  scale_fill_gradient(low="lightpink",high="blue",n.breaks=5) + #Le damos colores de blanco a azul
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema

# Gráfica cobertura neta secundaria
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapmup,aes(fill=CNS),col="darkgray",linetype="solid") + #aes fill=media_pun, el color de relleno dependera de media_pun
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Cobertura/nNeta/nSecundaria") + #Nombre Ejes
  scale_fill_gradient(low="lightpink",high="blue",n.breaks=5) + #Le damos colores de blanco a azul
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema


# Punto 5 ####
# Criterios de información

## M1 ####
# DIC
LP1        <- as.numeric(chain_M1$LL$ll)
theta_hat  <- mean(chain_M1$THETA$theta)
sigma2_hat <- mean(chain_M1$THETA$sig2)
lpyth_m1   <- sum(dnorm(x = y, mean = theta_hat, sd = sqrt(sigma2_hat), log = T))
pDIC_m1    <- 2*(lpyth_m1 - mean(LP1))
dic_m1     <- -2*lpyth_m1 + 2*pDIC_m1

tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
# WAIC
WAIC_1 <- foreach(i = 1:n, .combine = '+') %dopar% {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain_M1$THETA$theta, sd = sqrt(chain_M1$THETA$sig2))
  lppd_m1 <- log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain_M1$THETA$theta, sd =  sqrt(chain_M1$THETA$sig2), log = T)
  pWAIC_m1 <- 2*(log(mean(tmp1)) - mean(tmp2))
  WAIC_1 <- c(lppd_m1,pWAIC_m1)
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()
waic_m1_p <- -2*WAIC_1[1] + 2*WAIC_1[2]

## M2 ####
# DIC
LP2        <- as.numeric(chain_M2$LL$ll)
theta_hat  <- colMeans(chain_M2$THETA[,1:m])
sigma2_hat <- mean(chain_M2$THETA$sig2)
lpyth_m2   <- sum(dnorm(x = y, mean = rep(theta_hat, nj), sd = sqrt(sigma2_hat), log = T))
pDIC_m2    <- 2*(lpyth_m2 - mean(LP2))
dic_m2     <- -2*lpyth_m2 + 2*pDIC_m2

tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
# WAIC
#tratamiento de datos
# Y  : puntaje de los estudiantes (list)
# g  : identificador secuencial de los departamentos (c)
Y <- vector(mode = "list", length = m) #Creo lista de tamaño m
g <- rep(NA, n) #Vector del tamaño de las observaciones
for (j in 1:m) {
  idx <- datos$COLE_COD_DEPTO_UBICACION == unique(datos$COLE_COD_DEPTO_UBICACION)[j]
  #Con unique miramos si cada observación tiene el mismo código de
  #departamento que el departamento j, si si rellena con un TRUE, si no
  #rellena con un FALSE, queda un vector de n de largo lleno
  #De true o false.
  g[idx] <- j #Luego en el vector g de tamaño n, colocaré el indice j que
  #se le asino a cada uno de los 32 departamentos
  Y[[j]] <- y[idx]#Luego las observación donde idx sea true, entonces
  #se guardará el puntaje global, logrando así agrupar los puntajes por
  #departamentos
}
WAIC_2 <- foreach(i = 1:n, .combine = '+') %dopar% {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain_M2$THETA[,g[i]], sd = sqrt(chain_M2$THETA$sig2))
  lppd_m2 <- log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain_M2$THETA[,g[i]], sd =  sqrt(chain_M2$THETA$sig2), log = T)
  pWAIC_m2 <- 2*(log(mean(tmp1)) - mean(tmp2))
  WAIC_2 <- c(lppd_m2,pWAIC_m2)
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()
waic_m2_p <- -2*WAIC_2[1] + 2*WAIC_2[2]

## M3 ####
# DIC
LP3        <- as.numeric(chain_M3$LL$ll)
theta_hat  <- colMeans(chain_M3$THETA[,1:m])
sigma2_hat <- colMeans(chain_M3$THETA[,(m+1):(2*m)])
lpyth_m3   <- sum(dnorm(x = y, mean = rep(theta_hat, nj), sd = sqrt(rep(sigma2_hat, nj)), log = T))
pDIC_m3    <- 2*(lpyth_m3 - mean(LP3))
dic_m3     <- -2*lpyth_m3 + 2*pDIC_m3

tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
# WAIC
WAIC_3 <- foreach(i = 1:n, .combine = '+') %dopar% {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain_M3$THETA[,g[i]], sd = sqrt(chain_M3$THETA[,m+g[i]]))
  lppd_m3 <- log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain_M3$THETA[,g[i]], sd = sqrt(chain_M3$THETA[,m+g[i]]), log = T)
  pWAIC_m3 <- 2*(log(mean(tmp1)) - mean(tmp2))
  WAIC_3 <- c(lppd_m3,pWAIC_m3)
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()
waic_m3_p <- -2*WAIC_3[1] + 2*WAIC_3[2]

## M4 ####
# DIC
LP4        <- as.numeric(chain_M4$LL$ll)
zeta_hat  <- colMeans(chain_M4$THETA[,1:length(njk)]) # medias de los zetas
kapa2_hat <- mean(chain_M4$THETA$kap2) # media de kapa
lpyth_m4   <- sum(dnorm(x = y, mean = rep(zeta_hat, njk), sd = sqrt(kapa2_hat), log = T))
pDIC_m4    <- 2*(lpyth_m4 - mean(LP4))
dic_m4     <- -2*lpyth_m4 + 2*pDIC_m4

tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
# WAIC
Y_mun <- vector(mode = "list", length = 1112) #Creo lista de tamaño 1112 que es la cantidad de municipios
g_mun <- rep(NA, n) #Vector del tamaño de las observaciones
for (j in 1:1112) {
  idx <- datos$COLE_COD_MCPIO_UBICACION == unique(datos$COLE_COD_MCPIO_UBICACION)[j]
  g_mun[idx] <- j
  Y_mun[[j]] <- y[idx]
}
WAIC_4 <- foreach(i = 1:n, .combine = '+') %dopar% {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain_M4$THETA[,g_mun[i]], sd = sqrt(chain_M4$THETA$kap2))
  lppd_m4 <- log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain_M4$THETA[,g_mun[i]], sd = sqrt(chain_M4$THETA$kap2), log = T)
  pWAIC_m4 <- 2*(log(mean(tmp1)) - mean(tmp2))
  WAIC_4 <- c(lppd_m4,pWAIC_m4)
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()
waic_m4_p <- -2*WAIC_4[1] + 2*WAIC_4[2]

## M5 ####
# DIC
LP5       <- as.numeric(chain_M5$LL$ll)
zeta_hat  <- colMeans(chain_M5$THETA[,1:length(njk)]) # medias de los zetas
kapa2_hat <- mean(chain_M5$THETA$kap2) # media de kapa
lpyth_m5   <- sum(dnorm(x = y, mean = rep(zeta_hat, njk), sd = sqrt(kapa2_hat), log = T))
pDIC_m5    <- 2*(lpyth_m5 - mean(LP5))
dic_m5     <- -2*lpyth_m5 + 2*pDIC_m5

tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
# WAIC
WAIC_5 <- foreach(i = 1:n, .combine = '+') %dopar% {
  # lppd
  tmp1 <- dnorm(x = y[i], mean = chain_M5$THETA[,g_mun[i]], sd = sqrt(chain_M5$THETA$kap2))
  lppd_m5 <- log(mean(tmp1))
  # pWAIC
  tmp2 <- dnorm(x = y[i], mean = chain_M5$THETA[,g_mun[i]], sd = sqrt(chain_M5$THETA$kap2), log = T)
  pWAIC_m5 <- 2*(log(mean(tmp1)) - mean(tmp2))
  WAIC_5 <- c(lppd_m5,pWAIC_m5)
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()
waic_m5_p <- -2*WAIC_5[1] + 2*WAIC_5[2]

# Punto 6 ####
# Media posterior e intervalo de credibilidad para mu
# En M1, mu es theta según el profe

## M1 ####
mean(chain_M1$THETA$theta)
quantile(chain_M1$THETA$theta, probs = c(0.025,0.975))

## M2 ####
mean(chain_M2$THETA$mu)
quantile(chain_M2$THETA$mu, probs = c(0.025,0.975))

## M3 ####
mean(chain_M3$THETA$mu)
quantile(chain_M3$THETA$mu, probs = c(0.025,0.975))

## M4 ####
mean(chain_M4$THETA$mu)
quantile(chain_M4$THETA$mu, probs = c(0.025,0.975))

## M5 ####
mean(chain_M5$THETA$mu)
quantile(chain_M5$THETA$mu, probs = c(0.025,0.975))

# Punto 7 ####

## Ranking frecuentista (promedio muestral) ####
par(mfrow = c(1,1), mar = c(4,10,1.5,1), mgp = c(2.5,0.75,0))
plot(x = c(180,320), y = c(1,m), type = "n", xlab = "Puntaje", ylab = "", main = "Ranking frecuentista (promedio muestral)", yaxt = "n")
abline(h = 1:m, col = "lightgray", lwd = 1)
abline(v = 250,col = "gray", lwd = 3)
for (l in 1:m) {
  j <- order(yb)[l]
  #points(x = Y[[j]], y = rep(l, nj[j]), pch = 16, cex = 0.3)
  li<-yb[j]-1.96*(sd(Y[[j]])/sqrt(nj[j]))
  ls<-yb[j]+1.96*(sd(Y[[j]])/sqrt(nj[j]))
  lines(x = li, y = l, type = "p", col ="blue", pch = 16, cex = 1.1)
  lines(x = ls, y = l, type = "p", col ="blue", pch = 16, cex = 1.1)
  if(ls<250){
    points(x = yb[j], y = l, col ="red", pch = 16, cex = 1.1)
  }else{
    if(li>250){
      points(x = yb[j], y = l, col ="green", pch = 16, cex = 1.1)
    }
    else{
      points(x = yb[j], y = l, col ="black", pch = 16, cex = 1.1)
    }
  }
}
axis(side = 2, at = 1:m, labels = estadisticos$nombre[order(yb)], las = 2)

## Ranking bayesiano ####
yb_bayes<-colMeans(chain_M5$THETA[,1113:1144]) # medias posteriores por depto

ic_bayes<-matrix(data = NA, nrow = 32, ncol = 2)
contador<-1
for(i in 1113:1144){
  ic_bayes[contador,]<-quantile(chain_M5$THETA[,i],probs = c(0.025,0.975))
  contador<-contador+1
}

par(mfrow = c(1,1), mar = c(4,10,1.5,1), mgp = c(2.5,0.75,0))
plot(x = c(180,320), y = c(1,m), type = "n", xlab = "Puntaje", ylab = "", main = "Ranking bayesiano (medias específicas)", yaxt = "n")
abline(h = 1:m, col = "lightgray", lwd = 1)
abline(v = 250,col = "gray", lwd = 3)
for (l in 1:m) {
  j <- order(yb_bayes)[l]
  #points(x = Y[[j]], y = rep(l, nj[j]), pch = 16, cex = 0.3)
  points(x = ic_bayes[j,], y = c(l,l), type = "p", col =c("blue","blue"), pch = 16, cex = 1.1)
  if(ic_bayes[j,2]<250){
    points(x = yb_bayes[j], y = l, col ="red", pch = 16, cex = 1.1)
  }else{
    if(ic_bayes[j,1]>250){
      points(x = yb_bayes[j], y = l, col ="green", pch = 16, cex = 1.1)
    }
    else{
      points(x = yb_bayes[j], y = l, col ="black", pch = 16, cex = 1.1)
    }
  }
}
axis(side = 2, at = 1:m, labels = estadisticos$nombre[order(yb_bayes)], las = 2)


# Punto 8 ####

### Matriz de icidencia ####
# k means para cada iteracion

k <-5
resultados <- matrix(NA,nrow=10000,ncol=32)
set.seed(17112000)
for (i in 1:10000) {
  fila <- t(chain_M5$THETA[i,1113:1144]) # Obtiene la fila i
  resultado_kmeans <- kmeans(fila, centers = k)  # Aplica K-Means a la fila
  resultados[i,] <- resultado_kmeans$cluster  # Almacena el resultado en la lista
}

B <- nrow(resultados)

# matriz de incidencia
A <- matrix(data = 0, nrow = m, ncol = m)
for (b in 1:B) {
  for (i in 1:(m-1)) {
    for (j in (i+1):m) {
      if (resultados[b,i] == resultados[b,j]) {
        A[i,j] <- A[i,j] + 1/B
      }
    }
  }
}
A <- A + t(A)
diag(A) <- 1
# se organizan las observaciones de acuerdo a la partición verdadera
yb_bayes<-as.vector(yb_bayes)
set.seed(17112000)
original<-kmeans(yb_bayes, centers = k)
xi<-original$cluster
mapita_punto8 <- cbind(punto1,xi)
mapita_punto8$xi<- as.character(mapita_punto8$xi)
indices <- order(yb_bayes)
A <- A[indices,indices]

Punto8<-punto1%>%mutate(pos = row_number())%>%as.data.frame()
str(Punto8)
indices<-as.data.frame(indices)

names<-indices%>%left_join(Punto8,by=c("indices"="pos"))
str(names)
colnames(A) <- names$Departamento
rownames(A) <- names$Departamento
# visualización de la matriz de incidencia
par(mar = c(2.75,2.75,0.5,0.5), mgp = c(1.7,0.7,0))
corrplot::corrplot(corr = A, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "lt", tl.cex=0.6, tl.col="black", title="Matriz de incidencia para Departamentos",mar=c(0,0,1,0))

### Mapa ####
#Usamos Left join para que haga el cruce por código de departamento y deje todas las columnas
#de deptoshp
mapdeptos <- deptoshp %>%
  left_join(mapita_punto8,by=c("DPTO_CCDGO"="CodDep")) %>%
  select(!(Departamento))

#Box calcula el rectangulo que envuelve a todo el conjunto de media_pun_tabla mapdeptos (locombia)
#box deja un poquito a la izquierda porque tiene en cuenta a SAN ANDRES
box <- st_bbox(mapdeptos)
box

# Gráfica
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=xi),col="darkgray",linetype="solid") + #aes fill=xi, el color de relleno dependera de xi
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Grupos") + #Nombre Ejes
  scale_fill_manual(values = c("blue", "darkgreen", "red", "darkorange", "mediumorchid")) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="black",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema

# Punto 9 ####
# Media posterior e ICredibilidad de la IPM 2018
## Regresion lineal simple ####
names(pobreza)<-c("coddep","Dep","IPM")
pobreza<-pobreza[-25,]
predDep<-matrix(data = NA, nrow = 10000, ncol = 8)

for(i in 1:10000){
  x<-as.vector(t(chain_M5$THETA[i,1113:1136])) # deptos observados en IPM
  regDep<-lm(pobreza$IPM~1+x)
  noDep<-as.vector(t(chain_M5$THETA[i,1137:1144])) # los que queremos predecir
  # almacenar
  predDep[i,]<- regDep$coefficients[1]+regDep$coefficients[2]*noDep
}

# Medias posteriores
mapita_punto9 <- cbind(punto1,c(rep(NA,24),colMeans(predDep)))
mapita_punto9 <- mapita_punto9[,c(1,5)]
colnames(mapita_punto9)[2]<-"Media_post"

# Intervalos de Credibilidad
icDep<-matrix(data = NA, nrow = 8, ncol = 2)
for(i in 1:8){
  icDep[i,]<-quantile(predDep[,i],probs = c(0.025,0.975))
}
icDep

## Mapa ####
#Usamos Left join para que haga el cruce por código de departamento y deje todas las columnas
#de deptoshp
mapdeptos <- deptoshp %>%
  left_join(mapita_punto9,by=c("DPTO_CCDGO"="CodDep"))

#Box calcula el rectangulo que envuelve a todo el conjunto de media_pun_tabla mapdeptos (locombia)
#box deja un poquito a la izquierda porque tiene en cuenta a SAN ANDRES
box <- st_bbox(mapdeptos)
box

# Gráfica de media puntaje global
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapdeptos,aes(fill=Media_post),col="darkgray",linetype="solid") + #aes fill=Media_post, el color de relleno dependera de Media_post
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  geom_sf_text(data=mapdeptos,aes(label=ifelse(!is.na(Media_post),DPTO_CNMBR,"")),col="black",
               fontface="bold",size=2.5,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Media posterior de\nlos departamentos\nsin IPM") + #Nombre Ejes
  scale_fill_gradient(low="yellow",high="darkgreen",n.breaks=5) + #Le damos colores de blanco a azul
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema


# Punto 10 ####
yb_mun_bayes<-colMeans(chain_M5$THETA[,1:1112]) # medias posteriores por municipio

### Matriz de icidencia ####
# k means para cada iteracion

k <-8
resultados <- matrix(NA,nrow=10000,ncol=1112)
set.seed(17112000)
for (i in 1:10000) {
  fila <- t(chain_M5$THETA[i,1:1112]) # Obtiene la fila i
  resultado_kmeans <- kmeans(fila, centers = k)  # Aplica K-Means a la fila
  resultados[i,] <- resultado_kmeans$cluster  # Almacena el resultado en la lista
}

B <- nrow(resultados)

# matriz de incidencia
A_mun <- matrix(data = 0, nrow = 1112, ncol = 1112)
for (b in 1:B) {
  for (i in 1:(1112-1)) {
    for (j in (i+1):1112) {
      if (resultados[b,i] == resultados[b,j]) {
        A_mun[i,j] <- A_mun[i,j] + 1/B
      }
    }
  }
}
A_mun <- A_mun + t(A_mun)
diag(A_mun) <- 1

# se organizan las observaciones de acuerdo a la partición verdadera
yb_mun_bayes<-as.vector(yb_mun_bayes)
set.seed(17112000)
original_mun<-kmeans(yb_mun_bayes, centers = k)
xi<-original_mun$cluster
mapita_punto10 <- cbind(punto2,xi)
mapita_punto10$xi<- as.character(mapita_punto10$xi)
indices <- order(yb_mun_bayes,decreasing = T)
A_mun <- A_mun[indices,indices]

# visualización de la matriz de incidencia
par(mar = c(2.75,2.75,0.5,0.5), mgp = c(1.7,0.7,0))
corrplot::corrplot(corr = A_mun, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "n", tl.cex=0.3, tl.col="black", title="Matriz de incidencia para municipios",mar=c(0,0,1,0))

## Mapa ####
#Usamos Left join para que haga el cruce por código de departamento y deje todas las columnas
#de deptoshp
mapmup <- mupshp %>%
  left_join(mapita_punto10,by=c("CodMup"="CodMup")) %>%
  select(!c(CodDepto,Departamento,Municipio))

#Box calcula el rectangulo que envuelve a todo el conjunto de media_pun_tabla mapdeptos (locombia)
#box deja un poquito a la izquierda porque tiene en cuenta a SAN ANDRES
box <- st_bbox(mapmup)
box

# Gráfica
ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mapmup,aes(fill=xi),col="darkgray",linetype="solid") + #aes fill=xi, el color de relleno dependera de xi
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) + #Aqui centramos el mapa en locombia
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Grupos") + #Nombre Ejes
  scale_fill_manual(values = c("darkgreen", "darkred", "red", "darkorange", "darkblue", "gold", "sienna", "darkviolet")) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="black",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema


# Punto 11 ####
listaMun<-datos%>%group_by(COLE_COD_MCPIO_UBICACION)%>%summarise(num=1)%>%as.data.frame()
cobertura$CÓDIGO_MUNICIPIO<-as.character(cobertura$CÓDIGO_MUNICIPIO)
punto11<-cobertura%>%left_join(listaMun,by=c("CÓDIGO_MUNICIPIO"="COLE_COD_MCPIO_UBICACION"))%>%arrange(CÓDIGO_MUNICIPIO)
punto11<-punto11[!is.na(punto11$num),]

## Regresion lineal simple ####
predMun<-matrix(data = NA, nrow = 10000, ncol = 2)
for(i in 1:10000){
  x<-data.frame(cbind(chain_M5$THETA[i,1:581],chain_M5$THETA[i,583:1097],chain_M5$THETA[i,1099:1112]))
  x<-as.vector(t(x)) # municipios observados en CSN
  regMun<-lm(punto11$COBERTURA_NETA_SECUNDARIA~x)
  noMun<-as.vector(t(chain_M5$THETA[i,c(582,1098)])) # los que queremos predecir
  # almacenar
  predMun[i,]<- regMun$coefficients[1]+regMun$coefficients[2]*noMun # predicciones para los dos municipios en cada iteracion
}

# Medias posteriores
mapita_punto11 <- cbind(punto2,c(rep(NA,574),punto2[575:605,6],
                                 rep(NA,490),punto2[1096:1101,6],
                                 rep(NA,11)))
mapita_punto11 <- mapita_punto11[,c(1,2,3,4,7)]
colnames(mapita_punto11)[5]<-"Media_post"
mapita_punto11 <- rbind(mapita_punto11[1:1099,],
                        c("94","GUAINIA","94884","PUERTO COLOMBIA",NA),
                        c("94","GUAINIA","94885","LA GUADALUPE",NA),
                        c("94","GUAINIA","94886","CACAHUAL",NA),
                        mapita_punto11[1100:1112,])
mapita_punto11$Media_post <- as.numeric(mapita_punto11$Media_post)

# Intervalos de Credibilidad
round(quantile(predMun[,1],probs = c(0.025,0.975)),3) # para Zeta 582
round(quantile(predMun[,2],probs = c(0.025,0.975)),3) # para Zeta 1098

## Mapa ####
### Choco ####
choco <- mupshp %>%
  left_join(mapita_punto11,by=c("CodMup"="CodMup")) %>%
  filter(CodDepto=="27")

box <- st_bbox(choco)
box

ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mupshp) +
  geom_sf(data=choco,aes(fill=Media_post),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=choco,aes(label=ifelse((CodMup==27615 | CodMup==27150),Municipio,"")),col="black",
               fontface="bold",size=3,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Chocó",fill="Media posterior de\nMunicipio sin CSN") +
  scale_fill_gradient(low="yellow",high="darkgreen",n.breaks=5) +
  theme(panel.background=element_rect(fill="lightblue")) + mitema

### Guainia ####
guainia <- mupshp %>%
  left_join(mapita_punto11,by=c("CodMup"="CodMup")) %>%
  filter(CodDepto=="94")

box <- st_bbox(guainia)
box

ggplot() +
  geom_sf(data=mupshp) +
  geom_sf(data=guainia,aes(fill=Media_post),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=guainia,aes(label=ifelse((CodMup==94343 | CodMup==94001 | CodMup==94888),Municipio,"")),col="black",
               fontface="bold",size=3.5,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Guainía",fill="Media posterior de\nMunicipio sin CSN") +
  scale_fill_gradient(low="yellow",high="darkgreen",n.breaks=5) + mitema

### Colombia ####
col_mup <- mupshp %>%
  left_join(mapita_punto11,by=c("CodMup"="CodMup"))

box <- st_bbox(col_mup)
box

ggplot() +
  geom_sf(data=mundocol) +
  geom_sf(data=mupshp) +
  geom_sf(data=col_mup,aes(fill=Media_post),col="darkgray",linetype="solid") +
  coord_sf(xlim=c(box$xmin,box$xmax),ylim=c(box$ymin,box$ymax),expand=FALSE) +
  geom_sf_text(data=col_mup,aes(label=ifelse((CodMup==1),Municipio,"")),col="black",
               fontface="bold",size=3.5,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(x="Longitud",y="Latitud",title="Colombia",fill="Media posterior de/nMunicipios sin CSN") +
  scale_fill_gradient(low="yellow",high="darkgreen",n.breaks=5) +
  annotate("text", x=c(-74.5,-68,-78,-69,-78.5), y=c(-2.5,0,-1,9,9), colour="blue",
           label=c("Perú","Brasil","Ecuador","Venezuela","Panamá")) + #Coloco anotaciones en ciertas coordenadas
  theme(panel.background=element_rect(fill="lightblue")) + mitema


# Punto 12 ####
##Bondad de Ajuste local Local ####
indice<-rep(1:1112,njk)
#Minimo
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
minimo <- numeric(0)
minimo <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  res <- rnorm(n = n, mean = rep(as.numeric(chain_M5$THETA[i,1:1112]),njk), sd = sqrt(chain_M5$THETA$kap2[i]))
  minimo <- c(tapply(res, indice, min))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()

#Máximo
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
maximo <- numeric(0)
maximo <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  res <- rnorm(n = n, mean = rep(as.numeric(chain_M5$THETA[i,1:1112]),njk), sd = sqrt(chain_M5$THETA$kap2[i]))
  maximo <- c(tapply(res, indice, max))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()


#IQR
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
iqr <- numeric(0)
iqr <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  res <- rnorm(n = n, mean = rep(as.numeric(chain_M5$THETA[i,1:1112]),njk), sd = sqrt(chain_M5$THETA$kap2[i]))
  iqr <- c(tapply(res, indice, IQR))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()


#Media
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
media <- numeric(0)
media <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  res <- rnorm(n = n, mean = rep(as.numeric(chain_M5$THETA[i,1:1112]),njk), sd = sqrt(chain_M5$THETA$kap2[i]))
  media <- c(tapply(res, indice, mean))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()


#Mediana
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
mediana <- numeric(0)
mediana <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  res <- rnorm(n = n, mean = rep(as.numeric(chain_M5$THETA[i,1:1112]),njk), sd = sqrt(chain_M5$THETA$kap2[i]))
  mediana <- c(tapply(res, indice, median))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()


#SD
tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
SD <- numeric(0)
SD <- foreach(i = 1:10000, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  res <- rnorm(n = n, mean = rep(as.numeric(chain_M5$THETA[i,1:1112]),njk), sd = sqrt(chain_M5$THETA$kap2[i]))
  SD <- c(tapply(res, indice, sd))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()

min_obs<-c(tapply(y, indice, min))
max_obs<-c(tapply(y, indice, max))
iqr_obs<-c(tapply(y, indice, IQR))
med_obs<-c(tapply(y, indice, mean))
median_obs<-c(tapply(y, indice, median))
SD_obs<-c(tapply(y, indice, sd))


tictoc::tic()
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
ppp <- numeric(0)
ppp <- foreach(i = 1:1112, .combine = "rbind") %dopar% {
  ppp<-c(mean(minimo[,i]<min_obs[i]),
         mean(maximo[,i]<max_obs[i]),
         mean(iqr[,i]<iqr_obs[i]),
         mean(media[,i]<med_obs[i]),
         mean(mediana[,i]<median_obs[i]),
         mean(SD[,i]<SD_obs[i]))
}
#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)
tictoc::toc()

colnames(ppp) <- c("Min","Max","IQR","Media","Mediana","SD")

ppp_df <- reshape2::melt(ppp)
ppp_df <- ppp_df[,2:3]
colnames(ppp_df) <- c("Grupo","Valor")

colores <- c("red", "#FF0077", "orange",
             "purple", "#007777", "green")
boxplot(Valor ~ Grupo, data = ppp_df, border = colores, col = "white")
# Puntos
stripchart(Valor ~ Grupo,data = ppp_df,method = "jitter", pch = 21,
           col = colores, vertical = TRUE,add = TRUE)

ggplot(ppp_df, aes(x = Grupo, y = Valor)) +
  geom_boxplot(outlier.colour = "transparent", color = colores, fill = "transparent") +
  geom_jitter(width = 0.2, aes(color = Grupo), alpha = 0.1) +
  scale_color_manual(values = colores) +
  labs(subtitle="Nivel Local", x="Estadístico de prueba", y="ppp")+
  theme(legend.position="none",
        plot.subtitle=element_text(face="bold",size=12,vjust=0.5,hjust=0.5,color="blue"),
        axis.title.x = element_text(hjust = 0.5, color = "blue"),
        axis.title.y = element_text(hjust = 0.5, color = "blue"))
