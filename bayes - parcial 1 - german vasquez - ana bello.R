#----------------------------------------------------------------------#
# Estadística Bayesiana - Caso de estudio I                            #
# Fecha: 11 de septiembre de 2023                                      #
# Integrantes: Ana Sofia Bello Dueñas                                  #
#              German Camilo   Vasquez Herrera                         #
#----------------------------------------------------------------------#

if (!require(readr)){install.packages("readr");library(readr)}
if (!require(dplyr)){install.packages("dplyr");library(dplyr)}
if (!require(doParallel)){install.packages("doParallel");library(doParallel)} ##Librería núcleos


getwd()
setwd("C:/Users/gcvh2/Documents/UNAL/7. 2023 - 2S/Estadística Bayesiana/Caso de estudio 1")

verizon <- read_csv("Verizon.csv")


#ANÁLISIS BAYESIANO############################################################################################################
##Punto 1: Ajuste modelos Gamma-Inversa-Exponencial##########################################################################################
#Separamos los datos por grupos de clientes
ILEC <- verizon %>%
  filter(Group=="ILEC") %>%
  as.data.frame()
ILEC <- ILEC[,1]

CLEC <- verizon %>%
  filter(Group=="CLEC") %>%
  as.data.frame()
CLEC <- CLEC[,1]

a <- 3
b <- 17
n_1 <- length(ILEC)
n_2 <- length(CLEC)
s_1 <- sum(ILEC)
s_2 <- sum(CLEC)
a_post_1 <- a+n_1
a_post_2 <- a+n_2
b_post_1 <- b+s_1
b_post_2 <- b+s_2
B <- 10000
# Ahora realizamos simulación
set.seed(17112000)
lambda1_mc <- 1/rgamma(n = B, shape = a_post_1, rate = b_post_1)
lambda2_mc <- 1/rgamma(n = B, shape = a_post_2, rate = b_post_2)
eta <- lambda1_mc-lambda2_mc
media <- mean(eta)
estadisticas <- c(media,sd(eta)/abs(media),quantile(eta, probs = c(0.025,0.975)))
names(estadisticas)[1:2] <- c("Media","CV")
estadisticas

##Punto 2:Análisis de sensitividad##########################################################################################

a <- c(3,2,3,2)
b <-  c(17,8.5,16.8,8.4,33,16.5)
a_post_1 <- a+n_1
a_post_2 <- a+n_2
b_post_1 <- b+s_1
b_post_2 <- b+s_2

#.combine = "c", predeterminado, me presenta los resultados en vector, matriz o lista, dependiendo
#.combine = "rbind" me devuelve los resultados en una matriz
#Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
estadisticas <- foreach(i = 1:4, .combine = "cbind") %dopar% {
  set.seed(17112000)
  lambda1_mc <- 1/rgamma(n = B, shape = a_post_1[i], rate = b_post_1[i])
  if (i<3) {
    lambda2_mc <- 1/rgamma(n = B, shape = a_post_2[i], rate = b_post_2[i])
    media_priori1 <- b[i]/(a[i]-1)
    media_priori2 <- media_priori1
  } else {
    lambda2_mc <- 1/rgamma(n = B, shape = a_post_2[i], rate = b_post_2[i+2])
    media_priori1 <- b[i]/(a[i]-1)
    media_priori2 <- b[i+2]/(a[i]-1)
  }
  if(i%%2==0){cv_priori <- NA}else{cv_priori <- 1/sqrt(a[i]-2)}
  eta <- lambda1_mc-lambda2_mc
  media <- mean(eta)
  estadisticas <- c(media,sd(eta)/abs(media),quantile(eta, probs = c(0.025,0.975)),
                    media_priori1,cv_priori,media_priori2,cv_priori)
  return(estadisticas)
}
colnames(estadisticas) <- c("Previa 1","Previa 2","Previa 3","Previa 4")
rownames(estadisticas)[1:2] <- c("Media","CV")
rownames(estadisticas)[5:8] <- c("Media a priori 1","CV a priori 1","Media a priori 2","CV a priori 2")
estadisticas <- round(estadisticas, digits = 3)
estadisticas

#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)

##Punto 3: Bondad de ajuste##########################################################################################
#Calculamos estadísticas de muestras originales
class(CLEC)
tobs <- c(mean(ILEC),sd(ILEC),mean(CLEC),sd(CLEC))
names(tobs) <- c("Media ILEC","Devest ILEC","Media CLEC","Devest CLEC")
tobs
#Calculamos estadísticas por medio de la predictiva posterior
##Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)

res_esta <- foreach(i = 1:B, .combine = "rbind") %dopar% {
  set.seed(17112000+i)
  #Predictiva
  simul1 <- rexp(n = n_1, rate = 1/lambda1_mc[i])
  simul2 <- rexp(n = n_2, rate = 1/lambda2_mc[i])
  #Estadisticas de prueba
  res_esta <- c(mean(simul1),sd(simul1),mean(simul2),sd(simul2))
}
colnames(res_esta) <- c("Media ILEC","Devest ILEC","Media CLEC","Devest CLEC")
rownames(res_esta) <- paste("Theta",1:B, sep = "")
res_esta <- round(res_esta, digits = 3)
head(res_esta)

#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)

#ppp valores
ppp <- c(mean(res_esta[,1]<tobs[1]),mean(res_esta[,2]<tobs[2]),
        mean(res_esta[,3]<tobs[3]),mean(res_esta[,4]<tobs[4]))
names(ppp) <- c("Media ILEC","Devest ILEC","Media CLEC","Devest CLEC")
ppp <- round(ppp, digits = 3)
ppp

if (!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if (!require(ggExtra)){install.packages("ggExtra");library(ggExtra)}

df_ILEC<-res_esta[,1:2]
df_ILEC<-as.data.frame(df_ILEC)
str(df_ILEC)
head(df_ILEC)

p1 <- ggplot(df_ILEC, aes(x = `Media ILEC`, y = `Devest ILEC`)) +
  geom_point()

df_CLEC<-res_esta[,3:4]
df_CLEC<-as.data.frame(df_CLEC)
str(df_CLEC)
head(df_CLEC)

p2 <- ggplot(df_CLEC, aes(x = "Media CLEC", y = "Devest ILEC")) +
  geom_point()
#ANÁLISIS FRECUENTISTA############################################################################################################
##Normalidad asintótica##########################################################################################
# Preparacion de los datos
str(verizon)
## Estimacion de la media
### Primero estimamos los lambda de cada grupo con el MLE=s/n
lambda_c<-mean(CLEC);lambda_c # 16.50913
lambda_i<-mean(ILEC);lambda_i # 8.411611

### Ahora estimamos eta con la resta de esos lambdas
eta_mle<-lambda_i-lambda_c
eta_mle # -8.09752

## Estimacion del coeficiente de variacion

### Primero toca hallar la info observada de Fisher en cada grupo para poder obtener la var de eta
infofish_c<-(2*s_2/lambda_c^3)-(n_2/lambda_c^2)
infofish_i<-(2*s_1/lambda_i^3)-(n_1/lambda_i^2)  ##Simplificar esto pal for

### Ahora estimamos la varianza de eta
var_est_eta_mle<-1/infofish_c+1/infofish_i
desv_est_eta_mle<-sqrt(var_est_eta_mle) # 3.448562

### El coeficiente de variación estimado seria
cv_est_eta_mle<-desv_est_eta_mle/abs(eta_mle)
cv_est_eta_mle # 0.4258788

## Estimacion del IC
LI<-eta_mle-1.96*desv_est_eta_mle # -14.8567
LS<-eta_mle+1.96*desv_est_eta_mle # -1.338338


##Bootstrap paramétrico##########################################################################################

## Vector con las estimaciones de lambda de CLEC
boot_c<-NULL
boot_i<-NULL

set.seed(17112000)
for(i in 1:B){
  boot_c[i]<-mean(rexp(n_2,rate=1/lambda_c)) # muestra de tamaño n_2 y parametro 1/lambda_c
  boot_i[i]<-mean(rexp(n_1,rate=1/lambda_i)) # muestra de tamaño n_1 y parametro 1/lambda_i
}

## Calculamos la estimacion de los etas
boot_etas_par<-NULL
boot_etas_par<-boot_i-boot_c

media_para <- mean(boot_etas_par);media_para # estimacion de eta
sd_para <- sd(boot_etas_par);sd_para # desviacion de eta
cv_para <- sd_para/abs(media_para);cv_para # cv estimado de eta
IC_para <- quantile(boot_etas_par,probs=c(0.025,0.975)) # ic estimado de eta

##Bootstrap NO paramétrico##########################################################################################

## Vector con las estimaciones de lambda de CLEC
boot_c<-NULL
boot_i<-NULL

set.seed(17112000)
for(i in 1:B){
  boot_c[i]<-mean(sample(CLEC,size=n_2,replace = TRUE)) # muestra de tamaño n_2 y parametro 1/lambda_c
  boot_i[i]<-mean(sample(ILEC,size=n_1,replace = TRUE)) # muestra de tamaño n_2 y parametro 1/lambda_c
}

## Calculamos la estimacion de los etas
boot_etas_no_par<-NULL
boot_etas_no_par<-boot_i-boot_c

media_no_para <- mean(boot_etas_no_par);media_no_para # estimacion de eta
sd_no_para <- sd(boot_etas_no_par);sd_no_para # desviacion de eta
cv_no_para <- sd_no_para/abs(media_no_para);cv_no_para # cv estimado de eta
IC_no_para <- quantile(boot_etas_no_par,probs=c(0.025,0.975));IC_no_para # ic estimado de eta


#SIMULACIÓN############################################################################################################
y_barra <- c(mean(ILEC),mean(CLEC));y_barra
#El verdadero valor de Eta es -8.1 porque somos Dios y lo sabemos
tm <- c(10,20,50,100)
B <- 2000
cm <-100000
cont_bayes <- 0
##Especificamos los núcleos a usar en el pc
cl <- makeCluster(7)
registerDoParallel(cl)
tabla_final <- NULL
boot_c <- NULL
boot_i <- NULL
boot_etas_par <- NULL
boot_etas_no_par <- NULL
for(j in 1:4){
  contador <- foreach(i= 1:cm, .combine = "+") %dopar% {
    set.seed(17112000+i)
    muestra1 <- rexp(n = tm[j], rate = 1/y_barra[1])
    muestra2 <- rexp(n = tm[j], rate = 1/y_barra[2])
    #Bayesiano
    s_1 <- sum(muestra1)
    s_2 <- sum(muestra2)
    a_post_1 <- 3+tm[j]
    a_post_2 <- 3+tm[j]
    b_post_1 <- 17+s_1
    b_post_2 <- 17+s_2
    lambda1_mc <- 1/rgamma(n = B, shape = a_post_1, rate = b_post_1)
    lambda2_mc <- 1/rgamma(n = B, shape = a_post_2, rate = b_post_2)
    eta <- lambda1_mc-lambda2_mc
    IC_bayes <- c(quantile(eta, probs = c(0.025,0.975)))
    cont_bayes <- ifelse((IC_bayes[1] < (-8.1) && (-8.1) < IC_bayes[2]),1,0)
    #Asintotico
    lambda_c <- mean(muestra2)
    lambda_i <- mean(muestra1)
    eta_mle <- lambda_i-lambda_c
    infofish_c<-(2*s_2/lambda_c^3)-(tm[j]/lambda_c^2)
    infofish_i<-(2*s_1/lambda_i^3)-(tm[j]/lambda_i^2)
    var_est_eta_mle<-1/infofish_c+1/infofish_i
    desv_est_eta_mle<-sqrt(var_est_eta_mle)
    LI<-eta_mle-1.96*desv_est_eta_mle
    LS<-eta_mle+1.96*desv_est_eta_mle
    cont_asin <- ifelse((LI < (-8.1) && (-8.1) < LS),1,0)
    #Bootstrap paramétrico
    for(k in 1:B){
      boot_c[k]<-mean(rexp(tm[j],rate=1/lambda_c))
      boot_i[k]<-mean(rexp(tm[j],rate=1/lambda_i))
    }
    boot_etas_par<-boot_i-boot_c
    IC_para <- quantile(boot_etas_par,probs=c(0.025,0.975))
    cont_para <- ifelse((IC_para[1] < (-8.1) && (-8.1) < IC_para[2]),1,0)
    #Bootstrap no paramétrico
    for(h in 1:B){
      boot_c[h]<-mean(sample(muestra2,size=tm[j],replace = TRUE))
      boot_i[h]<-mean(sample(muestra1,size=tm[j],replace = TRUE))
    }
    boot_etas_no_par<-boot_i-boot_c
    IC_no_para <- quantile(boot_etas_no_par,probs=c(0.025,0.975))
    cont_no_para <- ifelse((IC_no_para[1] < (-8.1) && (-8.1) < IC_no_para[2]),1,0)
    contador <- c(cont_bayes,cont_asin,cont_para,cont_no_para)
  }
  tabla_final <- rbind(tabla_final,contador)
}
colnames(tabla_final) <- c("Bayesiano","Asintotico","Parametrico","No parametrico")
rownames(tabla_final) <- paste("Escenario",1:4, sep = " ")
tabla_final <- round(tabla_final/cm, digits = 3)
tabla_final

#Le quitamos lo de los nucleos, lo dejamos normal
stopCluster(cl)









