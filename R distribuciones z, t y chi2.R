# Establece el directorio trabajo

#Integrales

# Integral de la funcion de densidad de la distribucion normal de 
# - infinito a infinito = 1
# Correcto porque es una distribucion de probabilidad
integrate(dnorm, mean=200, sd=20, lower= -Inf, upper= Inf, abs.tol = 0)$value

#Funcion para colorear
colorArea <- function(from, to, density, ..., col="blue", dens=NULL){
  y_seq <- seq(from, to, length.out=500)
  d <- c(0, density(y_seq, ...), 0)
  polygon(c(from, y_seq, to), d, col=col, density=dens)
}

#Integral de la funcion de densidad de la distribucion normal a una, dos y 
#tres desviaciones estandar

#Una desviacion estandar
integrate(dnorm, mean=200, sd=20, lower= 200-20, upper= 200+20, abs.tol = 0)$value

curve(dnorm(x,200,20), from=100, to=300, 
      main = "Normal Distibution", 
      ylab = "Probability Density",
      xlab = "X")
colorArea(from=100, to=180, dnorm, mean=200, sd=20, col=2, dens=20)
colorArea(from=220, to=300, dnorm, mean=200, sd=20, col=2, dens=20)

text(200,0.01,labels=c("0.6827"),cex=0.55,col=c("red"))

#Dos desviaciones estandar
integrate(dnorm, mean=200, sd=20, lower= 200-2*20, upper= 200+2*20, abs.tol = 0)$value

colorArea(from=100, to=160, dnorm, mean=200, sd=20, col=3, dens=20)
colorArea(from=240, to=300, dnorm, mean=200, sd=20, col=3, dens=20)
text(200,0.006,labels=c("0.9545"),cex=0.75,col=c("green"))

#Tres desviaciones estandar
integrate(dnorm, mean=200, sd=20, lower= 200-3*20, upper= 200+3*20, abs.tol = 0)$value

colorArea(from=100, to=140, dnorm, mean=200, sd=20, col=4, dens=20)
colorArea(from=260, to=300, dnorm, mean=200, sd=20, col=4, dens=20)
text(200,0.002,labels=c("0.9973"),cex=0.95,col=c("blue"))

#La distribucion normal estandar
curve(dnorm(x,0,1), from=-3, to=3, 
      main = "Standar Normal Distibution", 
      ylab = "Probability Density",
      xlab = "z")

integrate(dnorm, mean=0, sd=1, lower= -Inf, upper= Inf, abs.tol = 0)$value

#Otros comandos
#Como vimos anteriormente dnorm da el valor de la funcion
#de densidad de probabilidad para la distribucion normal dados
#los parametros x, \mu y \sigma
# f(x|\mu,\sigma)=1/(\sigma \sqrt(2\pi)) \exp(-(x-\mu)^2/(2\sigma^2))
#funciona individualmente, no solo dentro de la integral

#dnorm(x,\mu,\sigma)

#Para el ejemplo anterior
curve(dnorm(x,200,20), from=100, to=300, 
      main = "Standar Normal Distibution", 
      ylab = "Probability Density",
      xlab = "z")
dnorm(200, mean = 200, sd = 20)

#Para el caso de la distribucion normal estandar
dnorm(0, mean = 0, sd = 1)
curve(dnorm(x,0,1), from=-3, to=3, 
      main = "Standar Normal Distibution", 
      ylab = "Probability Density",
      xlab = "z")
#Los argumentos por default de dnorm son
#media=0, sd=1
dnorm(0)

#La funcion pnorm nos da la integral de -\infty
#a q de la funcion de densidad de probabilidad de la
#distribucion normal 

#pnorm(q,\mu,\sigma)

integrate(dnorm, mean=200, sd=20, lower= -Inf, upper= 200-2*20, abs.tol = 0)$value
pnorm(200-2*20, mean = 200, sd = 20)

#si ponemos el argumento lower.tail=FALSE obtenemos la
#integral de q a infinito

integrate(dnorm, mean=200, sd=20, lower= 200-2*20, upper= Inf, abs.tol = 0)$value
pnorm(200-2*20, mean = 200, sd = 20, lower.tail = FALSE)

#pnorm(q)
#nos da por default los valores de la distribucion normal estandar

integrate(dnorm, mean=0, sd=1, lower= -Inf, upper= 1, abs.tol = 0)$value
pnorm(1)

curve(dnorm(x), from=-3, to=3, 
      main = "Standar Normal Distibution", 
      ylab = "Probability Density",
      xlab = "z")
colorArea(from=-3, to=1, dnorm, col=4, dens=20)
text(0,0.05,labels=c("0.8413"),cex=0.75,col=c("red"))

#La funcion qnorm es el inverso de la funcion de densidad
#de probabilidad, la utilizamos para determinar cual
# es el Z-score de la distribucion normal

#qnorm(1-\alpha/2)

#donde el nivel de confianza esta dado por 1-\alpha

#Para 90% de nivel de confianza, 1-\alpha=0.90, entonces alpha=0.1,
#entonces el argumento de qnorm = 1-\alpha/2=0.95

qnorm(.95)

pnorm(1.644854)

integrate(dnorm, mean=0, sd=1, lower= -1.644854, upper= 1.644854, abs.tol = 0)$value

curve(dnorm(x), from=-3, to=3, 
      main = "Standar Normal Distibution", 
      ylab = "Probability Density",
      xlab = "z")
colorArea(from=1.644854, to=3, dnorm, col=4, dens=20)
colorArea(from=-3, to=-1.644854, dnorm, col=4, dens=20)
text(0,0.2,expression(1-alpha==0.90),cex=0.75,col=c("red"))
text(-2.5,0.06,expression(frac(alpha,2)==0.05),cex=0.55,col=c("red"))
text(2.5,0.06,expression(frac(alpha,2)==0.05),cex=0.55,col=c("red"))
text(-1.4,0,expression(-1.645),cex=0.55,col=c("red"))
text(1.4,0,expression(+1.645),cex=0.55,col=c("red"))

#Para 95% de nivel de confianza, 1-\alpha=0.95, entonces alpha=0.05,
#entonces el argumento de qnorm = 1-/alpha/2=0.975
qnorm(.975)

pnorm(1.959964)

#Para 99% de nivel de confianza 1-\alpha=0.99 entonces alpha=0.001,
#entonces argumento de qnorm = 1-/alpha/2=0.995
qnorm(.995)

pnorm(2.575829)

#Para generar un vector de n numeros aleatorios normalmente distribuidos
#utilizamos rnorm


#rnorm(n,\mu,\sigma)


rnorm(5)
rnorm(5)

#cada vez que ejecutamos el comando obtenemos distintos resultados

#set.seed es una funcion para poder volver a reproducir el set de
#numeros aleatorios para que los resultados sean reproducibles

#set.seed(fecha)

set.seed(06-08-2021)
rnorm(5)

set.seed(06-08-2021)
rnorm(5)



#ahora cuando ejecutamos set.seed y rnorm obtenemos los mismos numeros

set.seed(06-05-2021)
n10 <- rnorm(10, mean = 70, sd = 5)
n10
set.seed(06-05-2021)
n100 <- rnorm(100, mean = 70, sd = 5)
set.seed(06-05-2021)
n10000 <-  rnorm(10000, mean = 70, sd = 5)
n10

# Para dibujar dos graficas juntas
oldpar <- par()
par(mfrow=c(1,3))

# con el argumento de breaks especificamos el numero de barras que queremos
hist(n10, breaks = 5)
hist(n100, breaks = 20)
hist(n10000, breaks = 100)

# Para recuperar los settings originales del graficado
par(oldpar)

################################################################################
################################################################################
################################################################################

#Ahora, para la distribucion t utilizamos comandos parecidos

#Para la funcion de densidad de probabilidad utilizamos
#dt(x, df)
#con df=grados de libertad

curve(dt(x,3000), from=-3, to=3, 
      main = "t-Distibution", 
      ylab = "Probability Density",
      xlab = "x", col="green")

curve(dt(x,15), from=-3, to=3, 
      main = "t-Distibution", 
      ylab = "Probability Density",
      xlab = "x", col="red", add = TRUE)

curve(dt(x,3), from=-3, to=3, 
      main = "t-Distibution", 
      ylab = "Probability Density",
      xlab = "x", col="blue", add = TRUE)

#Para la integral de dt usamos
# pt(q, df, lower.tail = TRUE)

pt(-1,6)
integrate(dt, df= 6, lower= -Inf, upper= -1, abs.tol = 0)$value

curve(dt(x,6), from=-3, to=3, 
      main = "t-Distibution", 
      ylab = "Probability Density",
      xlab = "x")
colorArea(from=-3, to=-1, dt, df=6, col=4, dens=20)


pt(-1,6,lower.tail = FALSE)
integrate(dt, df= 6, lower= -1, upper= Inf, abs.tol = 0)$value

curve(dt(x,6), from=-3, to=3, 
      main = "t-Distibution", 
      ylab = "Probability Density",
      xlab = "x")
colorArea(from=-1, to=3, dt, df=6, col=4, dens=20)

#Para la inversa t_(1-\alpha/2) de la funcion de densidad de probabilidad usamos
# qt(p, df, lower.tail = TRUE)

qt(0.95,6)
pt(1.94318,6)

qt(0.975,26)
pt(2.055529,26)

#Para generar un vector de n numeros aleatorios distribuidos con la distribucion t
# rt(n, df)

rt(5,6)

rt(10,6)

#recuerden utilizar set.seed para poder reproducir el mismo vector aleatorio mas
#adelante

################################################################################
################################################################################
################################################################################

#Utilizamos comandos similares para la distribucion chi-cuadrada

# dchisq(x, df)
# pchisq(q, df, lower.tail = TRUE)
# qchisq(p, df, lower.tail = TRUE)
# rchisq(n, df)

curve(dchisq(x,2), from=0, to=20, 
      main = "chi-Distibution", 
      ylab = "Probability Density",
      xlab = "x", col="green")

curve(dchisq(x,3), from=0, to=20, 
      main = "chi-Distibution", 
      ylab = "Probability Density",
      xlab = "x", col="red", add = TRUE)

curve(dchisq(x,10), from=0, to=20, 
      main = "chi-Distibution", 
      ylab = "Probability Density",
      xlab = "x", col="blue", add = TRUE)

#Para obtener \chi^2_(\alpha/2)^df

qchisq(.025,6)
pchisq(1.237344,6)

#Para obtener \chi^2_(1-\alpha/2)^df

qchisq(.975,6)
pchisq(14.44938,6)

################################################################################
################################################################################
################################################################################

#Utilizamos comandos similares para la distribucion F
#df(x, df1, df2, ncp, log = FALSE)
#pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
#qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
#rf(n, df1, df2, ncp)

curve(df(x,3,109), from=0, to=4, 
      main = "F-Distibution", 
      ylab = "Probability Density",
      xlab = "F", col="black")
colorArea(from=2.68791, to=4, df, df1=3, df2=109, col="red", dens=20)
text(2.5,0.03,2.6879,cex=0.55,col=c("red"))

#Por que es crucial la suposicion de normalidad de la poblacion de la cual se 
#extrae una muestra para estimar la media o la varianza de la poblacion??
#Porque la distribucion t y la distribucion chi^2 se generan a partir
#de una poblacion distribuida normalmente

#Usaremos dplyr para manipular los dataframes con mayor facilidad
library(dplyr)
#Usaremos moments para calcular sesgo y la curtosis



library(moments)

data<-read.csv("students.csv", header=TRUE) 
View(data)

mean(data$height)

#Extraemos unicamente la variable que nos interesa, en este caso las alturas
HEIGHT<-data.frame(data$height)
View(HEIGHT)

#Calculamos la media y desviacion estandar de las alturas de la poblacion
mu1 <- mean(data$height)
sigma1 <- sd(data$height)

mu1
sigma1
#Dibujemos el histograma de las alturas y dibujemos una curva normal con \mu y \sigma

hist(data$height, density=20, breaks=20, prob=TRUE, ylim=c(0, .04),
     col = "lightgray", xlab="Alturas", 
     main="Histograma")
curve(dnorm(x, mean=mu1, sd=sigma1), from=140, to=200, 
      col="darkblue", lwd=1, add=TRUE, yaxt="n")

mean(data$height) #La media de las alturas es 171.38
sd(data$height) #La desviacion estandar es de 11.08
skewness(data$height) #El sesgo es de -0.009, que es casi cero entonces es simetrica
kurtosis(data$height) #La curtosis es 2.524, una distribucion normal deberia tener curtosis 3
#Como es menor que tres, entonces los datos estan mas aplanados que una distribucion normal
#Ojo, como la definimos en clase la curtosis deberia ser cero

#Extraemos una muestra aleatoria de tamanio 50
x1<-HEIGHT[sample(nrow(HEIGHT), 50), ]
x1

#Ahora creamos 500 vectores, para 500 muestras aleatorias de tamanio 100
muestras1 <-500
n1 <- 100
for(i in 1:muestras1) { 
  nam <- paste("x", i, sep = "")
  assign(nam, HEIGHT[sample(nrow(HEIGHT), n1), ])
}

x1
x2
x3
x500

mean(x1)
mean(x2)
mean(x3)
mu1
#Calcula las medias para cada x_i y las guarda en un dataframe
medias<-data.frame(sapply(paste('x', 1:muestras1, sep=""), function(x) mean(get(x))))
View(medias)
#Cambia el nombre de la columna a "medias"
colnames(medias)[1] <- "medias"
#Agrega el nombre de las filas como columna en el data frame y lo remueve de las filas
medias <- tibble::rownames_to_column(medias, "ID")
medias
#Ahora hacemos lo mismo para las varianzas
varianzas<-data.frame(sapply(paste('x', 1:muestras1, sep=""), function(x) var(get(x))))
colnames(varianzas)[1] <- "varianzas"
varianzas <- tibble::rownames_to_column(varianzas, "ID")
varianzas

#Unimos los dos dataframes a traves de la variable ID
total <- merge(medias,varianzas,by="ID")
View(total)

#Ahora construimos la distribucion-t

total$distribucion_t <- (total$medias - mu1)/ (sqrt(total$varianzas)/sqrt(n1))
View(total)

#Ahora dibujemos el histograma de la distribucion-t
hist (total$distribucion_t,main ="t-distribution", density=20, breaks=8,
      col = "lightgray", xlab=expression(bar(x)), 
      prob=TRUE, ylim=c(0,.4))
curve(dt(x, n1-1), from=-3, to=3, 
      col="darkblue", lwd=1, add=TRUE, yaxt="n")

mean(total$distribucion_t) #La media de una distrib-t debe ser cero
var(total$distribucion_t) #La varianza de una distrib-t debe ser dof/(dof-2)=1.0206 donde dof=99
skewness(total$distribucion_t) #El sesgo deberia ser cero
kurtosis(total$distribucion_t) #La cursosis deberia ser 6/(dof-4)=3+0.0632

#Ahora construimos la distribucion-chi cuadrada

total$distribucion_chi2 <- (n1-1)*total$varianzas/sigma1^2
View(total)

#Ahora dibujemos el histograma de la distribucion-chi2
hist (total$distribucion_chi2,main ="chi2-distribution", density=20, breaks=8,
      col = "lightgray", xlab=expression(bar(x)), 
      prob=TRUE, ylim=c(0,.05))
curve(dchisq(x, n1-1), from=25, to=140, 
      col="darkblue", lwd=1, add=TRUE, yaxt="n")

mean(total$distribucion_chi2) #La media de una distrib-chi2 debe ser = dof = 99
var(total$distribucion_chi2) #La varianza de una distrib-chi2 debe ser = 2 dof = 198
skewness(total$distribucion_chi2) #El sesgo = sqrt(8/dof) = 0.2843
kurtosis(total$distribucion_chi2) #La cursosis =12/dof = 3+0.1212

################################################################################
################################################################################
################################################################################
################################################################################

#Ahora repitamos lo mismo pero con datos que si se distribuyen normalmente

NORMAL<-data.frame(n10000)
mu2 <- mean(NORMAL$n10000)
sigma2 <- sd(NORMAL$n10000)

#Dibujemos el histograma y dibujemos una curva normal con \mu y \sigma
hist(NORMAL$n10000, density=20, breaks=20, prob=TRUE, ylim=c(0,.1),
     col = "lightgray", xlab="Alturas", 
     main="Histograma")
curve(dnorm(x, mean=mu2, sd=sigma2), from=50, to=90, 
      col="darkblue", lwd=1, add=TRUE, yaxt="n")

mean(NORMAL$n10000) #La media deberia ser 70
sd(NORMAL$n10000) #La desviacion estandar deberia ser 5
skewness(NORMAL$n10000) #El sesgo deberia ser 0, entonces es simetrica
kurtosis(NORMAL$n10000) #La curtosis deberia ser 3

#Ahora vamos a extraer 500 muestras de tamanio 100 y 
#vamos a calcular las medias y varianzas de cada muestra
muestras2 <- 500
n2 <-100
for(i in 1:muestras2) { 
  nam <- paste("z", i, sep = "")
  assign(nam, NORMAL[sample(nrow(NORMAL), n2), ])
}

medias2<-data.frame(sapply(paste('z', 1:muestras2, sep=""), function(x) mean(get(x))))
colnames(medias2)[1] <- "medias"
medias2 <- tibble::rownames_to_column(medias2, "ID")
varianzas2<-data.frame(sapply(paste('z', 1:muestras2, sep=""), function(x) var(get(x))))
colnames(varianzas2)[1] <- "varianzas"
varianzas2 <- tibble::rownames_to_column(varianzas2, "ID")
total2 <- merge(medias2,varianzas2,by="ID")

#Ahora construimos la distribucion-t
total2$distribucion_t <- (total2$medias - mu2)/ (sqrt(total2$varianzas)/sqrt(n2))

#Ahora dibujemos el histograma de la distribucion-t
hist (total2$distribucion_t,main ="t-distribution", prob=TRUE, ylim=c(0,0.5),
      density=20, breaks=15,
      col = "lightgray", xlab="Alturas")
curve(dt(x, n2-1), from=-4, to=4, 
      col="darkblue", lwd=1, add=TRUE, yaxt="n")

mean(total2$distribucion_t) #La media de una distrib-t debe ser cero
var(total2$distribucion_t) #La varianza de una distrib-t deberia ser dof/(dof-2)=1.0206
skewness(total2$distribucion_t) #El sesgo deberia ser cero
kurtosis(total2$distribucion_t) #La cursosis deberia ser 6/(dof-4)=3+0.0632 

#Ahora construimos la distribucion-chi cuadrada
total2$distribucion_chi2 <- (n2-1)*total2$varianzas/sigma2^2

#Ahora dibujemos el histograma de la distribucion-chi2
hist (total2$distribucion_chi2,main ="chi2-distribution", density=20, breaks=10,
      col = "lightgray", xlab=expression(bar(x)), 
      prob=TRUE, ylim=c(0,.04))
curve(dchisq(x, n2-1), from=25, to=150, 
      col="darkblue", lwd=1, add=TRUE, yaxt="n")

mean(total2$distribucion_chi2) #La media de una distrib-chi2 debe ser = dof = 99
var(total2$distribucion_chi2) #La varianza de una distrib-chi2 debe ser = 2 dof = 198
skewness(total2$distribucion_chi2) #El sesgo = sqrt(8/dof) = 0.2843
kurtosis(total2$distribucion_chi2) #La cursosis =12/dof = 3+0.1212

