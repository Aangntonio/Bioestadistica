# Establece el directorio trabajo
setwd("C:/Users/Antonio González/Downloads")
#Enlistar objetos
objects()

#Eliminar objetos
rm(data)

# Instalacion de un paquete con comandos (si ya esta instalado se vuelve a instalar, actualizar)
install.packages("tseries")

# Cargar un paquete ya instalado
library(tseries)


# Operaciones simples

(5+4)+(7+2)

(5*5)/16

pi

12+4+3i+7i

sqrt(81)

abs(-10)

log(4)

exp(8)

4^2

sin(2+pi)

tan(2*pi)

# Vectores y operaciones

# Definir un vector
x<- c(1, 2, 3, 4, 5)
x

# No importa el orden en que hacemos la definicion
c(10, 0.5, 1, 0.2, 2) -> z
z
1/z

f<- (2*x)+z
f

x^2

#Comandos asociados a vectores

y<-c(2.5,3.1,7.8,3.4,5.5,7.9,6,3)

length(y)

max(y)

min(y)

sum(y)

mean(y)

median(y)

var(y)

sd(y)

sort(y)

#Formar matrices a partir de vectores:

i<-c(1,0,0)
j<-c(0,1,0)
k<-c(0,0,1)

# Vectores como columnas, cbind() une los vectores columna

cbind(i,j,k)

# Vectores como renglones

rbind(i,j,k)

#Leer un archivo .csv y asignarlo a un objeto data
#Usaremos la base de 
#https://sedac.ciesin.columbia.edu/data/set/icwq-change-in-chlorophyll-a-concentration-1998-2007/data-download

#El componente Cambio en las concentraciones de clorofila-a 1998-2007 de la 
#coleccion: Indicadores de la calidad del agua costera, representa una serie de 
#tiempo tabular de las concentraciones de clorofila-a para cada celda de la 
#cuadricula, derivada del conjunto de datos "Indicadores de la calidad del agua costera: 
#Clorofila-a Concentracion Anual 1998-2007".

#Las concentraciones de clorofila-a son un indicador de la abundancia y biomasa 
#del fitoplancton en aguas costeras y estuarinas. Pueden ser una medida eficaz 
#del estado trofico (se denomina nivel trofico a cada uno de los conjuntos de 
#especies o de organismos de un ecosistema que coinciden, por la posicion o 
#turno que ocupan, en el flujo de energia y nutrientes, es decir, a los que 
#ocupan un lugar equivalente en la cadena alimenticia)
#son indicadores potenciales de la tasa fotosintetica maxima y son una medida 
#de uso comun de la calidad del agua.

#Los niveles altos a menudo indican una mala calidad del agua (Algunas algas 
#producen toxinas que pueden ser un problema de salud publica cuando se encuentran 
#en altas concentraciones) y los niveles bajos a menudo sugieren buenas condiciones. 
#Sin embargo, las concentraciones 
#elevadas de clorofila-a no son necesariamente algo malo. Es la persistencia a 
#largo plazo de niveles elevados lo que constituye un problema. Por esta razon, 
#las concentraciones medias anuales de clorofila-a en una via fluvial son un 
#indicador importante en el estado del medio ambiente.

data<-read.csv("E:/Cursos Ciencias/Cursos 2024-2/Ejercicios Software/R introduction/mexico_poland.csv", header=TRUE) 
data
View(data)

#Fijando la ruta del archivo de antemano

setwd("E:/Cursos Ciencias/Cursos 2023-2/Ejercicios Software/R introduction")
data1<-read.csv("mexico_poland.csv", header=TRUE) 
View(data)

# Forma alterna de leer el archivo .csv

read.table("mexico_poland.csv", sep=",", header=TRUE)

#LLamar a la columna Contry (Pais) en el objeto data
names(data)
data$Country

#Ver los datos, sus nombres y usar attach en el objeto

View(data) #Ver los datos en forma tabular
names(data)
attach(data)

#Una vez usado attach ya solo usamos el nombre P1_1 para llamar a ese objeto

Country

#Variable categorica
#summary no funciona porque es una variable categorica
summary(data$Country)
#table nos da el conteo para las categorias del pais
table(data$Country)
#table tambien nos permite conocer el conteo cruzado de paises por region
table(data$Region, data$Country)

#summary Proporciona resumen cinco numeros mas la media (minimo, primer cuartil, mediana, media, tercer cuartil y maximo)
#Variable continua
summary(as.numeric(data$x1998)) 
#tapply Proporciona el descriptivo clasificado por pais
tapply(data$x1998, data$Country,function(x) 
  format(summary(x), scientific = F))

#El paquete dplyr nos ayuda a manipular un data frame con mayor facilidad
library(dplyr)

#Si por ejemplo queremos continuar el trabajo con solo unas pocas de las 
#variables de nuestro data frame podemos usar la funcion select(). 
#Esto guardara solo las variables que seleccionemos.

x2007_country <- select(data,Country,x2007)
x2007_country

#Otra forma de escribir el comando
x2007_country <- data %>% select(Country,x2007) 
#Para ayudarte a entender por que lo hemos escrito asi, vamos a revisarlo por 
#partes. Primero hemos llamado al data frame "data" y se lo hemos pasado al 
#siguiente paso, que es la funcion select(), usando el simbolo del pipe %>%. 
#En este caso no especificamos que objeto de datos vamos a usar en la funcion 
#select() porque esto se obtiene del resultado de la instruccion previa al 
#pipe. Dato curioso: es muy posible que te hayas encontrado con pipes antes en 
#la terminal de unix. En R el simbolo del pipe es %>%, mientras que en la 
#terminal es |, pero el concepto es el mismo.

#Si ahora queremos continuar con lo de arriba, pero solo con Mexico podemos 
#combinar select y filter.
mexico1998_2007 <- data %>%
  filter(Country=="Mexico") %>%
  select(GRIDCODE,Country,x1998,x1999,x2000,x2001,x2002,x2003,x2004,x2005,x2006,x2007)

View(mexico1998_2007)

#Ahora, summary nos da el descriptivo de todas las observaciones
summary(x2007)
#Pero group_by nos permite obtenerlo desagregado por pais
group_by(data, Country) %>% 
  summarise(
    conteo = n(), 
    media = mean(x2007, na.rm = F),
    DesEst = sd(x2007, na.rm = FALSE),
    var=var(x2007),
    min=min(x2007)
    
  )
#Tambien podemos obtener la informacion desagregada con tapply
tapply(data$x2007, data$Country,function(x) format(summary(x), scientific = F))
#Pero la ventaja de group_by es que le puedes decir que funcion quieres

#Medidas de dispersion

#Varianza 
var(x2007)
sd(x2007)

group_by(data, Country) %>% 
  summarise(
    count = n(), 
    variance = var(x2007, na.rm = TRUE),
    sd = sd(x2007, na.rm = TRUE)
  )

#Un percentil cualquiera , e.g. decil para la variable x2007
quantile(data$x2007,0.8)

group_by(data, Country) %>% 
  summarise(
    count = n(), 
    media = mean(x2007, na.rm = TRUE),
    decil1 = quantile(x2007,0.1),
    cuartil1 = quantile(x2007,0.25),
    cuartil2 = quantile(x2007,0.5),
    cuartil3 = quantile(x2007,0.75)
  )

#Diagrama de puntos

stripchart(data$x2007, method = "stack", pch = 5)


#Grafica boxplot de dos variables
#En una grafica de bigotes la linea media central representa la mediana
#La caja representa el rango intercuartil, el limite inferior es el 
#percentil 25 y el limite superior es el percentil 75
#El bigote inferior marca el valor mas peque?o dentro de 1.5 veces el rango
#intercuartil por debajo del percentil 25
#El bigote superior marca el valor mas grande dentro de 1.5 veces el rango
#intercuartil por arriba del percentil 75
#Se representan como puntos externos si su valor es >1.5 veces y <3 veces el 
#rango intercuartil mas alla de cualquier limite de la caja

boxplot(data$x1998, data$x1999, main="1998 vs 2007",ylab="Valor")

boxplot(mexico1998_2007$x1998,mexico1998_2007$x2007, main="Mexico 1998 vs 2007",ylab="Concentracion clorofila-a")

#El paquete ggplot nos ayuda a mejorar nuestros graficos
library(ggplot2)
ggplot(data, aes(x=Country, y=x2007)) + 
  geom_boxplot()

ggplot(data, aes(x=Region, y=x2007)) + 
  geom_boxplot() +
    coord_flip()

ggplot(data, aes(x=Region, y=x2007, fill=Country)) + 
  geom_boxplot() +
  facet_wrap(~Region, scale="free")

#Si se quiere el boxplot de una sola variable, en este caso x2007
boxplot(x2007,main="2007",ylab="Concentracion clorofila-a")

# recode n.s. (not significant) a missing para variable p
# Seleccionar filas donde p es significativo y recode column p
data$p_recode<-data$p
table(data$p_recode)
data$p_recode[data$p_recode=="n.s."]<- NA
data$p_recode[data$p_recode=="<0.001"|data$p_recode=="<0.01"|data$p_recode=="<0.05"]<- 1

#Filtramos solo los datos con cambios significativos
significant1998_2007 <- data %>%
  filter(p_recode==1) %>%
  select(Country,Region,x1998,x1999,x2000,x2001,x2002,x2003,x2004,x2005,x2006,x2007)
table(significant1998_2007$Country)
table(data$Country)

#Graficamos por pais y region
ggplot(significant1998_2007, aes(x=Region, y=x2007, fill=Country)) + 
  geom_boxplot() +
  facet_wrap(~Region, scale="free")

#Histogramas, con el numero de clases automatizado
hist (data$x1998,main ="1998")
hist (significant1998_2007$x2007,main ="2007")
#Si se quiere en terminos de frecuencias relativas
hist (significant1998_2007$x2007, prob=TRUE, main ="2007")
#Histograma con un numero fijo de clases
hist (significant1998_2007$x2007,nclass=5)
hist (significant1998_2007$x2007,nclass=20)

#Histograma con poligono de frecuencias
A<-hist (significant1998_2007$x2007,main ="x2007")
lines(c(min(A$breaks),A$mids,max(A$breaks)),c(0,A$counts,0),type="l",col="red")

#Diagrama de dispersion

plot(significant1998_2007$x2007,significant1998_2007$x1998, main="Gr?fico dispersi?n",xlab="2007",ylab="1998")

#DATOS CUALITATIVOS

#Diagramas de pie

#Pie
#Primero hacemos la tabla de frecuencias
frec<-table(data$Region)
frec
pie(frec)
#Convertir la tabla de frecuencias en un data frame
data_frec<-as.data.frame(frec)
data_frec
#Graficar con ggplot
ggplot(data_frec, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
#Mejorar la apariencia
ggplot(data_frec, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels

#Ahora con los paises
frec2<-table(data$Country)
frec2
#Convertir la tabla de frecuencias en un data frame
data_frec2<-as.data.frame(frec2)
data_frec2
#Mejorar la apariencia
ggplot(data_frec2, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels
#Al ser tantas categorias, se pierde el detalle de la grafica
#Entonces seleccionamos unicamente los paises de America Latina
america_latina <- data %>%
  filter(Region=="Latin America and the Caribbean") %>%
  select(Country,Region,x1998,x1999,x2000,x2001,x2002,x2003,x2004,x2005,x2006,x2007)

frec3<-table(america_latina$Country)
frec3
pie(frec3)
#Convertir la tabla de frecuencias en un data frame
data_frec3<-as.data.frame(frec3)
data_frec3
ggplot(data_frec3, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels

#Diagrama de barras de la variable para Country
frec4<-table(data$Country)
barplot(frec4,legend=F,beside=T,main="Pais")
#Convertir la tabla de frecuencias en un data frame
data_frec4<-as.data.frame(frec4)
data_frec4
#Mejorar apariencia
ggplot(data_frec4, aes(x=Var1, y=Freq, color=Var1)) +
  geom_bar(stat="identity", fill="white")



