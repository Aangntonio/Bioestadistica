 #### LEctura de datos
library(tseries)

library(dplyr)


data1 <- read.csv("AirMOSS_HarvardForest_SoilMoisture_2012-2013.csv" , header = TRUE)
data2 <- read.csv('HarvardForest_dbh_ba_2012-10.csv',header = TRUE)

View(data1)

attach(data1)



temp_suelo <-na_if(soil_temp  , -9999.0)
conduc_soil <- na_if(conductivity_0.5.8, -9999.0)

#Estaditico de la temperatura del suelo
summary(temp_suelo)
var(temp_suelo,na.rm = T)


#estaditico de la conductividad
summary(conduc_soil)
var(conduc_soil,na.rm = TRUE)

# Suponiendo que ya tienes una variable llamada prueba
# Recodificar -999.0 como NA en la variable prueba

