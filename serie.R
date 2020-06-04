# Install the package
#install.packages("quantmod")
library(quantmod)

#MAQUETES EMDIAS MÓVILES


install.packages('TTR') 

install.packages("tidyquant") 


requiere(tidyquant)  

require(TTR) 

#Importar datos de Yahoo finanzas en la var "accion"
accion <-getSymbols("MRNA", from="2020-01-01",src="yahoo",auto.assign = F)[,6]

#Convertir los datos en serie de tiempo
AccionST<-ts(accion)

#VERIFICAR DATOS 
names(accion)

#Calcular la media móvil  
sma20<-SMA(accion,n=20)

head(sma20,n=50)

#calcular el promedio movil de 40 periodos

sma40<-SMA(accion,n=40)

head(sma40,n=50)

# Maldito Grafico

library("ggplot2")
library("reshape2")


shareDf= data.frame(accion, sma20, sma40)
colnames(shareDf) = c("acción", "sma20", "sma40")
date = index(accion)

ggplot(shareDf, aes(date)) + 
  geom_line(aes(y = accion, colour = "acción")) + 
  geom_line(aes(y = sma20, colour = "sma20")) +
  geom_line(aes(y = sma40, colour = "sma40")) +
  scale_colour_manual(values=c("black","green","blue"))
