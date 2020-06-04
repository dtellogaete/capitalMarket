# Install the package
#install.packages('mFilter')
#install.packages("quantmod")


# Importar librería
library(mFilter)
library(quantmod)

accion<-getSymbols("TSLA", from="2020-01-01",src="yahoo",auto.assign = F)[,6]
seriedetiempo<-ts(accion)

plot(seriedetiempo)


seriefiltrada<-hpfilter(seriedetiempo,freq=c(13.9, 12),type=c("lambda","frequency"),drift=TRUE)
                        
plot(seriefiltrada, legend(NULL))


                        
                        
                        