# Importar dataset

#install.packages("quantmod")
library(quantmod)

sharesIpc = c("KIMBERA.MX", "BOLSAA.MX", "OMAB.MX", "PINFRA.MX", "ASURB.MX", "IENOVA.MX",
           "CEMEXCPO.MX", "ALSEA.MX", "BIMBOA.MX", "ALPEKA.MX", "LABB.MX", "MEGACPO.MX",
           "GMEXICOB.MX", "GCC.MX", "AC.MX", "GAPB.MX", "GRUMAB.MX", "GCARSOA1.MX",
           "CUERVO.MX", "GENTERA.MX", "AMXL.MX", "TLEVISACPO.MX", "RA.MX", 
           "GFNORTEO.MX", "BBAJIOO.MX")

nrowShare = length(as.numeric(getSymbols("BOLSAA.MX", 
                                         from="2020-01-01",
                                         src="yahoo",
                                         auto.assign = F)[,6]))

dataset = as.data.frame(matrix(nrow = nrowShare, ncol = length(sharesIpc)))
colnames(dataset) = sharesIpc

iniCol = 1
for (i in sharesIpc){
  dataset[iniCol]<-as.numeric(getSymbols(i, from="2020-01-01",src="yahoo",auto.assign = F)[,6])
  iniCol = iniCol+1
}

# Calculo del rendimiento por acción

sharesPerf = as.data.frame(matrix(nrow = nrowShare-1, ncol = length(sharesIpc)))
colnames(sharesPerf) = sharesIpc

for (i in 1:(nrowShare-1)){
  for (j in 1:length(sharesIpc)){
    sharesPerf[i,j] = log(dataset[i+1,j])-log(dataset[i,j])
  }
}



