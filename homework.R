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

#Calculo de la media y desviacion estandar del dataset

library(matrixStats)

summaryDataset = data.frame(matrix(nrow = 2, ncol = length(sharesIpc)))
colnames(summaryDataset) = sharesIpc

for (i in 1:length(sharesIpc)){
  summaryDataset[1,i]= colMeans(sharesPerf[i])
  summaryDataset[2,i]= colSds(as.matrix.data.frame(sharesPerf[i]))
  
}

# Tabla de frecuencias relativas muy relativas

library(dplyr) 

summaryDataset = t(summaryDataset)

frecTable = data.frame(matrix(nrow =(length(sharesIpc)+1),ncol=10))
colnames(frecTable) = c("share","0.5 S", "1.0 S", "1.5 S","2.0 S",
                       "2.5 S","3.0 S", "4.0 S", "5.0 S",
                       ">5.0 S")
shareIndex = c("Unit Normal","KIMBERA.MX", "BOLSAA.MX", "OMAB.MX", "PINFRA.MX", "ASURB.MX", "IENOVA.MX",
               "CEMEXCPO.MX", "ALSEA.MX", "BIMBOA.MX", "ALPEKA.MX", "LABB.MX", "MEGACPO.MX",
               "GMEXICOB.MX", "GCC.MX", "AC.MX", "GAPB.MX", "GRUMAB.MX", "GCARSOA1.MX",
               "CUERVO.MX", "GENTERA.MX", "AMXL.MX", "TLEVISACPO.MX", "RA.MX", 
               "GFNORTEO.MX", "BBAJIOO.MX")

frecTable$share = shareIndex

# primera colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*0.5+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,2] = cont/length(sharesPerf$KIMBERA.MX)
}

# segunda colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*1+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,3] = cont/length(sharesPerf$KIMBERA.MX)
}

# tercera colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*1.5+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,4] = cont/length(sharesPerf$KIMBERA.MX)
}


# cuarta colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*2.0+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,5] = cont/length(sharesPerf$KIMBERA.MX)
}

# quinta colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*2.5+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,6] = cont/length(sharesPerf$KIMBERA.MX)
}

# sexta colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*3.0+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,7] = cont/length(sharesPerf$KIMBERA.MX)
}

# septima colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*4.0+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,8] = cont/length(sharesPerf$KIMBERA.MX)
}


# octava colummna
for(i in  1:length(sharesPerf)){
  cont = 0
  print(i)
  for(j in 1:length(sharesPerf$KIMBERA.MX)){
    value = summaryDataset[i,2]*5.0+summaryDataset[i,1]
    if(sharesPerf[j,i]<=value){
      cont = cont+1
    }
  }
  frecTable[i+1,9] = cont/length(sharesPerf$KIMBERA.MX)
}

frecTable$`>5.0 S` = 1-frecTable$`5.0 S`

unitNormal = c(0.3830, 0.6826, 0.8664, 0.9545,  0.9876, 0.9973, 0.999938,
               0.9999994, 0.0000006)



for(i in 1:length(unitNormal)){
  frecTable[1,i+1] = unitNormal[i]
  
}



