setwd("C:/Users/Personal/Downloads")
datadeslizamiento <- read.csv("Base_DEN.csv",header = TRUE, sep=";",dec = ".")
View(datadeslizamiento)
# Extraer datos

Topografia <- as.numeric(datadeslizamiento$Topografía)
Pendiente <- as.numeric(datadeslizamiento$Pendiente)
Aspecto <- as.numeric(datadeslizamiento$Aspecto)
PerfCu <- as.numeric(datadeslizamiento$Perfil.de.Curvatura)
PlanCu <- as.numeric(datadeslizamiento$Plano.de.Curvatura)
MaxTemp <- as.numeric(datadeslizamiento$Max_Temper)
MinTemp <- as.numeric(datadeslizamiento$Min_Temper)
Viento <- as.numeric(datadeslizamiento$Wind)
Solar <- as.numeric(datadeslizamiento$Solar)
Humedad_Relativa <- as.numeric(datadeslizamiento$H_relativa)
Precipitacion3 <- as.numeric(datadeslizamiento$Precip_3)
Precipitacion5 <- as.numeric(datadeslizamiento$Precip_5)
Precipitacion7 <- as.numeric(datadeslizamiento$Precip_7)
Precipitacion10 <- as.numeric(datadeslizamiento$Precip_10)
Precipitacion15 <- as.numeric(datadeslizamiento$Precip_15)
Precipitacion20 <- as.numeric(datadeslizamiento$Precip_20)
Precipitacion30 <- as.numeric(datadeslizamiento$Precip_30)
Deslizamientos <-datadeslizamiento$Deslizamientos

datadeslizamiento1 <- data.frame(Deslizamientos,Topografia,Pendiente,Aspecto,PerfCu,PlanCu,MaxTemp,MinTemp,Viento,Solar,Humedad_Relativa,Precipitacion3,Precipitacion5,Precipitacion7,Precipitacion10,Precipitacion15,Precipitacion20,Precipitacion30)
datadeslizamiento1 <-na.omit(datadeslizamiento1) #na.omit elimina espacios vacios
View(datadeslizamiento1)
# Normailización de datos
install.packages('RSNNS')
install.packages('Rcpp')
library(RSNNS)
library(Rcpp)
normalizaciondeslizacmiento <- normalizeData(datadeslizamiento1[,2:18],type='0_1')
deslizamientos_normalizados <- as.data.frame(normalizaciondeslizacmiento)
deslizamientos_normalizados$V18 <- as.factor(datadeslizamiento1$Deslizamientos)
View(deslizamientos_normalizados)
deslizamientos_normalizados$NL1 <- as.numeric(datadeslizamiento$NL1)
deslizamientos_normalizados$NL2 <- as.numeric(datadeslizamiento$NL2)
deslizamientos_normalizados$NL3 <- as.numeric(datadeslizamiento$NL3)
deslizamientos_normalizados$NL4 <- as.numeric(datadeslizamiento$NL4)
deslizamientos_normalizados$NV1 <- as.numeric(datadeslizamiento$NV1)
deslizamientos_normalizados$NV2 <- as.numeric(datadeslizamiento$NV2)
deslizamientos_normalizados$NV3 <- as.numeric(datadeslizamiento$NV3)
str(deslizamientos_normalizados)
# Division de datos

install.packages('caTools')
library(caTools)
set.seed(1234)
split=sample.split(deslizamientos_normalizados$V18,SplitRatio = 0.8)
training_set=subset(deslizamientos_normalizados,split==TRUE)
test_set=subset(deslizamientos_normalizados,split==FALSE)

# Red Neuronal 

install.packages('neuralnet')
library(neuralnet)
red_neural=neuralnet(V18~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V17+NL1+NL2+NL3+NL4+NV1+NV2+NV3,data = training_set ,hidden = 3,act.fct = 'tanh')
plot(red_neural)
