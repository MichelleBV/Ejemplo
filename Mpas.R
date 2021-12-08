setwd("~/Universidad/Software/Database")
datadeslizamiento <- read.csv("Base_RNR1.csv",header = TRUE, sep=";",dec = ".")
View(datadeslizamiento)

# Extraer datos

Topografia <- as.numeric(datadeslizamiento$Topografía)
Pendiente <- as.numeric(datadeslizamiento$Pendiente2)
Aspecto <- as.numeric(datadeslizamiento$Aspecto)
PerfCu <- as.numeric(datadeslizamiento$Perfil.de.Curvatura)
PlanCu <- as.numeric(datadeslizamiento$Plano.de.Curvatura)
MaxTemp <- as.numeric(datadeslizamiento$Tmax)
MinTemp <- as.numeric(datadeslizamiento$Tmin)
Viento <- as.numeric(datadeslizamiento$Viento)
Solar <- as.numeric(datadeslizamiento$Solar)
Humedad_Relativa <- as.numeric(datadeslizamiento$Humedad)
Precipitacion3 <- as.numeric(datadeslizamiento$Precip3)
Precipitacion5 <- as.numeric(datadeslizamiento$Precip5)
Precipitacion7 <- as.numeric(datadeslizamiento$Precip7)
Precipitacion10 <- as.numeric(datadeslizamiento$Precip10)
Precipitacion15 <- as.numeric(datadeslizamiento$Precip15)
Precipitacion20 <- as.numeric(datadeslizamiento$Precip20)
Precipitacion30 <- as.numeric(datadeslizamiento$Precip30)
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
deslizamientos_normalizados$V18 <-as.numeric(datadeslizamiento1$Deslizamientos)
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
training_set
test_set=subset(deslizamientos_normalizados,split==FALSE)

# Red Neuronal 

install.packages('neuralnet')
library(neuralnet)
red_neural=neuralnet(V18~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V17+NL1+NL2+NL3+NL4+NV1+NV2+NV3,data = training_set ,hidden = 4,act.fct = 'tanh')
plot(red_neural)



setwd("~/Universidad/Software/Database")
completa <- read.csv("BPN.csv", header=TRUE, sep=";", dec=".")

X<-as.numeric(completa$POINT_X) #Coordenada X
y<-as.numeric(completa$POINT_Y) #Coordenada Y
Tp<-as.numeric(completa$Topografía)
Pd<-as.numeric(completa$Pendiente) #Pendiente
Asp<-as.numeric(completa$Aspecto) #Distancia
PlanoC<-as.numeric(completa$Plano.de.Curvatura) #Precipitacion
PerfilC<-as.numeric(completa$Perfil.de.Curvaruta) #Uso de Suelo
Tmax1<-as.numeric(completa$Tmax) #Geologia
Tmin1<-as.numeric(completa$Tmin) #Geologia
Solar1<-as.numeric(completa$Solar) #Geologia
Humedad1<-as.numeric(completa$Humedad) #Geologia
Viento1<-as.numeric(completa$Viento) #Geologia
P1<-as.numeric(completa$Precip3) #Geologia
P2<-as.numeric(completa$Precip5) #Geologia
P3<-as.numeric(completa$Precip7) #Geologia<-as.numeric(inundaciones_1$Litologia) #Geologia
P4<-as.numeric(completa$Precip10) #Geologia
P5<-as.numeric(completa$Precip15) #Geologia
P6<-as.numeric(completa$Precip20) #Geologia
P7<-as.numeric(completa$Precip30) #Geologia


completa1<-data.frame(X,y,Tp,Pd,Asp,PlanoC,PerfilC,Tmax1,Tmin1,Solar1,Humedad1,Viento1,P1,P2,P3,P4,P5,P6,P7)
completa1<-na.omit(completa1)

library(RSNNS)
normalizacion2<-normalizeData(completa1[,1:19],type='0_1')
n3<-as.data.frame(normalizacion2)
View(completa1)

n3$NL1 <- as.numeric(completa$NL1)
n3$NL2 <- as.numeric(completa$NL2)
n3$NL3 <- as.numeric(completa$NL3)
n3$NL4 <- as.numeric(completa$NL4)
n3$NV1 <- as.numeric(completa$NV1)
n3$NV2 <- as.numeric(completa$NV2)
n3$NV3 <- as.numeric(completa$NV3)

library(caret)
predecir8<-predict(red_neural,newdata=n3)
as.character(predecir8)
View(predecir8)
write.csv(predecir8,file = "Datos_Pre_Red6.csv")
