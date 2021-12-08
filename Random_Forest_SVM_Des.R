setwd("C:/Users/Personal/Downloads")
datadeslizamiento <- read.csv("Base_Reducida_Normalizada.csv",header = TRUE, sep=";",dec = ".")
View(datadeslizamiento)
# Extraer datos

Topografia <- as.numeric(datadeslizamiento$Topografía)
Pendiente <- as.numeric(datadeslizamiento$Pendiente)
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
deslizamientos_normalizados$V18 <-as.factor(datadeslizamiento1$Deslizamientos)
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

## Arbol de Decisión

library(rpart)
set.seed(123)
Modelo1<-rpart(V18~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V17+NL1+NL2+NL3+NL4+NV1+NV2+NV3,data=training_set,control=rpart.control(cp=0,00001), method ="class")
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(Modelo1, digits = -1, type=2, extra = 101, cex=0.7, nn=TRUE)

summary(Modelo1)
printcp(Modelo1)

#Prediccion arbol decision
install.packages("caret")
install.packages('e1071')
library(caret)
pred <- predict(Modelo1, test_set, type="class")
confusionMatrix(pred, as.factor(test_set$V18))

#RandomForest

install.packages("randomForest")
library(randomForest)
set.seed(1234)
Modelo2<-randomForest(V18~.,data=training_set)

#Prediccion RandomForest
pred1<-predict(Modelo2,test_set)
confusionMatrix(pred1, as.factor(test_set$V18))

install.packages("pROC")
library(pROC)
SVMROC1 <- roc(test_set$V18 ~ as.numeric(pred3),plot=TRUE,
           print.auc=TRUE,col="green",lwd =2,legacy.axes=TRUE,main="ROC Curves", nn=TRUE)

radomROC1 <- roc(test_set$V18 ~ as.numeric(pred1),plot=TRUE,
             print.auc=TRUE,col="blue",lwd =2,print.auc.y=0.4,legacy.axes=TRUE, add=TRUE)

## SVM

install.packages('e1071')
library(e1071)
mod_svm<-svm(V18~.,data=training_set,kernel='radial', gamma=1, cost=10 ) 
summary(mod_svm)

#Prediccion SVM
library(caret)
pred3 <- predict(mod_svm, test_set, type='response')
confusionMatrix(pred3,test_set$V18)
table(pred3,test_set$V18)

