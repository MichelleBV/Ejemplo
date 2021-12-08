setwd("C:/Users/Personal/Downloads")
datos <- read.csv("Datos.csv",header = T, dec = ".", sep = ";")

plot(datos)

cor(datos)
