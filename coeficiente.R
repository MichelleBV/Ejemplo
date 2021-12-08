setwd("C:/Users/Personal/Downloads")
datos <- read.csv("Base_CP.csv",header = T, dec = ".", sep=";")

plot(datos)
cor(datos)
