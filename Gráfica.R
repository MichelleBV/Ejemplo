setwd("C:/Users/Personal/Desktop")
datos <- read.csv("TVARIABLE.csv",header = TRUE, sep=";",dec = ".")

df <- data.frame(ColorCoche = c("Cobertura terrestre","Geológica","Geomorfologica","Hidrológica","Meteorológica","Otras"),
                 num = c(32,25,97,19,14,4))
barplot(height = df$num, names = df$ColorCoche,
        col = c("purple", "white", "skyblue", "blue"),
        horiz = 'TRUE',
        main = 'Variables Temáticas', 
        xlab = 'Variable',
        ylab = 'Cantidad')

df <- data.frame(ColorCoche = c("Cobertura terrestre","Geológica","Geomorfologica","Hidrológica","Meteorológica","Otras"),
                 num = c(32,25,97,19,14,4))
barplot(height = df$num, names = df$ColorCoche,
        col = c("purple", "white", "skyblue", "blue"),
        main = 'Variables Temáticas', 
        xlab = 'Variable',
        ylab = 'Cantidad')
