###Generación de los vectores Practica (factor) y Tasa de Secuestro de Carbono (numérico)
###Cada nivel está repetido 200 veces, mientras que se generaron aleatoriamente 200 valores de Tasa para cada Nivel
Practica<-rep(c("Composta","Fertilizacion","Laboreo"), each=200)
TasaSecuestro<-c(
  rnorm(200,0.5,0.13),
  rnorm(200,0.9,0.13),
  rnorm(200,0.1,0.13)
  )

##Generación del data frame, se declara a la variable Practica como factor, y se crea la segunda columna con la variable TasaSecuestro
carbono<-data.frame(Practica=factor(Practica),
                    TasaSecuestro=TasaSecuestro)
##Generación de boxplot de la Tasa de Secuestro de Carbono en función de la Práctica
##Ajuste de márgenes
par(mar = c(8, 6, 2, 4))

boxplot(carbono$TasaSecuestro~carbono$Practica, 
        xlab="Práctica",
        ylab="Tasa de secuestro", cex.axis=1)


