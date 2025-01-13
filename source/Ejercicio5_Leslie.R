### Visualización de datos

data<-read.csv("Urea.csv")
str(data)

###Gráfica 1 Dispersión con línea de tendencia
Dispersion<- plot(data$Urea, data$Progesterona, xlab="Urea", 
                  ylab="Progesterona", pch=19)
abline(lm(data$Progesterona~data$Urea),col="blue")

###Media de Urea en función de la Dosis
UD<-aggregate(Urea~Dosis, data=data, FUN= mean)
str(UD)
UD
length(UD)
### Gráfica2 Barras Urea dependiendo de la Dosis
barplot(UD$Urea~UD$Dosis, xlab="Dosis", ylab="Urea")
barplot(UD$Urea, names.arg= c("1","2","3"), xlab="Dosis", ylab="Urea")
barplot(UD$Urea, names.arg= UD$Dosis, xlab="Dosis", ylab="Urea")

###Media de Progesterona en función de la dosis
PD<-aggregate(Progesterona~Dosis, data=data, FUN=mean)

### Tamaño de la muestra y sd

PD$N<-aggregate(Progesterona~Dosis, data=data,FUN=length)$Progesterona ## Cálculo de n
PD$SD <-aggregate(Progesterona ~ Dosis, data = data, FUN = sd)$Progesterona ## Cálculo desviación estándar
PD$SE <-PD$SD / sqrt(PD$N) ## Cálculo error estandar

### Cálculo de márgenes de error respecto a la media. En conjunto forman el IC del 95%
PD$IC_inf<- PD$Progesterona -qt(0.975, df=PD$N -1)* PD$SE  ## A la media se le resta el IC
PD$IC_sup <-PD$Progesterona +qt(0.975, df=PD$N -1)* PD$SE   ## A la media se le suma el IC

###Gráfica 3 Barras con Intervalos de confianza

library(ggplot2)

ggplot(PD, aes(x = Dosis, y = Progesterona)) +
  geom_bar(stat = "identity", fill = c("skyblue", "turquoise","royalblue"), color = "black") + # Barras
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +  # Intervalos de confianza
  labs(
    x = "Dosis",
    y = "Progesterona"
  ) 

### Gráfica 4 Dispersión Progesterona en función de la Urea, considerando el nivel de dosis en color distinto.

PUD<-plot(data$Urea, data$Progesterona, xlab= "Urea", ylab="Progesterona", pch=19,
     col=as.factor(data$Dosis))
abline(lm(data$Progesterona~data$Urea),col="blue")
legend("topright", legend= unique(data$Dosis),
       col=1:length(unique(data$Dosis)),
       pch=19,
       title="Dosis")

### Gráfica 4 Dispersión Progesterona en función de la Urea, considerando el nivel de dosis en color distinto con línea de regresión para cada dosis.
library(ggplot2)

PUD2<-ggplot(data, aes(x = Urea, y = Progesterona, color = as.factor(Dosis))) +
  geom_point(size = 3) + # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) + # Líneas de regresión por dosis
  labs(x = "Urea", y = "Progesterona", color = "Dosis") + # Etiquetas
  theme_minimal()

### Gráfica 4 Dispersión Progesterona en función de la Urea, considerando el nivel de dosis en color distinto con línea de regresión para cada dosis en paneles separados.
### Cada panel actúa como un gráfico independiente, pero todos comparten los mismos ejes y escalas, facilitando la comparación.
###utilizando plot
niveles_dosis<-unique(data$Dosis)
par(mfrow = c(1, length(niveles_dosis))) # Crear 1 fila y tantas columnas como niveles de Dosis, es decir tres paneles horizontales.

PUD2<-for (i in niveles_dosis) { # Iterar por cada nivel de Dosis
  subset_dosis <- data[data$Dosis == i, ] # Filtrar los datos para el nivel actual de Dosis
  plot(subset_dosis$Urea, subset_dosis$Progesterona, # Crear el gráfico de dispersión
       main = paste("Dosis:", i), # Título del panel
       xlab = "Urea", 
       ylab = "Progesterona", 
       pch = 19, col = "blue")
  abline(lm(Progesterona ~ Urea, data = subset_dosis), col = "red") # Añadir línea de regresión
}

## utilizando ggplot

PUD3<-ggplot(data, aes(x = Urea, y = Progesterona), pch=19)+
  geom_point(size = 2, col=as.factor(data$Dosis)) + # Puntos de dispersión
  geom_smooth(method = "lm", color = "blue", se = FALSE) + # Línea de regresión
  facet_wrap(~ Dosis) + # Crear un panel por nivel de dosis
  labs(x = "Urea", 
       y = "Progesterona")


