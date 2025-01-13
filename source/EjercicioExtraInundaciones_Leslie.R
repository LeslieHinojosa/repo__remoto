###Importar base de datos

datos<-read.csv("CONTRERAS.csv",
         header=T,
         encoding ="UTF-8",
         check.names =F)

### Verificar estructura y nombre de las variables
str(datos)
names(datos)

##Generar nueva variable llamada Tipo a partir de la variable Dictamen 
datos$Tipo<-
  ifelse(datos$DICTAMEN == "ATARJEA OBSTRUIDA","Obstruction",
         ifelse(datos$DICTAMEN == "COLADERA OBSTRUIDA","Obstruction",
  ifelse(datos$DICTAMEN == "FALTA DE INFRAESTRUCTURA","Sewage overflow",
         ifelse(datos$DICTAMEN == "INSUFICIENCIA DE ATARJEA Y COLECTOR","Sewage overflow",
                ifelse(datos$DICTAMEN == "INSUFICIENCIA DE GRIETA","Sewage overflow",
    ifelse(datos$DICTAMEN == "RUPTURA DE TUBO DE AGUA POTABLE","Breaking off", 
           ifelse(datos$DICTAMEN == "INEXISTENTE AL MOMENTO DE LA INSPECCION [NO SE APRECIAN DIMENSIONES]", "Lacking info",
                  ifelse(datos$DICTAMEN == "INEXISTENTE AL MOMENTO DE LA INSPECCION", "Lacking info",
                         ifelse(datos$DICTAMEN == "NO SE OPERO CARCAMO DE BOMBEO", "Pump Failure",
                                ifelse(datos$DICTAMEN == "HUNDIMIENTO DE CARPETA ASFALTICA","Sinking", "otros")))))))))) 


##Crear dos subgrupos. Formados por dos rangos de años distintos. 
datos$subgrupo<-ifelse(datos$Año%in%c(2007,2008,2009),"A", ifelse(datos$Año%in%c(2010,2011,2012,2013,2014),"B","otros"))

##Generar una nueva variable llamada Frecuencia
datos$Frecuencia<- rep(1,length(174))

### Frecuencia por año y tipo
FAñoTipo<-aggregate(Frecuencia~ Año+Tipo, data= datos, FUN= sum)
str(FAñoTipo)
##Graficar frecuencia en función del año y en función del tipo

GraficaAño<-boxplot(FAñoTipo$Frecuencia~FAñoTipo$Año) ##Forma1 para graficar
GraficaAño<-boxplot(Frecuencia~Año, data=FAñoTipo, xlab= "Año", ylab= "Frecuencia", cex.axis=0.7) ##Forma2
GraficaAño

GraficaTipo<-boxplot(Frecuencia~Tipo, data=FAñoTipo, xlab= "Tipo", ylab= "Frecuencia", cex.axis= 0.7)
GraficaTipo



