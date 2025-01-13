###Los datos se obtuvieron del inventario forestal para 10 parcelas de 1.0 ha cada una. 
###Cada parcela tiene información sobre el número de árboles distribuidos en 10 clases diametrales de 2 cm cada una. 
###El objetivo del ejercicio es calcular el área basal total (m2/ha) para cada una de las 10 parcelas y guardar la información en un vector llamado Area_Basal.

###Lectura de datos
datos<-read.csv("AreaBasal.csv",header=T)
str(datos)

### Vector DAP promedio por clase diametral
DAP<-c(2.3,4.5,5.5,6.0,6.4,6.8,7.7,7.9,8.1,9.0)

#Área basal de un árbol de cada clase diametral
ABA<-(3.142*(DAP^2/40000))

# Multiplicación del área de un árbol por el número total de árboles de cada parcela de cada clase diametral
prueba<-ABA*datos[1,]

# Área basal de parcela 1
ABTP1<-(sum(prueba))

###Loop para obtener el área de las 10 parcelas. Resultado se guarda en un vector en donde cada elemento es el cálculo del área total para cada parcela. 
Area_Basal<-numeric(length(DAP))  ###
  
  for(a in 1:length(DAP)){
    prueba<-ABA*(datos[a,])
    ABTP1<-sum(prueba)
    Area_Basal[a]<-ABTP1
  }
  
Area_Basal

### Suma del área basal de todas las parcelas
round(sum(Area_Basal), digits=3)
 