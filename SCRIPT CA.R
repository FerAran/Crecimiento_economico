#importar

library(readxl)
CA <- read_excel("CA.xls")
View(CA)


#Extraer variables de PIB por año

aux2 <- CA[6:64]

View(aux2)

#Borrar la variable que está de más: [- fila, -col]
#Rowmeans Calcula la media de cada fila del objeto indicado

Mean_anual_Pais <- rowMeans(aux2)


#Separación de la columna log_pibpct-1960 COMO PIB BASE

pib_base <- CA[5]

#Graficar el PIB BASE, contra el promedio anual


plot(x=pib_base$`log_pibpct-1960`,y=Mean_anual_Pais,pch=25,main = "Convergencia \n Absoluta",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
abline(lm(Mean_anual_Pais~pib_base$`log_pibpct-1960`), col = "green")

#Graficar el PIB pctde 1960, contra el promedio anual

plot(x= CA$PIB_PCT_1960,y=Mean_anual_Pais,pch=25,main = "Convergencia \n Absoluta",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")



