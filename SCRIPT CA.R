#importar

library(readxl)
CA <- read_excel("CA.xls")
View(CA)


#Extraer variables de PIB por año

aux_gen <- CA[6:64]

View(aux_gen)

#Borrar la variable que está de más: [- fila, -col]
#Rowmeans Calcula la media de cada fila del objeto indicado

Mean_anual_Pais <- rowMeans(aux_gen)


#Separación de la columna log_pibpct-1960 COMO PIB BASE

pib_base <- CA[5]

#Graficar el PIB BASE, contra el promedio anual


plot(x=pib_base$`log_pibpct-1960`,y=Mean_anual_Pais,pch=1,main = "Convergencia \n Absoluta",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
abline(lm(Mean_anual_Pais~pib_base$`log_pibpct-1960`), col = "green")

#Graficar el PIB pctde 1960, contra el promedio anual

plot(x= CA$PIB_PCT_1960,y=Mean_anual_Pais,pch=25,main = "Convergencia \n Absoluta",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
text(pib_base$`log_pibpct-1960`,Mean_anual_Pais,CA$`Country Code`,cex = 0.6,pos = 4,col = "red")

#Filtrar por tipo de ingreso en la gráfica
#plot(x=pib_base$`log_pibpct-1960`,y=Mean_anual_Pais,pch=1,main = "Convergencia \n Absoluta",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
#abline(lm(Mean_anual_Pais~pib_base$`log_pibpct-1960`), col = "green")
#text(pib_base$`log_pibpct-1960`,Mean_anual_Pais,CA$`Country Code`,cex = 0.6,pos = 4,col = "red")
#suavizar diagrama de dispersión (no recomendado)
#scatter.smooth(x=pib_base$`log_pibpct-1960`,y=Mean_anual_Pais)

#ACTIVAR LIBRERÍA

library(scatterplot3d)
#Crear variable ID (de tiempo)

ID_gen <- time(CA$PAÍS)
View(ID)
#crear columna con números 1:63
#ID <- (1:63)

#solicitar gráfica

scatterplot3d(ID_gen,pib_base$`log_pibpct-1960`,Mean_anual_Pais)

#Etiquetas por país

text(pib_base$`log_pibpct-1960`,Mean_anual_Pais,CA$`Country Code`,cex = 0.6,pos = 4,col = "red")

#Agregar barras a los puntos y guardar gráfica

G3D <- scatterplot3d(ID_gen,pib_base$`log_pibpct-1960`,Mean_anual_Pais,pch = 16,highlight.3d = TRUE,type = "h",main = "convergencia",xlab = "País", ylab = "1961",zlab = "Tasa crecimiento")

#AGREGAR NOMBRES A LOS PUNTOS

text(G3D$xyz.convert(ID_gen,pib_base$`log_pibpct-1960`,Mean_anual_Pais),labels = CA$`Country Code`,cex = 0.1,col ="steelblue")

#Agregar línea de regresión

fit <- lm(Mean_anual_Pais ~ ID_gen+pib_base$`log_pibpct-1960`)
G3D$plane3d(fit)




