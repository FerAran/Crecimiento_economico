#Extraer variables (unicamente para ingreso bajo)
aux_IB_name <- CA[CA$ID_ingreso == "Países de ingreso bajo",1:64]
aux_IB <- CA[CA$ID_ingreso == "Países de ingreso bajo", 6:64]
aux_IB_log <- CA[CA$ID_ingreso == "Países de ingreso bajo", 5]
aux_IB_log <- aux_IB_log$`log_pibpct-1960`
View(aux_IB)

#Calcular media por países

Mean_anual_P_IB <- rowMeans(aux_IB)
View(Mean_anual_P_IB)

#graficar en 2d

plot(x=aux_IB_log,y = Mean_anual_P_IB,pch =1, main = "Convergencia condicional\n (ingreso bajo)",xlab = "Base 1960",ylab = "Crecimiento PIB PCT", col="blue")
abline(lm(Mean_anual_P_IB ~ aux_IB_log),col="orchid")
text(aux_IB_log,Mean_anual_P_IB,aux_IB_name$`Country Code`,cex = 0.6,pos = 4,col = "red")

#suavizar diagrama de dispersión (no recomendado)

scatter.smooth(x=aux_IB_log,y=Mean_anual_P_IB,main = "Convergencia condicional\n (ingreso bajo)",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
text(aux_IB_log,Mean_anual_P_IB,aux_IB_name$`Country Code`,cex = 0.6,pos = 4,col = "red")


#graficar en 3d

library(scatterplot3d)
#Crear variable ID para eje z 
#identical(length(Mean_anual_P_IA),length(aux_IA_log),length(ID_IA)) debe ser TRUE


ID_IB <- time(aux_IB_log)
View(ID_IB)
#crear columna con números 1:63 
#ID <- (1:63)

#solicitar gráfica

scatterplot3d(ID_IB,aux_IB_log,Mean_anual_P_IB)

#Etiquetas por país

text(aux_IB_log,Mean_anual_P_IB,aux_IB_name$`Country Code`,cex = 0.6,pos = 2,col = "red")

#Agregar barras a los puntos y guardar gráfica

G3D <- scatterplot3d(ID_IB,aux_IB_log,Mean_anual_P_IB,pch = 16,highlight.3d = TRUE,type = "h",main = "Convergencia condicional \n (ingreso bajo)",xlab = "País", ylab = "1961",zlab = "Tasa crecimiento")

#AGREGAR NOMBRES A LOS PUNTOS

text(G3D$xyz.convert(ID_IB,aux_IB_log,Mean_anual_P_IB),labels = aux_IB_name$`Country Code`,cex = 0.1,col ="steelblue")

#Agregar línea de regresión

fit <- lm(Mean_anual_P_IB ~ ID_IB+aux_IB_log)
G3D$plane3d(fit)


