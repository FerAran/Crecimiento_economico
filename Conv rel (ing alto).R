#Extraer variables (unicamente para ingreso alto)
aux_IA_name <- CA[CA$ID_ingreso == "Ingreso alto",1:64]
aux_IA <- CA[CA$ID_ingreso == "Ingreso alto", 6:64]
aux_IA_log <- CA[CA$ID_ingreso == "Ingreso alto", 5]
aux_IA_log <- aux_IA_log$`log_pibpct-1960`
View(aux_IA)

#Calcular media por países

Mean_anual_P_IA <- rowMeans(aux_IA)
View(Mean_anual_P_IA)

#graficar en 2d

plot(x=aux_IA_log,y = Mean_anual_P_IA,pch =1, main = "Convergencia condicional\n (ingreso alto)",xlab = "Base 1960",ylab = "Crecimiento PIB PCT", col="blue")
abline(lm(Mean_anual_P_IA ~ aux_IA_log),col="orchid")
text(aux_IA_log,Mean_anual_P_IA,aux_IA_name$`Country Code`,cex = 0.6,pos = 4,col = "red")

#suavizar diagrama de dispersión (no recomendado)

scatter.smooth(x=aux_IA_log,y=Mean_anual_P_IA,main = "Convergencia condicional\n (ingreso alto)",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
text(aux_IA_log,Mean_anual_P_IA,aux_IA_name$`Country Code`,cex = 0.6,pos = 4,col = "red")


#graficar en 3d

library(scatterplot3d)
#Crear variable ID para eje z 
#identical(length(Mean_anual_P_IA),length(aux_IA_log),length(ID_IA)) debe ser TRUE


ID_IA <- time(aux_IA_log)
View(ID_IA)
#crear columna con números 1:63 
#ID <- (1:63)

#solicitar gráfica

scatterplot3d(ID_IA,aux_IA_log,Mean_anual_P_IA)

#Etiquetas por país

text(aux_IA_log,Mean_anual_P_IA,aux_IA_name$`Country Code`,cex = 0.6,pos = 2,col = "red")

#Agregar barras a los puntos y guardar gráfica

G3D <- scatterplot3d(ID_IA,aux_IA_log,Mean_anual_P_IA,pch = 16,highlight.3d = TRUE,type = "h",main = "Convergencia condicional \n (ingreso alto)",xlab = "País", ylab = "1961",zlab = "Tasa crecimiento")

#AGREGAR NOMBRES A LOS PUNTOS

text(G3D$xyz.convert(ID_IA,aux_IA_log,Mean_anual_P_IA),labels = aux_IA_name$`Country Code`,cex = 0.1,col ="steelblue")

#Agregar línea de regresión

fit <- lm(Mean_anual_P_IA ~ ID_IA+aux_IA_log)
G3D$plane3d(fit)


