#Extraer variables (unicamente para ingreso bajo)
aux_IM_name <- CA[CA$ID_ingreso == "Ingreso mediano alto",1:64]
aux_IM <- CA[CA$ID_ingreso == "Ingreso mediano alto", 6:64]
aux_IM_log <- CA[CA$ID_ingreso == "Ingreso mediano alto", 5]
aux_IM_log <- aux_IM_log$`log_pibpct-1960`
View(aux_IM)

#Calcular media por países

Mean_anual_P_IM <- rowMeans(aux_IM)
View(Mean_anual_P_IM)

#graficar en 2d

plot(x=aux_IM_log,y = Mean_anual_P_IM,pch =1, main = "Convergencia condicional\n (ingreso mediano)",xlab = "Base 1960",ylab = "Crecimiento PIB PCT", col="blue")
abline(lm(Mean_anual_P_IM ~ aux_IM_log),col="orchid")
text(aux_IM_log,Mean_anual_P_IM,aux_IM_name$`Country Code`,cex = 0.6,pos = 4,col = "red")

#suavizar diagrama de dispersión (no recomendado)

scatter.smooth(x=aux_IM_log,y=Mean_anual_P_IM,main = "Convergencia condicional\n (ingreso medio)",xlab = "Año base 1960", ylab = "Tasa crec. PIB-PCT", col="blue")
text(aux_IM_log,Mean_anual_P_IM,aux_IM_name$`Country Code`,cex = 0.6,pos = 4,col = "red")

#graficar en 3d

library(scatterplot3d)
#Crear variable ID para eje z 
#identical(length(Mean_anual_P_IA),length(aux_IA_log),length(ID_IA)) debe ser TRUE


ID_IM <- time(aux_IM_log)
View(ID_IM)
#crear columna con números 1:63 
#ID <- (1:63)

#solicitar gráfica

scatterplot3d(ID_IM,aux_IM_log,Mean_anual_P_IM)

#Etiquetas por país

text(aux_IM_log,Mean_anual_P_IM,aux_IM_name$`Country Code`,cex = 0.6,pos = 2,col = "red")

#Agregar barras a los puntos y guardar gráfica

G3D <- scatterplot3d(ID_IM,aux_IM_log,Mean_anual_P_IM,pch = 16,highlight.3d = TRUE,type = "h",main = "Convergencia condicional \n (ingreso mediano alto)",xlab = "País", ylab = "1961",zlab = "Tasa crecimiento")

#AGREGAR NOMBRES A LOS PUNTOS

text(G3D$xyz.convert(ID_IM,aux_IM_log,Mean_anual_P_IM),labels = aux_IM_name$`Country Code`,cex = 0.1,col ="steelblue")

#Agregar línea de regresión

fit <- lm(Mean_anual_P_IM ~ ID_IM+aux_IM_log)
G3D$plane3d(fit)
