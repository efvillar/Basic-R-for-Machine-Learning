# Código para entrenar modelos para predecir el número de unidades
# matriculadas en el RUNT
# 10 de septiembre de 2018
# Juan Fernando Agudelo
# Grupo Bancolombia


# Última modificación: 10 de septiembre de 2018

# Se cargan los datos en formato CSV. Los datos fueron obtenidos
# del RUNT 
datos<-read.csv("datos_ventas_autos.csv",sep=";")

# Se retira la columna Fecha
datos_estimacion<-datos[1:2192,-1]

# Gráfico de tendencia
plot(datos_estimacion$dia_num,datos_estimacion$und,type="l")

# Estimación de un bosque aleatorio
library(randomForest) # carga del paquete
# Estimación del modelo con todos los parámetros por defecto
modelo<-randomForest(und~.,data=datos_estimacion)
# Resultado:
print(modelo)

# Gráfico predichos vs. reales:

y_pred<-predict(modelo) # valores predichos
plot(datos_estimacion$und,y_pred,las=1,xlab="Unidades vendidas",
ylab="Pronóstico unidades vendidas") # Gráfico de puntos
abline(a=0,b=1,lwd=2,col="red") # recta identidad
grid() # Cuadrícula


# Estimación de un modelo lineal:
modelo_lm<-lm(und~.,data=datos_estimacion)
# Diagnóstico estadístico
summary(modelo_lm)
# Diagnóstico visual
plot(modelo_lm)

# Estimación de un modelo lineal generalizado
modelo_glm<-glm(und~.,data=datos_estimacion,family="poisson")
summary(modelo_glm)
y_pred_glm<-predict(modelo_glm,type="response")

# Gráfico de predichos vs observados
plot(datos_estimacion$und,y_pred_glm,las=1,xlab="Unidades vendidas",
ylab="Pronóstico unidades vendidas") # Gráfico de puntos
abline(a=0,b=1,lwd=2,col="red") # recta identidad
grid() # Cuadrícula

# Gráfico de tendencia valores predichos y estimados
# Gráfico de tendencia
plot(datos_estimacion$dia_num,datos_estimacion$und,type="l")
lines(datos_estimacion$dia_num,y_pred_glm,col="red")
