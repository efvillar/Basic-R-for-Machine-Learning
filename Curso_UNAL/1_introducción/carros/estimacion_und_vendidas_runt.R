# C�digo para entrenar modelos para predecir el n�mero de unidades
# matriculadas en el RUNT
# 10 de septiembre de 2018
# Juan Fernando Agudelo
# Grupo Bancolombia


# �ltima modificaci�n: 10 de septiembre de 2018

# Se cargan los datos en formato CSV. Los datos fueron obtenidos
# del RUNT 
datos<-read.csv("datos_ventas_autos.csv",sep=";")

# Se retira la columna Fecha
datos_estimacion<-datos[1:2192,-1]

# Gr�fico de tendencia
plot(datos_estimacion$dia_num,datos_estimacion$und,type="l")

# Estimaci�n de un bosque aleatorio
library(randomForest) # carga del paquete
# Estimaci�n del modelo con todos los par�metros por defecto
modelo<-randomForest(und~.,data=datos_estimacion)
# Resultado:
print(modelo)

# Gr�fico predichos vs. reales:

y_pred<-predict(modelo) # valores predichos
plot(datos_estimacion$und,y_pred,las=1,xlab="Unidades vendidas",
ylab="Pron�stico unidades vendidas") # Gr�fico de puntos
abline(a=0,b=1,lwd=2,col="red") # recta identidad
grid() # Cuadr�cula


# Estimaci�n de un modelo lineal:
modelo_lm<-lm(und~.,data=datos_estimacion)
# Diagn�stico estad�stico
summary(modelo_lm)
# Diagn�stico visual
plot(modelo_lm)

# Estimaci�n de un modelo lineal generalizado
modelo_glm<-glm(und~.,data=datos_estimacion,family="poisson")
summary(modelo_glm)
y_pred_glm<-predict(modelo_glm,type="response")

# Gr�fico de predichos vs observados
plot(datos_estimacion$und,y_pred_glm,las=1,xlab="Unidades vendidas",
ylab="Pron�stico unidades vendidas") # Gr�fico de puntos
abline(a=0,b=1,lwd=2,col="red") # recta identidad
grid() # Cuadr�cula

# Gr�fico de tendencia valores predichos y estimados
# Gr�fico de tendencia
plot(datos_estimacion$dia_num,datos_estimacion$und,type="l")
lines(datos_estimacion$dia_num,y_pred_glm,col="red")
