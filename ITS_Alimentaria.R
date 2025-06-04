#Universidad Nacional Autónoma de México
#Interrupted Time Series (ITS)
#Autor: Cynthia Pimentel Vazquez
#-----------------------------------------------------------------------------------------------------------------------
#Instalar paqueterias
#-----------------------------------------------------------------------------------------------------------------------


install.packages("dplyr")
install.packages("ggplot2")
install.packages("foreign")
install.packages("readxl") 
install.packages("readr")
install.packages("tidyr") #cambiar estructura de bases de datos 
install.packages("ggthemes") 
install.packages("tidyverse")
install.packages("wavelets")
install.packages("nlme")

#-----------------------------------------------------------------------------------------------------------------------
#Cargar paqueterias
#-----------------------------------------------------------------------------------------------------------------------

library(wavelets)
library(car)
library(tidyr)#cambiar estructura de bases de datos 
library(dplyr) #Manipular bases de datos 
library(ggplot2) #Visualizacion 
library(foreign) #Importar bases de datos stata, dbf, csv 
library(readxl) #Importar bases de datos excel 
library(readr)  #Importar bases de datos fwf, csv
library(tidyverse)
library(nlme)
library(car)

#-----------------------------------------------------------------------------------------------------------------------
#Leer datos
#-----------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/CYN/Desktop/10._DÉCIMO SEMESTRE/INTERRUPTED TIME SERIES/Modelo_Tesis/bases")
#Leer base de datos, hoja "imai"
base1 <- read_excel("data.xlsx",sheet = "imai")
base2 <- read_excel("data.xlsx",sheet = "pot")
base3 <- read_excel("data.xlsx",sheet = "ing_nac")
base4 <- read_excel("data.xlsx",sheet = "ing_extr")
base5 <- read_excel("data.xlsx",sheet = "htpo")
base6 <- read_excel("data.xlsx",sheet = "iai")
base7 <- read_excel("data.xlsx",sheet = "rem_alim")
base8 <- read_excel("data.xlsx",sheet = "vp")
base9 <- read_excel("data.xlsx",sheet = "vv")
base10 <- read_excel("data.xlsx",sheet = "ipo")
base11 <- read_excel("data.xlsx",sheet = "iht")
base12 <- read_excel("data.xlsx",sheet = "irmsa")
base13 <- read_excel("data.xlsx",sheet = "irmsu")
base14 <- read_excel("data.xlsx",sheet = "igae_manu")


view(base3)

#-----------------------------------------------------------------------------------------------------------------------
#Gráfica inicial
#-----------------------------------------------------------------------------------------------------------------------


# 1) Indicador Mensual de Actividad Industrial (IMAI) de la industria alimentaria

# Gráfica: variable de resultado versus el tiempo
plot(base1$time,base1$imai_alim,
     ylab="IMAI",
     ylim=c(90,130),
     xlab="Año",
     type="l",
     col="red",
     xaxt="n")

# Añadir etiquetas de año en el eje-x(x-axis)
axis(1, at=1:60, labels=base1$year)

# Agregar los puntas en la figura 
points(base1$time,base1$imai_alim,
       col="red",
       pch=20)

# etiqueta del cambio 
abline(v=30.5,lty=2)


#-----------------------------------------------------------------------------------------------------------------------
#6. ANÁLISIS PRELIMINAR
#-----------------------------------------------------------------------------------------------------------------------


# A preliminary OLS regression

model_ols <- lm(imai_alim ~ time + level + trend, data=base1)
summary(model_ols)

#El intercepto es de 107.9081 y es estadísticamente significativo

#/ El coeficiente del tiempo indica que la tendencia existente era un
#un incremento sobre el tiempo de 0.223 por cierto en el  IMAI de la 
#industria alimentaria, y es estadisticamente significativo

#El nivel muestra una caída de -0.71909 en el indice del vólumen físico, pero no
# es estadísticamente significativo.

#Por último la tendencia muestra una caída de -0.0881 en el vólumen fisico,oero
# no es estadísticamente significativo.

confint(model_ols)

#-----------------------------------------------------------------------------------------------------------------------
# 7. AUTOCORRELACIÓN
#-----------------------------------------------------------------------------------------------------------------------

# Puerba Durbin-watson, 12 time periods
dwt(model_ols,max.lag=12,alternative="two.sided")

# Grafica de los residuos de la regresión OLS para verificar errores correlacionados en serie
plot(base1$time,
     residuals(model_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Gráfico ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model_ols))
acf(residuals(model_ols),type='partial')
# Note decay in ACF, significant spike at 10 in PACF, model p=10



#-----------------------------------------------------------------------------------------------------------------------
# 8. CORRER MODELO FINAL
#-----------------------------------------------------------------------------------------------------------------------

library(nlme)
# Ajustar el modelo de regresión GLS 
model_p10 <- gls(imai_alim ~ time + level + trend,
                 data=base1,
                 correlation=corARMA(p=10,form=~time),
                 method="ML")
summary(model_p10)



#-----------------------------------------------------------------------------------------------------------------------
# Diagnostic tests
#-----------------------------------------------------------------------------------------------------------------------


# Likelihood-ratio tests to check AR process

#Modelo p=10, q=1
model_p10q1 <- update(model_p10,correlation=corARMA(p=10,q=1,form=~time))
anova(model_p10,model_p10q1)
summary(model_p10q1)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa

#Modelo p=10, q=2
model_p10q2 <- update(model_p10,correlation=corARMA(p=10,q=2,form=~time))
anova(model_p10,model_p10q2)
summary(model_p10q2)
#todos los coeficientes son significativos
#Se tiene un intercepto en 
#Se rechaza la hipótesis nula de que estos modelos son iguales
#Se elige puesto que se rechaza la hipotesis nula


#Modelo p=4, q=1
model_p4q1 <- update(model_p10,correlation=corARMA(p=4,q=1,form=~time))
anova(model_p10,model_p4q1)
summary(model_p4q1)
#todos los coeficientes SON significativos
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales

#Modelo p=6, q=2
model_p6q2 <- update(model_p10,correlation=corARMA(p=6,q=2,form=~time))
anova(model_p10,model_p6q2)
summary(model_p6q2)
#todos los coeficientes SON significativos
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
n<-1e-04
formatC(n, format = "f", digits = 4)


#Modelo p=3, q=1
model_p3q1 <- update(model_p10,correlation=corARMA(p=3,q=1,form=~time))
anova(model_p10,model_p3q1)
summary(model_p3q1)
#todos los coeficientes SON significativos
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales

#-----------------------------------------------------------------------------------------------------------------------

#MODELO FINAL
#-----------------------------------------------------------------------------------------------------------------------

##Modelo p=10, q=2
model_p10q2 <- update(model_p10,correlation=corARMA(p=10,q=2,form=~time))
anova(model_p10,model_p10q2)
summary(model_p10q2)
#todos los coeficientes son significativos
#Se tiene un intercepto en 
#Se rechaza la hipótesis nula de que estos modelos son iguales
#Se elige puesto que se rechaza la hipotesis nula


#-----------------------------------------------------------------------------------------------------------------------

# Residual plot
# Null Hypo: Null Hypo: los residuos de un modelo especificado correctamente 
#se distribuyen de forma independiente; los residuos son ruido blanco

par(mfrow=c(1,1))
qqPlot(residuals(model_p10q2))
#Los residuales se comportan de manera normal, por lo que no se rechaza la hipotesis nula.


#-----------------------------------------------------------------------------------------------------------------------

# 9. GRAFICAR LOS RESULTADOS
#-----------------------------------------------------------------------------------------------------------------------

# Produce the plot, first plotting the raw data points
plot(base1$time,base1$imai_alim,
     ylim=c(95,130),
     ylab="IMAI",
     xlab="Año",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=base1$year)

# Add line indicating weather pattern change
abline(v=30.5,lty="dotted")

# Plot the first line segment
lines(base1$time[1:30], fitted(model_p10q2)[1:30], col="red",lwd=2)
# Plot the second line segment
lines(base1$time[31:60], fitted(model_p10q2)[31:60], col="red",lwd=2)

# And the counterfactual
segments(1,
         model_p10q2$coef[1]+model_p10q2$coef[2],
         60,
         model_p10q2$coef[1]+model_p10q2$coef[2]*60,
         lty=2,
         lwd=2,
         col='red')


#-----------------------------------------------------------------------------------------------------------------------

# 10. PREDECIR CAMBIOS RELATIVOS Y ABSOLUTOS
#-----------------------------------------------------------------------------------------------------------------------


# Predicted value at 6 months after the weather change
pred <- fitted(model_p10q2)[36]

# Then estimate the counterfactual at the same time point
cfac <- model_p10q2$coef[1] + model_p10q2$coef[2]*36

# Absolute change at  6 months
pred - cfac
# Relative change at 6 months
(pred - cfac) / cfac


print(n)
#=======================================================================================================================
#=======================================================================================================================
#=======================================================================================================================


setwd("C:/Users/CYN/Desktop/10._DÉCIMO SEMESTRE/INTERRUPTED TIME SERIES/Modelo_Tesis/bases")
#Leer base de datos, hoja "imai"

base7 <- read_excel("data.xlsx",sheet = "ipo")


#=======================================================================================================================
#MODELO 2: Indice de personal ocupado (ipo), industria alimentaria
#=======================================================================================================================

view(base7)
#-----------------------------------------------------------------------------------------------------------------------
#Gráfica inicial
#-----------------------------------------------------------------------------------------------------------------------


# 1) Indice de población ocupada de la industria alimentaria

# Gráfica: variable de resultado versus el tiempo
plot(base7$time,base7$ipo_alim,
     ylab="Indice de población ocupada",
     ylim=c(100,117),
     xlab="periodo",
     type="l",
     col="red",
     xaxt="n")

# Añadir etiquetas de año en el eje-x(x-axis)
axis(1, at=1:60, labels=base7$year)

# Agregar los puntas en la figura 
points(base7$time,base7$ipo_alim,
       col="red",
       pch=20)

# etiqueta del cambio 
abline(v=30.5,lty=2)


#-----------------------------------------------------------------------------------------------------------------------
#6. ANÁLISIS PRELIMINAR
#-----------------------------------------------------------------------------------------------------------------------


# A preliminary OLS regression

model2_ols <- lm(ipo_alim ~ time + level + trend, data=base7)
summary(model2_ols)

#El intercepto es de 106.47195 y es estadísticamente significativo

#/ El coeficiente del tiempo indica que la tendencia existente era un
#un incremento sobre el tiempo de 0.0659 en el  Indíce de población ocupada de la 
#industria alimentaria, y es estadisticamente significativo

#El nivel muestra una caída de -0.70652 en el indice de población ocupada, pero no
# es estadísticamente significativo.

#Por último la tendencia muestra un aumento de -0.07998 en el indice de población ocupada
#y si es estadísticamente significativo.

confint(model2_ols)

#-----------------------------------------------------------------------------------------------------------------------
# 7. AUTOCORRELACIÓN
#-----------------------------------------------------------------------------------------------------------------------

# Puerba Durbin-watson, 12 time periods
dwt(model2_ols,max.lag=12,alternative="two.sided")

# Grafica de los residuos de la regresión OLS para verificar errores correlacionados en serie
plot(base7$time,
     residuals(model2_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)
#Se detecta autocorrelación

# Gráfico ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model2_ols))
acf(residuals(model2_ols),type='partial')
# Note decay in ACF, significant spike at 4 in PACF, model p=4



#-----------------------------------------------------------------------------------------------------------------------
# 8. CORRER MODELO FINAL
#-----------------------------------------------------------------------------------------------------------------------

library(nlme)
# Ajustar el modelo de regresión GLS 
model2_p5 <- gls(ipo_alim ~ time + level + trend,
                 data=base7,
                 correlation=corARMA(p=5,form=~time),
                 method="ML")
summary(model2_p5)



#-----------------------------------------------------------------------------------------------------------------------
# Diagnostic tests
#-----------------------------------------------------------------------------------------------------------------------


# Likelihood-ratio tests to check AR process

#Modelo p=3, q=2
model2_p3q2 <- update(model2_p5,correlation=corARMA(p=3,q=1,form=~time))
anova(model2_p5,model2_p3q2)
summary(model2_p3q2)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo
n<-6e-04
formatC(n, format = "f", digits = 5)

#Modelo p=4, q=1
model2_p4q1 <- update(model2_p5,correlation=corARMA(p=4,q=1,form=~time))
anova(model2_p5,model2_p4q1)
summary(model2_p4q1)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo

#Modelo p=5, q=1
model2_p5q1 <- update(model2_p5,correlation=corARMA(p=5,q=1,form=~time))
anova(model2_p5,model2_p5q1)
summary(model2_p5q1)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo

#Modelo p=4, q=2
model2_p4q2 <- update(model2_p5,correlation=corARMA(p=4,q=2,form=~time))
anova(model2_p5,model2_p4q2)
summary(model2_p4q2)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo

#Modelo p=5, q=4
model2_p5q4 <- update(model2_p5,correlation=corARMA(p=5,q=4,form=~time))
anova(model2_p5,model2_p5q4)
summary(model2_p5q4)
# Se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo

#Modelo p=7, q=1
model2_p7q1 <- update(model2_p5,correlation=corARMA(p=7,q=1,form=~time))
anova(model2_p5,model2_p7q1)
summary(model2_p7q1)
# Se descarte denido a que no se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo

##########################################

model2_p3 <- update(model2_p5,correlation=corARMA(p=3,form=~time))
anova(model2_p5,model2_p3)
summary(model2_p3)

#Modelo p=7, q=1
model2_p3q1 <- update(model2_p5,correlation=corARMA(p=3,q=1,form=~time))
anova(model2_p5,model2_p3q1)
summary(model2_p3q1)
# Se descarte denido a que no se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo




###################3


#posible solución
#Modelo p=5, q=4
model2_p5q4 <- update(model2_p5,correlation=corARMA(p=5,q=4,form=~time))
anova(model2_p5,model2_p5q4)
summary(model2_p5q4)
# Se rechaza de hipotesis nula de que estos modelos 
#sean iguales
#El nivel no es estadisticamente significativo


#-----------------------------------------------------------------------------------------------------------------------

#MODELO FINAL
#-----------------------------------------------------------------------------------------------------------------------

##Modelo p=5, q=4
model2_p5q4 <- update(model2_p5,correlation=corARMA(p=5,q=4,form=~time))
anova(model2_p5,model2_p5q4)
summary(model2_p5q4)
#todos los coeficientes son significativos, con excepción del nivel
#Se tiene un intercepto en 
#Se rechaza la hipótesis nula de que estos modelos son iguales
#Se elige puesto que se rechaza la hipotesis nula


#-----------------------------------------------------------------------------------------------------------------------

# Residual plot
# Null Hypo: Null Hypo: los residuos de un modelo especificado correctamente 
#se distribuyen de forma independiente; los residuos son ruido blanco

par(mfrow=c(1,1))
qqPlot(residuals(model2_p5q4))
shapiro.test(model2_p5q4$residuals)
#Los residuales se comportan de manera normal, por lo que no se rechaza la hipotesis nula.


#-----------------------------------------------------------------------------------------------------------------------

# 9. GRAFICAR LOS RESULTADOS
#-----------------------------------------------------------------------------------------------------------------------

# Produce the plot, first plotting the raw data points
plot(base7$time,base7$ipo_alim,
     ylim=c(100,117),
     ylab="Indice de población ocupada",
     xlab="Periodo",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=base7$year)

# Add line indicating weather pattern change
abline(v=30.5,lty="dotted")

# Plot the first line segment
lines(base7$time[1:30], fitted(model2_p5q4)[1:30], col="red",lwd=2)
# Plot the second line segment
lines(base7$time[31:60], fitted(model2_p5q4)[31:60], col="red",lwd=2)

# And the counterfactual
segments(1,
         model2_p5q4$coef[1]+model2_p5q4$coef[2],
         60,
         model2_p5q4$coef[1]+model2_p5q4$coef[2]*60,
         lty=2,
         lwd=2,
         col='red')


#-----------------------------------------------------------------------------------------------------------------------

# 10. PREDECIR CAMBIOS RELATIVOS Y ABSOLUTOS
#-----------------------------------------------------------------------------------------------------------------------


# Predicted value at 6 months after the weather change
pred <- fitted(model2_p5q4)[36]

# Then estimate the counterfactual at the same time point
cfac <- model2_p5q4$coef[1] + model2_p5q4$coef[2]*36

# Absolute change at  6 months
pred - cfac
# Relative change at 6 months
(pred - cfac) / cfac



#-----------------------------------------------------------------------------------------------------------------------

# 10. PREDECIR CAMBIOS RELATIVOS Y ABSOLUTOS
#-----------------------------------------------------------------------------------------------------------------------


# Predicted value at 6 months after the weather change
pred <- fitted(model2_p5q4)[36]

# Then estimate the counterfactual at the same time point
cfac <- model2_p5q4$coef[1] + model2_p5q4$coef[2]*36

# Absolute change at  6 months
pred - cfac
# Relative change at 6 months
(pred - cfac) / cfac

n<-100*(-0.001256732 )
print(n)
#############################el modelo pasa las pruebas######################################################

#=======================================================================================================================
#=======================================================================================================================
#=======================================================================================================================

#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------


#MODELO 3: Ingresos por susministro de bienes y servicios nacional: Industria alimentaria
#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------
#Gráfica inicial
#-----------------------------------------------------------------------------------------------------------------------


# 1) Ingresos por susministro de bienes y servicios nacional: Industria alimentaria
summary(base3$ingna_alim)
# Gráfica: variable de resultado versus el tiempo
plot(base3$time,base3$ingna_alim,
     ylab="IMAI",
     ylim=c(17338275,27731367),
     xlab="Tiempo",
     type="l",
     col="red",
     xaxt="n")

# Añadir etiquetas de año en el eje-x(x-axis)
axis(1, at=1:60, labels=base3$year)

# Agregar los puntas en la figura 
points(base3$time,base3$ingna_alim,
       col="red",
       pch=20)

# etiqueta del cambio 
abline(v=30.5,lty=2)


#-----------------------------------------------------------------------------------------------------------------------
#6. ANÁLISIS PRELIMINAR
#-----------------------------------------------------------------------------------------------------------------------


# A preliminary OLS regression

model3_ols <- lm(ingna_alim ~ time + level + trend, data=base3)
summary(model3_ols)


confint(model3_ols)

#-----------------------------------------------------------------------------------------------------------------------
# 7. AUTOCORRELACIÓN
#-----------------------------------------------------------------------------------------------------------------------

# Puerba Durbin-watson, 12 time periods
dwt(model3_ols,max.lag=12,alternative="two.sided")

# Grafica de los residuos de la regresión OLS para verificar errores correlacionados en serie
plot(base3$time,
     residuals(model3_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Gráfico ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model3_ols))
acf(residuals(model3_ols),type='partial')
# Note decay in ACF, significant spike at 10 in PACF, model p=10



#-----------------------------------------------------------------------------------------------------------------------
# 8. CORRER MODELO FINAL
#-----------------------------------------------------------------------------------------------------------------------

library(nlme)
# Ajustar el modelo de regresión GLS 
model3_p6 <- gls(ingna_alim ~ time + level + trend,
                 data=base3,
                 correlation=corARMA(p=6,form=~time),
                 method="ML")
summary(model3_p6)



#-----------------------------------------------------------------------------------------------------------------------
# Diagnostic tests
#-----------------------------------------------------------------------------------------------------------------------


# Likelihood-ratio tests to check AR process

#Modelo p=10, q=1
model3_p10q1 <- update(model3_p6,correlation=corARMA(p=10,q=1,form=~time))
anova(model3_p6,model3_p10q1)
summary(model3_p10q1)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa

#Modelo p=6, q=5
model3_p6q5 <- update(model3_p6,correlation=corARMA(p=6,q=5,form=~time))
anova(model3_p6,model3_p6q5)
summary(model3_p6q5)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa

#Modelo p=6, q=3
model3_p11 <- update(model3_p6,correlation=corARMA(p=11,form=~time))
anova(model3_p6,model3_p11)
summary(model3_p11)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa


#Modelo p=5
model3_p5q2 <- update(model3_p6,correlation=corARMA(p=5,form=~time))
anova(model3_p6,model3_p5)
summary(model3_p5)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa

#Modelo p=9
model3_p9 <- update(model3_p6,correlation=corARMA(p=9,form=~time))
anova(model3_p6,model3_p9)
summary(model3_p9)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa

#Modelo p=2, q=2
model3_pq2 <- update(model3_p6,correlation=corARMA(p=2,q=2,form=~time))
anova(model3_p6,model3_p2q2)
summary(model3_p2q2)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa
#Modelo p=2
model3_p8q1 <- update(model3_p6,correlation=corARMA(p=8,q=1,form=~time))
anova(model3_p6,model3_p10q1)
summary(model3_p10q1)
# Se descarta puesto que no se rechaza de hipotesis nula de que estos modelos sean iguales
#La tendencia no es estadísticamente significativa

#-----------------------------------------------------------------------------------------------------------------------

# Residual plot
# Null Hypo: Null Hypo: los residuos de un modelo especificado correctamente 
#se distribuyen de forma independiente; los residuos son ruido blanco

par(mfrow=c(1,1))
qqPlot(residuals(model3_p6))
shapiro.test(model3_p6$residuals)

#Los residuales se comportan de manera normal, por lo que no se rechaza la hipotesis nula.
#Los residuales se comportan de manera normal, por lo que no se rechaza la hipotesis nula.


#-----------------------------------------------------------------------------------------------------------------------

# 9. GRAFICAR LOS RESULTADOS
#-----------------------------------------------------------------------------------------------------------------------

# Produce the plot, first plotting the raw data points
plot(base3$time,base3$ingna_alim,
     ylim=c(17338275,27731367),
     ylab="Ingresos por suministros de bienes y servicios",
     xlab="tiempo",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=base3$year)

# Add line indicating weather pattern change
abline(v=30.5,lty="dotted")

# Plot the first line segment
lines(base3$time[1:30], fitted(model3_p6)[1:30], col="red",lwd=2)
# Plot the second line segment
lines(base3$time[31:60], fitted(model3_p6)[31:60], col="red",lwd=2)

# And the counterfactual
segments(1,
         model3_p6$coef[1]+model3_p6$coef[2],
         60,
         model3_p6$coef[1]+model3_p6$coef[2]*60,
         lty=2,
         lwd=2,
         col='red')


#-----------------------------------------------------------------------------------------------------------------------

# 10. PREDECIR CAMBIOS RELATIVOS Y ABSOLUTOS
#-----------------------------------------------------------------------------------------------------------------------


# Predicted value at 6 months after the weather change
pred <- fitted(model3_p6)[36]

# Then estimate the counterfactual at the same time point
cfac <- model3_p6$coef[1] + model3_p6$coef[2]*36

# Absolute change at  6 months
pred - cfac
# Relative change at 6 months
(pred - cfac) / cfac

n<-(100*((pred - cfac) / cfac))
n

#=======================================================================================================================
#=======================================================================================================================
#========================================================================================================================

#-----------------------------------------------------------------------------------------------------------------------


#MODELO 4: Indice de actividad industrial : industria alimentaria
#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------
#Gráfica inicial
#-----------------------------------------------------------------------------------------------------------------------


# 1) Ingresos por susministro de bienes y servicios nacional: Industria alimentaria
summary(base6$iai_alim)
# Gráfica: variable de resultado versus el tiempo
plot(base6$time,base6$iai_alim,
     ylab="IMAI",
     ylim=c(105,120),
     xlab="Tiempo",
     type="l",
     col="red",
     xaxt="n")

# Añadir etiquetas de año en el eje-x(x-axis)
axis(1, at=1:60, labels=base6$year)

# Agregar los puntas en la figura 
points(base6$time,base6$iai_alim,
       col="red",
       pch=20)

# etiqueta del cambio 
abline(v=30.5,lty=2)


#-----------------------------------------------------------------------------------------------------------------------
#6. ANÁLISIS PRELIMINAR
#-----------------------------------------------------------------------------------------------------------------------


# A preliminary OLS regression

model4_ols <- lm(iai_alim ~ time + level + trend, data=base6)
summary(model4_ols)


confint(model4_ols)

#-----------------------------------------------------------------------------------------------------------------------
# 7. AUTOCORRELACIÓN
#-----------------------------------------------------------------------------------------------------------------------

# Puerba Durbin-watson, 12 time periods
dwt(model4_ols,max.lag=12,alternative="two.sided")

# Grafica de los residuos de la regresión OLS para verificar errores correlacionados en serie
plot(base6$time,
     residuals(model4_ols),
     type='o',
     pch=16,
     xlab='Time',
     ylab='OLS Residuals',
     col="red")
abline(h=0,lty=2)

# Gráfico ACF and PACF
# Set plotting to two records on one page
par(mfrow=c(1,2))

# Produce plots
acf(residuals(model4_ols))
acf(residuals(model4_ols),type='partial')
# Note decay in ACF, significant spike at 10 in PACF, model p=10



#-----------------------------------------------------------------------------------------------------------------------
# 8. CORRER MODELO FINAL
#-----------------------------------------------------------------------------------------------------------------------

library(nlme)
# Ajustar el modelo de regresión GLS 
model4_p6 <- gls(iai_alim ~ time + level + trend,
                 data=base6,
                 correlation=corARMA(p=6,form=~time),
                 method="ML")
summary(model4_p6)



#-----------------------------------------------------------------------------------------------------------------------
# Diagnostic tests
#-----------------------------------------------------------------------------------------------------------------------

#Modelo p=7
model3_p7 <- update(model3_p6,correlation=corARMA(p=7,form=~time))
anova(model3_p6,model3_p7)
summary(model3_p7)
#Modelo p=7
model3_p2 <- update(model3_p6,correlation=corARMA(p=2,form=~time))
anova(model3_p6,model3_p2)
summary(model3_p2)

#Modelo p=7
model3_p7q2 <- update(model3_p6,correlation=corARMA(p=7,q=2,form=~time))
anova(model3_p6,model3_p7q1)
summary(model3_p7q1)

#Modelo p=10
model3_p11 <- update(model3_p6,correlation=corARMA(p=11,form=~time))
anova(model3_p6,model3_p1)
summary(model3_p11)

#-----------------------------------------------------------------------------------------------------------------------

# Residual plot
# Null Hypo: Null Hypo: los residuos de un modelo especificado correctamente 
#se distribuyen de forma independiente; los residuos son ruido blanco

par(mfrow=c(1,1))
qqPlot(residuals(model4_p6))
shapiro.test(model4_p6$residuals)

#Los residuales se comportan de manera normal, por lo que no se rechaza la hipotesis nula.
#Los residuales se comportan de manera normal, por lo que no se rechaza la hipotesis nula.


#-----------------------------------------------------------------------------------------------------------------------

# 9. GRAFICAR LOS RESULTADOS
#-----------------------------------------------------------------------------------------------------------------------

# Produce the plot, first plotting the raw data points
plot(base6$time,base6$iai_alim,
     ylim=c(100,127),
     ylab="Indice de actividad industrial",
     xlab="tiempo",
     pch=20,
     col="pink",
     xaxt="n")

# Add x axis with dates
axis(1, at=1:60, labels=base6$year)

# Add line indicating weather pattern change
abline(v=30.5,lty="dotted")

# Plot the first line segment
lines(base6$time[1:30], fitted(model4_p6)[1:30], col="red",lwd=2)
# Plot the second line segment
lines(base6$time[31:60], fitted(model4_p6)[31:60], col="red",lwd=2)

# And the counterfactual
segments(1,
         model4_p6$coef[1]+model4_p6$coef[2],
         60,
         model4_p6$coef[1]+model4_p6$coef[2]*60,
         lty=2,
         lwd=2,
         col='red')


#-----------------------------------------------------------------------------------------------------------------------

# 10. PREDECIR CAMBIOS RELATIVOS Y ABSOLUTOS
#-----------------------------------------------------------------------------------------------------------------------


# Predicted value at 6 months after the weather change
pred <- fitted(model4_p6)[36]

# Then estimate the counterfactual at the same time point
cfac <- model4_p6$coef[1] + model4_p6$coef[2]*36

# Absolute change at  6 months
pred - cfac
# Relative change at 6 months
(pred - cfac) / cfac

n<- 100*((pred-cfac)/cfac)
print(n)

