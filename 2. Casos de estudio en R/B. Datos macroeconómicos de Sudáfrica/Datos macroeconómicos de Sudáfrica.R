#################### Series de Tiempo Multivariantes
####### Caso de Estudio: Datos macroecon�micos de Sud�frica
####### PASOS:
#1. An�lisis exploratorios de los datos.
#2. Dividir la serie en conjuntos de entrenamiento y prueba.
#3. Prueba de estacionariedad.
#4. Transformar la serie de entrenamiento si es necesario. 
#5. Construir un modelo VAR sobre las series transformadas.
#6. Causalidad de Granger.
#7. Diagn�stico del modelo.
#8. Realizar pron�sticos utilizando el modelo finalmente elegido.
#9. Transformaci�n inversa del pron�stico a la escala original.
#10. Realizar una evaluaci�n del pron�stico.


####### Cargando los datos reales "Datos macroecon�micos de Sud�frica"
# Contiene tres variables macroecon�micas: GDP (PIB), INF (tasa de inflaci�n), INT (tasa de inter�s).
library(readr)
dat= read_csv("data_sa.csv")
class(dat)
head(dat)

# Librer�a para el test ADF de estacionariedad
library(tseries)


####### An�lisis exploratorio
# Convertir a objeto ts las dos series
gdp <- ts(dat$gdp, start = c(1981, 2), freq = 4) # freq = 4, dado que la frequencia es trimentral y c(1981,2) es para indicarle el a�o y el trimestre en el que comienzan los datos.
inf <- ts(dat$inf, start = c(1981, 2), freq = 4)
int <- ts(dat$int, start = c(1981, 2), freq = 4)

dat.mts=cbind(gdp, inf, int)

plot(dat.mts)

# Con autoplot:
library(ggplot2)
library(ggfortify)

autoplot(dat.mts)


## Dividir la serie en conjunto de entrenamiento y de prueba
library(dplyr)

n_obs=10
end=dim(dat.mts)[1]
X_train = dat.mts [1:(end-n_obs),]
X_test = dat.mts [(end-n_obs+1):end,]
dim(X_test)


####### Prueba de estacionariedad
apply(X_train, 2, adf.test) #2 para especificar que lo queremos aplicar por columnas

#######
#######
# Vamos a estudiar un modelo para ver la relaci�n entre la tasa de inflaci�n 
# y la tasa de inter�s INF e INT, as� que obviamos el hecho 
# de que GDP no sea estacionaria porque no la vamos a utilizar.
X_train_new <- X_train[,2:3]



####### VAR modeling
# Identificaci�n del orden del modelo
library(vars)
VARselect(X_train_new, type = "none", lag.max = 12)


# Creando el modelo
var.a <- vars::VAR(X_train_new,
                   
                   lag.max = 12,
                   
                   ic = "AIC",
                   
                   type = "const")

summary(var.a)


####### Diagnosis del modelo (Portmanteau test para objetos var)

bv.serial= serial.test(var.a)
bv.serial

# Deseamos obtener un p-valor > 0.05, en este caso lo obtenemos.
# Posibles soluciones si es < 0.05:
# a) Cambiar el orden del modelo.
# b) Cambiar el tipo de modelo.
# c) A�adir otro paso de diferenciaci�n o transformar con logaritmos.

plot(bv.serial, names = "int")
plot(bv.serial, names = "inf")

####### Forecasting usando el modelo VAR (Hallando los pron�sticos)
predictions <- predict(var.a, n.ahead = 10, ci = 0.90)
plot(predictions, names = "int")

predictions <- predict(var.a, n.ahead = 10, ci = 0.90)
plot(predictions, names = "inf")

# Otro gr�fico 
fanchart(predictions, names = "int")
fanchart(predictions, names = "inf")

#### Evaluando el modelo
pred=predictions$fcst
rmse=sqrt(mean((X_test[,2]-pred$inf)^2))
cat('RMSE inf: ', rmse)
rmse=sqrt(mean((X_test[,3]-pred$int)^2))
cat('RMSE int: ', rmse)


##### Con todos los datos
# Creando el modelo
var.a <- vars::VAR(dat.mts[,2:3],
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "const")

summary(var.a)


####### Diagnosis del modelo (Portmanteau test para objetos var)

bv.serial= serial.test(var.a)
bv.serial

# Deseamos obtener un p-valor > 0.05, en este caso lo obtenemos.
# Posibles soluciones si es < 0.05:
# a) Cambiar el orden del modelo.
# b) Cambiar el tipo de modelo.
# c) A�adir otro paso de diferenciaci�n o transformar con logaritmos.

plot(bv.serial, names = "int")
plot(bv.serial, names = "inf")

####### Forecasting usando el modelo VAR (Hallando los pron�sticos)
predictions <- predict(var.a, n.ahead = 10, ci = 0.95)
plot(predictions, names = "int")

predictions <- predict(var.a, n.ahead = 10, ci = 0.95)
plot(predictions, names = "inf")


####### M�s adelante en el futuro "n.ahead=50"
predictions <- predict(var.a, n.ahead = 50, ci = 0.95)
plot(predictions, names = "int")

predictions <- predict(var.a, n.ahead = 50, ci = 0.95)
plot(predictions, names = "inf")


### Tama�o de los intervalos de confianza
diff_IC_int=predictions$fcst$int[,3]-predictions$fcst$int[,2]
plot(diff_IC_int, main="Longitud de los IC vs cantidad de pron�sticos a futuro - INT", xlab='Cantidad de datos pronosticados en el futuro', ylab='Longitud del IC')

diff_IC_inf=predictions$fcst$inf[,3]-predictions$fcst$inf[,2]
plot(diff_IC_inf, main="Longitud de los IC vs cantidad de pron�sticos a futuro - INF", xlab='Cantidad de datos pronosticados en el futuro', ylab='Longitud del IC')


