#################### Series de Tiempo Multivariantes
####### Caso de Estudio: Datos macroecon�micos de Estados Unidos
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


# Antes vamos a ver un ejemplo de como generar un dataframe aleatorio
x = rnorm(100, 1)
y = rnorm(100, 30)
z = rnorm(100, 500)
xyz = data.frame(x, y, z)
class(xyz)

# Convertirlo a mts
mymts = ts(xyz,
           
           frequency = 12,
           
           start = c(1940, 4))

# Herramientas exploratorias b�sicas
plot(mymts)
head(mymts)
class(mymts)


####### Cargando los datos reales "Datos macroecon�micos de Estados Unidos"
# Contiene dos variables de series de tiempo sobre la producci�n (PIB - en ingl�s GDP) y el desempleo en los Estados Unidos.
library(readr)
dat = read_csv("blanchQua.csv")
View(dat)

class(dat)
head(dat)

####### Principales paquetes en R que tienen el modelo VAR - problema: en ambos la funci�n se llama igual pero los par�metros son diferentes
install.packages("vars")
install.packages("MTS")
??vars
??MTS

# Librer�a para el test ADF de estacionariedad
library(tseries)


####### An�lisis exploratorio
# Convertir a objeto ts las dos series
# Opci�n 1
dat_ts = ts(dat, start = c(1948,2), frequency = 4) 
# Aqu� le estamos indicando que la frecuencia es trimestral ya que 
# hay 4 trimestres en el a�o y que comienza desde el 2 trimestre

# Opci�n 2
gdp <- ts(dat$GDP, start = c(1948, 2), freq = 4)
une <- ts(dat$U, start = c(1948, 2), freq = 4)


# Gr�fico con plot:
dat.bv <- cbind(gdp, une)
plot(dat.bv)

# Con autoplot:
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)

autoplot(dat.bv )


## Dividir la serie en conjunto de entrenamiento y de prueba
library(dplyr)

n_obs=10
end=dim(dat.bv)[1]
X_train = dat.bv[1:(end-n_obs),]
X_test = dat.bv[(end-n_obs+1):end,]
dim(X_test)


####### Prueba de estacionariedad
apply(X_train, 2, adf.test) #2 para especificar que lo queremos aplicar por columnas

# Todos los p-valores son < 0.05, no hay que diferenciar las series


####### VAR modeling
# Identificaci�n del orden del modelo
library(vars)

VARselect(X_train, type = "none", lag.max = 10)


# Creando el modelo
var.a <- vars::VAR(X_train,  #vars:: es para indicarle de que librer�a tomar� la funci�n VAR
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "const")

summary(var.a)



####### Causalidad de Granger 
causality(var.a, cause = c("gdp"))
causality(var.a, cause = c("une"))



####### Diagnosis del modelo (Portmanteau test para objetos var)

bv.serial= serial.test(var.a)
bv.serial

# Deseamos obtener un p-valor > 0.05, en este caso lo obtenemos.
# Posibles soluciones si es < 0.05:
# a) Cambiar el orden del modelo.
# b) Cambiar el tipo de modelo.
# c) A�adir otro paso de diferenciaci�n o transformar con logaritmos.

plot(bv.serial, names = "gdp")
plot(bv.serial, names = "une")

####### Forecasting usando el modelo VAR (Hallando los pron�sticos)
predictions <- predict(var.a, n.ahead = 10, ci = 0.95)
plot(predictions, names = "gdp")

predictions <- predict(var.a, n.ahead = 10, ci = 0.95)
plot(predictions, names = "une")

# Otro gr�fico 
fanchart(predictions, names = "gdp")
fanchart(predictions, names = "une")

#### Evaluando el modelo
pred=predictions$fcst
rmse=sqrt(mean((X_test[,1]-pred$gdp)^2))
cat('RMSE gdp: ', rmse)
rmse=sqrt(mean((X_test[,2]-pred$une)^2))
cat('RMSE une: ', rmse)


###############################    Entrenando el modelo con todos los datos   ##################################33

####### VAR modeling
# Identificaci�n del orden del modelo
VARselect(dat.bv, type = "none", lag.max = 10)


# Creando el modelo
var.a <- vars::VAR(dat.bv,
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "const")

summary(var.a)



####### Causalidad de Granger 
causality(var.a, cause = c("gdp"))
causality(var.a, cause = c("une"))



####### Diagnosis del modelo (Portmanteau test para objetos var)

bv.serial= serial.test(var.a)
bv.serial

# Deseamos obtener un p-valor > 0.05, en este caso lo obtenemos.
# Posibles soluciones si es < 0.05:
# a) Cambiar el orden del modelo.
# b) Cambiar el tipo de modelo.
# c) A�adir otro paso de diferenciaci�n o transformar con logaritmos.

plot(bv.serial, names = "gdp")
plot(bv.serial, names = "une")

####### Forecasting usando el modelo VAR (Hallando los pron�sticos)
predictions <- predict(var.a, n.ahead = 15, ci = 0.95)
plot(predictions, names = "gdp")

predictions <- predict(var.a, n.ahead = 15, ci = 0.95)
plot(predictions, names = "une")

# Otro gr�fico 
fanchart(predictions, names = "gdp")
fanchart(predictions, names = "une")

### M�s en el futuro "n.ahead = 50" (perdemos precisi�n)
predictions <- predict(var.a, n.ahead = 50, ci = 0.95)
plot(predictions, names = "gdp")

predictions <- predict(var.a, n.ahead = 50, ci = 0.95)
plot(predictions, names = "une")

# Otro gr�fico 
fanchart(predictions, names = "gdp")
fanchart(predictions, names = "une")

### Tama�o de los intervalos de confianza
diff_IC_gdp=predictions$fcst$gdp[,3]-predictions$fcst$gdp[,2]
plot(diff_IC_gdp, main="Longitud de los IC vs cantidad de pron�sticos a futuro - GDP", xlab='Cantidad de datos pronosticados en el futuro', ylab='Longitud del IC')

diff_IC_une=predictions$fcst$gdp[,3]-predictions$fcst$une[,2]
plot(diff_IC_une, main="Longitud de los IC vs cantidad de pron�sticos a futuro - UNE", xlab='Cantidad de datos pronosticados en el futuro', ylab='Longitud del IC')


