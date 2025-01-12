#################### Series de Tiempo Multivariantes
####### Caso de Estudio: Datos Datos comodities: Oro y Plata
####### PASOS:
#1. An�lisis exploratorios de los datos.
#2. Dividir la serie en conjuntos de entrenamiento y prueba.
#3. Prueba de estacionariedad.
#4. Transformar la serie de entrenamiento si es necesario. 
#5. Construir un modelo VAR sobre las series transformadas.
#6. Causalidad de Granger.
#7. Diagn�stico del modelo. (Analizamos los residuos del modelo)
#8. Realizar pron�sticos utilizando el modelo finalmente elegido.
#9. Transformaci�n inversa del pron�stico a la escala original.
#10. Realizar una evaluaci�n del pron�stico.


####### Cargando los datos reales "Datos comodities: Oro y Plata"
# Contiene 4 variables 
library(readr)
setwd("~/Github projects/series-temporales-multivariantes/2. Casos de estudio en R/D. Comodities Oro y Plata")
dat= read_csv("comodity_price.csv")
View(dat)

plot(dat$gold)

#Le aplicaremos logaritmo a las series ya que vemos que hay un crecimiento exponencial
# esto lo que har� es ayudarnos con el tema de la varianza (Homoscedasticidad)
# m�s no con el tema de la media (estacionariedad) ya que se conitunar� viendo una tendencia

# Crear las series de tipo ts
gold <- ts(log(dat$gold),start=c(1993,11),frequency=12)
silver <- ts(log(dat$silver),start=c(1993,11),frequency=12)
plat <- ts(log(dat$plat),start=c(1993,11),frequency=12)
pall <- ts(log(dat$pall),start=c(1993,11),frequency=12)

# Plot Gold y Silver
par(mfrow=c(1,1))
plot.ts(cbind(gold,silver), plot.type="single", ylab="",col = 4:3)
legend("topleft",legend=c("gold","silver"),col=4:3,lty=1,bty='n')


# Una sola serie con las dos
data <- ts.union(gold,silver)

# Prueba estacionariedad
apply(data, 2, adf.test) #2 para especificar que lo queremos aplicar por columnas

# No son estacionarias

# Diferenciamos
library(MTS)
stnry = diffM(data)

# Volviendo a hacer el test:
apply(stnry, 2, adf.test)

# Ya son estacionarias con 1 sola diferenciaci�n

plot.ts(stnry)


# Identificaci�n del orden del modelo
library(vars)
VARselect(stnry, type = "none", lag.max = 10)


# Creando el modelo
var.a <- vars::VAR(stnry,
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "none")

summary(var.a)

## Diagnosis del modelo (Portmanteau test para objetos var)
bv.serial=serial.test(var.a)
bv.serial
plot(bv.serial, names = "gold")
plot(bv.serial, names = "silver")


######## Forecasting usando el modelo VAR (Hallando los pron�sticos)

fcast = predict(var.a, n.ahead = 30)
plot(fcast)

######### Forecast solo para gold
gold = fcast$fcst[1]; gold 

# Extrayendo la columna de pron�sticos
x = gold$gold[,1]; x

# Invirtiendo la diferenciaci�n
tail(data)

x = cumsum(x) + 7.424118

plot.ts(x)

# Combinando los datos reales y la predicci�n en una sola serie de tiempo
goldinv =ts(c(data[,1], x),
           start = c(1993,11), frequency = 12)


# Dibujando todo 
plot(goldinv)
plot.ts(goldinv[200:261])


# Plot avanzado con separaci�n visual entre lo real y lo pronosticado
library(lattice)
library(grid)
library(zoo)

# Objeto zoo
xx = zoo(goldinv[200:261])


# En el par�metro grid.clip ponemos la cantidad de observaciones que son reales dentro de las 
# que hemos elegido. Hemos cogido 62 de las que 30 son pron�sticos, as� que grid.clip ser�a 32-1

xyplot(xx, grid=TRUE, panel = function(xx, y, ...){
  
  panel.xyplot(xx, y, col="red", ...)
  
  grid.clip(unit(31, "native"), just=c("right")) 
  
  panel.xyplot(xx, y, col="green", ...) })

# Como vemos si nos vamos demasiado lejos en el futuro se aplana la predicci�n


######### Forecast silver
fcast = predict(var.a, n.ahead = 30)

# Solo para silver 
silver = fcast$fcst[2]; silver 

# Extrayendo la columna de pron�sticos
y = silver$silver[,1]; y

# Invirtiendo la diferenciaci�n
tail(data)

y = cumsum(y) + 3.411313

plot.ts(y)

# Combinando los datos reales y la predicci�n en una sola serie de tiempo
silverinv =ts(c(data[,2], y),
            start = c(1993,11), frequency = 12)


# Dibujando todo 
plot(silverinv)
plot.ts(silverinv[200:261])


# Plot avanzado con separaci�n visual entre lo real y lo pronosticado
library(lattice)
library(grid)
library(zoo)

# Objeto zoo
xx = zoo(silverinv[200:261])


# En el par�metro grid.clip ponemos la cantidad de observaciones que son reales dentro de las 
# que hemos elegido. Hemos cogido 62 de las que 30 son pron�sticos, as� que grid.clip ser�a 32-1

xyplot(xx, grid=TRUE, panel = function(xx, y, ...){
  
  panel.xyplot(xx, y, col="red", ...)
  
  grid.clip(unit(31, "native"), just=c("right")) 
  
  panel.xyplot(xx, y, col="green", ...) })

# Como vemos si nos vamos demasiado lejos en el futuro se aplana la predicci�n (este modelo tiene lag de orden 1 solamente
# es un VAR(1) por lo cual no puede pronosticar 30 meses a futuro.




