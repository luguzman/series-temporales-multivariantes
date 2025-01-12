# Tarea 1: An�lisis completo del dataset de alta dimensi�n.

#1.- Carga los datos. 
library(readr)
setwd("~/Github projects/series-temporales-multivariantes/2. Casos de estudio en R/E. Datos macroecon�micos de mayor dimensi�n")
dat= read_csv("Mac_Ric.csv")
View(dat)

# Guarda en variables separadas y convierte en objeto ts las siguientes 7 variables 
# end�genas: LREERS, RIRR, LRGDPPCR, LPR2COMM5, OPENY, FBYA, NFAOFPY. 
LREERS = ts(dat$LREERS, start = c(1970,1), frequency = 4)
RIRR = ts(dat$RIRR, start = c(1970,1), frequency = 4)
LRGDPPCR = ts(dat$LRGDPPCR, start = c(1970,1), frequency = 4)
LPR2COMM5 = ts(dat$LPR2COMM5, start = c(1970,1), frequency = 4)
OPENY = ts(dat$OPENY, start = c(1970,1), frequency = 4)
FBYA = ts(dat$FBYA, start = c(1970,1), frequency = 4)
NFAOFPY = ts(dat$NFAOFPY, start = c(1970,1), frequency = 4)

# Guarda la uni�n de todas ellas en un solo objeto (que ser� de tipo mts) 
data = ts.union(LREERS, RIRR, LRGDPPCR, LPR2COMM5, OPENY, FBYA, NFAOFPY)

# Haz un plot de eso. Copia y pega la imagen, que debe ser en una sola imagen 
# un plot de todas esas series.
plot.ts(data, plot.type = "single", ylab="", col = 1:7)
#legend("topleft",legend=c("LREERS", "RIRR", "LRGDPPCR", "LPR2COMM5", "OPENY", 
#                      "FBYA", "NFAOFPY"),col=1:7,lty=1,bty='n')

## Dividir la serie en conjunto de entrenamiento y de prueba
library(dplyr)
n_obs=30
end=dim(data)[1]
X_train = data[1:(end-n_obs),]
X_test = data[(end-n_obs+1):end,]
dim(X_test)

# 2.- Haz la prueba de estacionariedad a esa serie multivariante. 
library(tseries)
apply(X_train, 2, adf.test)

#�Son estacionarias las series univariantes? �Qu� habr�a que hacer?
# Ninuguna es estacionaria por lo que diferenciare las series.

# 3.- Diferencia la serie y repite el test de estacionariedad. 
library(MTS)
stnry = diffM(X_train)

#�Son estacionarias las series univariantes? 
apply(stnry, 2, adf.test)

# Copia y pega un plot de las series transformadas 
# (en una sola imagen que salgan los 7 gr�ficos).
plot.ts(stnry, plot.type = "single", ylab="", col = 1:7)

#4.- Crea un modelo VAR (pon lag.max = 10) y di de qu� orden es el modelo obtenido.
# Identificaci�n del orden del modelo
library(vars)
VARselect(stnry, type = "none", lag.max = 10)
# El orden seg�n el AIC es de 10

# Creando el modelo
var.a <- vars::VAR(stnry,
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "none")

summary(var.a)

#5.- Realiza el test Portmanteau de diagnosis del modelo y explica el resultado 
# (seg�n el p.valor obtenido).
## Diagnosis del modelo (Portmanteau test para objetos var)
bv.serial=serial.test(var.a)
bv.serial
# Dado que el p-value es de 0.7927 podemos decir que tenemos una buena diagnosis

#6.- Calcula los pron�sticos en un intervalo de 30 per�odos hacia el futuro. 
# Guarda solamente los de la primera variable LREERS, invierte la transformaci�n 
# y devuelve un plot sin ampliar de los datos originales de esa variable y de la 
# predicci�n.
fcast = predict(var.a, n.ahead = 30)
plot(fcast, names = "LREERS")


######### Forecast solo para gold
LREERS = fcast$fcst[1]; LREERS 

# Extrayendo la columna de pron�sticos
x = LREERS$LREERS[,1]; x

# Invirtiendo la diferenciaci�n
tail(X_train)

x = cumsum(x) + 4.602934

plot.ts(x)

# Combinando los datos reales y la predicci�n en una sola serie de tiempo
LREERSinv =ts(c(X_train[,1], x),
            start = c(1970,1), frequency = 4)

# Dibujando todo 
plot(LREERSinv)

#7.- Ampl�a el gr�fico (escoge solamente los valores del 100:159) y haz el mismo 
# gr�fico pero con colores que separen los pron�sticos de los datos del pasado.
plot.ts(LREERSinv[100:159])

# Plot avanzado con separaci�n visual entre lo real y lo pronosticado
library(lattice)
library(grid)
library(zoo)

# Objeto zoo
xx = zoo(LREERSinv[100:159])


# En el par�metro grid.clip ponemos la cantidad de observaciones que son reales dentro de las 
# que hemos elegido. Hemos cogido 101 de las que 30 son pron�sticos, as� que grid.clip ser�a 71-1

xyplot(xx, grid=TRUE, panel = function(xx, y, ...){
  
  panel.xyplot(xx, y, col="red", ...)
  
  grid.clip(unit(68, "native"), just=c("right")) 
  
  panel.xyplot(xx, y, col="green", ...) })
