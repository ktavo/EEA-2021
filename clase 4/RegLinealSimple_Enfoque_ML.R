#Regresión Lineal Simple: Enfoque Machine Learning

rm(list=ls())
gc()

library(tidyverse)
library(modelr)
library(plotly)
options(na.action = na.warn)

#Datos sim1

ggplot(sim1, aes(x, y)) + 
  geom_point() +
  theme_bw()

#Para empezar, generemos aleatoriamente varios modelos lineales para ver qué forma tienen. Para eso, podemos usar
#geom_abline () que toma una pendiente e intercepto como parámetros.

models <- tibble(
  a1 = runif(250, -20, 40), # Función para tomar valores al azar de una distribución uniforme
  a2 = runif(250, -5, 5) # Función para tomar valores al azar de una distribución uniforme
)

#A simple vista podemos apreciar que algunos modelos son mejores que otros. Pero necesitamos una forma de cuantificar 
#cuales son los mejores modelos.

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() +
  theme_bw()

#Buscaremos el modelo que disminuya las distancias con los puntos del dataset
dist1 <- sim1 %>% 
  mutate(
    dodge = rep(c(-1, 0, 1) / 20, 10),
    x1 = x + dodge, # para que se vean mejor las distancias, corremos un poquito cada punto sobre el eje x
    pred = 7 + x1 * 1.5
  )

ggplot(dist1, aes(x1, y)) + 
  geom_abline(intercept = 7, slope = 1.5, colour = "grey40") +
  geom_point(colour = "grey40") +
  geom_linerange(aes(ymin = y, ymax = pred), colour = "#3366FF") +
  theme_bw()

#Ejercicio 1
#Crear una función que reciba un vector con los parametros del modelo, y el set de datos, y genere la predicción:

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

#Ejercicio 2
#Necesitamos una función, que reciba un vector con los parametros del modelo y el set de datos, que calcule el promedio
#de los errores cuadráticos (ECM)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(diff^2)
}
measure_distance(c(7, 1.5), sim1)











