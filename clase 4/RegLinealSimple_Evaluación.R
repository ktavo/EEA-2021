#CLase 4 EEA
#Regresión Lineal Simple: Evaluación y Diagnóstico


rm(list=ls())
gc()

#El objetivo es crear un modelo lineal simple para explicar el sueldo neto de los Data Analysts, Data Scientists y Data Engineers en Argentina.
#salarioNeto=β0+β1X+ϵ 

# Carga de librerías
library(tidyverse)
library(tidymodels)

# Cargamos el dataset limpio
encuesta_sueldos = read_csv("encuesta_sueldos_sysarmy_limpia.csv")
str(encuesta_sueldos)

# Modelo Edad
modelo_edad = lm(formula = salario_neto ~ edad, data = encuesta_sueldos)
# Modelo experiencia
modelo_experiencia = lm(formula = salario_neto ~ anos_de_experiencia, data = encuesta_sueldos)
# Modelo empresa
modelo_empresa = lm(formula = salario_neto ~ anos_en_la_empresa_actual, data = encuesta_sueldos)

#En el notebook previo sólo habíamos interpretado el valor de los parámetros estimados. 
#Ahora buscaremos responder preguntas tales como:
#¿La relación entre la variable dependiente e independiente es estadísticamente significativa?
#¿Qué proporción de la variabilidad logra explicar el modelo? ¿Cómo decidir que modelo explica mejor el fenómeno?
#¿El modelo cumple con los supuestos del modelo lineal?

#Tidymodels : Broom
#tidy(): resume información sobre los componentes del modelo
#glance(): reporta información sobre todo el modelo
#augment(): agrega información de las observaciones según el modelo al dataset

resumen_modelo_edad = summary(modelo_edad)
resumen_modelo_edad

#Inferencia de β1 (test de significatividad individual)

#En la inferencia de β1 nos interesa responder si la relación entre la variable independiente y la variable a 
#explicar es estadísticamente significativa. Nuestro test de hipótesis es:
# H0:β1=0
# H1:β1≠0
#El valor de t value nos indica el valor del estadístico T para este test.
#El valor de Pr(>|t|) nos indica el p-valor para dicho test, acompañado de una ayuda visual sobre los niveles de 
#significancia.
#Como el p-valor es extremadamente pequeño concluimos que se rechaza la hipótesis nula, es decir, β1 (parámetro poblacional) 
#es distinto de cero.
#Este test también se conoce como test de significatividad individual del parámetro.

tidy(modelo_edad)

#Ahora podemos calcular el ECM para todos los modelos del dataframe models. Para eso utilizamos el paquete purrr, 
#para ejecutar varias veces la misma función sobre varios elementos.







