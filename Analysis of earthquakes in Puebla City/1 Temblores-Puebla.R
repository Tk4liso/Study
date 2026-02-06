#Análisis de temblores/terremotos en la ciudad de Puebla, México.
#Por: Taisen Romero Bañuelos.


dataSSN <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Mis vainas\\Datos\\1 SSNMX_catalogo_PUE.csv", header=TRUE, skip=4, sep=",", fill=TRUE)
dataSSN <- dataSSN[1:(nrow(dataSSN) - 7), ]

dataSSN$Magnitud[dataSSN$Magnitud == "no calculable"] <- NA
dataSSN$Magnitud <- as.numeric(dataSSN$Magnitud)


# ==== Análisis exploratorio de datos (EDA) ====

library(naniar)
library(tidyverse)
library(ggplot2)
library(naniar)
library(corrplot)

head(dataSSN)
str(dataSSN)
summary(dataSSN)

colSums(is.na(dataSSN))
sum(is.na(dataSSN))
vis_miss(dataSSN)


# Hay 104 valores faltanes en magnitud, esta cifra representa alrededor del 0.6% del total de observaciones.
# Lo más sencillo sería impuutar la media en los NA, pero veo en esta situación la oportunidad perfecta
# para aplicar los conocimientos que recién estoy aprendiendo sobre MICE y PMM, así que trataré a los 
# datos faltantes aplicando MICE con PMM.

library(mice)

mag_original <- dataSSN$Magnitud

vars_mice <- dataSSN[,c("Magnitud", "Latitud", "Longitud", "Profundidad")]

imp <- mice(
  vars_mice, 
  m=1, 
  method = "pmm", 
  seed = 123, 
  printFlag = FALSE)

dataSSN$Magnitud<-complete(imp)$Magnitud #Sólo cambia magnitud


#Comparar antes vs después 
par(mfrow=c(1,2))

hist(mag_original, main = "Magnitud original", xlab = "Magnitud", col = "gray", breaks = 30)
hist(dataSSN$Magnitud, main = "Magnitud imputada (PMM)", xlab = "Magnitud", col = "lightblue", breaks = 30)

c(
  media_original = mean(mag_original, na.rm = TRUE),
  media_imputada = mean(dataSSN$Magnitud)
)

c(
  varianza_original = var(mag_original, na.rm = TRUE),
  varianza_imputada = var(dataSSN$Magnitud)
)




# Continuando con el EDA

dataSSN$Fecha <- as.Date(dataSSN$Fecha)

dataSSN$mes <- format(dataSSN$Fecha, "%m")
dataSSN$dia_semana <- weekdays(dataSSN$Fecha)



tabla_mes <- table(dataSSN$mes)
porc_mes <- prop.table(tabla_mes) * 100

data.frame(
  Mes = names(tabla_mes),
  Temblores = as.numeric(tabla_mes),
  Porcentaje = round(as.numeric(porc_mes), 2)
)



tabla_dia <- table(dataSSN$dia_semana)
porc_dia <- prop.table(tabla_dia) * 100

data.frame(
  Dia = names(tabla_dia),
  Temblores = as.numeric(tabla_dia),
  Porcentaje = round(as.numeric(porc_dia), 2)
)


#Chi test para analizar posible sesgo en los días (viernes y sábado tienen mucha proporción de sismos)
chisq.test(tabla_dia)

# Rechazamos con mucha confianza que los temblores estén uniformemente distribuidos entre los días de la semana.
# Los viernes no  necesariamente tiembla más, significa que el proceso de registro no es neutral al día



# - PLOTS -

barplot(porc_mes, main = "Porcentaje de temblores por mes", ylab = "Porcentaje", xlab = "Mes", col = "gray")
barplot(porc_dia, main = "Porcentaje de temblores por día", ylab = "Porcentaje", xlab = "Día", col = "gray")

#Analizar la tendencia anual. ¿Hay más temblores o solo mejor detección?
dataSSN$anio <- format(dataSSN$Fecha, "%Y")
tabla_anio <- table(dataSSN$anio)

#REINICIAR EL LAYOUT
dev.off()

plot(
  as.numeric(names(tabla_anio)),
  as.numeric(tabla_anio),
  type="l",
  xlab="Año",
  ylab="Número de sismos",
  main="Sismos por año"
)



dataSSN$clase_mag <- ifelse(dataSSN$Magnitud < 5, "Bajo", "Moderado +")
tabla_anio_mag <- table(dataSSN$anio, dataSSN$clase_mag)

matplot(
  as.numeric(rownames(tabla_anio_mag)),
  tabla_anio_mag,
  type = "l",
  lty = 1,
  xlab = "Año",
  ylab = "Temblores",
  main = "Temblores por año según magnitud"
)

legend("topleft", colnames(tabla_anio_mag), lty = 1)





dataSSN$year_month <- format(dataSSN$Fecha, "%Y-%m")
conteo_mensual <- table(dataSSN$year_month)

ts_sismos <- ts(as.numeric(conteo_mensual), frequency = 12)

plot(ts_sismos, main = "Serie temporal mensual de temblores", ylab = "Conteo")






dataSSN$clase_mag <- cut(
  dataSSN$Magnitud,
  breaks = c(-Inf, 4, 6, Inf),
  labels = c("<4", "4–6", ">6")
)

dataSSN$mes_nombre <- months(dataSSN$Fecha)
tabla_mes_mag <- table(dataSSN$mes_nombre, dataSSN$clase_mag)
meses_orden <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")

tabla_mes_mag <- tabla_mes_mag[meses_orden, ]

barplot(
  t(tabla_mes_mag),
  beside = FALSE,
  col = c("#4CAF50", "#FFC107", "#F44336"),
  ylab = "Conteo de sismos",
  xlab = "Mes",
  main = "Sismos por mes según magnitud"
)

legend(
  "top",
  legend = colnames(tabla_mes_mag),
  fill = c("#4CAF50", "#FFC107", "#F44336"),
  title = "Magnitud",
  cex = 0.7,        # tamaño del texto
  pt.cex = 0.7,     # tamaño “visual” de los cuadros
  x.intersp = 0.4,  # espacio horizontal
  y.intersp = 0.5,  # espacio vertical
  inset = 0.02      # la mete un poquito hacia adentro
)



# En el análisis exploratorio del catálogo de sismos (Puebla, 1900–2026) se observaron patrones temporales importantes
# y también señales de sesgo de registro. Primero, el conteo anual muestra un incremento marcado a partir de los años 
# 2000, lo cual es consistente con mejoras en la instrumentación, cobertura de estaciones y procedimientos de 
# detección/reporte, más que con un aumento real de la actividad sísmica. En la descomposición por día de la semana 
# se detectó una distribución no uniforme (con p-value significativo en la prueba chi-cuadrada), lo que sugiere que 
# el proceso de registro/revisión introduce efectos administrativos (por ejemplo, consolidación de reportes) y que 
# esas variables calendáricas no deben interpretarse como “causas” físicas del fenómeno. En el desglose mensual se 
# aprecian diferencias moderadas entre meses (posible estacionalidad), pero al estratificar por rangos de magnitud 
# se ve que la mayor parte de los eventos son de magnitud menor (<4), mientras que los eventos mayores son escasos 
# y más estables. En conjunto, el EDA sugiere que para tareas de pronóstico conviene trabajar con conteos agregados 
# (mensuales/semanales) y/o con umbrales de magnitud (p.ej., ≥4) para reducir el impacto de cambios en detección y sesgos de reporte.



