getwd()
setwd("/home/dani/Workspace/Curso R Usach/Control1Final")
rm(list = ls())
library(ggplot2)
library(dplyr)
library(readxl)

###################################################################################

database <- read.csv("/home/dani/Workspace/Curso R Usach/Control1Final/BankChurners.csv")
datos <- data.frame(database$Total_Trans_Ct, database$Credit_Limit)
rm(database)
names(datos) <- c("Transacciones", "LimiteDeCredito")

################################ 1) a. Categorizar ################################

# Categorizar Total de Transferencias
datos[which(datos$Transacciones >= 10 & datos$Transacciones <= 29), "NivelUsoTarjeta"] <- "Muy poco uso"
datos[which(datos$Transacciones >= 30 & datos$Transacciones <= 59), "NivelUsoTarjeta"] <- "Poco uso"
datos[which(datos$Transacciones >= 60 & datos$Transacciones <= 79), "NivelUsoTarjeta"] <- "Mediano uso"
datos[which(datos$Transacciones >= 80 & datos$Transacciones <= 99), "NivelUsoTarjeta"] <- "Bastante uso"
datos[which(datos$Transacciones >= 100 & datos$Transacciones <= 140), "NivelUsoTarjeta"] <- "Frecuente uso"
## Categorizar Limite de Credito
datos[which(datos$LimiteDeCredito >= 1000 & datos$LimiteDeCredito <= 1999), "NivelIngresos"] <- "Muy bajo"
datos[which(datos$LimiteDeCredito >= 2000 & datos$LimiteDeCredito <= 4999), "NivelIngresos"] <- "Bajo"
datos[which(datos$LimiteDeCredito >= 5000 & datos$LimiteDeCredito <= 9999), "NivelIngresos"] <- "Medio"
datos[which(datos$LimiteDeCredito >= 10000 & datos$LimiteDeCredito <= 19999), "NivelIngresos"] <- "Alto"
datos[which(datos$LimiteDeCredito >= 20000 & datos$LimiteDeCredito <= 40000), "NivelIngresos"] <- "Muy alto"

################################ 1) b. Tablas de Frecuencia Absoluta ################################

# Tabla de frecuencia absoluta de NivelUsoTarjeta
table(datos$NivelUsoTarjeta)
FAUsoTarjeta <- as.matrix(table(datos$NivelUsoTarjeta))
colnames(FAUsoTarjeta) <- ("Frecuencia")
# Tabla de frecuencia absoluta de NivelIngresos
table(datos$NivelIngresos)
FAIngresos <- as.matrix(table(datos$NivelIngresos))
colnames(FAIngresos) <- ("Frecuencia")

################################ 1) c. Tablas de Frecuencia Relativa ################################

# Tabla de frecuencia relativa de NivelUsoTarjeta
RelativaTarjeta <- prop.table(table(datos$NivelUsoTarjeta))
RelativaTarjeta <- as.matrix(RelativaTarjeta)
colnames(RelativaTarjeta) <- ("Frecuencia Relativa")
# Tabla de frecuencia relativa de NivelIngresos
RelativaIngresos <- prop.table(table(datos$NivelIngresos))
RelativaIngresos <- as.matrix(RelativaIngresos)
colnames(RelativaIngresos) <- ("Frecuencia Relativa")
# Tabla de frecuencias completa
Frecuencias <- cbind(FAUsoTarjeta, RelativaTarjeta, FAIngresos, RelativaIngresos)
colnames(Frecuencias) <- c("Frecuencia Absoluta Uso Tarjeta", "Frecuencia Relativa Uso Tarjeta",
                           "Frecuencia Absoluta Ingresos", "Frecuencia Relativa Ingresos")

############################ 1) d. Tablas de Frecuencia Absoluta de Doble Entrada ############################

# Tabla de frecuencia absoluta de doble entrada
tablaDobleEntrada <- ftable(datos$NivelIngresos, datos$NivelUsoTarjeta)
tablaDobleEntrada <- addmargins(tablaDobleEntrada)
colnames(tablaDobleEntrada) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntrada) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
tablaDobleEntrada # Tabla de Doble entrada con Frecuencias Absolutas, se muestra por consola

############################# 1) e. a. Frecuencia Relativa Respecto al Total #############################

tablaDobleEntradaRelativa <- prop.table(ftable(datos$NivelIngresos, datos$NivelUsoTarjeta))
tablaDobleEntradaRelativa <- addmargins(tablaDobleEntradaRelativa)
colnames(tablaDobleEntradaRelativa) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntradaRelativa) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
tablaDobleEntradaRelativa <- round(tablaDobleEntradaRelativa * 100, 2)
tablaDobleEntradaRelativa # Tabla de Doble entrada con Frecuencias Relativas, se muestra por consola

###################### 1) e. b. Frecuencia Relativa Respecto al Nivel de Uso (Columnas) ######################

tablaDobleEntradaRelUso <- prop.table(tablaDobleEntrada, margin = 1) * 2
colnames(tablaDobleEntradaRelUso) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
rownames(tablaDobleEntradaRelUso) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto", "Total")
tablaDobleEntradaRelUso # Tabla de Doble entrada con Frecuencias Relativas, se muestra por consola

###################### 1) e. c. Frecuencia Relativa Respecto al Nivel de Ingresos (Filas) ######################

tablaDobleEntradaRelIngreso <- prop.table(tablaDobleEntrada, margin = 2) * 2
colnames(tablaDobleEntradaRelIngreso) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
rownames(tablaDobleEntradaRelIngreso) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto", "Total")
tablaDobleEntradaRelIngreso # Tabla de Doble entrada con Frecuencias Relativas, se muestra por consola

################################# Graficos #################################

# Histogramas
hist(datos$Transacciones, main = "Histograma de Transacciones",
     xlab = "Transacciones",
     ylab = "Frecuencia",
     col = "#6BAED6",
     breaks = 15)

hist(datos$LimiteDeCredito, main = "Histograma de Limite de Credito",
     xlab = "Limite de Credito",
     ylab = "Frecuencia",
     col = "#6BAED6",
     breaks = 15)

# Grafico de Barras
categorias <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
datos$NivelIngresos <- factor(datos$NivelIngresos, levels = categorias)
tablaDobleEntrada <- ftable(datos$NivelIngresos, datos$NivelUsoTarjeta)
datos_grafico <- as.data.frame(tablaDobleEntrada)
ggplot(data = datos_grafico, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(title = "Nivel de Ingresos y Frecuencia de Uso de la Tarjeta",
       x = "Nivel de Ingresos",
       y = "Frecuencia",
       fill = "Frecuencia de Uso") +
  scale_fill_manual(values = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#08519C"),
                    labels = c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso"))

# Grafico de Lineas de 3 series



# Regresion
regresion <- lm(datos$Transacciones ~ datos$LimiteDeCredito)
# Grafico de dispersion
plot(datos$Transacciones ~ datos$LimiteDeCredito,
     main = "Grafico de dispersion de Transacciones y Limite de Credito",
     xlab = "Limite de Credito",
     ylab = "Transacciones",
     col = "#6BAED6")
abline(regresion, col = "red")
summary(regresion)

###### Diferencia Grafico de Barra e histogramas #####
# En el grafico de barra se puede ver la frecuencia del uso de la tarjeta cruzad0  con el nivel de ingresos,
# en cambio en el histograma se puede ver la distribucion de las variables.
################################# ED #################################
# Media
mean(datos$Transacciones)
mean(datos$LimiteDeCredito)
sd(datos$Transacciones)
sd(datos$LimiteDeCredito)
# Covarianza
cov(datos$Transacciones, datos$LimiteDeCredito)
# Coeficiente de correlacion
cor(datos$Transacciones, datos$LimiteDeCredito)

# Interpretacion
# En cuanto a la regresion lineal,tenemos una relacion significativa entre las dos variables sin embargo
# el coeficiente de determinacion es muy bajo, por lo que no es un buen modelo de prediccion.
# El comportamiento de la variable de respuesta puede ser explicado por otras variables no consideradas
# en este estudio.
# La media es de las transacciones y del limite de credito es de 64.85 y 8631 respectivamente y la desviacion
# estandar es de 23.47 y 9088.78 respectivamente, esto nos indica que los datos estan muy dispersos.
# En cuanto al coeficiente de correlacion este es de 0.075, lo que nos indica que no hay una relacion lineal.