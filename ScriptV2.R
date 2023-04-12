# Importar Base de datos CSV
database <- read.csv("/home/dani/Workspace/Control1/BankChurners.csv")
################################
# Crear dataframe


################################ 1) a. Categorizar ################################

datos <- data.frame(database$Total_Trans_Ct, database$Credit_Limit)
names(datos) <- c("Transacciones", "LimiteDeCredito")

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
FRUsoTarjeta <- prop.table(table(datos$NivelUsoTarjeta))
#Convertir en Matriz
FRUsoTarjeta <- as.matrix(FRUsoTarjeta)
colnames(FRUsoTarjeta) <- ("Frecuencia Relativa")

# Tabla de frecuencia relativa de NivelIngresos
FRIngresos <- prop.table(table(datos$NivelIngresos))
#Convertir en Matriz
FRIngresos <- as.matrix(FRIngresos)
colnames(FRIngresos) <- ("Frecuencia Relativa")


############################ 1) d. Tablas de Frecuencia Absoluta de Doble Entrada ############################

# Tabla de frecuencia absoluta de doble entrada
tablaDobleEntrada <- ftable(datos$NivelIngresos, datos$NivelUsoTarjeta)
tablaDobleEntrada <- addmargins(tablaDobleEntrada)
colnames(tablaDobleEntrada) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntrada) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")


############################# 1) e. a. Frecuencia Relativa Respecto al Total #############################

tablaDobleEntradaRelativa <- prop.table(ftable(datos$NivelIngresos, datos$NivelUsoTarjeta))
tablaDobleEntradaRelativa <- addmargins(tablaDobleEntradaRelativa)
colnames(tablaDobleEntradaRelativa) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntradaRelativa) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
tablaDobleEntradaRelativa <- round(tablaDobleEntradaRelativa * 100, 2)


###################### 1) e. b. Frecuencia Relativa Respecto al Nivel de Uso (Columnas) ######################

tablaDobleEntradaRelUso <- prop.table(tablaDobleEntrada, margin = 1) *2
colnames(tablaDobleEntradaRelUso) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
rownames(tablaDobleEntradaRelUso) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto", "Total")


###################### 1) e. c. Frecuencia Relativa Respecto al Nivel de Ingresos (Filas) ######################

tablaDobleEntradaRelIngreso <- prop.table(tablaDobleEntrada, margin = 2) *2
colnames(tablaDobleEntradaRelIngreso) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
rownames(tablaDobleEntradaRelIngreso) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto", "Total")
