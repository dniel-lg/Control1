## Importar CSV
database <- read.csv("/home/dani/Workspace/Control1/BankChurners.csv")

class(database)
class(database$Total_Trans_Ct)
class(database$Credit_Limit)

## Crear matriz de Datos
columnas <- c("Total_Trans_Ct", "Credit_Limit")
datos <- database[, columnas]
as.matrix(datos)

########## 1) a. Categorizar ##########

## Categorizar Total de Transferencias
datos[which(datos$Total_Trans_Ct >= 10 & datos$Total_Trans_Ct <= 29), "NivelUsoTarjeta"] <- "Muy poco uso"
datos[which(datos$Total_Trans_Ct >= 30 & datos$Total_Trans_Ct <= 59), "NivelUsoTarjeta"] <- "Poco uso"
datos[which(datos$Total_Trans_Ct >= 60 & datos$Total_Trans_Ct <= 79), "NivelUsoTarjeta"] <- "Mediano uso"
datos[which(datos$Total_Trans_Ct >= 80 & datos$Total_Trans_Ct <= 99), "NivelUsoTarjeta"] <- "Bastante uso"
datos[which(datos$Total_Trans_Ct >= 100 & datos$Total_Trans_Ct <= 140), "NivelUsoTarjeta"] <- "Frecuente uso"

## Categorizar Limite de Credito
datos[which(datos$Credit_Limit >= 1000 & datos$Credit_Limit <= 1999), "NivelIngresos"] <- "Muy bajo"
datos[which(datos$Credit_Limit >= 2000 & datos$Credit_Limit <= 4999), "NivelIngresos"] <- "Bajo"
datos[which(datos$Credit_Limit >= 5000 & datos$Credit_Limit <= 9999), "NivelIngresos"] <- "Medio"
datos[which(datos$Credit_Limit >= 10000 & datos$Credit_Limit <= 19999), "NivelIngresos"] <- "Alto"
datos[which(datos$Credit_Limit >= 20000 & datos$Credit_Limit <= 40000), "NivelIngresos"] <- "Muy alto"


########## 1) b. Tablas de Frecuencia Absoluta ##########

# Tabla de frecuencia absoluta de NivelUsoTarjeta
table(datos$NivelUsoTarjeta)
FAUsoTarjeta <- as.matrix(table(datos$NivelUsoTarjeta))
colnames(FAUsoTarjeta) <- ("Frecuencia")

# Tabla de frecuencia absoluta de NivelIngresos
table(datos$NivelIngresos)
FAIngresos <- as.matrix(table(datos$NivelIngresos))
colnames(FAIngresos) <- ("Frecuencia")


########## 1) c. Tablas de Frecuencia Relativa ##########

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


########## 1) d. Tabla de Frecuencia Relatica de Doble Entrada ##########

# Tabla de frecuencia absoluta de doble entrada
tablaDobleEntrada <- ftable(datos$NivelIngresos, datos$NivelUsoTarjeta)
tablaDobleEntrada <- addmargins(tablaDobleEntrada)
colnames(tablaDobleEntrada) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntrada) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")

# Tabla de frecuencia relativa de doble entrada respecto del total general

tablaDobleEntradaRelativa <- prop.table(ftable(datos$NivelIngresos, datos$NivelUsoTarjeta))
tablaDobleEntradaRelativa <- addmargins(tablaDobleEntradaRelativa)
colnames(tablaDobleEntradaRelativa) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntradaRelativa) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
tablaDobleEntradaRelativa <- round(tablaDobleEntradaRelativa * 100, 2)

# Tabla de frecuencia relativa de doble entrada respecto de la frecuencia de cada nivel de uso (Columnas)

tablaDobleEntradaRelUso <- prop.table(tablaDobleEntrada, margin = 1) *2
colnames(tablaDobleEntradaRelUso) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
rownames(tablaDobleEntradaRelUso) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto", "Total")

# Tabla de frecuencia relativa de doble entrada respecto de la frecuencia de cada nivel de ingresos (Filas)
tablaDobleEntradaRelIngreso <- prop.table(tablaDobleEntrada, margin = 2) *2
colnames(tablaDobleEntradaRelIngreso) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")
rownames(tablaDobleEntradaRelIngreso) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto", "Total")
