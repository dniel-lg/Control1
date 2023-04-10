## Importar CSV
database <- read.csv("/home/dani/Workspace/Control 1/BankChurners.csv")

class(database)
class(database$Total_Trans_Ct)
class(database$Credit_Limit)

## Crear matriz de Datos
columnas <- c("Total_Trans_Ct", "Credit_Limit")
datos <- database[, columnas]
as.matrix(datos)

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

## Crear tablas de Frecuencia Absoluta (TFA = Tabla de Frecuencia Absoluta)
TFAUso <- table(datos$NivelUsoTarjeta)
TFACredito <- table(datos$NivelIngresos)
print(TFAUso)
print(TFACredito)

## Crear tablas de Frecuencia Relativa (TFR = Tabla de Frecuencia Relativa)
TFRUso <- prop.table(TFAUso)
TFRCredito <- prop.table(TFACredito)
print(TFRUso)
print(TFRCredito)

## Crear tabla de frecuencia absoluta de doble entrada

tablaDobleEntrada <- ftable(datos$NivelIngresos, datos$NivelUsoTarjeta)
tablaDobleEntrada <- addmargins(tablaDobleEntrada)
colnames(tablaDobleEntrada) <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto", "Total")
rownames(tablaDobleEntrada) <- c("Muy poco uso", "Poco uso", "Mediano uso", "Bastante uso", "Frecuente uso", "Total")

## Tabla de frecuencia relativa de doble entrada
tablaFrecRelativa <- prop.table(tablaDobleEntrada, margin = 2)
print(tablaFrecRelativa)