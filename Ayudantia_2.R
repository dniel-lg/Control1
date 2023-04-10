rm(list=ls())
cat("\014")


getwd()
setwd("C:/Users/benja/Desktop/Ayudantia Computacion")
getwd()
dir()

##############################ALTERNATIVA PREFERENCIAL#######################################

set.seed(123)
#Alternativa 1

#1
Wage <- data.frame(Salario = matrix(sample(410:5000, size = 10000, TRUE), nrow = 10000, ncol = 1))

#A)

Wage[which(Wage$Salario>=410 & Wage$Salario <700), "Categorias"] <- "Muy Bajo"
Wage[which(Wage$Salario>=700 & Wage$Salario <1000), "Categorias"] <- "Bajo"
Wage[which(Wage$Salario>=1000 & Wage$Salario <2000), "Categorias"] <- "Medio"
Wage[which(Wage$Salario>=2000 & Wage$Salario <4000), "Categorias"] <- "Alto"
Wage[which(Wage$Salario>=4000 & Wage$Salario <=5000), "Categorias"] <- "Muy Alto"

#B)

mean(Wage$Salario)
median(Wage$Salario)
sd(Wage$Salario)
max(Wage$Salario)
min(Wage$Salario)

#C)

Wage$Categorias <- as.factor(Wage$Categorias)
summary(Wage)

#################################ALTERNATIVA#############################################


Salarios <- matrix(sample(410:5000, size = 10000, TRUE), nrow = 10000, ncol = 1)
Categorias <- cut(Salarios, breaks = c(410, 700, 1000, 2000, 4000, 5000),
                  labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"), ordered= TRUE)


Wage <- data.frame(Salarios, Categorias)

##############################################################################################
#D

year_educ <- sample(1:28, size = 10000, T)
year_educ <- as.data.frame(year_educ)
Wage <- data.frame(Wage, year_educ)
rm(year_educ)
######################################ALTERNATIVA#######################################
Educacion <- cut(year_educ, breaks= c(0,8,12,17,19,28),
                 labels =c("Basica", "Media", "Universitario", "Magister", "Doctorado"), 
                 ordered= TRUE)
#######################################################################################
#Alternativa de preferencia

#E)

Wage[which(Wage$year_educ >= 1 & Wage$year_educ <= 8 ), "Educacion"] <- "Basico"
Wage[which(Wage$year_educ >=9 & Wage$year_educ <= 12 ), "Educacion"] <- "Media"
Wage[which(Wage$year_educ >= 13 & Wage$year_educ <= 17 ), "Educacion"] <- "Universitario"
Wage[which(Wage$year_educ >= 18 & Wage$year_educ <= 19 ), "Educacion"] <- "Magister"
Wage[which(Wage$year_educ >= 20 & Wage$year_educ <= 28 ), "Educacion"] <- "Doctorado"

class(Wage$Educacion)
###############################################################

#F

Educacion <- c("Basico", "Media", "Universitario", "Magister", "Doctorado")
Wage$Educacion <- factor(Wage$Educacion, levels = Educacion)

summary(Wage)

###################################################################################
#######################################
#creacion de variables dummy

Wage$dummy1 <- 0
Wage[which(Wage$Educacion == "Media"), "dummy1"] <- 1
mean(Wage$dummy1)

Wage$dummy2 <- 0
Wage[which(Wage$Educacion == "Universitario"), "dummy2"] <- 1
mean(Wage$dummy2)

Wage$dummy3<- 0
Wage[which(Wage$Educacion == "Magister"), "dummy3"] <- 1
mean(Wage$dummy3)

Wage$dummy4 <- 0
Wage[which(Wage$Educacion == "Doctorado"), "dummy4"] <- 1
mean(Wage$dummy4)

##########################################################################################
#asignamos variable dummy

#FRP: salario= beta0 + beta1*media + beta2*universitario + beta3*magister + beta4*doctorado + epsilon

beta0 <- 550000
beta1 <- 300000
beta2 <- 1200000
beta3 <- 1900000
beta4 <- 2400000

Wage$salario_bas <- beta0 + beta1*Wage$dummy1 + beta2*Wage$dummy2 + beta3*Wage$dummy3 +
  beta4*Wage$dummy4 + rnorm(10000,0,100000)

tapply(Wage$salario_bas, Wage$Educacion, mean)
hist(Wage$salario_bas)

