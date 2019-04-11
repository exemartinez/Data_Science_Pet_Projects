###########################
# TP1 - Analisis de Datos #
###########################
####################
# Ejercicio 1      #
####################
# 1 - Carga de los datos
install.packages("readxl")
install.packages("XQuartz")
library(readxl)

X11()
personasDieta <- read_excel("Datos trabajo 1.xls")

#Constantes
DATOF = 999.99

# 2 - Estandarizacion de los valores "no disponibles" del set de datos.
for(i in 1:4){ #Hacemos esto, dado que replace solo vale para vectores, no matrices, y es requisito utilizar este comando.
  personasDieta[,i] = replace(personasDieta[,i], personasDieta[,i]==DATOF, NA)
}

show(personasDieta)
dim(personasDieta)
# C - Descripcion de los datos.
summary(personasDieta[,1:3])
sd(personasDieta$Grasas_sat, na.exclude(TRUE))
sd(personasDieta$Calorías, na.exclude(TRUE))
sd(personasDieta$Alcohol, na.exclude(TRUE))

# Para las grasas saturadas
boxplot(personasDieta$Grasas_sat, main = "Grasas saturadas")
plot(personasDieta$Grasas_sat, pch=20, col="blue")
hist(personasDieta$Grasas_sat)

par(mfrow=c(1,3))
boxplot(personasDieta$Alcohol, col="lightgrey", main="Alcohol")
boxplot(personasDieta$Calorías, col="lightblue", main="Calorías")
boxplot(personasDieta$Grasas_sat, col="lightgreen", main="Grasas saturadas")
dev.off()

#Para el Alcohol
boxplot(personasDieta$Alcohol, main="Alcohol")
plot(personasDieta$Alcohol, pch=20, col="black", xlab="Muestra/Posición", ylab="Alcohol mg", main="Consumo de Alcohol")
abline(v =length(personasDieta$Alcohol)/2,lty="dashed",col = "grey") 
abline(h =mean(personasDieta$Alcohol, na.rm=TRUE),lty="dashed",col = "grey") 
hist(personasDieta$Alcohol, main="Alcohol", col="grey", breaks = 'FD', ylab="Frecuencia", xlab="Alcohol mg")
wilcox.test(personasDieta$Alcohol)

#Para las Grasas Saturadas
boxplot(personasDieta$Grasas_sat, main="Grasas saturadas")
qqnorm(personasDieta$Grasas_sat,main="QQ-plot Scores")
qqline(personasDieta$Grasas_sat)
plot(personasDieta$Grasas_sat, pch=20, col="black", xlab="Muestra/Posición", ylab="Gramos de grasa sat.", main="Consumo de grasas saturadas")
hist(personasDieta$Grasas_sat, col="grey", breaks = 'FD', ylab="Frecuencia", xlab="Gramos de grasas saturadas", main="Consumo de grasas saturadas", prob=TRUE)
shapiro.test(personasDieta$Grasas_sat)

cor(personasDieta$Grasas_sat,personasDieta$Calorías, use="na.or.complete")
cor(personasDieta$Grasas_sat,personasDieta$Alcohol, use="na.or.complete")
cor(personasDieta$Calorías,personasDieta$Alcohol, use="na.or.complete")

#Para las Calorias
boxplot(personasDieta$Calorías, col="blue", main="Calorías")
plot(personasDieta$Calorías, pch=20, col="black", xlab="Muestra/Posición", ylab="KCal", main="Consumo de calorías")
hist(personasDieta$Calorías, col="grey", breaks = 'FD', ylab="Frecuencia", xlab="KCal", main="Consumo de calorías")
shapiro.test(personasDieta$Calorías)
t.test(personasDieta$Calorías)

# D - En funcion del sexo.

boxplot(personasDieta$Grasas_sat~personasDieta$Sexo, xlab="Sexo", ylab="Grasas saturadas", main="Consumo de grasas saturadas por sexo")
boxplot(personasDieta$Calorías~personasDieta$Sexo, xlab="Sexo", ylab="Calorías", main = "Consumo de calorías por sexo")
boxplot(personasDieta$Alcohol~personasDieta$Sexo, xlab="Sexo", ylab="Alcohol", main = "Consumo de alcohol por sexo")

par(mfrow=c(1,2))

#Alcohol
hist(personasDieta$Alcohol[personasDieta$Sexo=="F"], ylab="Alcohol", xlab="", main="Femenino", col="white", probability = TRUE)        
hist(personasDieta$Alcohol[personasDieta$Sexo=="M"], ylab="Alcohol",xlab="", main="Masculino", col="grey", probability = TRUE)

summary(personasDieta$Alcohol[personasDieta$Sexo=="F"], na.rm=TRUE)
length(personasDieta$Alcohol[personasDieta$Sexo=="F"])
summary(personasDieta$Alcohol[personasDieta$Sexo=="M"], na.rm=TRUE)
length(personasDieta$Alcohol[personasDieta$Sexo=="M"])

#Calorías
hist(personasDieta$Calorías[personasDieta$Sexo=="F"], ylab="Calorías", xlab="", main="Femenino", col="white", probability = TRUE, breaks='FD')        
hist(personasDieta$Calorías[personasDieta$Sexo=="M"], ylab="Calorías",xlab="", main="Masculino", col="grey", probability = TRUE, breaks='FD')

summary(personasDieta$Calorías[personasDieta$Sexo=="F"], na.rm=TRUE)
length(personasDieta$Calorías[personasDieta$Sexo=="F"])
summary(personasDieta$Calorías[personasDieta$Sexo=="M"], na.rm=TRUE)
length(personasDieta$Calorías[personasDieta$Sexo=="M"])

#Grasas Saturadas
hist(personasDieta$Grasas_sat[personasDieta$Sexo=="F"],ylab="Grasas saturadas", xlab="", main="Femenino", col="white", probability = TRUE, breaks='FD')        
hist(personasDieta$Grasas_sat[personasDieta$Sexo=="M"],ylab="Grasas saturadas",xlab="", main="Masculino", col="grey", probability = TRUE, breaks='FD')

summary(personasDieta$Grasas_sat[personasDieta$Sexo=="F"], na.rm=TRUE)
length(personasDieta$Grasas_sat[personasDieta$Sexo=="F"])
summary(personasDieta$Grasas_sat[personasDieta$Sexo=="M"], na.rm=TRUE)
length(personasDieta$Grasas_sat[personasDieta$Sexo=="M"])

dev.off()

# E - Describa el comportamiento de los datos para la variable alcohol de acuerdo a las calorias

cor(personasDieta$Alcohol,personasDieta$Calorías, use="na.or.complete", method="spearman")
cov(personasDieta$Alcohol,personasDieta$Calorías, use="na.or.complete", method="spearman")
var(personasDieta$Alcohol,personasDieta$Calorías, use="na.or.complete")



plot(personasDieta$Calorías, personasDieta$Alcohol,pch=1, col="black", xlab="Calorías", ylab="Alcohol" , main="Relación entre calorías y alcohol")
abline(v =mean(personasDieta$Calorías, na.rm=TRUE),lty="dashed",col = "grey") #calorias
abline(h =mean(personasDieta$Alcohol, na.rm=TRUE),lty="dashed",col = "grey") #alcohol

#Por categoria: CATE 1: 0 < Calorias <= 1100 # CATE 2: 1100 < Calorias <= 1700 # CATE 3: 1700 < Calorias <= Inf
cate=c(1100,1700) #Enumerations

personasDietaCate <- replace(personasDieta,"Cate",0)

#Cuidado! la aplicacion de la segunda categoria funciona porque se aplica la tercera!
personasDietaCate$Cate <- replace(personasDietaCate$Cate, personasDietaCate$Calorías <= cate[1], 1)
personasDietaCate$Cate <- replace(personasDietaCate$Cate, personasDietaCate$Calorías >= cate[1], 2) 
personasDietaCate$Cate <- replace(personasDietaCate$Cate, personasDietaCate$Calorías > cate[2], 3) 

#Graficos para realizar el analisis
boxplot(personasDietaCate$Alcohol~personasDietaCate$Cate, xlab="Categoría", ylab="Alcohol", main="Consumo de alcohol por categoría de calorías")
hist(personasDietaCate$Cate, breaks=8,xlim=c(1,3),ylab="Observaciones",xlab="Categoría", main="Consumo de Alcohol", col="grey", border="black")

sd(personasDietaCate$Alcohol[personasDietaCate$Cate == 3], na.rm=TRUE)
median(personasDietaCate$Alcohol[personasDietaCate$Cate == 3], na.rm=TRUE)
mean(personasDietaCate$Alcohol[personasDietaCate$Cate == 3], na.rm=TRUE)

#Este grafico no suma nada, solo lo hace el boxplot
plot(personasDietaCate$Cate, personasDietaCate$Alcohol,pch=1, col="black", xlab="Categorías", ylab="Alcohol")
abline(v =mean(personasDieta$Calorías, na.rm=TRUE),lty="dashed",col = "grey") #calorias
abline(h =mean(personasDieta$Alcohol, na.rm=TRUE),lty="dashed",col = "grey") #alcohol

library(lattice)
parallelplot(personasDietaCate[,c(2,5)],data=personasDietaCate)

show(personasDietaCate)

##### vvv Hice este metodo solo para practicar comandos, no lo utilice para graficar vvv #####
library(CorReg)#instalar antes el paquete
cate=c(1100,1700)

cate1 = personasDieta$Alcohol[personasDieta$Calorías<=cate[1]]
cate2 = personasDieta$Alcohol[personasDieta$Calorías > cate[1]||personasDieta$Calorías<=cate[2]]
cate3 = personasDieta$Alcohol[personasDieta$Calorías > cate[2]]

X=data.frame(Alcohol=c(cate1,cate2,cate3),
             Cate=c(rep("1",times=length(cate1)),rep("2",times=length(cate2)),rep("3",times=length(cate3))))

BoxPlot(X$Alcohol,X$Cate,data=X,ylab="num",main="boxplot com intervalos de confianza")
#####^^^ Hice este metodo solo para practicar comandos, no lo utilice para graficar ^^^ #####

####################
# Ejercicio 2      #
####################
data(swiss)
dim(swiss)
show(swiss)

correlacion <- cor(swiss)
dim(correlacion)
show(correlacion) #Hay algunos datos correlacionados, pero muchos no. Las variables son muchas. Mejor Mahalanobis...

covarianzas <- cov(swiss)
show(covarianzas)

#Estos graficos no aportan valor - solo se hicieron para practicar y explorar alternativas.
plot(correlacion,pch=1, col="black", xlab="", ylab="", main="Matriz de correlacion - Suiza")
abline(v =mean(correlacion, na.rm=TRUE),lty="dashed",col = "grey") #calorias
abline(h =mean(correlacion, na.rm=TRUE),lty="dashed",col = "grey") #alcohol

plot(covarianzas,pch=1, col="black", xlab="", ylab="", main="Matriz de covarianzas - Suiza")
abline(v =mean(covarianzas, na.rm=TRUE),lty="dashed",col = "grey") #calorias
abline(h =mean(covarianzas, na.rm=TRUE),lty="dashed",col = "grey") #alcohol

#Datos estadisticos duros
summary(swiss)
names(swiss)
mahaRes <- mahalanobis(swiss, colMeans(swiss,na.rm=TRUE),covarianzas)
show(mahaRes)
str(mahaRes)

hist(mahaRes,ylab="Observaciones",xlab="Distancia", main="Distancias Mahalanobis", col="grey", border="black", breaks = "FD")

#Concluyo que debe ser mayor a 12 para ser un "outlier"
OUT <- 11

hist(mahaRes,ylab="Observaciones",xlab="Distancia", main="Distancias Mahalanobis", col="grey", border="black", breaks = "FD")
abline(v =OUT,lty="dotted",col = "black") 

print("Outliers:")

for(i in 1:length(mahaRes)){
  if (mahaRes[i] > OUT){
    print(mahaRes[i])
  }
}

#Ploteo las distancias de Mahalanobis
plot(mahaRes,pch=1, col="black", xlab="Cantones", ylab="Distancia", main="Distancias Mahalanobis - Suiza")
abline(h =OUT,lty="dotted",col = "black") 
#points(mahaRes[mahaRes > OUT],pch="x",col="red")

boxplot(mahaRes, xlab="Cantones", ylab="Dist. Mahalanobis", main = "Distancias de Mahalanobis")
#Ploteo los datos
plot(swiss[,1:6],pch=".", col="black", main="Datos Suiza")

length(mahaRes)
summary(mahaRes)
names(mahaRes)

data()

