## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: ANÁLISIS DISCRIMINANTE LINEAL Y CUADRÁTICO EN R [Parte 4]
## 
##  4. Fuentes:
##     https://www.r-bloggers.com/2018/11/linear-quadratic-and-regularized-discriminant-analysis/"


## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## # Antes de trabajar si es necesario, leer las bases de datos, en mi caso no lo es, y aque con el .RData es suficiente:
## 
## Auto <- read.csv(file = "Auto.csv")
## Auto2 <- read.csv(file = "Auto2.csv")
## Auto.test <- read.csv(file = "Auto_test.csv")
## Auto.train <- read.csv(file = "Auto_train.csv")


## ------------------------------------------------------------------------------------
# Leer la base de datos que contiene todos los dataframe y modelos:
#load(".RData")

# Guardamos todas las salidas:
sink("OUTPUTS.txt")


## ------------------------------------------------------------------------------------
# Evaluación del modelo LDA con los datos de test
lda.pred <-  predict(object = modelo.lda, newdata = Auto.test)

# Observanos los nombres del anterior predicción:
print("# Observanos los nombres del anterior predicción:")
print(names(lda.pred))

# Resultado:
"La función predict() ha calculado las probabilidades a posteriori de que cada nueva observación pertenezca a la clase con menor rendimiento (mpg01 = 0)."



"calculamos el porcentaje de observaciones incorrectamente clasificadas comparando la predicción con la verdadera clase del set de datos de test, obteniendo así el test error rate del modelo discriminante:"

# Matriz de confusion del modelo LDA
print("# Matriz de confusion del modelo LDA")
print(table(lda.pred$class, Auto.test$mpg01, dnn = c("Clase predicha", "Clase real")))


# Aplicando un threshold del 50% a las probabilidades a posterior podemos recrear las predicciones del modelo.
print("# Aplicando un threshold del 50% a las probabilidades a posterior podemos recrear las predicciones del modelo.")
print(sum(lda.pred$posterior[,1] >= 0.5))

print(sum(lda.pred$posterior[,1] < 0.5))

# Test error rate del modelo LDA
print("# Test error rate del modelo LDA")

print(mean(lda.pred$class != Auto.test$mpg01))

# Porcentaje de aciertos del modelo LDA
print("# Porcentaje de aciertos del modelo LDA")

print(mean(lda.pred$class == Auto.test$mpg01))

#Resultado:
"El test error rate es muy bajo con este modelo (5%). En prácticamente el 95% de los casos las observaciones son correctamente predichas."

"NOTA: Podríamos cambiar el threshold de decisión si las predicciones no fueran lo bastante buenas en el sentido que nos interesa."


## ------------------------------------------------------------------------------------
# Evaluación del modelo QDA con los datos de test
print("# Evaluación del modelo QDA con los datos de test")
qda.pred <-  predict(object = modelo.qda, newdata = Auto.test)

# Matriz de confusion del modelo QDA
print("# Matriz de confusion del modelo QDA")
table(qda.pred$class, Auto.test$mpg01, dnn = c("Clase predicha", "Clase real"))


# Test error rate del modelo QDA
print("# Test error rate del modelo QDA")
mean(qda.pred$class != Auto.test$mpg01)

# Resultado:
"El test error rate del modelo QDA es ligeramente superior (8,8%), por lo que en este caso optaríamos por el LDA, que cuenta con más porcentaje de aciertos globales."


## ------------------------------------------------------------------------------------
# Instalar el paquete:
#install.packages("caret")
library(caret)

#tabulación cruzada de Auto.tests y predichas con modelo.lda.caret
print("#tabulación cruzada de Auto.tests y predichas con modelo.lda.caret")
confusionMatrix(as.factor(Auto.test$mpg01), predict(modelo.lda.caret, Auto.test))


## ------------------------------------------------------------------------------------
# LDA respecto a los datos de entrenamiento usados en el modelo

png(filename = "modelo_lda.png")

plot(modelo.lda)

title(main = "LDA respecto a los datos de\nentrenamiento usados en el modelo")

dev.off()

#instalamos los paquetes:
#install.packages("klaR")
library(klaR)

# Representación del LDA respecto a los datos de test

png(filename = "partimat_lda.png")

partimat(formula = as.factor(mpg01) ~ displacement + horsepower + weight, data = Auto.test, method = "lda", prec = 400, image.colors = c("darkgoldenrod1", "skyblue2"), col.mean = "firebrick", nplots.vert = 2)

title(sub = "Representación del LDA respecto a los datos de test")

dev.off()



# Representación del QDA respecto a los datos de test
png(filename = "partimat_qda.png")

partimat(formula = as.factor(mpg01) ~ displacement + horsepower + weight, data = Auto.test, method = "qda", prec = 400, image.colors = c("darkgoldenrod1", "skyblue2"), col.mean = "firebrick", nplots.vert = 2)


title(sub = "Representación del QDA respecto a los datos de test")

dev.off()



# Resultado:
"En base a las representaciones podemos observar que la función discriminante consigue separar mejor la clase 1, existiendo superposición respecto a la clase 0."

sink()
