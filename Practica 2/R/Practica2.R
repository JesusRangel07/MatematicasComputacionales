library(help = "datasets")
library(help = "mlbench")

data(ChickWeight)
head(ChickWeight)

#Resumen de la informacion
summary(ChickWeight)

#Ejemplo de tablas de frecuencia
y <-ChickWeight$Diet
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#correlacion
correlations <- cor(ChickWeight[,1:2])
print(correlations)

#Histograma
data("ChickWeight")
par(mfrow=c(1,2))
for(i in 1:2) {
  hist(ChickWeight[,i], main=names(ChickWeight)[i])
}

#Grafica de Correlaciones
library(corrplot)
correlations <- cor(ChickWeight[,1:2])
corrplot(correlations, method="circle")
pairs(ChickWeight)
pairs(weight~., data=ChickWeight, col=ChickWeight$weight)


#Fraficos de densidad por clase
library(caret)
x <- ChickWeight[,1:2]
y <- ChickWeight[,4]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


#Diagramas de caja #1
x <- ChickWeight[,1]
y <- ChickWeight[,4]
featurePlot(x=x, y=y, plot="box")

#Diagrama de caja #2
x <- ChickWeight[,1]
y <- ChickWeight[,3]
featurePlot(x=x, y=y, plot="box")


