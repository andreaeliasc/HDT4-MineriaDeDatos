library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(tidyr)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(rpart)
library(corrplot)

# Analisis Exploratorio
train<- read.csv("train.csv", stringsAsFactors = FALSE)
test<- read.csv("test.csv", stringsAsFactors = FALSE)
train<-train[1:1460,]

glimpse(train[1:10,])

#graficas de correlacion de las variables que se utilizan vs el precio de compra
scatter.smooth(train$LotFrontage, train$SalePrice)
scatter.smooth(train$LotArea, train$SalePrice)
scatter.smooth(train$GrLivArea, train$SalePrice)
scatter.smooth(train$YearBuilt, train$SalePrice)
scatter.smooth(train$BsmtUnfSF, train$SalePrice)
scatter.smooth(train$TotalBsmtSF, train$SalePrice)
scatter.smooth(train$X1stFlrSF, train$SalePrice)
scatter.smooth(train$GarageYrBlt, train$SalePrice)
scatter.smooth(train$GarageArea, train$SalePrice)
scatter.smooth(train$YearRemodAdd, train$SalePrice)

# Modelo de Regresion Lineal
porcentaje<-0.7
datos<-read.csv("train.csv", stringsAsFactors = FALSE)
set.seed(123)
flores<-iris
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

head(train)
head(test)

fit1<-lm(SalePrice~.,data = datos[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice")])
summary(fit1)


cat("SalePrice = ",round(fit1$coefficients[7],2), "YearRemodAdd + ", round(fit1$coefficients[6],2), "GarageArea + ", round(fit1$coefficients[5],2), "TotalBsmtSF", round(fit1$coefficients[4],2), "BsmtUnfSF + " , round(fit1$coefficients[3],2), "YearBuilt + ", round(fit1$coefficients[2],2), "GrLivArea", round(fit1$coefficients[1],2))

#Inciso 3
#correlacion
pairs(~GrLivArea + YearBuilt + BsmtUnfSF + TotalBsmtSF + GarageArea + YearRemodAdd + SalePrice, data = datos, main = "Datos")
correlacion <- cor(datos[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice")])
correlacion
corrplot(correlacion)
### Análisis de Residuales  
predL<-predict(fitMLM_SalePrice, newdata = test)
## La predicción se ve de esta manera:  
head(predL)
length(predL)
## A los valores de los residuos del modelo se puede acceder a través del parámetro *residuals* del modelo.  
head(fitMLM_SalePrice$residuals)
## En los siguientes gráficos se pueden analizar los residuales.
plot(fitMLM_SalePrice)
## En el gráfico ***Residuals vs Fitted***
hist(fitMLM_SalePrice$residuals)
boxplot(fitMLM_SalePrice$residuals)
qqnorm(fitMLM_SalePrice$residuals)
qqline(fitMLM_SalePrice$residuals, col="red")

library(nortest)
lillie.test(fitMLM_SalePrice$residuals)

## Eficiencia y Efectividad del modelo 
predMLM<-predict(fitMLM_SalePrice,newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice")])
library(caret)
RMSE(predMLM,test$SalePrice)

plot(test$SalePrice,col="blue")
points(predMLM, col="red")

summary(test$SalePrice-predMLM)
