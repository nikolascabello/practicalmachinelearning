#projecto final
library(caret)
library(corrplot)
library(tidyverse)
library(plotly)
library(mlr)
library(randomForest)
library(FNN)
library(cluster)
library(kknn)
library(kernlab)
library(rattle)
library(ggplot2)
library(neuralnet)

training<- pml.training
testing<- pml.testing

#con varianza cercana a 0  NAS fuera
training <- training[,-c(1:7)] 
training <- training[, colSums(is.na(training)) == 0]
depured0<- nearZeroVar(training)
trainingd0<-training[,-depured0]

#ok now the split
set.seed(1542)
traiN <- createDataPartition( trainingd0$classe, p=0.75,list = F)
trainingR<-trainingd0[traiN,]
validationR<- trainingd0[-traiN,]

#modeling rf
library(caret)
rfcv <- trainControl(method = "cv", number = 5)
rfcvmodel <- train(classe ~ .,data =trainingR, method = "rf", trControl = rfcv,tuneLength = 5,ntree=50)
print(rfcvmodel)

test_predictions <- predict(rfcvmodel, validationR)


cmrfcv <- confusionMatrix(test_predictions, as.factor(validationR$classe))

cmrfcv
#done

#arbol de decision 
rpartcvmodel <- train(classe~., data=trainingR, method="rpart", trControl = rfcv, tuneLength = 10)
fancyRpartPlot(rpartcvmodel$finalModel)
rpartcvmodel

test_predictions <- predict(rpartcvmodel, validationR)
cmrpartcv <- confusionMatrix(test_predictions, as.factor(validationR$classe))
cmrpartcv
#done 
#gradient boosted trees
gbmcvmodel <- train(classe~., data=trainingR, method="gbm", trControl = rfcv, tuneLength = 4, verbose = F)
test_predictions <- predict(gbmcvmodel, validationR)
cmcvgbm <- confusionMatrix(test_predictions, as.factor(validationR$classe))
cmcvgbm


# Print the accuracy
cat("Accuracy:", cmrfcv$overall['Accuracy'], "RF", cmrpartcv$overall['Accuracy'],"rpart", cmcvgbm$overall['Accuracy'], "gbm")
#random forest more accurate

#testeando el set 
predictionsforproject<- predict(rfcvmodel,testing)
predictionsforproject
plot(rfcvmodel)

#https://github.com/nikolascabello/practicalmachinelearning.git


