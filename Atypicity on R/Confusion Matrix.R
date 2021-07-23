library(caret)
library(klaR)
# load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]

# train a naive bayes model and build the model
model <- NaiveBayes(Species~., data=data_train)


#modelr<- train(Species~., data=data_train,method="rf"
#               ,trControl=ControlParamteres,tuneGrid=parameterGrid,preProcess=c('center','scale'))


lda1 <- lda(Species ~ ., 
          data_train, 
           prior = c(1,1,1)/3)

#plda1 <- predict(object = lda1,
                #newdata = data_test)

# make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
#On applique la règle sur modèle de test.
predictions <- predict(model, x_test)

# summarize results

#Comparaison entre nos données intiales et prédictes.
confusionMatrix(predictions$class, y_test)

