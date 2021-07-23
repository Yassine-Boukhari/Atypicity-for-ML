#Simulation selon les différents modèles
require(caret)
require(klaR)
require(MASS)

set.seed(123)

# The iris dataset
data(iris)

# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
#--------------------------------------#
#Modèle LDA 
lda1 <- lda(Species ~ ., 
            data_train, 
            prior = c(1,1,1)/3)

plda1 <- predict(object = lda1,
                 newdata = data_test)

x_test <- data_test[,1:4]
y_test <- data_test[,5]

#Matrice de confusion
confusionMatrix(plda1$class, y_test)

#Taux de Précision de bon classement
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

Accur_lda <- train(TrainData, TrainClasses,
                 method = "lda",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
#---------------------------------------------------------------#

#Modèle QDA

qda1 <- qda(Species ~ ., 
            data_train, 
            prior = c(1,1,1)/3)
#predicted model
pqda1 <- predict(object = qda1,
                 newdata = data_test)

x_test <- data_test[,1:4]
y_test <- data_test[,5]

#Matrice de confusion
confusionMatrix(pqda1$class, y_test)

#Taux de Précision de bon classement
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

Accur_qda <- train(TrainData, TrainClasses,
                   method = "qda",
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"))
