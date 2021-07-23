#Simulation selon les différents modèles
require(caret)
require(klaR)
require(MASS)

# The iris dataset
data(iris)
irisbis<-iris[-c(1:50),]

# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(irisbis$Species, p=split, list=FALSE)
data_trainbis <- irisbis[ trainIndex,]
data_testbis <- irisbis[-trainIndex,]

#--------------------------------------#
#Modèle LDA 
lda1bis <- lda(Species ~ ., 
            data_trainbis, 
            prior = c(1,1,1)/3)

plda1bis <- predict(object = lda1bis,
                 newdata = data_testbis)

x_testbis <- data_testbis[,1:4]
y_testbis <- data_testbis[,5]

#Matrice de confusion
confusionMatrix(plda1bis$class, y_testbis)

#Taux de Précision de bon classement
TrainDatabis <- iris[,1:4]
TrainClassesbis <- iris[,5]

#Alternative 
Accur_ldabis <- train(TrainDatabis, TrainClassesbis,
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