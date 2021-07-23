require(caret)
require(rattle)
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

#iris 2 classes
DF<- iris[-c(101:150),]
ind<-createDataPartition(DF$Species, p=2/3, list=FALSE)
trainDF<-DF[ind,]
testDF<-DF[-ind,]

#vérifier que  nous ponvons faire appel à la lda



#-------------------------------#
DataFrame<-iris
ind1<-createDataPartition(DataFrame$Species, p=2/3, list=FALSE)
trainDF1<-DataFrame[ind,]
testDF1<-DataFrame[-ind,]
#-------------------------------#
parameterGrid<-expand.grid(mtry=c(2)) 

ControlParameters<-trainControl(method = "cv",number=5,
                                savePredictions = TRUE,
                                classProbs = TRUE)


knnFit1 <- train(TrainData, TrainClasses,
                 method = "qda",
                 preProcess = c("center", "scale"),
                 tuneLength = 5,
                 trControl = trainControl(method="cv"))

knnFit2 <- train(TrainData, TrainClasses,
                 method = "lda",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))

#modelr<-train(Species~.,data = trainDF,method="lda",
#              trControl = ControlParameters,
#              tuneGrid = parameterGrid,
#              preProcess=c('center','scale'))

