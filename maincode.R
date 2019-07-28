# Package loading and Data processing
library(caret)

traindata<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testdata<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

FULLtraining<-read.csv(traindata,na.strings="NA")
testing <- read.csv(testdata,na.strings="NA")


## Removing the variables that have near 0 variance
zerovariances <- nearZeroVar(FULLtraining)
FULLtraining <- FULLtraining[, -zerovariances]
testing <- testing[, -zerovariances]



## Removing any variables that don't have any values
fullobsv <- apply(FULLtraining, 2, function(x) sum(is.na(x))) == 0
FULLtraining <- FULLtraining[, fullobsv]
testing <- testing[, fullobsv]

# Removing the first variables, used for identification of the observation
FULLtraining <- FULLtraining[, -(1:6)]
testing  <- testing[, -(1:6)]

# Subsetting a validation set   
set.seed(1011001)
inTrain <- createDataPartition(FULLtraining$classe, p=0.70, list=FALSE)
training <- FULLtraining[inTrain, ]
validation <- FULLtraining[-inTrain, ]

modelgbm <- train(classe ~ ., method="gbm", data=training,trControl=trainControl(method = "repeatedcv", number = 5, repeats = 1),verbose=FALSE)
gbmprediction <- predict(modelgbm, validation)
confusionMatrix(validation$classe, gbmprediction)

pred <- predict(modelgbm, newdata=testing)
pred