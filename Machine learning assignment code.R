library(AppliedPredictiveModeling)
library(caret)
library(ggplot2)
library(rpart)
library(MASS)
library(Hmisc)
library(psych)
library(klaR)
library(rattle)
tdata <- read.csv("pml-training.csv", na.strings = c("NA", "", "#DIV/0!"))
testdata <- read.csv("pml-testing.csv", na.strings = c("NA", "", "#DIV/0!"))
str(tdata)

##cleaning training data
nzv_cols <- nearZeroVar(tdata)
if(length(nzv_cols) > 0) tdata <- tdata [, -nzv_cols]

trainingclean <- tdata[, colSums(is.na(tdata)) < nrow(tdata) * 0.95]
trainingclean <- trainingclean[,-(1:6)]
str(trainingclean)

##cleaning testing data
nzvtest_cols <- nearZeroVar(testdata)
if(length(nzvtest_cols) > 0) testdata <- testdata [, -nzvtest_cols]

testclean <- testdata[, colSums(is.na(testdata)) < nrow(testdata) * 0.95]
testclean <- testclean[,-(1:6)]
str(testclean)

inTrain <- createDataPartition( y = trainingclean$classe, p = 0.7, list = FALSE)

training <- trainingclean[inTrain, ]
testing <- trainingclean[-inTrain,]
dim(training)
dim(testing)



## Random Forest

set.seed(82)
modrf <- train(classe~., data = training, method = "rf", prox = TRUE)
predrf <- predict(modrf, newdata = testing)
confusionMatrix(testing$classe, predrf)
varImp(modrf)
predict(modrf, newdata=testclean)

## Out of the sample error rate
1-sum(predrf == testing$classe) / length(predrf)


modrfred <- train(classe~roll_belt+pitch_forearm+yaw_belt+magnet_dumbbell_z+pitch_belt +
                    magnet_dumbbell_y+roll_forearm, data = training, method="rf", prox=TRUE)
summary(modrfred)

predrfred <- predict(modrfred, newdata = testing)
confusionMatrix(testing$classe, predrfred)
varImp(modrfred)
predict(modrfred, newdata=testclean)

## Out of sample error

1-sum(predrfred == testing$classe) / length(predrfred)


## rpart

set.seed(125)
moddt <- rpart ( classe ~ ., method = "class", data = training)



fancyRpartPlot(moddt, main = "Decission Tree")


preddt <- predict(moddt, newdata = testing, type = "class")

confusionMatrix(testing$classe, data = preddt)


1-sum(preddt == testing$classe) / length(preddt)
