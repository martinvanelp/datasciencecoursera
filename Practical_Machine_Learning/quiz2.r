# Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

#RIGHT
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#WRONG
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)

x <- training$Superplasticizer
y <- log(x)

summary(y)

# Question 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

head(training)
names(training)

trainingIL <- training[, c(1, 58:69)]

preProc <- preProcess(trainingIL[,-1], method = "pca", thresh = 0.8)
preProc$rotation

# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

head(training)
names(training)

trainingIL <- training[, c(1, 58:69)]
testingIL <- testing[, c(1, 58:69)]

## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = trainingIL)

predictions1 <- predict(modelFit, newdata = testingIL)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions1, testingIL$diagnosis)
print(C1)

A1 <- C1$overall[1]

## do similar steps with the caret package
modelFit <- train(diagnosis ~ ., method = "glm", preProcess = "pca", 
                  data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))

predictions2 <- predict(modelFit, testingIL)
C2 <- confusionMatrix(predictions2, testingIL$diagnosis)
print(C2)

A2 <- C2$overall[1]

c(A1, A2)

#ALTERNATIVE FOR GREPPING RIGHT PREDICTORS
ILstr <- grep("^IL", colnames(training), value = TRUE)
predictorsIL <- predictors[, ILstr]
