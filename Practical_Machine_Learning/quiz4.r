library(caret)

# Question 1
library(ElemStatLearn)

data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y  <- as.factor(vowel.test$y)

set.seed(33833)

fitRF  <- train(y ~ ., data = vowel.train, method = "rf")
fitGBM <- train(y ~ ., data = vowel.train, method = "gbm")

predRF  <- predict(fitRF, vowel.test)
predGBM <- predict(fitGBM, vowel.test)

confusionMatrix(predRF, vowel.test$y)
confusionMatrix(predGBM, vowel.test$y)

confusionMatrix(predRF[predRF == predGBM], vowel.test$y[predRF == predGBM])

# Question 2
library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

fitRF  <- train(diagnosis ~ ., data = training, method = "rf")
fitGBM <- train(diagnosis ~ ., data = training, method = "gbm")
fitLDA <- train(diagnosis ~ ., data = training, method = "lda")

predRF  <- predict(fitRF,  testing)
predGBM <- predict(fitGBM, testing)
predLDA <- predict(fitLDA, testing)

confusionMatrix(predRF,  testing$diagnosis)
confusionMatrix(predGBM, testing$diagnosis)
confusionMatrix(predLDA, testing$diagnosis)

tpredRF  <- predict(fitRF,  training)
tpredGBM <- predict(fitGBM, training)
tpredLDA <- predict(fitLDA, training)

tpredCMB <- data.frame(tpredRF, tpredGBM, tpredLDA, 
                      diagnosis=training$diagnosis)
fitSTK  <- train(diagnosis ~ ., data = tpredCMB, method = "rf")

predCMB <- data.frame(tpredRF=predRF, tpredGBM=predGBM, tpredLDA=predLDA)
predSTK <- predict(fitSTK, predCMB)

confusionMatrix(predSTK, testing$diagnosis)

# Question 3
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

fitLasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")

plot.enet(fitLasso$finalModel, xvar="penalty", use.color=TRUE)

# Question 4
library(lubridate)  # For year() function below

dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest  = ts(testing$visitsTumblr, start = 366)

library(forecast)
fit <- bats(training$visitsTumblr)
pred <- forecast(fit, 235)

plot(pred)
lines(tstest, col="red")

inBounds <- tstest[tstest < pred$upper[,2] & tstest > pred$lower[,2]]
withinB  <- length(inBounds) / length(tstest)
withinB

# Question 5
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
library(caret)
library(forecast)

set.seed(325)

fit <- train(CompressiveStrength ~ ., data = training, method = "svmRadial")
pred <- predict(fit, testing)
accuracy(pred, testing$CompressiveStrength)
