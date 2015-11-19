# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)

library(caret)

set.seed(125)
training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing  <- segmentationOriginal[segmentationOriginal$Case == "Test",]

set.seed(125)
modFit <- train(Class ~ ., method="rpart", data=training)

# a <- c(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1 = 2)
# b <- c(TotalIntench2 = 50000, FiberWidthCh1 = 10, VarIntenCh4 = 100)
# c <- c(TotalIntench2 = 57000, FiberWidthCh1 = 8, VarIntenCh4 = 100)
# d <- c(FiberWidthCh1 = 8, VarIntenCh4 = 100, PerimStatusCh1 = 2)
# 
# abcd <- as.matrix(rbind(a, b, c, d))
# 
# predict(modFit, newdata=abcd)

print(modFit$finalModel)

# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

modFit <- train(Area ~ ., method="rpart", data=olive)

newdata = as.data.frame(t(colMeans(olive)))

predict(modFit, newdata)

# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                data=trainSA, method="glm", family="binomial")
                        
missClass = function(values,prediction){
        sum(((prediction > 0.5)*1) != values)/length(values)
}

missClass(trainSA$chd,predict(modFit,trainSA))
missClass(testSA$chd,predict(modFit,testSA))

# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y  <- as.factor(vowel.test$y)

set.seed(33833)
modFit <- train(y ~ ., method="rf", prox=TRUE, data=vowel.train)
importance <- varImp(modFit)
importance
