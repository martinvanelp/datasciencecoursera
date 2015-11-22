##
## ASSIGNMENT PRACTICAL MACHINE LEARNING
##

## INFO ##
# In this project, your goal will be to use data from accelerometers on 
# the belt, forearm, arm, and dumbell of 6 participants. They were asked 
# to perform barbell lifts correctly and incorrectly in 5 different ways. 
# More information is available from the website here: 
# http://groupware.les.inf.puc-rio.br/har 
# (see the section on the Weight Lifting Exercise Dataset). 

## TASK ##
# The goal of your project is to predict the manner in which they did the
# exercise. This is the "classe" variable in the training set. You may use
# any of the other variables to predict with. You should create a report 
# describing how you built your model, how you used cross validation, what 
# you think the expected out of sample error is, and why you made the 
# choices you did. You will also use your prediction model to predict 20 
# different test cases. 

# DATA
# fetch training and test data
if (!file.exists("data")) { dir.create("data") }
if (!file.exists("./data/pml-training.csv")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileUrl, destfile = "./data/pml-training.csv")
}
if (!file.exists("./data/pml-testing.csv")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileUrl, destfile = "./data/pml-testing.csv")
}

# load data
input <- read.csv("./data/pml-training.csv")

# separating predictors (usable data: few missing etc.) and outcome 
# (classe) from the other input, after exploratory data analysis
exercise <- as.data.frame(input[,8:160])

# subset of data for exploration
subset <- exercise[floor(runif(100, min = 1, max  = 19623)),]

# drop useless columns
exercise <- exercise[, -c(5:29, 43:52, 62:76, 80:94, 96:105, 118:132, 134:143)]

# MODELLING
library(caret)
library(polycor)
library(rattle)
library(rpart)
library(randomForest)

#check correlations
#cor <- sapply(exercise[,-153], 
#              function(x) hetcor(x, exercise[,153])$correlations[2])

# trying tree (final Accuracy = 0.5093939)
set.seed(1234)
treeFit <- train(classe ~ ., data = exercise, method = "rpart")
fancyRpartPlot(treeFit$finalModel)
treeFit

trainPred <- predict(treeFit, exercise)

confusionMatrix(trainPred, exercise$classe)


# trying random forest
set.seed(1234)
rfFit <- train(classe ~ ., data = exercise, method = "rf")
rfFit$finalModel

# Predictions
rfTestPred <- predict(rfFit, testing)
answers <- rfTestPred

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)

# better model
InTrain<-createDataPartition(y=training$classe,p=0.3,list=FALSE)
trainingX<-training[InTrain,]

rf_model<-train(classe~.,data=trainingX,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)

require(caret)
require(ggplot2)
require(randomForest)

training_URL<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_URL<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training<-read.csv(training_URL,na.strings=c("NA",""))
test<-read.csv(test_URL,na.strings=c("NA",""))

training<-training[,7:160]
test<-test[,7:160]

mostly_data<-apply(!is.na(training),2,sum)>19621
training<-training[,mostly_data]
test<-test[,mostly_data]
dim(training)

InTrain<-createDataPartition(y=training$classe,p=0.3,list=FALSE)
training1<-training[InTrain,]

rf_model<-train(classe~.,data=training1,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)
print(rf_model)

print(rf_model$finalModel)

## predictions
rfTestPred2 <- predict(rf_model, test)
answers <- rfTestPred2

pml_write_files(answers)
