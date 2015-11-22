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

