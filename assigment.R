# Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible
# to collect a large amount of data about personal activity relatively
# inexpensively. These type of devices are part of the quantified self movement
# – a group of enthusiasts who take measurements about themselves regularly to
# improve their health, to find patterns in their behavior, or because they are
# tech geeks. One thing that people regularly do is quantify how much of a
# particular activity they do, but they rarely quantify how well they do it. In
# this project, your goal will be to use data from accelerometers on the belt,
# forearm, arm, and dumbell of 6 participants. They were asked to perform
# barbell lifts correctly and incorrectly in 5 different ways. More information
# is available from the website here: http://groupware.les.inf.puc-rio.br/har
# (see the section on the Weight Lifting Exercise Dataset).

# The goal of your project is to predict the manner in which they did the
# exercise. This is the "classe" variable in the training set. You may use any
# of the other variables to predict with. You should create a report describing
# how you built your model, how you used cross validation, what you think the
# expected out of sample error is, and why you made the choices you did. You
# will also use your prediction model to predict 20 different test cases.

# Data kindly made available by http://groupware.les.inf.puc-rio.br/har

library("caret")
set.seed(1208)

# Local file names
train <- "train.csv"
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

test <- "test.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Method = is download method, defaults to curl because this was created on a mac.
method="curl"

# Assume data is in working dir but will
# download data if it's not around
if(!file.exists(train)) {
    download.file(trainUrl,train,method=method)
}

if(!file.exists(test)) {
    download.file(testUrl,test,method="curl")
}

# Load data into var
trainData = read.csv("train.csv")
testData = read.csv("test.csv")

#Remove columns that contain NA or blank values
cleanTrainData[cleanTrainData==""] = NA
cleanTrainData = cleanTrainData[,colSums(is.na(cleanTrainData)) == 0]
#Remove columns that aren't likley to be useful e.g. username
cleanTrainData  = cleanTrainData[,-c(1:7)] 

#Create data partition to validate data
intrain<-createDataPartition(cleanTrainData$classe, p=0.7, list=FALSE)
trainDataTrain<-cleanTrainData[intrain,]
trainDataValidation<-cleanTrainData[-intrain,]


# Check to see we have a good split of data across predictor values
prop.table(table(trainDataTrain$classe))

# Setup training control to use cross validation
trainCtrl <- trainControl(method = "cv", number = 4, allowParallel = TRUE)

#Create model
model <- train(classe ~ .,data=trainDataTrain,method="rf",trControl=trainCtrl)

#Above to too slow
model <- randomForest(as.factor(classe) ~ ., data=trainDataTrain, importance=TRUE)

#Plot variabel importance
varImpPlot(model)

#Validate 
predictValidation <- predict(model, trainDataValidation)

#Accuracy
confusionMatrix(trainDataValidation$classe, predictValidation)




