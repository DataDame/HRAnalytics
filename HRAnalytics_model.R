install.packages("rpart")
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
library(caret)
library(car)

setwd("~/Documents/BI-Dashboards")
training <- read.csv("HRAnalytics_Model.csv")

dim(training)
summary(training)
head(training)

scatterplotMatrix(~Shift+BaseRate+Grade+Training+LastEval+LastPromotion+Age|Status,data=training)
scatterplotMatrix(~Education+StartGrade+StartRate+StartEval+Experience|Status,data=training)
scatterplotMatrix(~SalaryProg+JobLevelProg+AvgEval+Tenure+CareerAge|Status,data=training)
scatterplotMatrix(~EngagementIndex+CommitmentIndex+Unscheduled|Status,data=training)

partition <- createDataPartition(y = training$Status, p = 0.8, list = FALSE)
training <- training[partition, ]
testing <- training[-partition, ]

partition <- createDataPartition(y = training$Status, p = 0.6, list = FALSE)
traindata <- training[partition, ]
testdata <- training[-partition, ]

tree <- rpart(Status ~ ZipCode+Sex+Eeoc+Shift+BaseRate+Grade+Training+LastEval+StartEval+Education+StartGrade+StartRate+Experience+EngagementIndex+CommitmentIndex+Unscheduled+Tenure+Age+EntryAge+CareerAge+CareerStage+ServiceLength+SalaryProg+JobLevelProg+AvgEval+LastPromotion+TotalEmployment, data=traindata)
tree
printcp(tree)
prp(tree)

model.rf<-randomForest(traindata[,3:29],traindata$Status)
model.rf

testdata_pred <- predict(model.rf, testdata)
confusionMatrix(testdata_pred, testdata$Status)

tree<-getTree(model.rf, k=1, labelVar=TRUE)
prp(tree)

varImpPlot(model.rf,sort=TRUE,n.var=27)

var<-varImp(model.rf)
var$vars<-rownames(var)
order(var$Overall,decreasing=TRUE)
var[order(var$Overall,decreasing=TRUE),1:2]

model <- train(traindata[,3:29],traindata$Status, method = "rf", prox = TRUE, trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE))
training_pred <- predict(model, traindata)
confusionMatrix(training_pred, traindata$Status)

testdata_pred <- predict(model, testdata)
confusionMatrix(testdata_pred, testdata$Status)

varImp(model)

testing_pred <- predict(model, testing)
confusionMatrix(testing_pred, testing$Status)
testing.pred<-cbind(testing,testing_pred)
subset(testing.pred, Status!=testing_pred, select=c("EmployeePPPER","Status","testing_pred"))
