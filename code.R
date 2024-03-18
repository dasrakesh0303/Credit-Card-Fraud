Main_data <-creditcard[1:50000,]
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(Main_data$Class, p = .50,
                                  list = FALSE,
                                  times = 1)
trainSplit <- Main_data[ splitIndex,]
testSplit <- Main_data[-splitIndex,]



library(DMwR)
library(ROSE)



balancedtrainSplit <- ovun.sample(Class~., data=trainSplit,
                                  N=nrow(trainSplit), p=0.5, 
                                  seed=5678, method="both")$data

prop.table(table(balancedtrainSplit$Class))


balancedtrainSplit$Class[balancedtrainSplit$Class == 0] <- 'No'
balancedtrainSplit$Class[balancedtrainSplit$Class == 1] <- 'Yes'


testSplit$Class[testSplit$Class == 0] <- 'No'
testSplit$Class[testSplit$Class == 1] <- 'Yes'

balancedtrainSplit$Class <- as.factor(balancedtrainSplit$Class)
levels(balancedtrainSplit$Class)
testSplit$Class <- as.factor(testSplit$Class)
levels(testSplit$Class)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

tbmodel <- train(Class ~ .,
                 data = balancedtrainSplit,
                 method = 'knn',
                 tuneLength = 20,
                 trControl = ctrl,
                 metric = "ROC")
plot(tbmodel)

pred1 <- predict(tbmodel, testSplit)

testSplit$Class<- as.factor(testSplit$Class)
confusionMatrix( testSplit$Class,pred1)
library(pROC)
rocclass<- as.numeric(testSplit$Class)
rocpred1<- as.numeric(pred1)
plot.roc(rocclass,rocpred1)
auc(rocclass,rocpred1)
