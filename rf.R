library(caret)
library(ranger)

tbmodelrf <- train(Class ~ .,
                 data = balancedtrainSplit,
                 method = 'ranger',
                 tuneLength = 20,
                 trControl = ctrl,
                 metric = "ROC")
plot(tbmodelrf)

pred2 <- predict(tbmodelrf, testSplit)
confusionMatrix( testSplit$Class,pred2)

rocpred2<- as.numeric(pred2)
plot.roc(rocclass,rocpred2)
auc(rocclass,rocpred2)
