install.packages("rattle")
install.packages("randomForest")

library(caret)
library(rattle)
library(MLeval)
library(rpart)
library(randomForest)



#Decision tree with the whole questionnaire

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=5)

brfss.df.tree = train(HeartDiseaseorAttack ~ ., 
                  data=train, 
                  method="rpart", 
                  trControl = control,
                  tuneLength = 10)



#Results
fancyRpartPlot(brfss.df.tree$finalModel)

brfss.tree.pred <- predict(brfss.df.tree, newdata=test, type="raw")
confusionMatrix(brfss.tree.pred, factor(test$HeartDiseaseorAttack))

#Reference
#Prediction   No  Yes
#No  7587 2051
#Yes 1574 2759
#
#Accuracy : 0.7405          
#95% CI : (0.7332, 0.7478)
#No Information Rate : 0.6557          
#P-Value [Acc > NIR] : < 2.2e-16       
#
#Kappa : 0.4115          
#
#Mcnemar's Test P-Value : 2.659e-15       
#                                   
#   Sensitivity : 0.8282          
#            Specificity : 0.5736          
#         Pos Pred Value : 0.7872          
#         Neg Pred Value : 0.6367          
#             Prevalence : 0.6557          
#         Detection Rate : 0.5431          
#   Detection Prevalence : 0.6899          
#      Balanced Accuracy : 0.7009          
#                                          
#       'Positive' Class : No          
       
brfss.tree.ROC <- data.frame(predict(brfss.df.tree, test, type="prob"))
brfss.tree.ROC$obs <- as.factor(test$HeartDiseaseorAttack)
brfss.tree.ROC$Group <- 'brfss.tree'

rocCurve <- rbind(brfss.tree.ROC)
res <- evalm(rocCurve, title= "ROC Curve decision tree model without tuning")


#Tuned model for improved specificity
#Increase memory limit to overcome RAM limits
memory.limit(100000)
memory.limit()

#Do not run! Creates vector size of 13.1GB
#train$HeartDiseaseorAttack <- factor(train$HeartDiseaseorAttack)
#
#brfss.df.tree.tuned <- randomForest(HeartDiseaseorAttack ~ ., data=train, importance=TRUE,
#                        proximity=TRUE, ntree = 10, replace=TRUE)


tc <- trainControl(
    method = "boot",
    number = 20,
    classProbs = T,
    savePredictions = T
  )

gbmGrid <-  expand.grid(
  interaction.depth = c(1, 2, 5, 8),
  n.trees = seq(0, 60, by = 5),
  shrinkage = 0.1,
  n.minobsinnode = 1
)



brfss.df.tree.tuned = train(HeartDiseaseorAttack ~ ., 
                      data=train, 
                      method="gbm", 
                      trControl = tc,
                      tuneGrid = gbmGrid)

#Results
pretty(brfss.df.tree.tuned$bestTune)

brfss.tree.tuned.pred <- predict(brfss.df.tree.tuned, newdata=test, type="raw")
confusionMatrix(brfss.tree.tuned.pred, factor(test$HeartDiseaseorAttack))

#Confusion Matrix and Statistics
#
#Reference
#Prediction   No  Yes
#No  7890 1879
#Yes 1271 2931
#
#Accuracy : 0.7745          
#95% CI : (0.7675, 0.7814)
#No Information Rate : 0.6557          
#P-Value [Acc > NIR] : < 2.2e-16       
#
#Kappa : 0.4852          
#
#Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.8613          
#            Specificity : 0.6094          
#         Pos Pred Value : 0.8077          
#         Neg Pred Value : 0.6975          
#             Prevalence : 0.6557          
#         Detection Rate : 0.5647          
#   Detection Prevalence : 0.6992          
#      Balanced Accuracy : 0.7353          
#                                          
#       'Positive' Class : No 

brfss.tree.tuned.ROC <- data.frame(predict(brfss.df.tree.tuned, test, type="prob"))
brfss.tree.tuned.ROC$obs <- as.factor(test$HeartDiseaseorAttack)
brfss.tree.tuned.ROC$Group <- 'brfss.tree'

rocCurve <- rbind(brfss.tree.tuned.ROC)
res <- evalm(rocCurve, title= "ROC Curve decision tree model with tuning")

memory.limit(16115)

