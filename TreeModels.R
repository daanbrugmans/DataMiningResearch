library(caret)
install.packages("rattle")
library(rattle)
library(MLeval)
library(rpart)


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
control <-rpart.control(minsplit = 5000,
                         minbucket = round(5 / 3),
                         maxdepth = 8,
                         cp = 0,
                        xval = 5)

brfss.df.tree.tuned = train(HeartDiseaseorAttack ~ ., 
                      data=train, 
                      method="rpart", 
                      trControl = trainControl(method = "boot"),
                      control = control)

#Results
fancyRpartPlot(brfss.df.tree.tuned$finalModel)

brfss.tree.tuned.pred <- predict(brfss.df.tree.tuned, newdata=test, type="raw")
confusionMatrix(brfss.tree.tuned.pred, factor(test$HeartDiseaseorAttack))


brfss.tree.tuned.ROC <- data.frame(predict(brfss.df.tree.tuned, test, type="prob"))
brfss.tree.tuned.ROC$obs <- as.factor(test$HeartDiseaseorAttack)
brfss.tree.tuned.ROC$Group <- 'brfss.tree'

rocCurve <- rbind(brfss.tree.tuned.ROC)
res <- evalm(rocCurve, title= "ROC Curve decision tree model with tuning")


