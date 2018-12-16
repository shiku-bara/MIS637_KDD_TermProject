#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Decision Tree - CART using Rpart library
#  Team       : Team-1
#  Purpose    : This code will evaluate the models. The following metrics are used
#               1. Error Rate (FN/(FN+TP))
#               2. AU-ROC
#******************************************************************************************************************
#rm(list=ls())

#**************************************************************
#Step 1 - Confusion Matrix of all the models

#CART
confusionMatrix_CART <- table(Actual=IBMEmpAtt_test_Base$Attrition,CART=IBMEmpAtt_CART_predict)
confusionMatrix_CART_withLoss <- table(Actual=IBMEmpAtt_test_Base$Attrition,CART_withLoss=IBMEmpAtt_CART_predict_withLoss)
confusionMatrix_CART_pruned <- table(Actual=IBMEmpAtt_test_Base$Attrition,CART_pruned=IBMEmpAtt_CART_predict_pruned)
confusionMatrix_CART_withLoss_pruned <- table(Actual=IBMEmpAtt_test_Base$Attrition,CART_withLoss_pruned=IBMEmpAtt_CART_predict_withLoss_pruned)

#C5.0
confusionMatrix_C50 <- table(Actual=IBMEmpAtt_test_Base$Attrition,C50=IBMEmpAtt_C50_predict)

#Random Forest
confusionMatrix_RF <- table(Actual=IBMEmpAtt_test_Base$Attrition,RF=IBMEmpAtt_RF_predict)

#KNN
confusionMatrix_KNN <- table(Actual=IBMEmpAtt_test_Base$Attrition,KNN=IBMEmpAtt_KNN_predict)

#Ensemble Method
confusionMatrix_Ensemble <- table(Actual=IBMEmpAtt_test_Base$Attrition,Ensemble=IBMEmpAtt_Ensemble_predict)


#**************************************************************
#Step 2 - Error Rate of all the models

#CART
confusionMat_CART_Vec <- as.vector(confusionMatrix_CART)
confusionMat_CART_Vec_withLoss <- as.vector(confusionMatrix_CART_withLoss)
confusionMat_CART_Vec_pruned <- as.vector(confusionMatrix_CART_pruned)
confusionMat_CART_Vec_withLoss_pruned <- as.vector(confusionMatrix_CART_withLoss_pruned)

confusionMat_CART_Vec[2] / (confusionMat_CART_Vec[4]+confusionMat_CART_Vec[2])
confusionMat_CART_Vec_withLoss[2] / (confusionMat_CART_Vec_withLoss[4]+confusionMat_CART_Vec_withLoss[2])
confusionMat_CART_Vec_pruned[2] / (confusionMat_CART_Vec_pruned[4]+confusionMat_CART_Vec_pruned[2])
confusionMat_CART_Vec_withLoss_pruned[2] / (confusionMat_CART_Vec_withLoss_pruned[4]+confusionMat_CART_Vec_withLoss_pruned[2])

#C5.0
confusionMat_C50_Vec <- as.vector(confusionMatrix_C50)

confusionMat_C50_Vec[2] / (confusionMat_C50_Vec[4]+confusionMat_C50_Vec[2])

#Random Forest
confusionMat_RF_Vec <- as.vector(confusionMatrix_RF)

confusionMat_RF_Vec[2] / (confusionMat_RF_Vec[4]+confusionMat_RF_Vec[2])

#KNN
confusionMat_KNN_Vec <- as.vector(confusionMatrix_KNN)

confusionMat_KNN_Vec[2] / (confusionMat_KNN_Vec[4]+confusionMat_KNN_Vec[2])

#Ensemble Method
confusionMat_Ensemble_Vec <- as.vector(confusionMatrix_Ensemble)

confusionMat_Ensemble_Vec[2] / (confusionMat_Ensemble_Vec[4]+confusionMat_Ensemble_Vec[2])

#**************************************************************
#Step 3 - AU-ROC of all the models

#CART
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_CART_predict)
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_CART_predict_withLoss)
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_CART_predict_pruned)
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_CART_predict_withLoss_pruned)

#C5.0
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_C50_predict)

#Random Forest
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_RF_predict)

#KNN
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_KNN_predict)

#Ensemble Method
roc.curve(IBMEmpAtt_test_Base$Attrition, IBMEmpAtt_Ensemble_predict)

#**************************************************************
#Step 4 - AU-ROC plot of all the models

library(ROCR)
# List of predictions
preds_list <- list(IBMEmpAtt_Pred_DF$IBMEmpAtt_KNN_predict,
                   IBMEmpAtt_Pred_DF$IBMEmpAtt_Ensemble_predict,
                   IBMEmpAtt_Pred_DF$IBMEmpAtt_RF_predict,
                   IBMEmpAtt_Pred_DF$IBMEmpAtt_CART_predict_withLoss_pruned,
                   IBMEmpAtt_Pred_DF$IBMEmpAtt_C50_predict
                   )

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(IBMEmpAtt_Pred_DF$V2), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m),lwd=3,print.auc=TRUE, main = "TestSet ROC Curves")
legend(x = "bottomright", 
       legend = c("KNN: 0.896","Ensemble: 0.809","Random Forest: 0.761","CART: 0.704", "C50: 0.673"),
       fill = 1:m,cex=0.8
       )
lines(x = c(0,100), y = c(0,100),col="brown")
  


# Plot & compare ROC curves
# 
# We conclude this course by plotting the ROC curves for all the models (one from each chapter) on the same graph. The ROCR package provides the prediction() and performance() functions which generate the data required for plotting the ROC curve, given a set of predictions and actual (true) values.
# 
# The more "up and to the left" the ROC curve of a model is, the better the model. The AUC performance metric is literally the "Area Under the ROC Curve", so the greater the area under this curve, the higher the AUC, and the better-performing the model is.
# The ROCR package can plot multiple ROC curves on the same plot if you plot several sets of predictions as a list.
# 
# The prediction() function takes as input a list of prediction vectors (one per model) and a corresponding list of true values (one per model, though in our case the models were all evaluated on the same test set so they all have the same set of true values). The prediction() function returns a "prediction" object which is then passed to the performance() function.
# The performance() function generates the data necessary to plot the curve from the "prediction" object. For the ROC curve, you will also pass along two measures, "tpr" and "fpr".
# Once you have the "performance" object, you can plot the ROC curves using the plot() method. We will add some color to the curves and a legend so we can tell which curves belong to which algorithm.

#**************************************************************
#Step 5 - Significant attrition attributes as per each model

#CART
IBMEmpAtt_CART.model$variable.importance
IBMEmpAtt_CART_withLoss.model$variable.importance
IBMEmpAtt_CART_pruned.model$variable.importance
IBMEmpAtt_CART_withLoss_pruned.model$variable.importance

table(Actual=IBMEmpAtt_test_Base$Attrition,CART=IBMEmpAtt_CART_predict)

IBMEmpAtt_test_labels <- (IBMEmpAtt_test_Base$Attrition)

CXX <- data.frame(cbind(IBMEmpAtt_test_labels,IBMEmpAtt_CART_predict))
CXX$X1 <- factor(CXX$X1)
CXX$X2 <- factor(CXX$X2)

str(CXX)
library(caret)
confusionMatrix(IBMEmpAtt_CART_predict,IBMEmpAtt_test_labels, positive="Yes")


preds_list <- list(IBMEmpAtt_CART_predict_prob[,"Yes"])
actuals_list <- list(CXX$IBMEmpAtt_test_labels)

pred <- prediction(preds_list, actuals_list)
perf <- performance(pred, measure="prec", x.measure="rec")
test <- data.frame(cut=perf@alpha.values[[1]],recall=perf@x.values[[1]], precision=perf@y.values[[1]])
plot(perf)

perf_r <- performance(pred, measure="tpr", x.measure="fpr")
test <- data.frame(cut=perf@alpha.values[[1]],fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
plot(perf_r,lwd=3, col="orange" )

install.packages("PRROC")
library(PRROC)


prscores <- data.frame(IBMEmpAtt_CART_predict_prob[,"Yes"], CXX$IBMEmpAtt_test_labels)
colnames(prscores) <- c("prediction","label")
str(prscores)
pr <- pr.curve(scores.class0=prscores[prscores$label=="2",]$prediction,
               scores.class1=prscores[prscores$label=="1",]$prediction,
               curve=T)
plot(pr)

shroc <- roc.curve(scores.class0=prscores[prscores$label=="2",]$prediction,
               scores.class1=prscores[prscores$label=="1",]$prediction,
               curve=T)
plot(shroc)

y <- as.data.frame(pr$curve)
ggplot(y, aes(y$V1, y$V2))+geom_path()+ylim(0,1)

