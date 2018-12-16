#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Ensemble Model
#  Team       : Team-1
#  Purpose    : This code will perform the following functionalities

#******************************************************************************************************************
#rm(list=ls())
#**************************************************************
#Step 1 - Create an ensemble model 

IBMEmpAtt_Pred_DF <- data.frame(cbind(KNN_scores$Test_label,
                                      KNN_scores$KNN_prediction,
                                      CART_scores$CART_prediction,
                                      C50_scores$C50_prediction,
                                      RF_scores$RF_prediction))

colnames(IBMEmpAtt_Pred_DF) <- c("TargetLabel","KNN","CART","C50","RF")

IBMEmpAtt_Pred_DF$Ensemble <- with(IBMEmpAtt_Pred_DF, (rowSums(IBMEmpAtt_Pred_DF[,c("KNN","CART","RF")])/3))

IBMEmpAtt_Pred_DF$Ensemble_Label <- ifelse((IBMEmpAtt_Pred_DF$Ensemble >=0.5),2,1)

IBMEmpAtt_Pred_DF$Ensemble_Label <- factor(IBMEmpAtt_Pred_DF$Ensemble_Label)

IBMEmpAtt_Pred_DF$TargetLabel <- factor(IBMEmpAtt_Pred_DF$TargetLabel)

#**************************************************************
#Step 2 - Build the confusion Matrix

confusionMatrix(IBMEmpAtt_Pred_DF$Ensemble_Label,IBMEmpAtt_Pred_DF$TargetLabel, positive="2")

#**************************************************************
#Step 3 - Build the ROC and PR Curve

library(PRROC)


Ensemble_PR <- pr.curve(scores.class0=IBMEmpAtt_Pred_DF[IBMEmpAtt_Pred_DF$TargetLabel=="2",]$Ensemble,
                  scores.class1=IBMEmpAtt_Pred_DF[IBMEmpAtt_Pred_DF$TargetLabel=="1",]$Ensemble,
                  curve=T)
plot(RF_PR)

Ensemble_ROC <- roc.curve(scores.class0=IBMEmpAtt_Pred_DF[IBMEmpAtt_Pred_DF$TargetLabel=="2",]$Ensemble,
                          scores.class1=IBMEmpAtt_Pred_DF[IBMEmpAtt_Pred_DF$TargetLabel=="1",]$Ensemble,
                          curve=T)
plot(RF_ROC)

