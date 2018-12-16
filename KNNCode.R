#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - KNN
#  Team       : Team-1

#******************************************************************************************************************
#Step 1 - Import KNN dataset

IBMEmpAtt_KNN_predict_df <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Emp_Att_KNN_Output_1.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_KNN_predict <- factor(IBMEmpAtt_KNN_predict_df$predicted_value)

#**************************************************************
#Step 2 - Build the confusion Matrix

confusionMatrix(factor(IBMEmpAtt_KNN_predict_df$predicted_value),IBMEmpAtt_test_model_CART_dataset$Attrition, positive="Yes")

#**************************************************************
#Step 3 - Build the ROC and PR Curve
library(PRROC)

KNN_scores <- data.frame(IBMEmpAtt_KNN_predict_df[,"Yes"], IBMEmpAtt_test_model_CART_dataset$Attrition)
colnames(KNN_scores) <- c("KNN_prediction","Test_label")

KNN_PR <- pr.curve(scores.class0=KNN_scores[KNN_scores$Test_label=="Yes",]$KNN_prediction,
                    scores.class1=KNN_scores[KNN_scores$Test_label=="No",]$KNN_prediction,
                    curve=T)
plot(KNN_PR)

KNN_ROC <- roc.curve(scores.class0=KNN_scores[KNN_scores$Test_label=="Yes",]$KNN_prediction,
                      scores.class1=KNN_scores[KNN_scores$Test_label=="No",]$KNN_prediction,
                      curve=T)
plot(KNN_ROC)
