#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Decision Tree - CART using Rpart library
#  Team       : Team-1
#  Purpose    : This code will use various models (CART, C5.0, RandomForest, KNN, ensemble of all these models) to predict the target 
#               variable in test dataset
#******************************************************************************************************************
#rm(list=ls())

#**************************************************************
#Step 1 - Import the test dataset 

IBMEmpAtt_test_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_test_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_test_Base <- IBMEmpAtt_test_Raw

#**************************************************************
#Step 2 - Perform the same data transformations on test dataset which are performed on the training dataset
#a. Convert the nominal attributes to unordered factor variables 

nominal_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                        "OverTime")

for (nominal_col in nominal_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_test_Base[[nominal_col]] <- factor(IBMEmpAtt_test_Base[[nominal_col]], ordered = FALSE)
}

rm(nominal_attributes)
rm(nominal_col)
#**************************************************************
#b. Convert the ordinal(categorical) attributes to ordered factor variables  

ordinal_attributes <- c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating",
                        "RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")

for (ordinal_col in ordinal_attributes)
{
  ordinal_col <- noquote(ordinal_col)
  IBMEmpAtt_test_Base[[ordinal_col]] <- factor(IBMEmpAtt_test_Base[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#c. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_test_Base[continuous_attributes] <- lapply(IBMEmpAtt_test_Base[continuous_attributes],as.numeric)

rm(continuous_attributes)

#**************************************************************
#Step 3 - Predict the target variable using various models built in early phase

IBMEmpAtt_test_model_dataset <- subset(IBMEmpAtt_test_Base, select = -c(X))

#CART Model
IBMEmpAtt_CART_predict_prob <-predict(IBMEmpAtt_CART.model,IBMEmpAtt_test_model_dataset, type="prob")
IBMEmpAtt_CART_predict <- ifelse((IBMEmpAtt_CART_predict_prob[,"Yes"] >= 0.5),"Yes","No")
IBMEmpAtt_CART_predict <- factor(IBMEmpAtt_CART_predict)

IBMEmpAtt_CART_predict_withLoss <-predict(IBMEmpAtt_CART_withLoss.model,IBMEmpAtt_test_model_dataset, type="class")
IBMEmpAtt_CART_predict_pruned <-predict(IBMEmpAtt_CART_pruned.model,IBMEmpAtt_test_model_dataset, type="class")
IBMEmpAtt_CART_predict_withLoss_pruned <-predict(IBMEmpAtt_CART_withLoss_pruned.model,IBMEmpAtt_test_model_dataset, type="class")

#C5.0 Model
IBMEmpAtt_C50_predict <-predict(IBMEmpAtt_C50.model,IBMEmpAtt_test_model_dataset, type="class")

#Random Forest Model
IBMEmpAtt_RF_predict <-predict(IBMEmpAtt_RF.model,IBMEmpAtt_test_model_dataset, type="class")

#KNN


#**************************************************************
#Step 1 - Create an ensemble model 








