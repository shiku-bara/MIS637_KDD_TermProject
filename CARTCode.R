#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Decision Tree - CART using Rpart library
#  Team       : Team-1
#  Purpose    : This code will perform the following functionalities
#               1. Import the raw training dataset and create a working version
#               2. Perform the following data transformations required for CART
#                     a. Convert the nominal attributes to unordered factor variables 
#                     b. Convert the ordinal(categorical) attributes to ordered factor variables 
#                     c. Convert the continuous attributes to numeric variables
#               3. Build the entire decision tree - CART model using Rpart library with complexity (CP) as -1 and incorporating 
#                  a loss matrix to penalize false negatives
#               4. Prune the tree by selecting the complexity from cross validation results (As a rule of thumb, it is best to prune
#                  a decision tree using the cp of smallest tree that is within one standard deviation of the tree with the 
#                  smallest cross-validation error [xerror])

#******************************************************************************************************************
rm(list=ls())

#**************************************************************
#Step 1 - Input the raw training dataset and create a working version

IBMEmpAtt_train_CART_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_training_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_train_CART_Base <- IBMEmpAtt_train_CART_Raw

#**************************************************************
#Step 2 - Do ROSE sampling on the base 
# str(IBMEmpAtt_train_Raw1)
# table(IBMEmpAtt_train_Raw1$Attrition)
# table(IBMEmpAtt_train_CART_Base$Attrition)
# table(IBMEmpAtt_train_CART_ROSE$Attrition)

#target_variable 

categorical_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                            "OverTime","EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating",
                            "RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")


for (nominal_col in categorical_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_train_CART_Raw[[nominal_col]] <- factor(IBMEmpAtt_train_CART_Raw[[nominal_col]], ordered = FALSE)
}

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_train_CART_Raw[continuous_attributes] <- lapply(IBMEmpAtt_train_CART_Raw[continuous_attributes],as.numeric)

#ROSE Sampling
IBMEmpAtt_train_CART_ROSE <- ROSE(Attrition~., data = IBMEmpAtt_train_CART_Raw, seed=101)$data

rm(categorical_attributes)
rm(continuous_attributes)

#**************************************************************
#Step 3 - Perform the following data transformations required for CART
#a. Convert the nominal attributes to unordered factor variables 

nominal_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                        "OverTime")

for (nominal_col in nominal_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_train_CART_ROSE[[nominal_col]] <- factor(IBMEmpAtt_train_CART_ROSE[[nominal_col]], ordered = FALSE)
}

rm(nominal_attributes)
rm(nominal_col)
#**************************************************************
#Step 3 - Perform the following data transformations required for CART
#b. Convert the ordinal(categorical) attributes to ordered factor variables  

ordinal_attributes <- c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating",
                        "RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")

for (ordinal_col in ordinal_attributes)
{
  ordinal_col <- noquote(ordinal_col)
  IBMEmpAtt_train_CART_ROSE[[ordinal_col]] <- factor(IBMEmpAtt_train_CART_ROSE[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#Step 3 - Perform the following data transformations required for CART
#c. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_train_CART_ROSE[continuous_attributes] <- lapply(IBMEmpAtt_train_CART_ROSE[continuous_attributes],as.numeric)

rm(continuous_attributes)
#**************************************************************
#Step 4 - Build the entire decision tree - CART model using Rpart library with complexity (CP) as -1 and incorporating 
#   a loss matrix to penalize false negatives

library(rpart)

IBMEmpAtt_train_model_ROSE_CART <- subset(IBMEmpAtt_train_CART_ROSE, select = -c(X))

#Create a loss matrix to penalize false negative in case of IBM Attrition. 
#lossmatrix <- matrix(c(0,1,5,0), byrow=TRUE, nrow=2)
# IBMEmpAtt_CART_withLoss.model <- rpart(Attrition~., data=IBMEmpAtt_train_model_CART, method = "class", 
#                                        control = rpart.control(cp = -1, xval=50),parms=list(loss=lossmatrix))

IBMEmpAtt_CART_ROSE.model <- rpart(Attrition~., data=IBMEmpAtt_train_model_ROSE_CART, method = "class", 
                              control = rpart.control(cp = -1, xval=50))


#rpart.plot(IBMEmpAtt_CART.model,extra=104, nn=TRUE)
#rpart.plot(IBMEmpAtt_CART_withLoss.model,extra=104, nn=TRUE)

#IBMEmpAtt_CART.model$variable.importance
#IBMEmpAtt_CART_withLoss.model$variable.importance

#printcp(IBMEmpAtt_CART.model)
#printcp(IBMEmpAtt_CART_withLoss.model)
#printcp(IBMEmpAtt_CART_ROSE.model)
#printcp(IBMEmpAtt_CART_withLoss_ROSE.model)


#**************************************************************
#Step 5 - Prune the tree by selecting the complexity from cross validation results (As a rule of thumb, it is best to prune
#   a decision tree using the cp of smallest tree that is within one standard deviation of the tree with the 
#   smallest cross-validation error [xerror])

#Without loss: cp = 0.0124225 or 0.0082817
#With loss: cp = 0.0267082 or 0.0149069
#ROSE without loss: cp = 0.0126050
#ROSE with loss: cp = 0.0154739

IBMEmpAtt_CART_ROSE_pruned.model <- rpart(Attrition~., data=IBMEmpAtt_train_model_ROSE_CART, method = "class", 
                                     control = rpart.control(cp = 0.0126050, xval=50))



#rpart.plot(IBMEmpAtt_CART_ROSE_pruned.model,extra=104, nn=TRUE)
#printcp(IBMEmpAtt_CART_ROSE_pruned.model)

#rm(lossmatrix)

#**************************************************************
#Step 6 - Import the test dataset 

IBMEmpAtt_test_CART_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_test_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_test_CART_Base <- IBMEmpAtt_test_CART_Raw

#**************************************************************
#Step 7 - Perform the same data transformations on test dataset which are performed on the training dataset
#a. Convert the nominal attributes to unordered factor variables 

nominal_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                        "OverTime")

for (nominal_col in nominal_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_test_CART_Base[[nominal_col]] <- factor(IBMEmpAtt_test_CART_Base[[nominal_col]], ordered = FALSE)
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
  IBMEmpAtt_test_CART_Base[[ordinal_col]] <- factor(IBMEmpAtt_test_CART_Base[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#c. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_test_CART_Base[continuous_attributes] <- lapply(IBMEmpAtt_test_CART_Base[continuous_attributes],as.numeric)

rm(continuous_attributes)

#**************************************************************
#Step 8 - Predict the target variable using CART model

IBMEmpAtt_test_model_CART_dataset <- subset(IBMEmpAtt_test_CART_Base, select = -c(X))
# IBMEmpAtt_CART_predict <- ifelse((IBMEmpAtt_CART_predict_prob[,"Yes"] >= 0.5),"Yes","No")
# IBMEmpAtt_CART_predict <- factor(IBMEmpAtt_CART_predict)
#CART Model

IBMEmpAtt_CART_predict_ROSE <-predict(IBMEmpAtt_CART_ROSE.model,IBMEmpAtt_test_model_CART_dataset, type="class")
IBMEmpAtt_CART_predict_pruned_ROSE <-predict(IBMEmpAtt_CART_ROSE_pruned.model,IBMEmpAtt_test_model_CART_dataset, type="class")
IBMEmpAtt_CART_predict_pruned_ROSE_prob <-predict(IBMEmpAtt_CART_ROSE_pruned.model,IBMEmpAtt_test_model_CART_dataset, type="prob")

#**************************************************************
#Step 10 - Build the confusion Matrix

confusionMatrix(IBMEmpAtt_CART_predict_ROSE,IBMEmpAtt_test_model_CART_dataset$Attrition, positive="Yes")
confusionMatrix(IBMEmpAtt_CART_predict_pruned_ROSE,IBMEmpAtt_test_model_CART_dataset$Attrition, positive="Yes")

rm(IBMEmpAtt_test_CART_Raw)
rm(IBMEmpAtt_test_CART_Base)
rm(IBMEmpAtt_train_CART_Raw)
rm(IBMEmpAtt_train_CART_Base)
rm(IBMEmpAtt_train_CART_ROSE)
rm(IBMEmpAtt_train_model_ROSE_CART)

#**************************************************************
#Step 10 - Build the ROC and PR Curve

library(PRROC)

CART_scores <- data.frame(IBMEmpAtt_CART_predict_pruned_ROSE_prob[,"Yes"], IBMEmpAtt_test_model_CART_dataset$Attrition)
colnames(CART_scores) <- c("CART_prediction","Test_label")

CART_PR <- pr.curve(scores.class0=CART_scores[CART_scores$Test_label=="Yes",]$CART_prediction,
               scores.class1=CART_scores[CART_scores$Test_label=="No",]$CART_prediction,
               curve=T)
plot(CART_PR)

CART_ROC <- roc.curve(scores.class0=CART_scores[CART_scores$Test_label=="Yes",]$CART_prediction,
                   scores.class1=CART_scores[CART_scores$Test_label=="No",]$CART_prediction,
                   curve=T)
plot(CART_ROC)

#**************************************************************
#Step 12 - Important attributes as per CART

IBMEmpAtt_CART_ROSE_pruned.model$variable.importance
  
  