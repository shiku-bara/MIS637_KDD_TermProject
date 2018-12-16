#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Decision Tree - C50
#  Team       : Team-1
#  Purpose    : This code will perform the following functionalities
#               1. Import the raw training dataset and create a working version
#               2. Perform the following data transformations required for CART
#                     a. Convert the nominal attributes to unordered factor variables 
#                     b. Convert the ordinal(categorical) attributes to ordered factor variables 
#                     c. Convert the continuous attributes to numeric variables
#               3. Build the entire decision tree - C50 model 

#******************************************************************************************************************
#rm(list=ls())

#**************************************************************
#Step 1 - Input the raw training dataset and create a working version

IBMEmpAtt_train_C50_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_training_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_train_C50_Base <- IBMEmpAtt_train_C50_Raw

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
  IBMEmpAtt_train_C50_Raw[[nominal_col]] <- factor(IBMEmpAtt_train_C50_Raw[[nominal_col]], ordered = FALSE)
}

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_train_C50_Raw[continuous_attributes] <- lapply(IBMEmpAtt_train_C50_Raw[continuous_attributes],as.numeric)

#ROSE Sampling
IBMEmpAtt_train_C50_ROSE <- ROSE(Attrition~., data = IBMEmpAtt_train_C50_Raw, seed=101)$data

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
  IBMEmpAtt_train_C50_ROSE[[nominal_col]] <- factor(IBMEmpAtt_train_C50_ROSE[[nominal_col]], ordered = FALSE)
}

rm(nominal_attributes)
rm(nominal_col)

#**************************************************************
#b. Combine certain levels in nominal variables

#-- Education
IBMEmpAtt_train_C50_ROSE$Education_C50 <- ifelse((IBMEmpAtt_train_C50_ROSE$Education == 5),4,IBMEmpAtt_train_C50_ROSE$Education)
IBMEmpAtt_train_C50_ROSE$Education_C50 <- factor(IBMEmpAtt_train_C50_ROSE$Education_C50,  ordered = FALSE, exclude=NULL)
#Original Labels => labels=c("1:Below College","2:College","3:Bachelor","4:Master","5:Doctor")  
#Transformed Labels => labels=c("1:Below College","2:College","3:Bachelor","4:Master/Doctor")

#-- EducationField
IBMEmpAtt_train_C50_ROSE$EducationField_C50 <- ifelse((IBMEmpAtt_train_C50_ROSE$EducationField == "Human Resources"),1,
                                                     ifelse((IBMEmpAtt_train_C50_ROSE$EducationField == "Life Sciences"),2,
                                                            ifelse((IBMEmpAtt_train_C50_ROSE$EducationField == "Marketing"),3,
                                                                   ifelse((IBMEmpAtt_train_C50_ROSE$EducationField == "Technical Degree"),4,5))))
IBMEmpAtt_train_C50_ROSE$EducationField_C50 <- factor(IBMEmpAtt_train_C50_ROSE$EducationField_C50, ordered = FALSE,exclude=NULL)    
#Original labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other")
#Transformed labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Technical Degree","5:Medical/Other")

#-- JobRole
IBMEmpAtt_train_C50_ROSE$JobRole_C50 <- ifelse((IBMEmpAtt_train_C50_ROSE$JobRole == "Human Resources"),1,
                                              ifelse((IBMEmpAtt_train_C50_ROSE$JobRole == "Laboratory Technician"),2,
                                                     ifelse((IBMEmpAtt_train_C50_ROSE$JobRole == "Research Scientist"),3,
                                                            ifelse((IBMEmpAtt_train_C50_ROSE$JobRole == "Sales Executive"),4,
                                                                   ifelse((IBMEmpAtt_train_C50_ROSE$JobRole == "Sales Representative"),5,6)))))

IBMEmpAtt_train_C50_ROSE$JobRole_C50 <- factor(IBMEmpAtt_train_C50_ROSE$JobRole_C50, ordered = FALSE, exclude=NULL)    
#Original labels="Human Resources","Laboratory Technician","Research Scientist","Sales Executive","Sales Representative"
#               "Healthcare Representative","Manager","Manufacturing Director","Research Director" 
#Transformed lables:
# 1 - "Human Resources"
# 2 - "Laboratory Technician" 
# 3 - "Research Scientist" 
# 4 - "Sales Executive"
# 5 - "Sales Representative"
# 6 - "Healthcare Representative"/"Manager"/"Manufacturing Director"/"Research Director" 

#-- JobLevel
IBMEmpAtt_train_C50_ROSE$JobLevel_C50 <- ifelse((IBMEmpAtt_train_C50_ROSE$JobLevel == 5),4, IBMEmpAtt_train_C50_ROSE$JobLevel)
IBMEmpAtt_train_C50_ROSE$JobLevel_C50 <- factor(IBMEmpAtt_train_C50_ROSE$JobLevel_C50, ordered = TRUE,exclude=NULL)    
#Original lables => labels=c(1,2,3,4,5)
#Transformed lables => labels=c(1,2,3,4/5)

#-- BusinessTravel
IBMEmpAtt_train_C50_ROSE$BusinessTravel_C50 <- ifelse((IBMEmpAtt_train_C50_ROSE$BusinessTravel == "Travel_Frequently"),1, 2)
IBMEmpAtt_train_C50_ROSE$BusinessTravel_C50 <- factor(IBMEmpAtt_train_C50_ROSE$BusinessTravel_C50, ordered = FALSE,exclude=NULL)    
#Original labels = Travel_Frequently, Travel_Rarely, Non-Travel
#Transformed labels = c(1:Travel_Frequently,2:Travel_Rarely/Non-Travel)

#**************************************************************
#c. Convert the ordinal(categorical) attributes to ordered factor variables  

ordinal_attributes <- c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating",
                        "RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")

for (ordinal_col in ordinal_attributes)
{
  ordinal_col <- noquote(ordinal_col)
  IBMEmpAtt_train_C50_ROSE[[ordinal_col]] <- factor(IBMEmpAtt_train_C50_ROSE[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#d. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_train_C50_ROSE[continuous_attributes] <- lapply(IBMEmpAtt_train_C50_ROSE[continuous_attributes],as.numeric)

rm(continuous_attributes)
#**************************************************************
#Step 4 - Build the entire decision tree - C50 model
# str(IBMEmpAtt_train_model_ROSE_C50)
# str(IBMEmpAtt_train_model_ROSE_C50_combined)

library('C50')

IBMEmpAtt_train_model_ROSE_C50 <- subset(IBMEmpAtt_train_C50_ROSE, select = -c(X,Education_C50,EducationField_C50,JobRole_C50,JobLevel_C50,BusinessTravel_C50))

IBMEmpAtt_train_model_ROSE_C50_combined <- subset(IBMEmpAtt_train_C50_ROSE, select = -c(X,Education,EducationField,JobRole,JobLevel,BusinessTravel))


IBMEmpAtt_C50.model <- C5.0(Attrition~.,data=IBMEmpAtt_train_model_ROSE_C50)

IBMEmpAtt_C50_combined.model <- C5.0(Attrition~.,data=IBMEmpAtt_train_model_ROSE_C50_combined)

#**************************************************************
#Step 6 - Import the test dataset 

IBMEmpAtt_test_C50_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_test_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_test_C50_Base <- IBMEmpAtt_test_C50_Raw

#**************************************************************
#Step 7 - Perform the same data transformations on test dataset which are performed on the training dataset
#a. Convert the nominal attributes to unordered factor variables 

nominal_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                        "OverTime")

for (nominal_col in nominal_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_test_C50_Base[[nominal_col]] <- factor(IBMEmpAtt_test_C50_Base[[nominal_col]], ordered = FALSE)
}

rm(nominal_attributes)
rm(nominal_col)

#**************************************************************
#b. Combine certain levels in nominal variables

#-- Education
IBMEmpAtt_test_C50_Base$Education_C50 <- ifelse((IBMEmpAtt_test_C50_Base$Education == 5),4,IBMEmpAtt_test_C50_Base$Education)
IBMEmpAtt_test_C50_Base$Education_C50 <- factor(IBMEmpAtt_test_C50_Base$Education_C50,  ordered = FALSE, exclude=NULL)
#Original Labels => labels=c("1:Below College","2:College","3:Bachelor","4:Master","5:Doctor")  
#Transformed Labels => labels=c("1:Below College","2:College","3:Bachelor","4:Master/Doctor")

#-- EducationField
IBMEmpAtt_test_C50_Base$EducationField_C50 <- ifelse((IBMEmpAtt_test_C50_Base$EducationField == "Human Resources"),1,
                                          ifelse((IBMEmpAtt_test_C50_Base$EducationField == "Life Sciences"),2,
                                                 ifelse((IBMEmpAtt_test_C50_Base$EducationField == "Marketing"),3,
                                                        ifelse((IBMEmpAtt_test_C50_Base$EducationField == "Technical Degree"),4,5))))
IBMEmpAtt_test_C50_Base$EducationField_C50 <- factor(IBMEmpAtt_test_C50_Base$EducationField_C50, ordered = FALSE,exclude=NULL)    
#Original labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other")
#Transformed labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Technical Degree","5:Medical/Other")

#-- JobRole
IBMEmpAtt_test_C50_Base$JobRole_C50 <- ifelse((IBMEmpAtt_test_C50_Base$JobRole == "Human Resources"),1,
                                   ifelse((IBMEmpAtt_test_C50_Base$JobRole == "Laboratory Technician"),2,
                                          ifelse((IBMEmpAtt_test_C50_Base$JobRole == "Research Scientist"),3,
                                                 ifelse((IBMEmpAtt_test_C50_Base$JobRole == "Sales Executive"),4,
                                                        ifelse((IBMEmpAtt_test_C50_Base$JobRole == "Sales Representative"),5,6)))))

IBMEmpAtt_test_C50_Base$JobRole_C50 <- factor(IBMEmpAtt_test_C50_Base$JobRole_C50, ordered = FALSE, exclude=NULL)    
#Original labels="Human Resources","Laboratory Technician","Research Scientist","Sales Executive","Sales Representative"
#               "Healthcare Representative","Manager","Manufacturing Director","Research Director" 
#Transformed lables:
# 1 - "Human Resources"
# 2 - "Laboratory Technician" 
# 3 - "Research Scientist" 
# 4 - "Sales Executive"
# 5 - "Sales Representative"
# 6 - "Healthcare Representative"/"Manager"/"Manufacturing Director"/"Research Director" 

#-- JobLevel
IBMEmpAtt_test_C50_Base$JobLevel_C50 <- ifelse((IBMEmpAtt_test_C50_Base$JobLevel == 5),4, IBMEmpAtt_test_C50_Base$JobLevel)
IBMEmpAtt_test_C50_Base$JobLevel_C50 <- factor(IBMEmpAtt_test_C50_Base$JobLevel_C50, ordered = TRUE,exclude=NULL)    
#Original lables => labels=c(1,2,3,4,5)
#Transformed lables => labels=c(1,2,3,4/5)

#-- BusinessTravel
IBMEmpAtt_test_C50_Base$BusinessTravel_C50 <- ifelse((IBMEmpAtt_test_C50_Base$BusinessTravel == "Travel_Frequently"),1, 2)
IBMEmpAtt_test_C50_Base$BusinessTravel_C50 <- factor(IBMEmpAtt_test_C50_Base$BusinessTravel_C50, ordered = FALSE,exclude=NULL)    
#Original labels = Travel_Frequently, Travel_Rarely, Non-Travel
#Transformed labels = c(1:Travel_Frequently,2:Travel_Rarely/Non-Travel)

#**************************************************************
#c. Convert the ordinal(categorical) attributes to ordered factor variables  

ordinal_attributes <- c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating",
                        "RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")

for (ordinal_col in ordinal_attributes)
{
  ordinal_col <- noquote(ordinal_col)
  IBMEmpAtt_test_C50_Base[[ordinal_col]] <- factor(IBMEmpAtt_test_C50_Base[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#d. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_test_C50_Base[continuous_attributes] <- lapply(IBMEmpAtt_test_C50_Base[continuous_attributes],as.numeric)

rm(continuous_attributes)

#**************************************************************
#Step 8 - Predict the target variable using CART model

IBMEmpAtt_test_model_C50_dataset <- subset(IBMEmpAtt_test_C50_Base, select = -c(X,Education_C50,EducationField_C50,JobRole_C50,JobLevel_C50,BusinessTravel_C50))

IBMEmpAtt_test_model_C50_dataset_combined <- subset(IBMEmpAtt_test_C50_Base, select = -c(X,Education,EducationField,JobRole,JobLevel,BusinessTravel))

# IBMEmpAtt_CART_predict <- ifelse((IBMEmpAtt_CART_predict_prob[,"Yes"] >= 0.5),"Yes","No")
# IBMEmpAtt_CART_predict <- factor(IBMEmpAtt_CART_predict)
#CART Model

IBMEmpAtt_C50_predict <-predict(IBMEmpAtt_C50.model,IBMEmpAtt_test_model_C50_dataset, type="class")
IBMEmpAtt_C50_predict_prob <-predict(IBMEmpAtt_C50.model,IBMEmpAtt_test_model_C50_dataset, type="prob")

IBMEmpAtt_C50_predict_combined <-predict(IBMEmpAtt_C50_combined.model,IBMEmpAtt_test_model_C50_dataset_combined, type="class")
#IBMEmpAtt_C50_predict_combined_prob <-predict(IBMEmpAtt_C50_combined.model,IBMEmpAtt_test_model_C50_dataset_combined, type="prob")

#**************************************************************
#Step 10 - Build the confusion Matrix

confusionMatrix(IBMEmpAtt_C50_predict,IBMEmpAtt_test_model_C50_dataset$Attrition, positive="Yes")

confusionMatrix(IBMEmpAtt_C50_predict_combined,IBMEmpAtt_test_model_C50_dataset$Attrition, positive="Yes")

rm(IBMEmpAtt_test_C50_Raw)
rm(IBMEmpAtt_test_C50_Base)
rm(IBMEmpAtt_train_C50_Raw)
rm(IBMEmpAtt_train_C50_Base)
rm(IBMEmpAtt_train_C50_ROSE)
rm(IBMEmpAtt_train_model_ROSE_C50)

#**************************************************************
#Step 10 - Build the ROC and PR Curve

library(PRROC)

C50_scores <- data.frame(IBMEmpAtt_C50_predict_prob[,"Yes"], IBMEmpAtt_test_model_C50_dataset$Attrition)
colnames(C50_scores) <- c("C50_prediction","Test_label")

C50_PR <- pr.curve(scores.class0=C50_scores[C50_scores$Test_label=="Yes",]$C50_prediction,
                    scores.class1=C50_scores[C50_scores$Test_label=="No",]$C50_prediction,
                    curve=T)
plot(C50_PR)

C50_ROC <- roc.curve(scores.class0=C50_scores[C50_scores$Test_label=="Yes",]$C50_prediction,
                      scores.class1=C50_scores[C50_scores$Test_label=="No",]$C50_prediction,
                      curve=T)
plot(C50_ROC)

#**************************************************************
#Step 11 - Important attributes as per C50

C5imp(IBMEmpAtt_C50.model, metric = "usage", pct = TRUE)
