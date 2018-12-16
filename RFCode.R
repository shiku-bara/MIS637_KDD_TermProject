#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Decision Tree - Random Forest
#  Team       : Team-1
#  Purpose    : This code will perform the following functionalities
#               1. Import the raw training dataset and create a working version
#               2. Perform the following data transformations required for CART
#                     a. Convert the nominal attributes to unordered factor variables 
#                     b. Convert the ordinal(categorical) attributes to ordered factor variables 
#                     c. Convert the continuous attributes to numeric variables
#               3. Build the entire decision tree - Random Forest Model

#******************************************************************************************************************
#rm(list=ls())

#**************************************************************
#Step 1 - Input the raw training dataset and create a working version

IBMEmpAtt_train_RF_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_training_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_train_RF_Base <- IBMEmpAtt_train_RF_Raw

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
  IBMEmpAtt_train_RF_Raw[[nominal_col]] <- factor(IBMEmpAtt_train_RF_Raw[[nominal_col]], ordered = FALSE)
}

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_train_RF_Raw[continuous_attributes] <- lapply(IBMEmpAtt_train_RF_Raw[continuous_attributes],as.numeric)

#ROSE Sampling
IBMEmpAtt_train_RF_ROSE <- ROSE(Attrition~., data = IBMEmpAtt_train_RF_Raw, seed=101)$data

rm(categorical_attributes)
rm(continuous_attributes)

#**************************************************************
#Step 3 - Perform the following data transformations required for RF
#a. Convert the nominal attributes to unordered factor variables 

nominal_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                        "OverTime")

for (nominal_col in nominal_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_train_RF_ROSE[[nominal_col]] <- factor(IBMEmpAtt_train_RF_ROSE[[nominal_col]], ordered = FALSE)
}

rm(nominal_attributes)
rm(nominal_col)
#**************************************************************
#Step 3 - Perform the following data transformations required for RF
#b. Convert the ordinal(categorical) attributes to ordered factor variables  

ordinal_attributes <- c("EnvironmentSatisfaction","JobInvolvement","JobLevel","JobSatisfaction","PerformanceRating",
                        "RelationshipSatisfaction","StockOptionLevel","WorkLifeBalance")

for (ordinal_col in ordinal_attributes)
{
  ordinal_col <- noquote(ordinal_col)
  IBMEmpAtt_train_RF_ROSE[[ordinal_col]] <- factor(IBMEmpAtt_train_RF_ROSE[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#Step 3 - Perform the following data transformations required for RF
#c. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_train_RF_ROSE[continuous_attributes] <- lapply(IBMEmpAtt_train_RF_ROSE[continuous_attributes],as.numeric)

rm(continuous_attributes)
#**************************************************************
#Step 4 - Build the entire decision tree - RF Model

library('randomForest')

#str(IBMEmpAtt_train_Base)

IBMEmpAtt_train_model_ROSE_RF <- subset(IBMEmpAtt_train_RF_ROSE, select = -c(X))

IBMEmpAtt_RF.model <- randomForest(Attrition~.,data=IBMEmpAtt_train_model_ROSE_RF, mtry=1, ntree=500)#, importance=TRUE)

IBMEmpAtt_RF_predict <-predict(IBMEmpAtt_RF.model,IBMEmpAtt_test_model_RF_dataset, type="class")
confusionMatrix(IBMEmpAtt_RF_predict,IBMEmpAtt_test_model_RF_dataset$Attrition, positive="Yes")

#**************************************************************
#Step 6 - Import the test dataset 

IBMEmpAtt_test_RF_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_test_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
IBMEmpAtt_test_RF_Base <- IBMEmpAtt_test_RF_Raw

#**************************************************************
#Step 7 - Perform the same data transformations on test dataset which are performed on the training dataset
#a. Convert the nominal attributes to unordered factor variables 

nominal_attributes <- c("Attrition", "BusinessTravel","Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                        "OverTime")

for (nominal_col in nominal_attributes)
{
  nominal_col <- noquote(nominal_col)
  IBMEmpAtt_test_RF_Base[[nominal_col]] <- factor(IBMEmpAtt_test_RF_Base[[nominal_col]], ordered = FALSE)
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
  IBMEmpAtt_test_RF_Base[[ordinal_col]] <- factor(IBMEmpAtt_test_RF_Base[[ordinal_col]], ordered = TRUE)
}

rm(ordinal_attributes)
rm(ordinal_col)
#**************************************************************
#c. Convert the continuous attributes to numeric variables  

continuous_attributes <- c("Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                           "TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion",
                           "YearsWithCurrManager")

IBMEmpAtt_test_RF_Base[continuous_attributes] <- lapply(IBMEmpAtt_test_RF_Base[continuous_attributes],as.numeric)

rm(continuous_attributes)

#**************************************************************
#Step 8 - Predict the target variable using CART model

IBMEmpAtt_test_model_RF_dataset <- subset(IBMEmpAtt_test_RF_Base, select = -c(X))
# IBMEmpAtt_CART_predict <- ifelse((IBMEmpAtt_CART_predict_prob[,"Yes"] >= 0.5),"Yes","No")
# IBMEmpAtt_CART_predict <- factor(IBMEmpAtt_CART_predict)
#CART Model

IBMEmpAtt_RF_predict <-predict(IBMEmpAtt_RF.model,IBMEmpAtt_test_model_RF_dataset, type="class")
IBMEmpAtt_RF_predict_prob <-predict(IBMEmpAtt_RF.model,IBMEmpAtt_test_model_RF_dataset, type="prob")

#**************************************************************
#Step 10 - Build the confusion Matrix

confusionMatrix(IBMEmpAtt_RF_predict,IBMEmpAtt_test_model_RF_dataset$Attrition, positive="Yes")

rm(IBMEmpAtt_test_RF_Raw)
rm(IBMEmpAtt_test_RF_Base)
rm(IBMEmpAtt_train_RF_Raw)
rm(IBMEmpAtt_train_RF_Base)
rm(IBMEmpAtt_train_RF_ROSE)

#**************************************************************
#Step 10 - Build the ROC and PR Curve

library(PRROC)

RF_scores <- data.frame(IBMEmpAtt_RF_predict_prob[,"Yes"], IBMEmpAtt_test_model_RF_dataset$Attrition)
colnames(RF_scores) <- c("RF_prediction","Test_label")

RF_PR <- pr.curve(scores.class0=RF_scores[RF_scores$Test_label=="Yes",]$RF_prediction,
                    scores.class1=RF_scores[RF_scores$Test_label=="No",]$RF_prediction,
                    curve=T)
plot(RF_PR)

RF_ROC <- roc.curve(scores.class0=RF_scores[RF_scores$Test_label=="Yes",]$RF_prediction,
                      scores.class1=RF_scores[RF_scores$Test_label=="No",]$RF_prediction,
                      curve=T)
plot(RF_ROC)

#**************************************************************
#Step 12 - Important attributes as per CART

# importance(IBMEmpAtt_RF.model)
# varImpPlot(IBMEmpAtt_RF.model)

#IBMEmpAtt_RF.model$ntree
#500
#IBMEmpAtt_RF.model$mtry
#5

#**************************************************************
#Step 13 - Model Tuning

IBMEmpAtt_train_model_ROSE_RF$Department <- ifelse((IBMEmpAtt_train_model_ROSE_RF$Department == "Human Resources"),1,
                                      ifelse((IBMEmpAtt_train_model_ROSE_RF$Department == "Research & Development"),2,3))
IBMEmpAtt_train_model_ROSE_RF$Department <- factor(IBMEmpAtt_train_model_ROSE_RF$Department, ordered = FALSE, exclude=NULL)    
#labels=c("1:Human Resources","2:Research & Development","3:Sales")

IBMEmpAtt_train_model_ROSE_RF$JobRole <- ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Human Resources"),1,
                                            ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Laboratory Technician"),2,
                                            ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Research Scientist"),3,
                                                 ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Sales Executive"),4,
                                                        ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Sales Representative"),5,
                                                               ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Healthcare Representative"),6,
                                                                     ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Manager"),7,
                                                                           ifelse((IBMEmpAtt_train_model_ROSE_RF$JobRole == "Manufacturing Director"),8,9))))))))

IBMEmpAtt_train_model_ROSE_RF$JobRole <- factor(IBMEmpAtt_train_model_ROSE_RF$JobRole, ordered = FALSE, exclude=NULL)  
#Original labels="Human Resources","Laboratory Technician","Research Scientist","Sales Executive","Sales Representative"
#               "Healthcare Representative","Manager","Manufacturing Director","Research Director" 
#Transformed lables:
# 1 - "Human Resources"
# 2 - "Laboratory Technician" 
# 3 - "Research Scientist" 
# 4 - "Sales Executive"
# 5 - "Sales Representative"
# 6 - "Healthcare Representative"
# 7 - "Manager"
# 8 - "Manufacturing Director"
# 9 - "Research Director" 

IBMEmpAtt_train_model_ROSE_RF$BusinessTravel <- ifelse((IBMEmpAtt_train_model_ROSE_RF$BusinessTravel == "Travel_Frequently"),1,
                                                      ifelse((IBMEmpAtt_train_model_ROSE_RF$BusinessTravel == "Travel_Rarely"),2,3))

IBMEmpAtt_train_model_ROSE_RF$BusinessTravel <- factor(IBMEmpAtt_train_model_ROSE_RF$BusinessTravel, ordered = FALSE, exclude=NULL)    
#Original labels = Travel_Frequently, Travel_Rarely, Non-Travel
#Transformed labels = c(1:Travel_Frequently,2:Travel_Rarely/Non-Travel)

IBMEmpAtt_train_model_ROSE_RF$EducationField <- ifelse((IBMEmpAtt_train_model_ROSE_RF$EducationField == "Human Resources"),1,
                                          ifelse((IBMEmpAtt_train_model_ROSE_RF$EducationField == "Life Sciences"),2,
                                                 ifelse((IBMEmpAtt_train_model_ROSE_RF$EducationField == "Marketing"),3,
                                                        ifelse((IBMEmpAtt_train_model_ROSE_RF$EducationField == "Technical Degree"),4,
                                                               ifelse((IBMEmpAtt_train_model_ROSE_RF$EducationField == "Medical"),5,6)))))
IBMEmpAtt_train_model_ROSE_RF$EducationField <- factor(IBMEmpAtt_train_model_ROSE_RF$EducationField, ordered = FALSE, exclude=NULL)    
#Original labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other")
#Transformed labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Technical Degree","5:Medical/Other")

str(IBMEmpAtt_train_model_ROSE_RF)
library(caret)

IBMEmpAtt_train_model_ROSE_RF <- subset(IBMEmpAtt_train_model_ROSE_RF, select = -c(JobLevel))

control <- trainControl(method="repeatedcv", number=5, repeats=3,
                        search="grid", sampling = "rose", summaryFunction = twoClassSummary ,classProbs = T)

seed <- 101
tunegrid <- expand.grid(.mtry=c(1:15))
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000)) {
  set.seed(seed)
  fit <- train(Attrition~., data=IBMEmpAtt_train_model_ROSE_RF, method="rf", metric="roc", tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
