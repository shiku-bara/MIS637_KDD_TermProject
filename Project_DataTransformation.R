#  Course     : MIS 637 - Knowledge Discovery Databases I 
#  First Name : Shivakumar 
#  Last Name	: Barathi
#  CWID		    : 10430051
#  Date       : 05-June-2018
#  Type       : Project - Data Transformation Phase
#******************************************************************************************************************
rm(list=ls())
#******************************************************************************************************************
#Load Packages
library(dplyr)
#install.packages("magrittr")
library(magrittr)
#install.packages("ggplot2")
library(ggplot2)
#******************************************************************************************************************
#Step 1 - Import the csv file
IBMEmpAtt_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/IBM_Employee_Attrition.csv", header=TRUE, stringsAsFactors=FALSE)

#******************************************************************************************************************
#Step 2 - Creating a base file from Raw file
IBMEmpAtt_Base <- IBMEmpAtt_Raw 
#str(IBMEmpAtt_Base)
#******************************************************************************************************************
#Step 3 - Factorizing the target variable 'Attrition'
#IBMEmpAtt_Base$Attrition_T %>% unique()
IBMEmpAtt_Base$Attrition_T <- ifelse((IBMEmpAtt_Base$Attrition == "No"),0,1)
IBMEmpAtt_Base$Attrition_T <- factor(IBMEmpAtt_Base$Attrition_T,exclude=NULL)
#labels=c("0:No","1:Yes")

#******************************************************************************************************************
#Step 4 - Transforming the categorical variables

IBMEmpAtt_Base$Education_T <- ifelse((IBMEmpAtt_Base$Education == 5),4,IBMEmpAtt_Base$Education)
IBMEmpAtt_Base$Education_T <- factor(IBMEmpAtt_Base$Education_T, exclude=NULL)
#Original Labels => labels=c("1:Below College","2:College","3:Bachelor","4:Master","5:Doctor")  
#Transformed Labels => labels=c("1:Below College","2:College","3:Bachelor","4:Master/Doctor")   

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBMEmpAtt_Base$EducationField_T <- ifelse((IBMEmpAtt_Base$EducationField == "Human Resources"),1,
                                        ifelse((IBMEmpAtt_Base$EducationField == "Life Sciences"),2,
                                               ifelse((IBMEmpAtt_Base$EducationField == "Marketing"),3,
                                                      ifelse((IBMEmpAtt_Base$EducationField == "Technical Degree"),4,5))))
IBMEmpAtt_Base$EducationField_T <- factor(IBMEmpAtt_Base$EducationField_T, exclude=NULL)    
#Original labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other")
#Transformed labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Technical Degree","5:Medical/Other")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBMEmpAtt_Base$EducationField_T <- ifelse((IBMEmpAtt_Base$EducationField == "Human Resources"),1,
                                          ifelse((IBMEmpAtt_Base$EducationField == "Life Sciences"),2,
                                                 ifelse((IBMEmpAtt_Base$EducationField == "Marketing"),3,
                                                        ifelse((IBMEmpAtt_Base$EducationField == "Technical Degree"),4,5))))
IBMEmpAtt_Base$EducationField_T <- factor(IBMEmpAtt_Base$EducationField_T, exclude=NULL)    
#Original labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other")
#Transformed labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Technical Degree","5:Medical/Other")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBMEmpAtt_Base$MaritalStatus_T <- ifelse((IBMEmpAtt_Base$MaritalStatus == "Single"),1,
                                          ifelse((IBMEmpAtt_Base$MaritalStatus == "Married"),2,3))
IBMEmpAtt_Base$MaritalStatus_T <- factor(IBMEmpAtt_Base$MaritalStatus_T, exclude=NULL)    
#labels=c("1:Single","2:Married","3:Divorced")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBMEmpAtt_Base$Department_T <- ifelse((IBMEmpAtt_Base$Department == "Human Resources"),1,
                                         ifelse((IBMEmpAtt_Base$Department == "Research & Development"),2,3))
IBMEmpAtt_Base$Department_T <- factor(IBMEmpAtt_Base$Department_T, exclude=NULL)    
#labels=c("1:Human Resources","2:Research & Development","3:Sales")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$JobRole_T %>% unique()

IBMEmpAtt_Base$JobRole_T <- ifelse((IBMEmpAtt_Base$JobRole == "Human Resources"),1,
                                          ifelse((IBMEmpAtt_Base$JobRole == "Laboratory Technician"),2,
                                                 ifelse((IBMEmpAtt_Base$JobRole == "Research Scientist"),3,
                                                        ifelse((IBMEmpAtt_Base$JobRole == "Sales Executive"),4,
                                                               ifelse((IBMEmpAtt_Base$JobRole == "Sales Representative"),5,6)))))

IBMEmpAtt_Base$JobRole_T <- factor(IBMEmpAtt_Base$JobRole_T, exclude=NULL)    
#Original labels="Human Resources","Laboratory Technician","Research Scientist","Sales Executive","Sales Representative"
#               "Healthcare Representative","Manager","Manufacturing Director","Research Director" 
#Transformed lables:
# 1 - "Human Resources"
# 2 - "Laboratory Technician" 
# 3 - "Research Scientist" 
# 4 - "Sales Executive"
# 5 - "Sales Representative"
# 6 - "Healthcare Representative"/"Manager"/"Manufacturing Director"/"Research Director" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$JobLevel_T %>% unique()
IBMEmpAtt_Base$JobLevel_T <- ifelse((IBMEmpAtt_Base$JobLevel == 5),4, IBMEmpAtt_Base$JobLevel)
IBMEmpAtt_Base$JobLevel_T <- factor(IBMEmpAtt_Base$JobLevel_T, exclude=NULL)    
#Original lables => labels=c(1,2,3,4,5)
#Transformed lables => labels=c(1,2,3,4/5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$JobInvolvement %>% unique()
IBMEmpAtt_Base$JobInvolvement <- factor(IBMEmpAtt_Base$JobInvolvement, exclude=NULL)    
#Lables => labels=c(1,2,3,4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$OverTime_T %>% unique()
IBMEmpAtt_Base$OverTime_T <- ifelse((IBMEmpAtt_Base$OverTime == "No"),1, 2)
IBMEmpAtt_Base$OverTime_T <- factor(IBMEmpAtt_Base$OverTime_T, exclude=NULL)    
#Lables => labels=c(1:No,2:Yes)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$WorkLifeBalance %>% unique()
IBMEmpAtt_Base$WorkLifeBalance <- factor(IBMEmpAtt_Base$WorkLifeBalance, exclude=NULL)    
#Lables => labels=c(1,2,3,4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$BusinessTravel_T %>% unique()
IBMEmpAtt_Base$BusinessTravel_T <- ifelse((IBMEmpAtt_Base$BusinessTravel == "Travel_Frequently"),1, 2)
IBMEmpAtt_Base$BusinessTravel_T <- factor(IBMEmpAtt_Base$BusinessTravel_T, exclude=NULL)    
#Original labels = Travel_Frequently, Travel_Rarely, Non-Travel
#Transformed labels = c(1:Travel_Frequently,2:Travel_Rarely/Non-Travel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$JobSatisfaction %>% unique()
IBMEmpAtt_Base$JobSatisfaction <- factor(IBMEmpAtt_Base$JobSatisfaction, exclude=NULL)    
#Lables => labels=c(1,2,3,4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$RelationshipSatisfaction %>% unique()
IBMEmpAtt_Base$RelationshipSatisfaction <- factor(IBMEmpAtt_Base$RelationshipSatisfaction, exclude=NULL)    
#Lables => labels=c(1,2,3,4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#IBMEmpAtt_Base$EnvironmentSatisfaction %>% unique()
IBMEmpAtt_Base$EnvironmentSatisfaction <- factor(IBMEmpAtt_Base$EnvironmentSatisfaction, exclude=NULL)    
#Lables => labels=c(1,2,3,4)

#******************************************************************************************************************
#Step 5 - Transforming the numerical variables

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("Age","Age_T")] %>% distinct()
#IBMEmpAtt_Base$Age_T %>% unique()

IBMEmpAtt_Base$Age_T <- ifelse(between(IBMEmpAtt_Base$Age,18,33),1,
                                   ifelse(between(IBMEmpAtt_Base$Age,34,45),2,3))

IBMEmpAtt_Base$Age_T <- factor(IBMEmpAtt_Base$Age_T, exclude=NULL) 

#Levels => ("1:18-33","2:34-45","3:46-60")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("Age","Age_T")] %>% distinct()
#IBMEmpAtt_Base$Age_T %>% unique()

IBMEmpAtt_Base$Age_T <- ifelse(between(IBMEmpAtt_Base$Age,18,33),1,
                               ifelse(between(IBMEmpAtt_Base$Age,34,45),2,3))

IBMEmpAtt_Base$Age_T <- factor(IBMEmpAtt_Base$Age_T, exclude=NULL) 

#Levels => ("1:18-33","2:34-45","3:46-60")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("NumCompaniesWorked","NumCompaniesWorked_T")] %>% distinct()
#IBMEmpAtt_Base$NumCompaniesWorked_T %>% unique()

IBMEmpAtt_Base$NumCompaniesWorked_T <- ifelse(between(IBMEmpAtt_Base$NumCompaniesWorked,0,1),1,
                                            ifelse(between(IBMEmpAtt_Base$NumCompaniesWorked,2,4),2,3))

IBMEmpAtt_Base$NumCompaniesWorked_T <- factor(IBMEmpAtt_Base$NumCompaniesWorked_T, exclude=NULL) 

#Levels => ("1:0-1","2:2-4","3:5-9")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("TotalWorkingYears","TotalWorkingYears_T")] %>% distinct()
#IBMEmpAtt_Base$TotalWorkingYears_T %>% unique()

IBMEmpAtt_Base$TotalWorkingYears_T <- ifelse(between(IBMEmpAtt_Base$TotalWorkingYears,0,12),1,
                                              ifelse(between(IBMEmpAtt_Base$TotalWorkingYears,13,26),2,3))

IBMEmpAtt_Base$TotalWorkingYears_T <- factor(IBMEmpAtt_Base$TotalWorkingYears_T, exclude=NULL) 

#Levels => ("1:0-12","2:13-26","3:27-40")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("YearsAtCompany","YearsAtCompany_T")] %>% distinct()
#IBMEmpAtt_Base$YearsAtCompany_T %>% unique()

IBMEmpAtt_Base$YearsAtCompany_T <- ifelse(between(IBMEmpAtt_Base$YearsAtCompany,0,1),1,
                                             ifelse(between(IBMEmpAtt_Base$YearsAtCompany,2,11),2,3))

IBMEmpAtt_Base$YearsAtCompany_T <- factor(IBMEmpAtt_Base$YearsAtCompany_T, exclude=NULL)

#Levels => ("1:0-1","2:2-11","3:12-40")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("YearsInCurrentRole","YearsInCurrentRole_T")] %>% distinct()
#IBMEmpAtt_Base$YearsInCurrentRole_T %>% unique()

IBMEmpAtt_Base$YearsInCurrentRole_T <- ifelse(between(IBMEmpAtt_Base$YearsInCurrentRole,0,1),1,
                                          ifelse(between(IBMEmpAtt_Base$YearsInCurrentRole,2,7),2,3))

IBMEmpAtt_Base$YearsInCurrentRole_T <- factor(IBMEmpAtt_Base$YearsInCurrentRole_T, exclude=NULL)

#Levels => ("1:0-1","2:2-7","3:8-18")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#test <- IBMEmpAtt_Base[,c("YearsWithCurrManager","YearsWithCurrManager_T")] %>% distinct()
#IBMEmpAtt_Base$YearsWithCurrManager_T %>% unique()

IBMEmpAtt_Base$YearsWithCurrManager_T <- ifelse(between(IBMEmpAtt_Base$YearsWithCurrManager,0,1),1,
                                              ifelse(between(IBMEmpAtt_Base$YearsWithCurrManager,2,7),2,3))

IBMEmpAtt_Base$YearsWithCurrManager_T <- factor(IBMEmpAtt_Base$YearsWithCurrManager_T, exclude=NULL)

#Levels => ("1:0-1","2:2-7","3:8-17")

#******************************************************************************************************************
#Step 6 - Removing the unwanted columns from the base file and create a train_test file
# removal_cols <- c("HourlyRate","DailyRate","MonthlyRate","EmployeeCount","Over18","StandardHours","Attrition","Gender","Education"
#   ,"EducationField","MaritalStatus","Department","JobRole","JobLevel","PerformanceRating","OverTime","BusinessTravel"
#   ,"StockOptionLevel","Age","DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike"
#   ,"TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion"
#   ,"YearsWithCurrManager")

removal_cols <- c("HourlyRate","DailyRate","MonthlyRate","EmployeeCount","Over18","StandardHours","Attrition","Gender","Education"
                  ,"EducationField","MaritalStatus","Department","JobRole","JobLevel","PerformanceRating","OverTime","BusinessTravel"
                  ,"StockOptionLevel")

IBMEmpAtt_train_test <- IBMEmpAtt_Base[ ,!(names(IBMEmpAtt_Base) %in% removal_cols)]

rm(removal_cols)

#******************************************************************************************************************
#Step 7 - Create training and testing sets
library(ROSE)
#install.packages("ROSE")

set.seed(501)
ind <- sample(2, nrow(IBMEmpAtt_train_test),replace=TRUE,prob=c(0.7,0.3))
IBMEmpAtt_train_random_withID <- IBMEmpAtt_train_test[ind==1,]
IBMEmpAtt_test_dataset_withID <- IBMEmpAtt_train_test[ind==2,]

IBMEmpAtt_train_random <- IBMEmpAtt_train_random_withID[ ,!(names(IBMEmpAtt_train_random_withID) %in% c("EmployeeNumber"))]
IBMEmpAtt_test_dataset <- IBMEmpAtt_test_dataset_withID[ ,!(names(IBMEmpAtt_test_dataset_withID) %in% c("EmployeeNumber"))]

table(IBMEmpAtt_train_random$Attrition_T)
table(IBMEmpAtt_test_dataset$Attrition_T)

rm(ind)

#Under-Over-Sampling
IBMEmpAtt_train_under_over <- ovun.sample(Attrition_T~., data = IBMEmpAtt_train_random, 
                                           method = 'both', p=0.4, seed=101)$data

#ROSE Method
IBMEmpAtt_train_ROSE <- ROSE(Attrition_T~., data = IBMEmpAtt_train_random, seed=101)$data

table(IBMEmpAtt_train_under_over$Attrition_T)
table(IBMEmpAtt_train_ROSE$Attrition_T)

#******************************************************************************************************************
#Step 8 - Decision Tree Model - CART
library(rpart)
library(rpart.plot)

IBMEmpAtt_CART_model.random <- rpart(Attrition_T~.,data=IBMEmpAtt_train_random)
IBMEmpAtt_CART_model.under_over <- rpart(Attrition_T~.,data=IBMEmpAtt_train_under_over)
IBMEmpAtt_CART_model.ROSE <- rpart(Attrition_T~.,data=IBMEmpAtt_train_ROSE)

rpart.plot(IBMEmpAtt_CART_model.random)
rpart.plot(IBMEmpAtt_CART_model.under_over)
rpart.plot(IBMEmpAtt_CART_model.ROSE)

# printcp(IBMEmpAtt_CART_model.random)
# printcp(IBMEmpAtt_CART_model.under_over)
# printcp(IBMEmpAtt_CART_model.ROSE)

# IBMEmpAtt_CART_model.random.pruned <- prune(IBMEmpAtt_CART_model.random, cp = IBMEmpAtt_CART_model.random$cptable[which.min(IBMEmpAtt_CART_model.random$cptable[,"xerror"]),"CP"])
# IBMEmpAtt_CART_model.under_over.pruned <- prune(IBMEmpAtt_CART_model.under_over, cp = IBMEmpAtt_CART_model.under_over$cptable[which.min(IBMEmpAtt_CART_model.under_over$cptable[,"xerror"]),"CP"])
# IBMEmpAtt_CART_model.ROSE.pruned <- prune(IBMEmpAtt_CART_model.ROSE, cp = IBMEmpAtt_CART_model.ROSE$cptable[which.min(IBMEmpAtt_CART_model.ROSE$cptable[,"xerror"]),"CP"])



IBMEmpAtt_CART_predict_random <-predict(IBMEmpAtt_CART_model.random,IBMEmpAtt_test_dataset, type="class")
IBMEmpAtt_CART_predict_under_over <-predict(IBMEmpAtt_CART_model.under_over,IBMEmpAtt_test_dataset, type="class")
IBMEmpAtt_CART_predict_ROSE <-predict(IBMEmpAtt_CART_model.ROSE,IBMEmpAtt_test_dataset, type="class")
# IBMEmpAtt_CART_predict_random_pruned <-predict(IBMEmpAtt_CART_model.random.pruned,IBMEmpAtt_test_dataset, type="class")
# IBMEmpAtt_CART_predict_under_over_pruned <-predict(IBMEmpAtt_CART_model.under_over.pruned,IBMEmpAtt_test_dataset, type="class")
# IBMEmpAtt_CART_predict_ROSE_pruned <-predict(IBMEmpAtt_CART_model.ROSE.pruned,IBMEmpAtt_test_dataset, type="class")

#IBMEmpAtt_CART_predict_cat<-ifelse(IBMEmpAtt_CART_predict[,1]<=.5,0,1)

#Confusion Matrix
table(Actual=IBMEmpAtt_test_dataset$Attrition_T,CART_Random=IBMEmpAtt_CART_predict_random)
table(Actual=IBMEmpAtt_test_dataset$Attrition_T,CART_UO=IBMEmpAtt_CART_predict_under_over)
table(Actual=IBMEmpAtt_test_dataset$Attrition_T,CART_Rose=IBMEmpAtt_CART_predict_ROSE)
# table(Actual=IBMEmpAtt_test_dataset$Attrition_T,CART_Random=IBMEmpAtt_CART_predict_random_pruned)
# table(Actual=IBMEmpAtt_test_dataset$Attrition_T,CART_UO=IBMEmpAtt_CART_predict_under_over_pruned)
# table(Actual=IBMEmpAtt_test_dataset$Attrition_T,CART_Rose=IBMEmpAtt_CART_predict_ROSE_pruned)

#AU-ROC value
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_CART_predict_random)
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_CART_predict_under_over)
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_CART_predict_ROSE)
# roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_CART_predict_random_pruned)
# roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_CART_predict_under_over_pruned)
# roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_CART_predict_ROSE_pruned)

# CART_wrong<-sum(IBMEmpAtt_test_IDRemoved$Attrition_T!=IBMEmpAtt_CART_predict)
# CART_error_rate<-CART_wrong/length(IBMEmpAtt_test_IDRemoved$Attrition_T)
# CART_error_rate

#CART_predict2<-predict(CART_class,test, type="class")
#CART_wrong2<-sum(test[,4]!=CART_predict2)
#CART_error_rate2<-CART_wrong2/length(test[,4])
#CART_error_rate2 


#******************************************************************************************************************
#Step 9 - Decision Tree Model - C50

library('C50')

IBMEmpAtt_C50_model.random <- C5.0(Attrition_T~.,data=IBMEmpAtt_train_random )
IBMEmpAtt_C50_model.under_over  <- C5.0(Attrition_T~.,data=IBMEmpAtt_train_under_over )
IBMEmpAtt_C50_model.ROSE <- C5.0(Attrition_T~.,data=IBMEmpAtt_train_ROSE )

summary(IBMEmpAtt_C50_model.random)
summary(IBMEmpAtt_C50_model.under_over)
summary(IBMEmpAtt_C50_model.ROSE)

#dev.off()
plot(IBMEmpAtt_C50_model.random)
plot(IBMEmpAtt_C50_model.under_over)
plot(IBMEmpAtt_C50_model.ROSE)


IBMEmpAtt_C50_predict_random <- predict(IBMEmpAtt_C50_model.random ,IBMEmpAtt_test_dataset , type="class" )
IBMEmpAtt_C50_predict_under_over <- predict(IBMEmpAtt_C50_model.under_over ,IBMEmpAtt_test_dataset , type="class" )
IBMEmpAtt_C50_predict_ROSE <- predict(IBMEmpAtt_C50_model.ROSE ,IBMEmpAtt_test_dataset , type="class" )

table(actual=IBMEmpAtt_test_dataset$Attrition_T,C50=IBMEmpAtt_C50_predict_random)
table(actual=IBMEmpAtt_test_dataset$Attrition_T,C50=IBMEmpAtt_C50_predict_under_over)
table(actual=IBMEmpAtt_test_dataset$Attrition_T,C50=IBMEmpAtt_C50_predict_ROSE)

roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_C50_predict_random)
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_C50_predict_under_over)
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_C50_predict_ROSE)

# wrong<- (IBMEmpAtt_test_IDRemoved$Attrition_T!=IBMEmpAtt_C50_predict)
# c50_rate<-sum(wrong)/length(IBMEmpAtt_test_IDRemoved$Attrition_T)
# c50_rate

#******************************************************************************************************************
#Step 9 - Ensemble Model - Randopm Forest

library('randomForest')

IBMEmpAtt_RF.random <- randomForest(Attrition_T~.,data=IBMEmpAtt_train_random)
IBMEmpAtt_RF.under_over <- randomForest(Attrition_T~.,data=IBMEmpAtt_train_under_over)
IBMEmpAtt_RF.ROSE <- randomForest(Attrition_T~.,data=IBMEmpAtt_train_ROSE)

summary(IBMEmpAtt_RF.random)
summary(IBMEmpAtt_RF.under_over)
summary(IBMEmpAtt_RF.ROSE)


#dev.off()
plot(IBMEmpAtt_RF.random)
plot(IBMEmpAtt_RF.under_over)
plot(IBMEmpAtt_RF.ROSE)

IBMEmpAtt_RF_predict_random <- predict(IBMEmpAtt_RF.random ,IBMEmpAtt_test_dataset , type="class" )
IBMEmpAtt_RF_predict_under_over <- predict(IBMEmpAtt_RF.under_over ,IBMEmpAtt_test_dataset , type="class" )
IBMEmpAtt_RF_predict_ROSE <- predict(IBMEmpAtt_RF.ROSE ,IBMEmpAtt_test_dataset , type="class" )

table(actual=IBMEmpAtt_test_dataset$Attrition_T,RF=IBMEmpAtt_RF_predict_random)
table(actual=IBMEmpAtt_test_dataset$Attrition_T,RF=IBMEmpAtt_RF_predict_under_over)
table(actual=IBMEmpAtt_test_dataset$Attrition_T,RF=IBMEmpAtt_RF_predict_ROSE)

roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_RF_predict_random)
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_RF_predict_under_over)
roc.curve(IBMEmpAtt_test_dataset$Attrition_T, IBMEmpAtt_RF_predict_ROSE)

# wrong<- (IBMEmpAtt_test_IDRemoved$Attrition_T!=IBMEmpAtt_RF_predict)
# RF_rate<-sum(wrong)/length(IBMEmpAtt_test_IDRemoved$Attrition_T)
# RF_rate

