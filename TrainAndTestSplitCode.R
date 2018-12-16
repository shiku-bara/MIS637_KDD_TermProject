#  Course     : Summer 2018 - MIS 637 - Knowledge Discovery Databases I 
#  Name       : Term Project - Training and Test Split
#  Team       : Team-1
#  Purpose    : This code will split the raw datafile into training and test data sets. Once split, both the files will be
#               written as csv files
#******************************************************************************************************************
rm(list=ls())
#**************************************************************
#Step 1 - Input the raw datafile

IBMEmpAtt_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition.csv", header=TRUE, stringsAsFactors=FALSE)
#str(IBMEmpAtt_Raw)
#**************************************************************
#Step 2 - Remove certain attributes which are either unary or has less predictive power

attributes_removed <- c("DailyRate","EmployeeCount","EmployeeNumber","HourlyRate","MonthlyRate","Over18","StandardHours")

IBMEmpAtt_Raw <- IBMEmpAtt_Raw[ ,!(names(IBMEmpAtt_Raw) %in% attributes_removed)]
#**************************************************************
#Step 3 - Set the seed and create training and test datasets

set.seed(501)
ind <- sample(2, nrow(IBMEmpAtt_Raw),replace=TRUE,prob=c(0.7,0.3))
IBMEmpAtt_train_dataset <- IBMEmpAtt_Raw[ind==1,]
IBMEmpAtt_test_dataset <- IBMEmpAtt_Raw[ind==2,]

#table(IBMEmpAtt_train_dataset$Attrition)
#table(IBMEmpAtt_test_dataset$Attrition)
#**************************************************************
#Step 4 - Write both training and test datasets into csv files

write.csv(IBMEmpAtt_train_dataset,file="~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_training_dataset.csv")
write.csv(IBMEmpAtt_test_dataset,file="~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_test_dataset.csv")

#**************************************************************
#Step 5 - Test whether the train and test datasets are getting imported properly

train_check <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_training_dataset.csv", header=TRUE, stringsAsFactors=FALSE)
test_check <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Raw_Training_Test datasets/IBM_Employee_Attrition_test_dataset.csv", header=TRUE, stringsAsFactors=FALSE)

#table(train_check$Attrition)
#table(test_check$Attrition)
