#  Course     : MIS 637 - Knowledge Discovery Databases I 
#  First Name : Shivakumar 
#  Last Name	: Barathi
#  CWID		    : 10430051
#  Date       : 05-June-2018
#  Type       : Project - Data Understanding Phase
#****************************************************************************
rm(list=ls())
#****************************************************************************
#Load Packages
library(dplyr)
install.packages("magrittr")
library(magrittr)
install.packages("ggplot2")
library(ggplot2)
#****************************************************************************
#Step 1 - Import the csv file
IBMEmpAtt_Raw <- read.csv("~/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/IBM_Employee_Attrition.csv", header=TRUE, stringsAsFactors=FALSE)

#Step 2 - Check the dimension and column names of the data frame
View(IBMEmpAtt_Raw)
dim(IBMEmpAtt_Raw)
str(IBMEmpAtt_Raw)
class(IBMEmpAtt_Raw$Attrition)
typeof(IBMEmpAtt_Raw)
attributes(IBMEmpAtt_Raw)
names(IBMEmpAtt_Raw)
colnames(IBMEmpAtt_Raw)
rownames(IBMEmpAtt_Raw)

#Step 3 - Check for the duplicates in the data frame
sum(duplicated(IBMEmpAtt_Raw))

#Step 4 - Use the UDF (fn_data_summary) to get the summary statistics of numerical variables and the distinct values of categorical 
# variables. It also includes the NULL check
data_summary_stats <- fn_data_summary(IBMEmpAtt_Raw)
write.csv(data_summary_stats,file="/Users/shiasi/Documents/Stevens - MS BIA/Summer 2018/MIS 637 - KDD/Project/Data_Summary_Stats.csv")

#Step 5 - Create working version of the dataset
IBMEmpAtt_v1 <- IBMEmpAtt_Raw 
IBMEmpAtt_v2 <- IBMEmpAtt_Raw %>% filter((YearsAtCompany==0)|(YearsAtCompany==1))


# Step 6 - Handling Missing Values
# The above summary dataframe outputs the count of null values in each column
table(is.na(IBMEmpAtt_v1)) # There are no missing values

# Step 7 - Handling misclassifications in categorical variables
fn_misclassification(IBMEmpAtt_v1)
"
Have to remove columns 'EmployeeCount', Over18', 'StandardHours' as all are unary columns
"

#Step 8 - Modify the necessary variables as factors
str(IBMEmpAtt_v1)
#IBMEmpAtt_v1$Attrition <- factor(IBMEmpAtt_v1$Attrition, exclude=NULL)
IBMEmpAtt_v1$Attrition <- factor(IBMEmpAtt_v1$Attrition, levels=c("No","Yes"), labels=c("1:No","2:Yes"), exclude=NULL)
IBMEmpAtt_v1$BusinessTravel <- factor(IBMEmpAtt_v1$BusinessTravel, levels=c("Travel_Rarely","Travel_Frequently","Non-Travel"), labels=c("1:Travel_Rarely","2:Travel_Frequently","3:Non-Travel"),exclude = NULL)
IBMEmpAtt_v1$Department <- factor(IBMEmpAtt_v1$Department,labels=c("1:Human Resources","2:Research&Development","3:Sales"),exclude = NULL)
IBMEmpAtt_v1$Education <- factor(IBMEmpAtt_v1$Education, levels=c(1,2,3,4,5), labels=c("1:Below College","2:College","3:Bachelor","4:Master","5:Doctor"),exclude = NULL)
IBMEmpAtt_v1$EducationField <- factor(IBMEmpAtt_v1$EducationField, levels=c("Human Resources","Life Sciences","Marketing","Medical","Technical Degree","Other"), labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other"), exclude=NULL)
IBMEmpAtt_v1$EnvironmentSatisfaction <- factor(IBMEmpAtt_v1$EnvironmentSatisfaction, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v1$Gender <- factor(IBMEmpAtt_v1$Gender,levels=c("Female","Male"),labels=c("1:Female","2:Male"), exclude=NULL)
IBMEmpAtt_v1$JobInvolvement <- factor(IBMEmpAtt_v1$JobInvolvement, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v1$JobLevel <- factor(IBMEmpAtt_v1$JobLevel, exclude=NULL)
IBMEmpAtt_v1$JobRole <- factor(IBMEmpAtt_v1$JobRole, exclude=NULL)
IBMEmpAtt_v1$JobSatisfaction <- factor(IBMEmpAtt_v1$JobSatisfaction, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v1$MaritalStatus <- factor(IBMEmpAtt_v1$MaritalStatus, levels=c("Single","Married","Divorced"), labels=c("1:Single","2:Married","3:Divorced"), exclude=NULL)
IBMEmpAtt_v1$OverTime <- factor(IBMEmpAtt_v1$OverTime, levels=c("No","Yes"), labels=c("1:No","2:Yes"), exclude=NULL)
IBMEmpAtt_v1$PerformanceRating <- factor(IBMEmpAtt_v1$PerformanceRating, levels=c(1,2,3,4),labels=c("1:Low"," 2:Good","3:Excellent","4:Outstanding"),exclude = NULL)
IBMEmpAtt_v1$RelationshipSatisfaction <- factor(IBMEmpAtt_v1$RelationshipSatisfaction, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v1$WorkLifeBalance <- factor(IBMEmpAtt_v1$WorkLifeBalance, levels=c(1,2,3,4), labels=c("1:Bad"," 2:Good","3:Better","4:Best"),exclude = NULL)
IBMEmpAtt_v1$StockOptionLevel <- factor(IBMEmpAtt_v1$StockOptionLevel, exclude=NULL)

IBMEmpAtt_v2$Attrition <- factor(IBMEmpAtt_v2$Attrition, levels=c("No","Yes"), labels=c("1:No","2:Yes"), exclude=NULL)
IBMEmpAtt_v2$BusinessTravel <- factor(IBMEmpAtt_v2$BusinessTravel, levels=c("Travel_Rarely","Travel_Frequently","Non-Travel"), labels=c("1:Travel_Rarely","2:Travel_Frequently","3:Non-Travel"),exclude = NULL)
IBMEmpAtt_v2$Department <- factor(IBMEmpAtt_v2$Department,labels=c("1:Human Resources","2:Research&Development","3:Sales"),exclude = NULL)
IBMEmpAtt_v2$Education <- factor(IBMEmpAtt_v2$Education, levels=c(1,2,3,4,5), labels=c("1:Below College","2:College","3:Bachelor","4:Master","5:Doctor"),exclude = NULL)
IBMEmpAtt_v2$EducationField <- factor(IBMEmpAtt_v2$EducationField, levels=c("Human Resources","Life Sciences","Marketing","Medical","Technical Degree","Other"), labels=c("1:Human Resources","2:Life Sciences","3:Marketing","4:Medical","5:Technical Degree","6:Other"), exclude=NULL)
IBMEmpAtt_v2$EnvironmentSatisfaction <- factor(IBMEmpAtt_v2$EnvironmentSatisfaction, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v2$Gender <- factor(IBMEmpAtt_v2$Gender,levels=c("Female","Male"),labels=c("1:Female","2:Male"), exclude=NULL)
IBMEmpAtt_v2$JobInvolvement <- factor(IBMEmpAtt_v2$JobInvolvement, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v2$JobLevel <- factor(IBMEmpAtt_v2$JobLevel, exclude=NULL)
IBMEmpAtt_v2$JobRole <- factor(IBMEmpAtt_v2$JobRole, exclude=NULL)
IBMEmpAtt_v2$JobSatisfaction <- factor(IBMEmpAtt_v2$JobSatisfaction, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v2$MaritalStatus <- factor(IBMEmpAtt_v2$MaritalStatus, levels=c("Single","Married","Divorced"), labels=c("1:Single","2:Married","3:Divorced"), exclude=NULL)
IBMEmpAtt_v2$OverTime <- factor(IBMEmpAtt_v2$OverTime, levels=c("No","Yes"), labels=c("1:No","2:Yes"), exclude=NULL)
IBMEmpAtt_v2$PerformanceRating <- factor(IBMEmpAtt_v2$PerformanceRating, levels=c(1,2,3,4),labels=c("1:Low"," 2:Good","3:Excellent","4:Outstanding"),exclude = NULL)
IBMEmpAtt_v2$RelationshipSatisfaction <- factor(IBMEmpAtt_v2$RelationshipSatisfaction, levels=c(1,2,3,4), labels=c("1:Low"," 2:Medium","3:High","4:Very High"),exclude = NULL)
IBMEmpAtt_v2$WorkLifeBalance <- factor(IBMEmpAtt_v2$WorkLifeBalance, levels=c(1,2,3,4), labels=c("1:Bad"," 2:Good","3:Better","4:Best"),exclude = NULL)
IBMEmpAtt_v2$StockOptionLevel <- factor(IBMEmpAtt_v2$StockOptionLevel, exclude=NULL)

summary(IBMEmpAtt_v1$WorkLifeBalance)

"


Numerical Variables
Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike,
StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear,YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,
YearsWithCurrManager
"
"
Integer Variables
EmployeeNumber 
"
#------------------------------------------------------------------------------------------------------------------------
# Step 7 - Handling outliers in numerical variables

hist(IBMEmpAtt_v1$Age,col = "blue",border = "black",xlab = "Age",ylab = "Counts",main = "Histogram of Age")
hist(IBMEmpAtt_v1$DailyRate,col = "blue",border = "black",xlab = "DailyRate",ylab = "Counts",main = "Histogram of DailyRate")
hist(IBMEmpAtt_v1$DistanceFromHome,col = "blue",border = "black",xlab = "DistanceFromHome",ylab = "Counts",main = "Histogram of DistanceFromHome")
hist(IBMEmpAtt_v1$HourlyRate,col = "blue",border = "black",xlab = "HourlyRate",ylab = "Counts",main = "Histogram of HourlyRate")
hist(IBMEmpAtt_v1$MonthlyIncome,col = "blue",border = "black",xlab = "MonthlyIncome",ylab = "Counts",main = "Histogram of MonthlyIncome")
hist(IBMEmpAtt_v1$MonthlyRate,col = "blue",border = "black",xlab = "MonthlyRate",ylab = "Counts",main = "Histogram of MonthlyRate")
hist(IBMEmpAtt_v1$NumCompaniesWorked,col = "blue",border = "black",xlab = "NumCompaniesWorked",ylab = "Counts",main = "Histogram of NumCompaniesWorked")
hist(IBMEmpAtt_v1$PercentSalaryHike,col = "blue",border = "black",xlab = "PercentSalaryHike",ylab = "Counts",main = "Histogram of PercentSalaryHike")
hist(IBMEmpAtt_v1$StockOptionLevel,col = "blue",border = "black",xlab = "StockOptionLevel",ylab = "Counts",main = "Histogram of StockOptionLevel")
hist(IBMEmpAtt_v1$TotalWorkingYears,col = "blue",border = "black",xlab = "TotalWorkingYears",ylab = "Counts",main = "Histogram of TotalWorkingYears")
hist(IBMEmpAtt_v1$TrainingTimesLastYear,col = "blue",border = "black",xlab = "TrainingTimesLastYear",ylab = "Counts",main = "Histogram of TrainingTimesLastYear")
hist(IBMEmpAtt_v1$YearsAtCompany,col = "blue",border = "black",xlab = "YearsAtCompany",ylab = "Counts",main = "Histogram of YearsAtCompany")
hist(IBMEmpAtt_v1$YearsInCurrentRole,col = "blue",border = "black",xlab = "YearsInCurrentRole",ylab = "Counts",main = "Histogram of YearsInCurrentRole")
hist(IBMEmpAtt_v1$YearsSinceLastPromotion,col = "blue",border = "black",xlab = "YearsSinceLastPromotion",ylab = "Counts",main = "Histogram of YearsSinceLastPromotion")
hist(IBMEmpAtt_v1$YearsWithCurrManager,col = "blue",border = "black",xlab = "YearsWithCurrManager",ylab = "Counts",main = "Histogram of YearsWithCurrManager")


# Make a box around the plot 
box(which = "plot",
lty = "solid", col="black")

par(mfrow = c(1,1))

boxplot(IBMEmpAtt_v1$Age,
        axes = FALSE,
        staplewex = 1,
        main="Age",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$Age)$stats, labels=boxplot.stats(IBMEmpAtt_v1$Age)$stats, x=1.3)

boxplot(IBMEmpAtt_v1$DistanceFromHome,
        axes = FALSE,
        staplewex = 1,
        main="DistanceFromHome",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$DistanceFromHome)$stats, labels=boxplot.stats(IBMEmpAtt_v1$DistanceFromHome)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$MonthlyIncome,
        axes = FALSE,
        staplewex = 1,
        main="MonthlyIncome",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$MonthlyIncome)$stats, labels=boxplot.stats(IBMEmpAtt_v1$MonthlyIncome)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$NumCompaniesWorked,
        axes = FALSE,
        staplewex = 1,
        main="NumCompaniesWorked",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$NumCompaniesWorked)$stats, labels=boxplot.stats(IBMEmpAtt_v1$NumCompaniesWorked)$stats, x=1.3)

boxplot(IBMEmpAtt_v1$PercentSalaryHike,
        axes = FALSE,
        staplewex = 1,
        main="PercentSalaryHike",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$PercentSalaryHike)$stats, labels=boxplot.stats(IBMEmpAtt_v1$PercentSalaryHike)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$StockOptionLevel,
        axes = FALSE,
        staplewex = 1,
        main="StockOptionLevel",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$StockOptionLevel)$stats, labels=boxplot.stats(IBMEmpAtt_v1$StockOptionLevel)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$TotalWorkingYears,
        axes = FALSE,
        staplewex = 1,
        main="TotalWorkingYears",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$TotalWorkingYears)$stats, labels=boxplot.stats(IBMEmpAtt_v1$TotalWorkingYears)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$TrainingTimesLastYear,
        axes = FALSE,
        staplewex = 1,
        main="TrainingTimesLastYear",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$TrainingTimesLastYear)$stats, labels=boxplot.stats(IBMEmpAtt_v1$TrainingTimesLastYear)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$YearsAtCompany,
        axes = FALSE,
        staplewex = 1,
        main="YearsAtCompany",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$YearsAtCompany)$stats, labels=boxplot.stats(IBMEmpAtt_v1$YearsAtCompany)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$YearsInCurrentRole,
        axes = FALSE,
        staplewex = 1,
        main="YearsInCurrentRole",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$YearsInCurrentRole)$stats, labels=boxplot.stats(IBMEmpAtt_v1$YearsInCurrentRole)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$YearsSinceLastPromotion,
        axes = FALSE,
        staplewex = 1,
        main="YearsSinceLastPromotion",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$YearsSinceLastPromotion)$stats, labels=boxplot.stats(IBMEmpAtt_v1$YearsSinceLastPromotion)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v1$YearsWithCurrManager,
        axes = FALSE,
        staplewex = 1,
        main="YearsWithCurrManager",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$YearsWithCurrManager)$stats, labels=boxplot.stats(IBMEmpAtt_v1$YearsWithCurrManager)$stats, x=1.3)

IBMEmpAtt_v1$YearsWithCurrManager_SQRT <- with(IBMEmpAtt_v1, YearsWithCurrManager^(1/2))


IBMEmpAtt_v1$EnvironmentSatisfaction <- as.numeric(IBMEmpAtt_v1$EnvironmentSatisfaction)
boxplot(IBMEmpAtt_v1$EnvironmentSatisfaction,
        axes = FALSE,
        staplewex = 1,
        main="YearsWithCurrManager",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v1$EnvironmentSatisfaction)$stats, labels=boxplot.stats(IBMEmpAtt_v1$EnvironmentSatisfaction)$stats, x=1.3)

(IBMEmpAtt_v1$YearsWithCurrManager)
hist(IBMEmpAtt_v1$YearsWithCurrManager_SQRT)

test <- IBMEmpAtt_v1 %>% filter(MonthlyIncome > 16555)
#Creating outlier flags
IBMEmpAtt_v1$MonthlyIncome_OF <- ifelse(IBMEmpAtt_v1$MonthlyIncome > 16555, 1, 0)
IBMEmpAtt_v1$NumCompaniesWorked_OF <- ifelse(IBMEmpAtt_v1$NumCompaniesWorked > 8, 1, 0)
IBMEmpAtt_v1$StockOptionLevel_OF <- ifelse(IBMEmpAtt_v1$StockOptionLevel > 2, 1, 0)
IBMEmpAtt_v1$TotalWorkingYears_OF <- ifelse(IBMEmpAtt_v1$TotalWorkingYears > 28, 1, 0)
IBMEmpAtt_v1$TrainingTimesLastYear_OF <- ifelse((IBMEmpAtt_v1$TrainingTimesLastYear < 1 | IBMEmpAtt_v1$TrainingTimesLastYear > 4), 1, 0)
IBMEmpAtt_v1$YearsAtCompany_OF <- ifelse(IBMEmpAtt_v1$YearsAtCompany > 18, 1, 0)
IBMEmpAtt_v1$YearsInCurrentRole_OF <- ifelse(IBMEmpAtt_v1$YearsInCurrentRole > 14, 1, 0)
IBMEmpAtt_v1$YearsSinceLastPromotion_OF <- ifelse(IBMEmpAtt_v1$YearsSinceLastPromotion > 7, 1, 0)
IBMEmpAtt_v1$YearsWithCurrManager_OF <- ifelse(IBMEmpAtt_v1$YearsWithCurrManager > 14, 1, 0)

table(IBMEmpAtt_v1$MonthlyIncome_OF)
table(IBMEmpAtt_v1$NumCompaniesWorked_OF)
table(IBMEmpAtt_v1$StockOptionLevel_OF)
table(IBMEmpAtt_v1$TotalWorkingYears_OF)
table(IBMEmpAtt_v1$TrainingTimesLastYear_OF )
table(IBMEmpAtt_v1$YearsAtCompany_OF)
table(IBMEmpAtt_v1$YearsInCurrentRole_OF)
table(IBMEmpAtt_v1$YearsSinceLastPromotion_OF)
table(IBMEmpAtt_v1$YearsWithCurrManager_OF)

IBMEmpAtt_v1$TotalOF <- rowSums(IBMEmpAtt_v1[,c("MonthlyIncome_OF","NumCompaniesWorked_OF","StockOptionLevel_OF","TotalWorkingYears_OF","TrainingTimesLastYear_OF","YearsAtCompany_OF","YearsInCurrentRole_OF","YearsSinceLastPromotion_OF","YearsWithCurrManager_OF")])

prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$TotalOF))

IBMEmpAtt_v2_WO <- IBMEmpAtt_v1 %>% filter(TotalOF < 1)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$Age,
        axes = FALSE,
        staplewex = 1,
        main="Age",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$Age)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$Age)$stats, x=1.3)

boxplot(IBMEmpAtt_v2_WO$DailyRate,
        axes = FALSE,
        staplewex = 1,
        main="DailyRate",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$DailyRate)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$DailyRate)$stats, x=1.3)

boxplot(IBMEmpAtt_v2_WO$DistanceFromHome,
        axes = FALSE,
        staplewex = 1,
        main="DistanceFromHome",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$DistanceFromHome)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$DistanceFromHome)$stats, x=1.3)

boxplot(IBMEmpAtt_v2_WO$HourlyRate,
        axes = FALSE,
        staplewex = 1,
        main="HourlyRate",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$HourlyRate)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$HourlyRate)$stats, x=1.3)

#Too many Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$MonthlyIncome,
        axes = FALSE,
        staplewex = 1,
        main="MonthlyIncome",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$MonthlyIncome)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$MonthlyIncome)$stats, x=1.3)

boxplot(IBMEmpAtt_v2_WO$MonthlyRate,
        axes = FALSE,
        staplewex = 1,
        main="MonthlyRate",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$MonthlyRate)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$MonthlyRate)$stats, x=1.3)

#Only 1 Outlier with this variable
boxplot(IBMEmpAtt_v2_WO$NumCompaniesWorked,
        axes = FALSE,
        staplewex = 1,
        main="NumCompaniesWorked",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$NumCompaniesWorked)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$NumCompaniesWorked)$stats, x=1.3)

boxplot(IBMEmpAtt_v2_WO$PercentSalaryHike,
        axes = FALSE,
        staplewex = 1,
        main="PercentSalaryHike",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$PercentSalaryHike)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$PercentSalaryHike)$stats, x=1.3)

#Outlier 1 outlier with this variable
boxplot(IBMEmpAtt_v2_WO$StockOptionLevel,
        axes = FALSE,
        staplewex = 1,
        main="StockOptionLevel",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$StockOptionLevel)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$StockOptionLevel)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$TotalWorkingYears,
        axes = FALSE,
        staplewex = 1,
        main="TotalWorkingYears",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$TotalWorkingYears)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$TotalWorkingYears)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$TrainingTimesLastYear,
        axes = FALSE,
        staplewex = 1,
        main="TrainingTimesLastYear",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$TrainingTimesLastYear)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$TrainingTimesLastYear)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$YearsAtCompany,
        axes = FALSE,
        staplewex = 1,
        main="YearsAtCompany",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$YearsAtCompany)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$YearsAtCompany)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$YearsInCurrentRole,
        axes = FALSE,
        staplewex = 1,
        main="YearsInCurrentRole",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$YearsInCurrentRole)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$YearsInCurrentRole)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$YearsSinceLastPromotion,
        axes = FALSE,
        staplewex = 1,
        main="YearsSinceLastPromotion",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$YearsSinceLastPromotion)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$YearsSinceLastPromotion)$stats, x=1.3)

#Outliers with this variable
boxplot(IBMEmpAtt_v2_WO$YearsWithCurrManager,
        axes = FALSE,
        staplewex = 1,
        main="YearsWithCurrManager",
        col="paleturquoise1")
text(y=boxplot.stats(IBMEmpAtt_v2_WO$YearsWithCurrManager)$stats, labels=boxplot.stats(IBMEmpAtt_v2_WO$YearsWithCurrManager)$stats, x=1.3)

#------------Exploring Categorical Variables
#Overlayed Bar Charts
par(mfrow = c(1,1))
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = IBMEmpAtt_v1$BusinessTravel,
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + 
  scale_x_discrete("BusinessTravel") + 
  scale_y_continuous("Percent") + 
  guides(fill=guide_legend(title="Attrition")) + 
  scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = IBMEmpAtt_v1$BusinessTravel,
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + 
  scale_x_discrete("BusinessTravel") + 
  scale_y_continuous("Percent") + 
  guides(fill=guide_legend(title="Attrition")) + 
  scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data=churn,
          aes(x = factor(churn$CustServ.Calls),
          fill = factor(churn$Churn.)), position = "fill") +
  scale_x_discrete("Customer Service Calls") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Churn")) + scale_fill_manual(values=c("blue", "red"))




#---------------------------
#A function which details the column names and the summary of each column
fn_data_summary <- function(dataframename) {
  df_name <- dataframename
  data_summary <- data.frame(matrix(ncol=10,nrow=0))
  colnames(data_summary) <- c("ColName","Type","Min","1stQu","Median","Mean","3rdQu","Max","NullCount","DistVals")
  colnames_vec <- colnames(df_name)
  
  for (colname in colnames_vec)
  {
    colname <- noquote(colname)
    if (class(df_name[[colname]]) == "integer")
    {
      sum_stat <- summary(df_name[[colname]])
      null_count <- sum(is.na(df_name[,colname]))
      row_vec <- c(colname,"Numerical",sum_stat[1],sum_stat[2],sum_stat[3],round(sum_stat[4], digits=2),sum_stat[5],sum_stat[6],null_count,".")
      row_df <- data.frame(lapply(row_vec, type.convert), stringsAsFactors=FALSE)
      colnames(row_df) <- c("ColName","Type","Min","1stQu","Median","Mean","3rdQu","Max","NullCount","DistVals")
      data_summary <- rbind(data_summary, row_df)
    }
    else
    {
      unique_val <- paste(as.vector(unique(df_name[,colname])), collapse=",")
      null_count <- sum(is.na(df_name[,colname]))
      row_vec <- c(colname,"Categorical",".",".",".",".",".",".",null_count,unique_val)
      row_df <- data.frame(lapply(row_vec, type.convert), stringsAsFactors=FALSE)
      colnames(row_df) <- c("ColName","Type","Min","1stQu","Median","Mean","3rdQu","Max","NullCount","DistVals")
      data_summary <- rbind(data_summary, row_df)
    }
  }
  rm(colnames_vec,colname,unique_val,sum_stat,null_count,row_vec,row_df)
  
  return (data_summary)
}

#---------------------------
#A function to check for misclassifications in categorical variables

fn_misclassification <- function(dataframename) {
  
  df_name <- dataframename
  
  colnames_vec <- colnames(df_name)
  
  for (colname in colnames_vec)
  {
    colname <- noquote(colname)
    if (class(df_name[[colname]]) == "character" | class(df_name[[colname]]) == "factor" )
    {
      cat(sprintf("----------------- %s ---------------------\n", colname))
      print(unique(df_name[,colname]))
      print(table(df_name[[colname]]))
      print(prop.table(table(df_name[[colname]])))
    }
  }
}

