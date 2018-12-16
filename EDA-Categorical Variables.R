#  Course     : MIS 637 - Knowledge Discovery Databases I 
#  First Name : Shivakumar 
#  Last Name	: Barathi
#  CWID		    : 10430051
#  Date       : 05-June-2018
#  Type       : Project - EDA - Categorical Variables 
#****************************************************************************

#List of Categorical Variables
#Gender, Education, EducationField, MaritalStatus, Department, JobRole, JobLevel, JobInvolvement, PerformanceRating, OverTime
#WorkLifeBalance, BusinessTravel, StockOptionLevel, JobSatisfaction, RelationshipSatisfaction, EnvironmentSatisfaction

#Understand the association between each predictor variable and the target variable (Attrition) using 
#Overlay Histogram and Normalized Overlay Histogram

IBMEmpAtt_v1 <- IBMEmpAtt_Raw 
IBMEmpAtt_v2 <- IBMEmpAtt_Raw %>% filter((YearsAtCompany==0)|(YearsAtCompany==1))

str(IBMEmpAtt_v1)
prop.table(table(IBMEmpAtt_v1$Attrition))
#1:No - 83.88%        2:yes - 16.12%

"******************************  Gender ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Gender),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("Gender") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Gender),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("Gender") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Gender, dnn=c("Attrition", "Gender")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Gender, dnn=c("Attrition", "Gender")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$Gender, dnn=c("Attrition", "Gender")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$Gender, dnn=c("Attrition", "Gender")), margin = 2),4)*100

"******************************  Education ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Education),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("Education") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Education),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("Education") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Education, dnn=c("Attrition", "Education")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Education, dnn=c("Attrition", "Education")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$Education, dnn=c("Attrition", "Education")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$Education, dnn=c("Attrition", "Education")), margin = 2),4)*100

"******************************  EducationField ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$EducationField),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("EducationField") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$EducationField),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("EducationField") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$EducationField, dnn=c("Attrition", "EducationField")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$EducationField, dnn=c("Attrition", "EducationField")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$EducationField, dnn=c("Attrition", "EducationField")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$EducationField, dnn=c("Attrition", "EducationField")), margin = 2),4)*100

"******************************  MaritalStatus ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$MaritalStatus),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("MaritalStatus") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$MaritalStatus),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("MaritalStatus") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$MaritalStatus, dnn=c("Attrition", "MaritalStatus")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$MaritalStatus, dnn=c("Attrition", "MaritalStatus")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$MaritalStatus, dnn=c("Attrition", "MaritalStatus")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$MaritalStatus, dnn=c("Attrition", "MaritalStatus")), margin = 2),4)*100

"******************************  Department ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Department),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("Department") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Department),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("Department") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Department, dnn=c("Attrition", "Department")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Department, dnn=c("Attrition", "Department")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$Department, dnn=c("Attrition", "Department")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$Department, dnn=c("Attrition", "Department")), margin = 2),4)*100

"******************************  JobRole ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobRole),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("JobRole") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobRole),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("JobRole") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobRole, dnn=c("Attrition", "JobRole")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobRole, dnn=c("Attrition", "JobRole")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobRole, dnn=c("Attrition", "JobRole")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobRole, dnn=c("Attrition", "JobRole")), margin = 2),4)*100

"******************************  JobLevel ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobLevel),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("JobLevel") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobLevel),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("JobLevel") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobLevel, dnn=c("Attrition", "JobLevel")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobLevel, dnn=c("Attrition", "JobLevel")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobLevel, dnn=c("Attrition", "JobLevel")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobLevel, dnn=c("Attrition", "JobLevel")), margin = 2),4)*100

"******************************  JobInvolvement ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobInvolvement),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("JobInvolvement") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobInvolvement),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("JobInvolvement") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobInvolvement, dnn=c("Attrition", "JobInvolvement")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobInvolvement, dnn=c("Attrition", "JobInvolvement")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobInvolvement, dnn=c("Attrition", "JobInvolvement")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobInvolvement, dnn=c("Attrition", "JobInvolvement")), margin = 2),4)*100

"******************************  PerformanceRating ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$PerformanceRating),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("PerformanceRating") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$PerformanceRating),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("PerformanceRating") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$PerformanceRating, dnn=c("Attrition", "PerformanceRating")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$PerformanceRating, dnn=c("Attrition", "PerformanceRating")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$PerformanceRating, dnn=c("Attrition", "PerformanceRating")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$PerformanceRating, dnn=c("Attrition", "PerformanceRating")), margin = 2),4)*100

"******************************  OverTime ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$OverTime),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("OverTime") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$OverTime),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("OverTime") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$OverTime, dnn=c("Attrition", "OverTime")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$OverTime, dnn=c("Attrition", "OverTime")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$OverTime, dnn=c("Attrition", "OverTime")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$OverTime, dnn=c("Attrition", "OverTime")), margin = 2),4)*100

"******************************  WorkLifeBalance ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$WorkLifeBalance),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("WorkLifeBalance") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$WorkLifeBalance),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("WorkLifeBalance") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$WorkLifeBalance, dnn=c("Attrition", "WorkLifeBalance")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$WorkLifeBalance, dnn=c("Attrition", "WorkLifeBalance")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$WorkLifeBalance, dnn=c("Attrition", "WorkLifeBalance")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$WorkLifeBalance, dnn=c("Attrition", "WorkLifeBalance")), margin = 2),4)*100

"******************************  BusinessTravel ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$BusinessTravel),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("BusinessTravel") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$BusinessTravel),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("BusinessTravel") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$BusinessTravel, dnn=c("Attrition", "BusinessTravel")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$BusinessTravel, dnn=c("Attrition", "BusinessTravel")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$BusinessTravel, dnn=c("Attrition", "BusinessTravel")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$BusinessTravel, dnn=c("Attrition", "BusinessTravel")), margin = 2),4)*100

"******************************  StockOptionLevel ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$StockOptionLevel),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("StockOptionLevel") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$StockOptionLevel),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("StockOptionLevel") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$StockOptionLevel, dnn=c("Attrition", "StockOptionLevel")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$StockOptionLevel, dnn=c("Attrition", "StockOptionLevel")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$StockOptionLevel, dnn=c("Attrition", "StockOptionLevel")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$StockOptionLevel, dnn=c("Attrition", "StockOptionLevel")), margin = 2),4)*100

"******************************  JobSatisfaction ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobSatisfaction),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("JobSatisfaction") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$JobSatisfaction),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("JobSatisfaction") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobSatisfaction, dnn=c("Attrition", "JobSatisfaction")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$JobSatisfaction, dnn=c("Attrition", "JobSatisfaction")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobSatisfaction, dnn=c("Attrition", "JobSatisfaction")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$JobSatisfaction, dnn=c("Attrition", "JobSatisfaction")), margin = 2),4)*100

"******************************  RelationshipSatisfaction ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$RelationshipSatisfaction),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("RelationshipSatisfaction") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$RelationshipSatisfaction),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("RelationshipSatisfaction") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$RelationshipSatisfaction, dnn=c("Attrition", "RelationshipSatisfaction")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$RelationshipSatisfaction, dnn=c("Attrition", "RelationshipSatisfaction")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$RelationshipSatisfaction, dnn=c("Attrition", "RelationshipSatisfaction")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$RelationshipSatisfaction, dnn=c("Attrition", "RelationshipSatisfaction")), margin = 2),4)*100

"******************************  EnvironmentSatisfaction ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$EnvironmentSatisfaction),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("EnvironmentSatisfaction") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$EnvironmentSatisfaction),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("EnvironmentSatisfaction") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$EnvironmentSatisfaction, dnn=c("Attrition", "EnvironmentSatisfaction")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$EnvironmentSatisfaction, dnn=c("Attrition", "EnvironmentSatisfaction")), margin = 2),4)*100

addmargins(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$EnvironmentSatisfaction, dnn=c("Attrition", "EnvironmentSatisfaction")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v2$Attrition, IBMEmpAtt_v2$EnvironmentSatisfaction, dnn=c("Attrition", "EnvironmentSatisfaction")), margin = 2),4)*100
