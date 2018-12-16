#  Course     : MIS 637 - Knowledge Discovery Databases I 
#  First Name : Shivakumar 
#  Last Name	: Barathi
#  CWID		    : 10430051
#  Date       : 05-June-2018
#  Type       : Project - EDA - Numerical Variables 
#****************************************************************************

#List of Numerical Variables
#Age, DistanceFromHome, MonthlyIncome, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, TrainingTimesLastYear,
#YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager

#Understand the association between each predictor variable and the target variable (Attrition) using 
#Overlay Histogram and Normalized Overlay Histogram

IBMEmpAtt_v1 <- IBMEmpAtt_Raw 
IBMEmpAtt_v2 <- IBMEmpAtt_Raw %>% filter((YearsAtCompany==0)|(YearsAtCompany==1))

str(IBMEmpAtt_v1)
prop.table(table(IBMEmpAtt_v1$Attrition))
#1:No - 83.88%        2:yes - 16.12%

"******************************  AGE ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Age),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("Age") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$Age),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("Age") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$Age),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("Age") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$Age),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("Age") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))



#Preferable bins
IBMEmpAtt_v1$Age_bin <- ifelse(between(IBMEmpAtt_v1$Age,18,33),1,ifelse(between(IBMEmpAtt_v1$Age,34,45),2,3))
#test the proportion of the bins w.r.t attrition
test <- IBMEmpAtt_v1[,c("Age","Age_bin")] %>% distinct()
addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Age_bin, dnn=c("Attrition", "Age_bin")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$Age_bin, dnn=c("Attrition", "Age_bin")), margin = 2),4)*100
#Convert the bin attribute as factor
IBMEmpAtt_v1$Age_bin <- factor(IBMEmpAtt_v1$Age_bin, levels=c(1,2,3), labels=c("1:18-33","2:34-45","3:46-60"), exclude=NULL)

"******************************  DistanceFromHome ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$DistanceFromHome),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("DistanceFromHome") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$DistanceFromHome),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("DistanceFromHome") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$DistanceFromHome),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("DistanceFromHome") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$DistanceFromHome),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("DistanceFromHome") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))


"******************************  MonthlyIncome ****************************** "
quantile(IBMEmpAtt_v1$MonthlyIncome)
IBMEmpAtt_v1$MonthlyIncome_bin <- ifelse(IBMEmpAtt_v1$MonthlyIncome<3001,"1-3000",ifelse(between(IBMEmpAtt_v1$MonthlyIncome,3001,5000),"3001-5000",ifelse(between(IBMEmpAtt_v1$MonthlyIncome,5001,8300),"5001-8300","8301-19999")))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$MonthlyIncome),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("MonthlyIncome_bin") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$MonthlyIncome),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("MonthlyIncome_bin") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$MonthlyIncome),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("MonthlyIncome_bin") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$MonthlyIncome),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("MonthlyIncome_bin") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))


"****************************** NumCompaniesWorked ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$NumCompaniesWorked),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("NumCompaniesWorked") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$NumCompaniesWorked),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("NumCompaniesWorked") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$NumCompaniesWorked),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("NumCompaniesWorked") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$NumCompaniesWorked),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("NumCompaniesWorked") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

#Preferable bins
IBMEmpAtt_v1$NumCompaniesWorked_bin <- ifelse(between(IBMEmpAtt_v1$NumCompaniesWorked,0,1),1,ifelse(between(IBMEmpAtt_v1$NumCompaniesWorked,2,4),2,3))
#test the proportion of the bins w.r.t attrition
test <- IBMEmpAtt_v1[,c("NumCompaniesWorked","NumCompaniesWorked_bin")] %>% distinct()
addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$NumCompaniesWorked_bin, dnn=c("Attrition", "NumCompaniesWorked_bin")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$NumCompaniesWorked_bin, dnn=c("Attrition", "NumCompaniesWorked_bin")), margin = 2),4)*100
#Convert the bin attribute as factor
IBMEmpAtt_v1$NumCompaniesWorked_bin <- factor(IBMEmpAtt_v1$NumCompaniesWorked_bin, levels=c(1,2,3), labels=c("1:0-1","2:2-4","3:5-9"), exclude=NULL)

"******************************  PercentSalaryHike ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$PercentSalaryHike),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("PercentSalaryHike") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$PercentSalaryHike),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("PercentSalaryHike") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$PercentSalaryHike),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("PercentSalaryHike") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$PercentSalaryHike),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("PercentSalaryHike") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

"******************************  TotalWorkingYears ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$TotalWorkingYears),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("TotalWorkingYears") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$TotalWorkingYears),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("TotalWorkingYears") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$TotalWorkingYears),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("TotalWorkingYears") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$TotalWorkingYears),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("TotalWorkingYears") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

#Preferable bins
IBMEmpAtt_v1$TotalWorkingYears_bin <- ifelse(between(IBMEmpAtt_v1$TotalWorkingYears,0,12),1,ifelse(between(IBMEmpAtt_v1$TotalWorkingYears,13,26),2,3))
#test the proportion of the bins w.r.t attrition
test <- IBMEmpAtt_v1[,c("TotalWorkingYears","TotalWorkingYears_bin")] %>% distinct()
addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$TotalWorkingYears_bin, dnn=c("Attrition", "TotalWorkingYears_bin")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$TotalWorkingYears_bin, dnn=c("Attrition", "TotalWorkingYears_bin")), margin = 2),4)*100
#Convert the bin attribute as factor
IBMEmpAtt_v1$TotalWorkingYears_bin <- factor(IBMEmpAtt_v1$TotalWorkingYears_bin, levels=c(1,2,3), labels=c("1:0-12","2:13-26","3:27-40"), exclude=NULL)

"******************************  TrainingTimesLastYear ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$TrainingTimesLastYear),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("TrainingTimesLastYear") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$TrainingTimesLastYear),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("TrainingTimesLastYear") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$TrainingTimesLastYear),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("TrainingTimesLastYear") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$TrainingTimesLastYear),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("TrainingTimesLastYear") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

"******************************  YearsAtCompany ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsAtCompany),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("YearsAtCompany") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsAtCompany),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("YearsAtCompany") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsAtCompany),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("YearsAtCompany") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsAtCompany),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("YearsAtCompany") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

#Preferable bins
IBMEmpAtt_v1$YearsAtCompany_bin <- ifelse(between(IBMEmpAtt_v1$YearsAtCompany,0,1),1,ifelse(between(IBMEmpAtt_v1$YearsAtCompany,2,11),2,3))
#test the proportion of the bins w.r.t attrition
test <- IBMEmpAtt_v1[,c("YearsAtCompany","YearsAtCompany_bin")] %>% distinct()
addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$YearsAtCompany_bin, dnn=c("Attrition", "YearsAtCompany_bin")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$YearsAtCompany_bin, dnn=c("Attrition", "YearsAtCompany_bin")), margin = 2),4)*100
#Convert the bin attribute as factor
IBMEmpAtt_v1$YearsAtCompany_bin <- factor(IBMEmpAtt_v1$YearsAtCompany_bin, levels=c(1,2,3), labels=c("1:0-1","2:2-11","3:12-40"), exclude=NULL)

"******************************  YearsInCurrentRole ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsInCurrentRole),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("YearsInCurrentRole") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsInCurrentRole),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("YearsInCurrentRole") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsInCurrentRole),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("YearsInCurrentRole") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsInCurrentRole),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("YearsInCurrentRole") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

#Preferable bins
IBMEmpAtt_v1$YearsInCurrentRole_bin <- ifelse(between(IBMEmpAtt_v1$YearsInCurrentRole,0,1),1,ifelse(between(IBMEmpAtt_v1$YearsInCurrentRole,2,7),2,3))
#test the proportion of the bins w.r.t attrition
test <- IBMEmpAtt_v1[,c("YearsInCurrentRole","YearsInCurrentRole_bin")] %>% distinct()
addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$YearsInCurrentRole_bin, dnn=c("Attrition", "YearsInCurrentRole_bin")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$YearsInCurrentRole_bin, dnn=c("Attrition", "YearsInCurrentRole_bin")), margin = 2),4)*100
#Convert the bin attribute as factor
IBMEmpAtt_v1$YearsInCurrentRole_bin <- factor(IBMEmpAtt_v1$YearsInCurrentRole_bin, levels=c(1,2,3), labels=c("1:0-1","2:2-7","3:8-18"), exclude=NULL)

"******************************  YearsSinceLastPromotion ****************************** "
ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsSinceLastPromotion),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("YearsSinceLastPromotion") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsSinceLastPromotion),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("YearsSinceLastPromotion") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsSinceLastPromotion),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("YearsSinceLastPromotion") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsSinceLastPromotion),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("YearsSinceLastPromotion") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

"******************************  YearsWithCurrManager ****************************** "

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsWithCurrManager),
               fill = IBMEmpAtt_v1$Attrition),
           position = "stack") + scale_x_discrete("YearsWithCurrManager") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v1,
           aes(x = factor(IBMEmpAtt_v1$YearsWithCurrManager),
               fill = IBMEmpAtt_v1$Attrition),
           position = "fill") + scale_x_discrete("YearsWithCurrManager") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsWithCurrManager),
               fill = IBMEmpAtt_v2$Attrition),
           position = "stack") + scale_x_discrete("YearsWithCurrManager") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

ggplot() +
  geom_bar(data = IBMEmpAtt_v2,
           aes(x = factor(IBMEmpAtt_v2$YearsWithCurrManager),
               fill = IBMEmpAtt_v2$Attrition),
           position = "fill") + scale_x_discrete("YearsWithCurrManager") + scale_y_continuous("Percent") + guides(fill=guide_legend(title="Attrition")) + scale_fill_manual(values=c("blue", "red"))

#Preferable bins
IBMEmpAtt_v1$YearsWithCurrManager_bin <- ifelse(IBMEmpAtt_v1$YearsWithCurrManager==0,1,ifelse(between(IBMEmpAtt_v1$YearsWithCurrManager,1,7),2,3))
#test the proportion of the bins w.r.t attrition
test <- IBMEmpAtt_v1[,c("YearsWithCurrManager","YearsWithCurrManager_bin")] %>% distinct()
addmargins(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$YearsWithCurrManager_bin, dnn=c("Attrition", "YearsWithCurrManager_bin")), FUN = sum)
round(prop.table(table(IBMEmpAtt_v1$Attrition, IBMEmpAtt_v1$YearsWithCurrManager_bin, dnn=c("Attrition", "YearsWithCurrManager_bin")), margin = 2),4)*100
#Convert the bin attribute as factor
IBMEmpAtt_v1$YearsWithCurrManager_bin <- factor(IBMEmpAtt_v1$YearsWithCurrManager_bin, levels=c(1,2,3), labels=c("1:0-1","2:2-7","3:8-17"), exclude=NULL)

"---- Takeaway from YearsWithCurrManager-Univariate EDA: Attrition is significant for people with < 1 years with current manager -----"


#--- Multivariate EDA

plot(IBMEmpAtt_v1$YearsInCurrentRole,
     IBMEmpAtt_v1$YearsWithCurrManager,
     xlim = c(0, 20),
     ylim = c(0, 20),
     xlab = "YearsInCurrentRole_bin",
     ylab = "YearsWithCurrManager_bin",
     main = "Scatterplot",
    col = ifelse(IBMEmpAtt_v1$Attrition=="2:Yes","red","blue"))
legend("topright",c("True","False"), col = c("red", "blue"),pch = 1,title = "Attrition")








