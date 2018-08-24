
#setwd("C:/Users/Rock/Downloads/PA-3-Un/PA-I_Case_Study_HR_Analytics")
setwd("C:/Users/user/Desktop/Data Science/Module 3/case study")

#Loading libraries
library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(stringr)
library(dplyr)
library(lubridate)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(GGally)


# reading database
in_time <- read.csv('in_time.csv', stringsAsFactors = FALSE)
out_time <- read.csv('out_time.csv', stringsAsFactors = FALSE)
emp_survey_data <- read.csv('employee_survey_data.csv', stringsAsFactors = FALSE)
mgr_survey_data <- read.csv('manager_survey_data.csv', stringsAsFactors = FALSE)
general_data <- read.csv('general_data.csv', stringsAsFactors = FALSE)


#Checking uniqness of records
sum(is.na(in_time$x)) # GJ-giving warning message
length(unique(in_time$X)) 

sum(is.na(out_time$x))
length(unique(out_time$X)) 

sum(is.na(emp_survey_data$EmployeeID))
length(unique(emp_survey_data$EmployeeID))

sum(is.na(mgr_survey_data$EmployeeID))
length(unique(mgr_survey_data$EmployeeID)) 

sum(is.na(general_data$EmployeeID))
length(unique(general_data$EmployeeID)) 

#checking emp ids across datasets
setdiff(general_data$EmployeeID,out_time$X) 
setdiff(general_data$EmployeeID,in_time$X) 
setdiff(general_data$EmployeeID,emp_survey_data$EmployeeID) 
setdiff(general_data$EmployeeID,mgr_survey_data$EmployeeID) 

##########################################################

# Removing all the col's which contains NA only i.e. removing holidays from the data
in_time <-  in_time[,apply(in_time , 2 , function(x)  !all(is.na(x)))]
out_time <- out_time[,apply(out_time , 2 , function(x)  !all(is.na(x)))]

# Converting wide - long format of in/out dataset
in_time_new <- gather(in_time, day, in_dttime, X2015.01.02:X2015.12.31)
out_time_new <- gather(out_time, day, out_dttime, X2015.01.02:X2015.12.31)

#merging day wise in/out timings
in_out_time<-merge(in_time_new,out_time_new,by=c("X", "day"))

# formatting date
in_out_time$in_dttime <- as.POSIXlt(in_out_time$in_dttime, format="%Y-%m-%d %H:%M:%S")
in_out_time$out_dttime <- as.POSIXlt(in_out_time$out_dttime, format="%Y-%m-%d %H:%M:%S")

# total  time spent
in_out_time$time_spent<-round(difftime(in_out_time$out_dttime,in_out_time$in_dttime,units = "hours"))
in_out_time$day<- gsub("X","",in_out_time$day) # removing "X" from day column
in_out_time$time_spent<-as.integer(in_out_time$time_spent)

in_out_time$in_dttime<-NULL
in_out_time$out_dttime<-NULL

day_grp<-group_by(in_out_time,X)
avg_time_spent<-summarise(day_grp,avg_time=mean(time_spent,na.rm = TRUE))
avg_time_spent<-as.matrix(avg_time_spent)
hist(avg_time_spent[,2])

##################################################################


#Changing categorical variables of emp_survey_data

emp_survey_data$EnvironmentSatisfaction <- ifelse(emp_survey_data$EnvironmentSatisfaction==1, "Low",
                                            ifelse(emp_survey_data$EnvironmentSatisfaction==2, "Medium",
                                            ifelse(emp_survey_data$EnvironmentSatisfaction==3, "High", "Very High")))

emp_survey_data$JobSatisfaction <- ifelse(emp_survey_data$JobSatisfaction==1, "Low",
                                   ifelse(emp_survey_data$JobSatisfaction==2, "Medium", 
                                   ifelse(emp_survey_data$JobSatisfaction==3, "High", "Very High")))

emp_survey_data$WorkLifeBalance <- ifelse(emp_survey_data$WorkLifeBalance==1, "Bad",
                                   ifelse(emp_survey_data$WorkLifeBalance==2, "Good", 
                                   ifelse(emp_survey_data$WorkLifeBalance==3, "Better", "Best")))

#######################################################


mgr_survey_data$JobInvolvement <- ifelse(mgr_survey_data$JobInvolvement==1, "Low",
                                  ifelse(mgr_survey_data$JobInvolvement==2, "Medium", 
                                  ifelse(mgr_survey_data$JobInvolvement==3, "High", "Very High")))


mgr_survey_data$PerformanceRating <- ifelse(mgr_survey_data$PerformanceRating==1, "Low",
                                     ifelse(mgr_survey_data$PerformanceRating==2, "Good", 
                                     ifelse(mgr_survey_data$PerformanceRating==3, "Excellent", "Outstanding")))

##################################################


#Changing categorical variables of general_data
general_data$Education <-   ifelse(general_data$Education==1, "Below College",
                            ifelse(general_data$Education==2, "College", 
                            ifelse(general_data$Education==3, "Bachelor",
                            ifelse(general_data$Education==4, "Master", "Doctor"))))

#Categorization of Age variable
#<=25 Youngster
#25 < Age <= 50 MidAged
#>50 OldAged

general_data$Age <-  ifelse(general_data$Age<=25, "Youngster",
                      ifelse(general_data$Age>25 & general_data$Age<=50, "MiddleAged","OldAged"))
                  




###########################################################################

#Merging datasets  general_data,emp_survey,mgr_survey & avg_time_spent

Final_data <- merge(general_data, emp_survey_data, by="EmployeeID")
Final_data <- merge(Final_data, mgr_survey_data, by="EmployeeID")
Final_data <- merge(Final_data, avg_time_spent, by.x="EmployeeID",by.y="X")



####Cleaning the final data ######################################################################


# Final data set has some columns with uniq values so removing it
Final_data <- Final_data[, apply(Final_data , 2 , function(x) !length(unique(x)) == 1)]

#Checking for NA values
sapply(Final_data, function(x) sum(is.na(x))) 
View(subset(Final_data, is.na(Final_data$TotalWorkingYears)))
View(subset(Final_data, is.na(Final_data$EnvironmentSatisfaction)))
View(subset(Final_data, is.na(Final_data$JobSatisfaction)))
View(subset(Final_data, is.na(Final_data$NumCompaniesWorked)))
View(subset(Final_data, is.na(Final_data$WorkLifeBalance)))

# 110 NAs were eleminated
Final_data <- na.omit(Final_data)
# EDA-categorical variables
ggplot(Final_data,aes(x=Department, fill=Attrition))+geom_bar(position = "fill")
# EDA-numeric variables
boxplot(DistanceFromHome~Attrition,Final_data) # Distance from home is not significant for Attrition


# creating a dataframe of categorical features # need to update above before executing following
Final_data_chr<- Final_data[,c(2,4,5,7,8,9,11,12,22,23,24,25,26)]
Final_data_chr
# converting categorical attributes to factor
Final_data_fact<- data.frame(sapply(Final_data_chr, function(x) factor(x)))
str(Final_data_fact)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Final_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =Final_data_fact))[,-1]))



#checking for outliers
sapply(Final_data[,c(6,13:21,26)], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) 


#Outlier identifed as 
#DistanceFromHome , monthlyincome, numcompaniesworked , %salaryhike, totalworkingyeras,yearsatcompany,yearsincelastpromotion,
#yearswithcurrmanager,

# team please vet the following
quantile(Final_data$NumCompaniesWorked,seq(0,1,0.01))
boxplot(Final_data$NumCompaniesWorked)
hist(Final_data$NumCompaniesWorked)
Final_data$NumCompaniesWorked[which(Final_data$NumCompaniesWorked>8)]<-8
boxplot(Final_data$NumCompaniesWorked)
boxplot(Final_data$avg_time)
 
#Mahesh to identify/vet all the outliers for the above col no's 6,13-21, 27
# can use the above code 


#Scalling ? required??
Final_data$MonthlyIncome<-scale(Final_data$MonthlyIncome)


#binding data for modellig

Final_data_model<- cbind(Final_data[,c(1,3,6,10,13:21,26)],dummies) 


