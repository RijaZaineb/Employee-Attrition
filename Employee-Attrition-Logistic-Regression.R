#Read in Datasets

employees <- read.csv(file="Employee-1.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(employees)
#4,653-Sample Size and 9 variables

#Show for first 6 rows of data
head(employees)

names(employees)
str(employees)

################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(employees))
#0 total missing values 

################################################################################################
#Graph Data
library(psych)
pairs.panels(employees[1:8],
             gap = 0,
             bg = c("red", "green")[employees$LeaveOrNot],
             pch = 21)

################################################################################################
## Converting categorical variables into numeric variables

## Gender
table(employees$Gender)
#Female - 1875 Male - 2778

library(plyr)
employees$Gender <- revalue(employees$Gender, c("Male"="1", "Female"="0"))
employees$Gender <- as.numeric(employees$Gender)
table(employees$Gender)
#0 - 1875 1 - 2778

## EverBenched
table(employees$EverBenched)
# No - 4175 Yes - 478
employees$EverBenched <- revalue(employees$EverBenched, c("Yes"="1", "No"="0"))
employees$EverBenched <- as.numeric(employees$EverBenched)
table(employees$EverBenched)
# 0 - 4175 1 - 478

## Education
table(employees$Education)
# Bachelors - 3601 Masters - 873 PHD - 179
employees$Education <- revalue(employees$Education, c("Bachelors"="1", "Masters"="2", "PHD"="3"))
employees$Education <- as.numeric(employees$Education)
table(employees$Education)
# 1 - 3601 2 - 873 3 - 179

## Creating dummy variables for City
## Creating k dummy variables for City i.e., 3 dummy variables
table(employees$City)
# Bangalore - 2228 New Delhi - 1157 Pune - 1268
employees$Bangalore <- ifelse(employees$City == 'Bangalore', 1, 0)
employees$NewDelhi <- ifelse(employees$City == 'New Delhi', 1, 0)
#employees$Pune <- ifelse(employees$City == 'Pune', 1, 0)
table(employees$Bangalore)
table(employees$NewDelhi)
names(employees)
str(employees)

##Removing city
employees2 <- employees[,c(1:2, 4:11)]
names(employees2)

######################################################################################################

#Libraries

library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance


# Create training (70%) and test (30%) sets

set.seed(123)  # use a set seed point for reproducibility
split <- initial_split(employees2, prop = .7, strata = "LeaveOrNot")
train <- training(split)
test  <- testing(split)
dim(train)
#3257 10
dim(test)
#1396 10

#Logistic Regression

#For explaining dependent variable LeaveOrNot

employees2$LeaveOrNot <- as.factor(employees2$LeaveOrNot)

log_reg <- glm(
  LeaveOrNot ~ Education + JoiningYear + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain + Bangalore + NewDelhi,
  family = "binomial", 
  data = employees2
)

summary(log_reg) #Coefficients Not in exponential form

tidy(log_reg) #Coefficients Not in exponential form

#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 

train$LeaveOrNot<- as.factor(train$LeaveOrNot)

#For Predicting dependent variable LeaveOrNot
log_reg = train(
  form = LeaveOrNot ~ Education + JoiningYear + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain + Bangalore + NewDelhi,
  data = train,
  method = "glm",
  family = "binomial"
)

#Confusion Matrix
confusionMatrix(predict(log_reg, test), as.factor(test$LeaveOrNot), positive = "1")

table(predict(log_reg, test),as.factor(test$LeaveOrNot))
#Variables of Importance

vip(log_reg, num_features = 10)

#ROC Curves

log_reg_train <- glm(LeaveOrNot ~ Education + JoiningYear + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain + Bangalore + NewDelhi, data=train, family=binomial)


#ROC Curves
library(ROCR)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$LeaveOrNot)

perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)

library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot)
autoplot(precrec_obj)

## Get AUCs
sm_aucs <- auc(precrec_obj)
## Shows AUCs
sm_aucs

precrec_obj2 <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot, mode="basic")
autoplot(precrec_obj2)


library(ROCit)
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score=log_reg_test_prob,class=test$LeaveOrNot)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$LeaveOrNot,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure

#Decision Tree

decision_tree = train(LeaveOrNot ~ Education + JoiningYear + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain + Bangalore + NewDelhi, 
                      data=train, 
                      method="rpart", 
)

decision_tree

summary(decision_tree$finalModel) # detailed summary of splits

#predict

confusionMatrix(predict(decision_tree, test), as.factor(test$LeaveOrNot), positive = "1")


# plot tree
library(rpart.plot)

plot.model <- rpart(LeaveOrNot ~ Education + JoiningYear + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain + Bangalore + NewDelhi, 
                    data=train, cp = .02)
rpart.plot(plot.model)
