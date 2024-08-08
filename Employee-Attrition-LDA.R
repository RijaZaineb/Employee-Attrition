#Read in Datasets

employees <- read.csv(file="Employee.csv", header=TRUE, sep=",")

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
employees$Gender_num <- revalue(employees$Gender, c("Male"="1", "Female"="0"))
employees$Gender_num <- as.numeric(employees$Gender_num)
table(employees$Gender_num)
#0 - 1875 1 - 2778


## EverBenched
table(employees$EverBenched)
# No - 4175 Yes - 478
employees$EverBenched_num <- revalue(employees$EverBenched, c("Yes"="1", "No"="0"))
employees$EverBenched_num <- as.numeric(employees$EverBenched_num)
table(employees$EverBenched_num)
# 0 - 4175 1 - 478

## Education
table(employees$Education)
# Bachelors - 3601 Masters - 873 PHD - 179
employees$Education_num <- revalue(employees$Education, c("Bachelors"="1", "Masters"="2", "PHD"="3"))
employees$Education_num <- as.numeric(employees$Education_num)
table(employees$Education_num)
# 1 - 3601 2 - 873 3 - 179

## Creating dummy variables for City
## Creating k dummy variables for City i.e., 3 dummy variables
table(employees$City)
# Bangalore - 2228 New Delhi - 1157 Pune - 1268
employees$Bangalore <- ifelse(employees$City == 'Bangalore', 1, 0)
employees$NewDelhi <- ifelse(employees$City == 'New Delhi', 1, 0)
#employees$Pune <- ifelse(employees$City == 'Pune', 1, 0)
names(employees)
str(employees)

#employees$PaymentTierCat <- as.factor(employees$PaymentTier)
#employees$LeaveOrNotFact <- as.factor(employees2$LeaveOrNot)
str(employees)
names(employees)
######################################################################################################
## Creating graph after converting categorical variables into numeric type
employees2 <- employees[,c(2, 4, 5, 8, 9, 10:14)]
names(employees2)
#employees3 <- employees2[,c(1:4, 6:11)]
#names(employees3)
#scale(employees2, center = TRUE, scale = TRUE)
#Graph Data
library(psych)
pairs.panels(employees2[1:4,6:10],
             gap = 0,
             bg = c("red", "green")[employees2$LeaveOrNot],
             pch = 21)


#With Cross Validation
# The dependent variable must be categorical
employees2$LeaveOrNot <- as.factor(employees2$LeaveOrNot)
names(employees2)
str(employees2)

employeesLDA <- lda(LeaveOrNot ~ ., data=employees2, CV=TRUE)
employeesLDA


#To Plot the Data, you cannot use CV
employeesLDA <- lda(LeaveOrNot ~ ., data=employees2)
employeesLDA

plot(employeesLDA, ylab = "LD1")

# Try to predict the class from the original data
p <- predict(employeesLDA, newdata=employees2[,c(1:4, 6:11)])$class
p
names(employees2)

# Compare the results of the prediction (Confusion Matrix)
table1<-table(p, employees2$LeaveOrNot)
table1
#Using Trace
sum(diag(table1)/sum(table1))
#0.73

accuracy <- (2738+669)/(2738+669+931+315)
accuracy
#0.73

mean(p == employees2$LeaveOrNot)
#0.73

#Creating Training and Testing Samples
require(caTools)  # loading caTools library
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(employees2,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(employees2,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(employees2, sample==FALSE)
dim(test)

library(topicmodels, lib.loc=".")
library(MASS)

# The dependent variable must be categorical (Assuming No Cross-Validation)
employeesLDA = lda(LeaveOrNot ~ ., data=train)
employeesLDA

plot(employeesLDA)


# Try to predict the class from the original data

prd<-predict(employeesLDA, test)$class
prd
#Stacked Histogram of LDA Functions
ldahist(data=prd$x[,1], g = test$LeaveOrNot)

# Compare the results of the prediction
table2 <- table(prd, test$LeaveOrNot)
table2
accuracy <- (190+823)/(190+823+113+269)
accuracy
#0.99



#LDA Visualization
#install.packages("devtools")
library(devtools)
install_github("fawda123/ggord", force=TRUE)
library(ggord)
ggord(employeesLDA, train$LeaveOrNot, ylim = c(-2, 2 ))

#Other Visualizations

newdata <- data.frame(train$LeaveOrNotFact, lda = prd$x)

library(ggplot2)
ggplot(newdata) + geom_point(aes(lda.LD1, colour = train$LeaveOrNotFact), size = 2.5)

#library(ggplot2)
#ggplot(data = employees2)+
 # geom_density(aes(LD1, fill = class), alpha = 0.1)


# Setting "CV = T" will have the lda function perform
# "Leave-one-out" cross-validation
employeesLDA2 = lda(LeaveOrNot ~ ., data=train, CV=T)
employeesLDA2

table(employeesLDA2$class, train$LeaveOrNot)

coef(employeesLDA)


library(caret)
modelFit<- train(LeaveOrNot ~ ., method='lda',preProcess=c('scale', 'center'), data=train)

#Confusion Matrix
confusionMatrix(train$LeaveOrNot, predict(modelFit, train), positive="1")



#Confusion Matrix
confusionMatrix(as.factor(test$LeaveOrNot), predict(modelFit, test), positive="1")
names(train)

library(ROCit)

lda_train <- glm(LeaveOrNot ~ Education_num + JoiningYear + PaymentTier + Age + Gender_num + EverBenched_num + ExperienceInCurrentDomain + Bangalore + NewDelhi, data=train, family=binomial)

lda_test_prob <- lda_train %>% predict(test, type = "response")
lda_test_prob
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_objlda <- rocit(score=lda_test_prob,class=test$LeaveOrNot)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$LeaveOrNot,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure

#Visualization
library(klaR)
partimat(LeaveOrNotFact ~ ., data = train, method = "lda")

