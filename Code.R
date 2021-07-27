#Assignment
#************************************************************
#               Data Exploration & Cleaning
#************************************************************
library(readr)
library(ggplot2)
library(dplyr)

training_data <- read_csv("training_data.csv")
View(training_data)
sum(is.na(training_data)) #2073 missing values 
data = training_data
attach(data)
str(data)
summary(data)
summary(data$Churn)
#Decide to remove missing values instead of imputing 
data = na.omit(data)
sum(is.na(data)) == 0 #No missing values in data 
nrow(data) #39148 observations after removing missing values 

#looking at the summary of our data, there are some negative values that 
#are impossible
data %>%
  filter(MonthlyRevenue < 0) 
data %>%
  filter(TotalRecurringCharge < 0)
data %>% 
  filter(CurrentEquipmentDays < 0) %>%
  select(Churn) %>%
  count(Churn == 'Yes') #we may be removing a lot of "no" churn data
  
#We cannot have negative Revenue therefore the safest is to remove this data 

data = subset(data, data$TotalRecurringCharge >= 0)
data = subset(data, data$CurrentEquipmentDays >= 0)
data = subset(data, data$MonthlyRevenue >= 0)

#Customer Churn Overview
ggplot(data, aes(x=Churn)) +
  geom_histogram(stat = "count", fill = c("lightblue", "pink")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#***********************************************************
#                   Removing Outliers
#***********************************************************
#Monthly Revenue Overview
ggplot(data, aes(x = MonthlyRevenue)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 10) 

boxplot(MonthlyRevenue)
summary(data)
data %>%
  filter(MonthlyRevenue >= 1200)

#removing outliers for Monthly Revenue
meanMR = mean(data$MonthlyRevenue)
sdMR = sd(data$MonthlyRevenue)
lowerOutlierMR = meanMR - 3*sdMR
upperOutlierMR = meanMR + 3*sdMR
upperOutlierMR
#use subset function to therefore remove the outliers 
data %>%
  filter(MonthlyRevenue >= upperOutlierMR)

#Monthly Minute Overview
ggplot(data, aes(x = MonthlyMinutes)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 10) 
max(MonthlyMinutes)
data %>% 
  filter(MonthlyMinutes > 7000)

mean(TotalRecurringCharge)

#Overage Minutes Overview 
boxplot(OverageMinutes)
data %>%
  filter(OverageMinutes >2000) %>%
  select(TotalRecurringCharge)
mean(TotalRecurringCharge)
#looks like there is an outlier which does not make sense 
#we should thus, remove it.

#Occupation 
ggplot(data, aes(x=Occupation)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#Roaming Calls Overview 

#AgeHH1 overview 
ggplot(data, aes(x = AgeHH1)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 5) 
summary(AgeHH1)
#remove age because there are alot of 0 entries and also the distribution of 
#age is fairly spread out 

#AgeHH2 overview 
ggplot(data, aes(x = AgeHH2)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 5) 
#similarly remove this column because there are too many 0 entries 
#note that later on we may be able to impute these entries but sack it rn 

#children overview 
ggplot(data, aes(x=ChildrenInHH)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#handsetPrice - remove 
ggplot(data, aes(x=HandsetPrice)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#prizm code - keep 
ggplot(data, aes(x=PrizmCode)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#occupation - possibly remove 
ggplot(data, aes(x=Occupation)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#marital status - keep for now but there are many unknowns 
ggplot(data, aes(x=MaritalStatus)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#owns motorcycle - keep 
ggplot(data, aes(x=OwnsMotorcycle)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#owns computer - keep
ggplot(data, aes(x=OwnsComputer)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#***********************************************************
#             Removing unnecessary variables
#***********************************************************
data = subset(data, select = -ServiceArea)
View(data)
summary(data)


#look for collinearity or multilinearity between predictors 
glm.fit = glm(as.factor(Churn)~., family="binomial",data=data)
summary(glm.fit)
#confint(glm.fit, level = .95)
#vif(glm.fit)


cormatrix = round(cor(data[sapply(data, is.numeric)]), digits = 2)
install.packages("lares")
library(lares)
corr_cross(data, max_pvalue = 0.05,
           top = 10)
#Using this ranked correlations, we can remove predictors 
data = subset(data, select = -c(RespondsToMailOffers, MadeCallToRetentionTeam,
                                HandsetModels, ReceivedCalls, PeakCallsInOut,
                                OffPeakCallsInOut))
data = subset(data, select = -c(CallForwardingCalls, CallWaitingCalls,
                                AgeHH1, AgeHH2))
data = subset(data, select = -c(HandsetPrice, CallWaitingCalls,
                                AgeHH1, AgeHH2))
data = subset(data, select = -c(TruckOwner, RVOwner))

data = subset(data, select = -c(HandsetPrice))
data = subset(data, select = -c(ServiceArea))
str(data)
install.packages("ff")
library(ff)

data = as.data.frame(unclass(data))
as.data.frame(data)
data$Churn = as.factor(data$Churn)
data[,c('Churn', 'ChildrenInHH', 'HandsetRefurbished',
        'HandsetWebCapable', 'BuysViaMailOrder',
        'OptOutMailings', 'OwnsComputer', 'HasCreditCard',
        'NewCellphoneUser', 'OwnsMotorcycle', 'CreditRating',
        'PrizmCode', 'Occupation', 'MaritalStatus')] = 
  lapply(data[,c('Churn', 'ChildrenInHH', 'HandsetRefurbished',
                 'HandsetWebCapable', 'BuysViaMailOrder',
                 'OptOutMailings', 'OwnsComputer', 'HasCreditCard',
                 'NewCellphoneUser', 'OwnsMotorcycle', 'CreditRating',
                 'PrizmCode', 'Occupation', 'MaritalStatus')], as.factor)
str(data)

#*******************************************************************
#                    Logistic Regression
#*******************************************************************

#Model 1 - All predictors
all.predictors_fit = glm(Churn ~., family = "binomial", data=data)
summary(all.predictors_fit)
View(data)
#removing high collinearity variables and MaritalStatus which had too many unknowns
data = subset(data, select = -c(DroppedCalls, BlockedCalls,
                                DroppedBlockedCalls, MaritalStatus))
install.packages("regclass")
library(regclass)
VIF(all.predictors_fit)

#best subset selection 
library(leaps)
regfit.full = regsubsets(Churn~., family = "binomial", data = data)

#backwards stepwise regression selection 
library(MASS)
library(ISLR)
library(boot)

mod_step <- stepAIC(all.predictors_fit,
                    direction = 'backward', trace = FALSE)
summary(mod_step)

#Use bootstrap resampling with replacement methd to assess
#consistency predictors selected with stepwise - sack this 
install.packages("bootStepAIC")
library(bootStepAIC)

mod_boot <- boot.stepAIC(all.predictors_fit, data, B = 50)
summary(mod_boot)


#k-fold Cross-Validation 
set.seed(1)
cv.error.10=rep(0 ,10)
for (i in 1:10){
  glm.fit=glm(Churn ~ ,data=Auto)
  cv.error.10[i] = cv.glm(Auto ,glm.fit ,K=10)$delta[1]
}

cv.error = cv.glm(data, mod_step, K=10)$delta[1]
cv.error

#Generate ROC curve -> only if you split data set into training and test and not use CV 
install.packages("caTools")
library(caTools)
model.AUC <- colAUC()

                              #******************
                              #  Ridge & Lasso  *
                              #******************

library(glmnet)


#ridge regression --> alpha = 1  
set.seed(42)

#data.train
#data.valid 
x.train = model.matrix(Churn~.,data.train)[,-c(1,2)]
y.train = data.train$Churn
x.test = model.matrix(Churn~.,data.valid)[,-c(1,2)]
y.test = data.valid$Churn


#ridge regression penalty using 10-fold CV to find optimal values for lamda
ridge.mod = cv.glmnet(x.train, y.train, alpha = 0 
                   ,family = "binomial")
ridge.mod0 = glmnet(x.train, y.train, alpha = 0,
                  family = "binomial", lambda = ridge.mod$lambda.1se)
coef(ridge.mod0)
#Make predictions on the test data 
probabilities = ridge.mod0 %>% predict(newx = x.test)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
p = sapply(probabilities, logit2prob)
predicted.classes = ifelse(p > 0.5, "Yes", "No")

#model accuracy 
observed.classes <- y.test
mean(predicted.classes == observed.classes)

confusionMatrix(table(predicted.classes,observed.classes))
           


#KNN 
library(class)
#scaling ??
data.frame(colnames(data))
standardised.X=scale(data[,c()])
Filter(is.factor, data)

#not scaled - using caret library 
install.packages("caret")
library(caret)

attach(data)
train <- createDataPartition(data[,'Churn'], p = 0.75, list =FALSE) #does not work lol 
p = 0.8
set.seed(2)
train.index <- sample.int(nrow(data), nrow(data)*p)
str(train.index)
summary(train.index)
data.train <- data[train.index,]
data.valid <- data[-train.index,]
str(data.train)
str(data.valid)

ctrl = trainControl(method = 'cv', number = 10) #does not do much but required for next line

install.packages("e1071")
library(e1071)
fit.cv <- train(Churn ~ MonthlyRevenue + MonthlyMinutes + TotalRecurringCharge +
                  UnansweredCalls + CustomerCareCalls, data = data.train, method = 'knn',
                trControl = ctrl,
                preProcess = c("center", "scale"), #scale the data
                tuneGrid = data.frame(k=100))
                #tune length = 50.

pred = predict(fit.cv, data.valid)
#pred.prob = predict(fit.cv, data.valid,type = 'prob')
confusionMatrix(table(data.valid$Churn,pred))


#not scaled using class package 

#Decision Trees 



