                      #********************************
                      #  Data Exploration & Cleaning  *
                      #********************************
library(readr)
library(ggplot2)
library(dplyr)
library(MASS)
library(ISLR)

#We have enough observations to risk losing missing values, imputing such values 
#would be 
training_data <- read_csv("training_data.csv")
View(training_data)
sum(is.na(training_data)) #2073 missing values 

data = training_data
attach(data)
str(data)
summary(data)
#lets first omit all the data 
sum(is.na(training_data)) #2073 missing values
data = na.omit(data)
nrow(data)/nrow(training_data)#we retain 97.87% of the original dataset observations
#removing negative values
data = subset(data, data$TotalRecurringCharge >= 0)
data = subset(data, data$CurrentEquipmentDays >= 0)
data = subset(data, data$MonthlyRevenue >= 0)
#retain 97.7% of all data


#Using the logistic regression and others, we can remove unnecessary variables 

#Service Area Overview
data = subset(data, select = -c(ServiceArea))
str(data)

#AgeHH1 overview 
ggplot(data, aes(x = AgeHH1)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 5) 
summary(AgeHH1)
data = subset(data, select = -c(AgeHH1, AgeHH2))

#remove age because there are alot of 0 entries and also the distribution of 
#age is fairly spread out 

#AgeHH2 overview 
ggplot(data, aes(x = AgeHH2)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 5) 
#similarly remove this column because there are too many 0 entries 
#note that later on we may be able to impute these entries but sack it rn 

#lets see what is significant or not through explanatory analysis 
#we can remove variables in both data pre-processing stage and in the model
#building stage

#we have too many variables which can cause overfitting, as well as multicollinearity that
#can lead to inflated variable importance 

cor(data)
nums  <- unlist(lapply(data, is.numeric))
nums.data = data[, nums] #only numerical columns 
cormatrix = round(cor(nums.data), digits = 2)
library(lares)
library(dplyr)
library(tidyr)
install.packages("radiant.data")
library(radiant.data)
df.corMatrix = data.frame(cormatrix) 

df.corMatrix %>% 
  rownames_to_column() %>%
  gather(key="variable", value = "correlation", -rowname) %>%
  filter(abs(correlation) > 0.7) %>%
  filter(abs(correlation) < 0.99)

#we can identify the highest collinearity and remove one variable accordingly 
#Handset, HandsetModels = 0.89
#MonthlyMinutes, ReceivedCalls = 0.83
#DroppedBlockedCalls, BlockedCalls =  0.82
#ReceivedCalls, MonthlyMinutes = 0.83

data = subset(data, select = -c(ReceivedCalls, OverageMinutes, DroppedBlockedCalls))

#Received calls overview
ggplot(data, aes(x = MonthlyMinutes)) +
  geom_histogram(color = "Gray", fill = "blue", binwidth = 5)
summary(MonthlyMinutes)



#lets run a simple logistic to see if AgeHH1 or AgeHH2 is significant 
p = 0.8
set.seed(2)
train.index <- sample.int(nrow(data), nrow(data)*p)
str(train.index)
summary(train.index)
data.train <- data[train.index,]
data.valid <- data[-train.index,]

all.predictors_fit = glm(Churn ~.-CustomerID, family = "binomial", data=data.train)
pred.allFit = predict(all.predictors_fit, data.valid, type = "response")
summary(all.predictors_fit)
summary(pred.allFit)
contrasts(Churn)
glm.pred = rep("No",nrow(data.valid))
glm.pred[pred.allFit>.5] = "Yes"
observed = data.valid$Churn
table(glm.pred, observed)
mean(glm.pred == observed) #71.1% accuracy 


#find high VIF predictors and remove (>10)
summary(all.predictors_fit)
str(data)
library(regclass)
VIF(all.predictors_fit)

#lets do forward and backwards stepwise selection 
set.seed(1)
step.forward = all.predictors_fit  
regfit.fwd = regsubsets(Churn~., data = data.train, method = "forward", nvmax = 46)
summary(regfit.fwd)
coef(regfit.fwd, 5) #gives us best 5 variable model 
contrasts(data$CreditRating)
contrasts(data$Churn)


#changing varaibles to dummy variables 
model.matrix( ~ CreditRating, data)
contrasts(data$CreditRating)
str(data)

#Turn all qualitative predictors to factors 
data[,c('Churn', 'ChildrenInHH', 'HandsetRefurbished',
        'HandsetWebCapable', 'BuysViaMailOrder','TruckOwner',
        'RVOwner', 'RespondsToMailOffers', 'MadeCallToRetentionTeam',
        'OptOutMailings', 'OwnsComputer', 'HasCreditCard',
        'NewCellphoneUser', 'OwnsMotorcycle', 'CreditRating',
        'PrizmCode', 'Occupation', 'MaritalStatus', 'IncomeGroup')] = 
  lapply(data[,c('Churn', 'ChildrenInHH', 'HandsetRefurbished',
                 'HandsetWebCapable', 'BuysViaMailOrder','TruckOwner',
                 'RVOwner', 'RespondsToMailOffers', 'MadeCallToRetentionTeam',
                 'OptOutMailings', 'OwnsComputer', 'HasCreditCard',
                 'NewCellphoneUser', 'OwnsMotorcycle', 'CreditRating',
                 'PrizmCode', 'Occupation', 'MaritalStatus', 'IncomeGroup')], as.factor)
str(data)

summary(data$HandsetPrice == "Unknown")

#handsetPrice - remove 
ggplot(data, aes(x=HandsetPrice)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")
# we need to remove because there are too many unknowns 
data = subset(data, select = -c(HandsetPrice))



#the NA's is most in AgeHH1 and AgeHH2 variables, therefore we can run a diagnostic 
#on these two variables to see if we are willing to omit these variables completely 

#AgeHH1 Overview and AgeHH2 Overview 
#From the initial logistic diagnostic is seems like ageHH1 may be very significant while AGHH2 
#may not be 


#contrasts(data$IncomeGroup) #gives us the dummy variables for the factors in dataset 
#str(data)
library(caret)
trControl <- trainControl(method = "cv", number = 10)
fit <- train(Churn ~.,
             method = "knn",
             tuneGrid = expand.grid(k = 200),
             trControl = trControl,
             metric = "Accuracy",
             data = data)
