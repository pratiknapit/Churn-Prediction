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



#********************************************************************************************************************************************************************

                  #********************************
                  #  Data Exploration & Cleaning  *
                  #********************************

library(readr)
library(ggplot2)
library(dplyr)
library(MASS)
library(ISLR)
library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)

training_data <- read_csv("training_data.csv")
View(training_data)
sum(is.na(training_data)) #2073 missing values 

data = training_data
sum(is.na(data))
summary(data)
attach(data)

#We cannot have negative Revenue therefore the safest is to remove this data 

data = subset(data, data$TotalRecurringCharge >= 0)
data = subset(data, data$CurrentEquipmentDays >= 0)
data = subset(data, data$MonthlyRevenue >= 0)


data %>% 
  filter(Churn == "Yes") %>%
  pull(MonthlyMinutes) %>%
  mean()

boxplot(MonthlyMinutes ~ Churn)

#AgeHH1 & AgeHH2 - contains 715 NA's - lets see if we can remove this variable
attach(data)
d.age = data %>% filter(AgeHH1 > 0)
boxplot(d.age$AgeHH1, d.age$Churn)


boxplot(AgeHH1 ~ Churn)
boxplot(AgeHH2 ~ Churn)
data = subset(data, select = -c(AgeHH1, AgeHH2))

#Now we can remove all other NA's 
data = na.omit(data) 

#remove customer ID 
data = subset(data, select = -c(CustomerID))


#handsetPrice - remove 
ggplot(data, aes(x=MaritalStatus)) +
  geom_histogram(stat = "count", fill = c("lightblue")) +
  labs(title = "Customer Churn", x = "Churn", y = "Frequency")

#removing predictors that are collinear/multilinear or are redundant

data = subset(data, select = -c(HandsetModels,ServiceArea,HandsetPrice,
                                MaritalStatus))
data = subset(data, select = -c(DroppedCalls, BlockedCalls,
                                PeakCallsInOut, OffPeakCallsInOut))
#PeakCallsInOut overview 
boxplot(PeakCallsInOut ~ Churn)
boxplot(OffPeakCallsInOut ~ Churn)

str(data)
nums = unlist(lapply(data, is.numeric))
scaled.data <- scale(data[, nums])
s.data = data
s.data$monthlyrev = scale(data[,3])
attach(s.data)
boxplot(monthlyrev ~ Churn)
s.data %>%
  filter(Churn == "No") %>%
  pull(monthlyrev) %>%
  mean()

glm(Churn~monthlyrev, data = s.data, family = binomial)


scaled.data$Churn <- data$Churn


summary(scaled.data)
str(scaled.data)
View(scaled.data)
attach(scaled.dat)

#remove outliers


#TruckOwner and RV Owner Overview 
ggplot(data, aes(TruckOwner, Churn)) +
  geom_col()

#Perfect for visualising individual effects 
ggplot(data, aes(x = TruckOwner, fill = Churn)) + 
  geom_bar(position = "fill") 

data = subset(data, select = -c(TruckOwner, RVOwner))
data = subset(data, select = -c(OwnsComputer, 
                                OwnsMotorcycle))
str(data)

df.corMatrix %>% 
  rownames_to_column() %>%
  gather(key="variable", value = "correlation", -rowname) %>%
  filter(abs(correlation) > 0.70) %>%
  filter(abs(correlation) < 0.99)

#thus is does not look significant and we can remove both variables 


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


#lets also standardize each numerical variable - not yet 



                      #********************************
                      #           Modeling            *
                      #********************************

#*Logistic Regression*
#*********************

library(caret) 
set.seed(430) 
samp = createDataPartition(data$Churn, p = 0.8, list = FALSE)
train.data = data[samp, ]
valid.data = data[-samp, ]

library(boot)
allP.train.logistic = glm(Churn~., family = binomial, data = train.data)
summary(glm.fit)
cv.error = cv.glm
pred.allLog = predict(allP.train.logistic, valid.data, type = "response")
library(pROC)
auc(valid.data$Churn, pred.allLog)

cv.error = cv.glm(data, )


#*Logistic Regression with subset selection*
#*******************************************




library(regclass)
VIF(glm.fit)
data = subset(data, select = -c(MadeCallToRetentionTeam, RespondsToMailOffers))
all.pred = predict(glm.fit, valid.data, type = "response")
summary(all.pred)
glm.pred = rep("No", nrow(valid.data))
glm.pred[all.pred>.5] = "Yes"
observed = valid.data$Churn
table(glm.pred, observed)
mean(glm.pred == observed)
summary(valid.data$Churn)


#using all predictors the accuracy is 0.711 which is useless 

#Now lets see if subset selection will help 

cv.lasso = cv.glmnet(data, Churn, alpha = 1, family = "binomial")




library(caret) 
set.seed(430) 
samp = createDataPartition(data$Churn, p = 0.75, list = FALSE) 
train.data = data[samp, ]
valid.data = data[-samp, ]


forward.model = train(Churn~., data = train.data,
                      method = "glmStepAIC", family = "binomial",
                      trControl = trControl,
                      nvmax = 10,
                      metric = "Accurary",
                      )
forward.model$coefnames

forward.model$results

forw.model = regsubsets(Churn~., data = train.data, 
                        nvmax = 10, method = "forward")
sum = summary(forw.model)
which.min(sum$which)

#all predictors logistic regression, no subset selection
def.glm.mod = train(Churn~., data = train.data,
                    trControl = trControl,
                    metric = "Accuracy",
                    method = "glm",
                    family = "binomial",
                    )
summary(def.glm.mod)
pred.allFit = predict(def.glm.mod, valid.data, type = "prob")

glm.pred = rep("No",nrow(valid.data))
glm.pred[pred.allFit>.4] = "Yes"
observed = valid.data$Churn
table(glm.pred, observed)
mean(glm.pred == observed) #71.1% accuracy 

#KNN using CV with 10 fold --> can be also used to find optimal k but computational expensive
trControl <- trainControl(method = "cv", number = 10)
fit <- train(Churn ~.,
             method = "knn",
             tuneGrid = expand.grid(k = 200),
             trControl = trControl,
             metric = "Accuracy",
             data = train.data)
knn.pred = predict(fit, valid.data, type = "prob")

predicted.classes = ifelse(knn.pred> 0.4, "Yes", "No")
table(predicted.classes, observed)

#subset selection 
install.packages("StepReg")
library(StepReg)
stepwiselogit(train.data, Churn, exclude = NULL, include = NULL, 
              selection = "forward", select = "SL", sle = 0.1, sls = 0.1)

#Ridge & Lasso Logistic Regression 
library(glmnet)
set.seed(42)

#data.train
#data.valid 
x.train = model.matrix(Churn~.,train.data)[,-c(1)]
y.train = train.data$Churn
x.test = model.matrix(Churn~.,valid.data)[,-c(1)]
y.test = valid.data$Churn

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
