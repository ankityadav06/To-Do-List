# LOAN  :- Logistic Regression
data1 <- read.csv ("D:/RTCS/loan.csv",header=TRUE,sep=",")
data2<-data.frame(data1)
head (data2)
tail(data2)
str(data2) 
names(data2)
class(data2)
summary(data2$Amount)
is.na(data2)
#To find out the correlation between the variables
corr <- cor.test(data2$Default, data2$Term, 
                 method = "pearson" )
corr

#building logistic regression model using glm on full data
fullmodel1 <-glm(Default~.,data = data2,family=binomial
                 (link=logit ))

summary(fullmodel1)

#removing insignificant variables in order to build final logistic model on full data
fullmodel2<-glm(Default~Checking_amount+Term+Credit_score
                +Saving_amount+Age,data = data2,family=binomial(link=logit))
summary(fullmodel2)

train_obs <- floor (0.7*nrow (data2))
print(train_obs)
#Set seed in order to reproduce the sample
set.seed(2) 
train_ind <- sample(seq_len(nrow(data2)),size=train_obs)
test <-  -train_ind
#No of observations in train dataset
train_data<-data2[train_ind,] 
# No of observations  in test dataset
test_data<-data2[-train_ind,]
testing_high = data2$Default[test]
#Building logistic regression model using glm on training data
model1<-glm(Default~.,data= train_data,family=binomial(link=logit))
summary(model1)
#After removing insignificant variable inorder to build final logistic model on training data
model2<-glm(Default~Checking_amount+Term+Credit_score
            +Emp_status+Saving_amount+Age,data= train_data,family=binomial(link=logit))
summary(model2)
#Check for variance inflation factor, VIF > 5 to 10 -high correlation
#install car package 
install.packages("car")
library(car)
vif(model2)
# Predicting the model using test data
Prob <-predict(model2,test_data,type ="response")
prob1<- data.frame(Prob)
# setting the cutoff for probability values
results <- ifelse(prob1 > 0.7,1,0)
#Display the confusion matrix or classification table
table(testing_high,results)
#Calculating the error rate
misclasificationerror <- mean(results != testing_high) 
misclasificationerror
# Calculating the accuracy rate	
accuracyrate <- 1-misclasificationerror
print(accuracyrate)

# Sale :- SARIMA Model
#Forecast the Retail Food & Beverages Sales: Seasonal ARIMA model in R 
data1=read.csv("D:/prerna_05/sale.csv") #data import
print(data1)
#Convert data into time series data
data2=ts(data1[,2],start=c(1992,1),frequency = 12)
print(data2)
#Perform exploratory data analysis to know about the time series data
#Displays the start date of the time series data
start(data2)
# displays the end date of the time series data
end(data2)
#Displays the frequency of the time series data whether   monthly, quaterly, weekly.
frequency(data2)
#Displays descriptive statistics of the time series data
summary(data2)
#Install astsa package
install.packages("astsa")
library(astsa)
#To see acf and pacf in original data 
acf2(data2,max.lag = 24)
#Seasonally differenced retail sales
datadifference12=diff(data2,12)
#Plot seasonally differenced retail sales
plot.ts(datadifference12)
#Trend and seasonally differenced retail sales
difference1and12=diff(datadifference12,1)
#Plot Trend and seasonally differenced retail sales
plot.ts(difference1and12)
#Plot Trend and seasonally differenced retail sales
acf2(difference1and12, max.lag = 36)
#install forecast package
install.packages("forecast")
library(forecast)
#building  seasonal  ARIMA model
model1=arima(data2,order=c(2,1,1),seasonal=list(order=c(2,1,1),period=12))
summary(model1)
# Portmanteau or Box-Ljung test to check whether residuals are white noise
Acf(residuals(model1))
Box.test(residuals(model1),lag=24,type="Ljung")
#Rebuilding 〖ARIMA(6,1,1)(2,1,2)〗_12 model with different non seasonal terms
model2 <- arima(data2,order=c(6,1,1),seasonal= list
                (order=c(2,1,2),period=12))
# Portmanteau and Box-Ljung test on model2 to check whether residual are white noise
Acf(residuals(model2))
Box.test(residuals(model2),lag=24,fitdf = 1,type="Ljung")
#Forecast for the next 30 month
Pred <- forecast(model2,h=30)
Pred
#Creating the plot for forecast retail sales
plot(Pred,ylab="sales (million in dollars)",xlab="Year")

# :- Decision Tree Model
#Read the data from the working directory, create your own working directly to read the dataset.
data1 <- read.csv ("D:/RTCS/cancer_data.csv",header=TRUE,sep=",")
data2<-data.frame(data1)
# remove Sample_No from data 2 
data2 <- data2[,-1]
#perform exploratory data analysis to know about the data 
# display top 6 rows of dataset to see how data look like
head (data2)
# display bottom 6 rows to see how data look like
tail(data2)
# describe the structure of data , it displays the datatype of each variable present in the data like whether that particular varibale is numeric , factor etc .	
str(data2) 
#display the column name of the data
names(data2)
# display the datatype
class(data2)
# Check the missing values present in the data 
is.na(data2)
#to check the percentage of benign and malignant breast cancer in the data 
table(data2$Outcome)/nrow(data2)
Model Building & Interpretation on Full Data
#install randomForest package
install.packages("randomForest")
library(randomForest)
#Building Random Forests model on full data
rf_model <- randomForest(Outcome ~ ., data=data2, ntree=1500,mtry=3,importance=TRUE)
rf_model
Splitting Data set into Training and Testing 
#Set seed in order to reproduce the sample
set.seed(2) 
#splitting data set into training and testing dataset in 70:30 
install.packages("caTools")
library(caTools)
sample <- sample.split(data2$Outcome,SplitRatio=0.70)
#No of observations in train dataset	
train_data <- subset(data2,sample==TRUE)
# No of observations  in test dataset
test_data <- subset(data2,sample==FALSE)
# Model Building & Interpretation on Training and Testing Data 
#Building Random Forests model using training data
#install randomForest package
install.packages("randomForest")
library(randomForest)
r_model <- randomForest(Outcome ~ ., data=train_data,ntree=1500,mtry=3, importance=TRUE)
print(r_model)
plot(r_model)
plot(margin(r_model,test_data$Outcome))
# Predicting the model using test data
ran_pred <- predict(r_model,test_data)
#Display the confusion matrix or classification table
table(test_data$Outcome ,ran_pred)
# Predicting the probability matrix  using test data
ran_prob <- predict(r_model,test_data,type = "prob" )
ran_prob
ran_prob1<-data.frame(ran_prob)

 # churn_dataset :- Random  Forest Model 
data1 <- read.csv ("D:/RTCS/churn_dataset.csv",header=TRUE,sep=",")
data2<-data.frame(data1)
#Perform exploratory data analysis to know about the data 
#Display top 6 rows of dataset to see how data look li
head (data2)
#Display bottom 6 rows
tail(data2)
#Describe the structure of data
str(data2) 
#Display the column name of the data
names(data2)
#Display the datatype
class(data2)
#Display the summary or descriptive statistics of the data
summary(data2$Monthly_Charges)
#Set seed in order to reproduce the sample
set.seed(123) 
#Splitting data set into training and testing dataset in 70:30 
install.packages("caTools")
library(caTools)
sample <- sample.split(data2$Churn,SplitRatio=0.70)
#No of observations in train dataset	
train_data <- subset(data2,sample==TRUE)
# No of observations  in test dataset
test_data <- subset(data2,sample==FALSE)
#Growing full decision tree model by using rpart on training data
install.packages("rpart")
library(rpart)
churn_model <- rpart(Churn ~ ., data=train_data, 
                     method = "class", parms = list(split = 'information'),cp=-1)
churn_model
#Installing rattle package and helper package like rpart.plot and RcolorBrewer package in order to create the decision tree             
install.packages("rattle")
library(rattle)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("RColorBrewer")
library(RColorBrewer)
#Creating the decision tree plot by using fancyRpartPlot function 
fancyRpartPlot(churn_model)
#List of options that control the rpart algorithm and are ways preventing a model from overfitting. 
tree_model <- rpart(Churn ~ ., data=train_data, 
                    method = "class",parms = list(split = 'information'),  maxdepth = 3, minsplit = 2, minbucket = 2 )
tree_model
#Creating the decision tree plot 
fancyRpartPlot(tree_model)
#Plotting it inorder to find cp value for pruning 
plotcp(tree_model)
#Finding the cross validation results 
printcp(tree_model)
#Pruned model
prune_model <- prune(tree_model, cp=.02)
prune_model
#Creating the pruned decision tree plot 
fancyRpartPlot(prune_model)
#Predicting the model using test data
test_data$Churn_Class <- predict(prune_model, 
                                 newdata = test_data, type="class")
#Display the confusion matrix or classification table
table(test_data$Churn ,test_data$Churn_Class)
# Predicting the probability matrix  using test data
pred <- predict(prune_model, newdata = test_data, type="prob") 
pred1 <- data.frame(pred)


