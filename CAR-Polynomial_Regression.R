
# Data Mining and Machine Learning continuous Assessment
# A00279933
# by
# K R ROOPESH BHARATWAJ

#PART 2- POLYNOMIAL  REGRESSION

# Step 1: Data Preprocessing 

# 1.1 Importing the dataset

install.packages('ggplot2')
library(ggplot2)
install.packages('caTools')
library(caTools)

dataset2 =read.csv("mpg.csv",header= TRUE, sep=",")
dataset2
summary(dataset2)
set.seed(200)

# 1.2 Splitting the dataset(80% for the Train and 20% for the Test)

index <-sample(1: nrow(dataset2), size=0.8*nrow(dataset2))
train =dataset2[index,]
test= dataset2[-index,]
colnames(train)
colnames(test)
summary(dataset2$mpg)
hist(dataset2$mpg)

#  1.3 scatter matrix for each variable 

plot(dataset2[1:9], main ="Scatter plot all variables")

#  1.4 scatter matrix of best fit in Linear Model

pairs(~ mpg+ cylinders+ displacement + horsepower + weight + acceleration  + model.year, data=dataset2, main="scatter matrix of best LM Fit")


# Step 2: Fitting Polynomial Regression with best model from linear regression

# Step 2.1: Data Model 1 # with degree 2

prmodel <- lm( mpg ~ poly( cylinders,displacement , horsepower, weight, origin  ,acceleration  , model.year, degree =2, raw =T),data=train)
summary(prmodel) # Multiple R-squared:  0.8896,	Adjusted R-squared:  0.8759 

# step 2.2 Model 2 # when degree is 3

prmodel1 <- lm( mpg ~ poly( cylinders,displacement , weight, origin  ,acceleration  , model.year,degree =3, raw =T),data=train )
summary(prmodel1) #Multiple R-squared:  0.9324,	Adjusted R-squared:  0.9088 

# step 2.3 Model 3 # when degree is 4, removing Horsepower because of High P value
prmodel2 <- lm( mpg ~ poly(cylinders,displacement , weight, origin  ,acceleration  , model.year,degree =4, raw =T), data=train)
summary(prmodel2) # 0.9664,	Adjusted R-squared:  0.9211 

# step 2.4 Model 4 # when degree is 5
prmodel3 <- lm( mpg ~ poly(cylinders,displacement , weight, origin  ,acceleration  , model.year,degree =5, raw =T), data=train)
summary(prmodel3)  # Multiple R-squared:      1,	Adjusted R-squared:    NaN ### (suspicious levels)

# step 2.5 Model 5 # when degree is 10 # for checking purpose only
prmodel4 <- lm( mpg ~ poly(cylinders,displacement , weight, origin  ,acceleration  , model.year,degree =10, raw =T), data=train)
summary(prmodel4) # Multiple R-squared:      1,	Adjusted R-squared:    NaN ### ### (suspicious levels)


#RESULT MODEL 1 IS BETTER THAN ALL OTHER MODELS


# Step 3: Predicting the Test set results (Prediction on Models)

# Step 3.1 Prediction on Model 1 

prediction1 = predict(prmodel,test)
predict(prmodel,test, interval = "confidence")
predict(prmodel,test, interval = "predict")

#
# Step 3.3 Prediction on Model 3
prediction3 = predict(prmodel2,test)
predict(prmodel2,test, interval = "confidence")
predict(prmodel2,test, interval = "predict")

# Step 3.4 Prediction on Model 4
prediction4 = predict(prmodel3,test)
predict(prmodel3,test, interval = "confidence")
predict(prmodel3,test, interval = "predict")


# Step 3.4 Prediction on Model 4
prediction5 = predict(prmodel4,test)
predict(prmodel4,test, interval = "confidence")
predict(prmodel4,test, interval = "predict")

# step 3.6 RSME calculation for models

# for model 1
sqrt(mean((test$mpg - prediction1)^2)) # 2.116572 # best one with lowest RMSE 
# for model 2
sqrt(mean((test$mpg - prediction2)^2)) # 3.115849  # ok, but still high compared with 2.11 
# for model 3
sqrt(mean((test$mpg - prediction3)^2)) # 58.31147 # over fitted
# for model 4
sqrt(mean((test$mpg - prediction4)^2)) # 117849   # over fitted
# for model 5
sqrt(mean((test$mpg - prediction5)^2))  # 704280007 # over fitted

# RESULT OF PREDICTION IS model 1 is better than other model 

# STEP 4 PLOTTING 

# Plotting the predicted values for the data set using the polynomial model
# step 4.1  Plotting Training set

ggplot()+geom_point(aes(x=train$displacement + train$horsepower+ train$weight+ train$origin  +train$acceleration  + train$model.year, y= train$mpg),colour="red")+
  geom_line(aes(x= train$displacement + train$horsepower+ train$weight+ train$origin  +train$acceleration  + train$model.year, y= predict(prmodel1, newdata = train)),colour ="blue")+ggtitle('mpg vs model1 train data  of Polynomial Regression')+ylab('mpg')

# step 4.2  Plotting Testing set

ggplot()+geom_point(aes(x=test$displacement + test$horsepower+ test$weight+ test$origin  +test$acceleration  + test$model.year, y= test$mpg),colour="red")+
geom_line(aes(x= test$displacement + test$horsepower+ test$weight+ test$origin  +test$acceleration  + test$model.year, y= predict(prmodel1, newdata = test)),colour ="blue")+ggtitle('mpg vs model1 test data of Polynomial Regression')+ylab('mpg')




# Final Result:  Best Fitted Model + Prediction is MODEL-1


