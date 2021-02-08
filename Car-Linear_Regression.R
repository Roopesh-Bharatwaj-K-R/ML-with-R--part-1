
# Data Mining and Machine Learning continuous Assessment
# A00279933
# by
# K R ROOPESH BHARATWAJ
#PART 1- LINEAR REGRESSION


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

# 1.3 correlation of all variables

cor(dataset2$cylinders,dataset2$mpg)  #-0.7753963
cor(dataset2$displacement,dataset2$mpg) #-0.8042028
cor(dataset2$horsepower,dataset2$mpg) #-0.7542762
cor(dataset2$weight,dataset2$mpg)  #-0.8317409
cor(dataset2$acceleration,dataset2$mpg) #  0.4202889
cor(dataset2$car.name,dataset2$mpg) # cannot be done  for strings
cor(dataset2$model.year,dataset2$mpg) #0.5792671
cor(dataset2$origin,dataset2$mpg) #0.5634504

#  1.4 scatter matrix for each variable 

plot(dataset2[1:9], main ="Scatter plot all variables")

#  1.5 Dropping variable which as not required

pairs(~ mpg+ cylinders+ displacement + horsepower + weight + acceleration +car.name + model.year, data=dataset2, main="scatter matrix-1")

# Add extra variables in the pairs function
pairs(~ mpg+  displacement + horsepower+ weight,  data=train, main="scatter matrix -2")

# Step 2: Fitting Multiple Linear Regression 

# Step 2.1: Data Model 1 #  REMOVED STRING Variable (car.name)
model1 <-lm(mpg~ cylinders+ displacement + horsepower+ weight+ origin  +acceleration  + model.year, data=train)
summary(model1) #  Multiple R-squared:  0.8179,	Adjusted R-squared:  0.8138 
plot(model1)
abline(model1, col="red")

# Step 2.2: Data Model 2 # +acceleration variable removed based high on P value
model2 <-lm(mpg~  displacement + horsepower+ weight+ origin + model.year, data=train)
summary(model2) # Multiple R-squared:  0.8168,	Adjusted R-squared:  0.8139 
plot(model2)
abline(model2, col="red")

# Step 2.3: Data Model 3: #  variable Horsepower is Removed because of high P value 
model3 <-lm(mpg~  displacement +  weight+ origin  + model.year, data=train)
summary(model3) # Multiple R-squared:  0.8152,	Adjusted R-squared:  0.8129 
plot(model3)
abline(model3, col="red")

# RESULT OF MODEL BASED ON THE R-Squared : Model 1 is better than Model 2,3

# step 2.4  #finding the best model using Means Squared Error

# model 1 Mean Squared Error

model1_summ<-summary(model1)
mean(model1_summ$residuals^2) # 11.41574 (BEST MODEL WITH LOWEST MSE on TRAIN DATA)
# model 2: Mean Squared Error
model2_summ<-summary(model2)
mean(model2_summ$residuals^2) # 11.48437
# model 3 : Mean Squared Error
model3_summ<-summary(model3)
mean(model3_summ$residuals^2) # 11.58157

# RESULT of MEAN SQUARED ERROR:  MODEL 1 IS BETTER THAN MODEL 2,3 

# Step 3: Predicting the Test set results (Prediction on Models 1,2,3)

# Step 3.1 Prediction on Model 1 

prediction1 = predict(model1,test)
predict(model1,test, interval = "confidence")
predict(model1,test, interval = "predict")

# Step 3.1 Prediction on Model 2
prediction2  = predict(model2,test)
predict(model2,test, interval = "confidence")
predict(model2,test, interval = "predict")

# Step 3.1 Prediction on Model 3
prediction3 = predict(model3,test)
predict(model3,test, interval = "confidence")
predict(model3,test, interval = "predict")


# step 3.2 RSME calculation for(ACTUAL-PREDICITED) models
# for model 1
sqrt(mean((test$mpg - prediction1)^2))  # 2.998772 BEST PREDICTED MODEL
# for model 2
sqrt(mean((test$mpg - prediction2)^2)) # 3.022216
# for model 3
sqrt(mean((test$mpg - prediction3)^2)) # 3.030346

# step 4 plotting the predicted values of best model

# step 4.1 Plotting for Training set
ggplot()+geom_point(aes(x=train$displacement +train$cylinders+ train$horsepower+ train$weight+ train$origin  +train$acceleration  + train$model.year, y= train$mpg),colour="red")+
geom_line(aes(x= train$displacement +train$cylinders+ train$horsepower+ train$weight+ train$origin  +train$acceleration  + train$model.year, y= train$mpg),colour ="blue")+ggtitle('mpg vs model1 train data')+ylab('mpg)')

# step 4.2 Plotting for Test Set

ggplot()+geom_point(aes(x=test$displacement +test$cylinders+ test$horsepower+ test$weight+ test$origin  +test$acceleration  + test$model.year, y= test$mpg),colour="red")+
geom_line(aes(x= test$displacement +test$cylinders+ test$horsepower+ test$weight+ test$origin  +test$acceleration  + test$model.year, y= test$mpg),colour ="blue")+ggtitle('mpg vs model1 test data')+ylab('mpg)')


# RESULT OF PREDICTION IS model 1 is better than model 2,3


# SUMMARY

# Final Result:  Best Fitted Model + Prediction is MODEL-1


















































































































































































































































































































































































































