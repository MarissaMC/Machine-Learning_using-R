### Multiple Linear Regression

# Load the MASS package in R, so we can get access
# to the Boston Dataset

library(MASS)
attach(Boston)


### we will use many predictors as our explantory variable, 
## and medv as our response variable

#split the data

train = 1:400
test = -train

training_data = Boston[train,]
testing_data = Boston[test,]


### let's try to add age variable, but first we need to check 
### for linearity

plot(age, medv)
cor(age, medv)

### we don't need to do any transofrmation because we 
### we don't see any non linear trent in the plot

### Create the model using training data

model = lm(medv~ log(lstat) + age, 
           data = training_data)
summary(model)

#Notice that R-squared has increased from 62% to 66%
#when we added the age predictor



#### Create a model with all variables included

model = lm(medv~.-lstat+log(lstat), data = training_data)
summary(model)

### to check for correlations

round(cor(training_data), digit =2)

### check visullay for linearity
pairs(training_data)

### zoom into fewer variables
pairs(training_data[,c(1,2,3,14)])


### We would like to check for collinearity in our dataset.
### i.e. if the relationship between the predictors (x's) is high

round(cor(training_data), digit =2)

## use the VIF criteria (Variance Inflation Factor)
## in R, fucntion vif() is in package car
library(car)
vif(model)

### rad and tax has the highest VIF, and this was 
### clear when we computed the correlation above (0.87)
### This means, that we need to get rid of one of them

#confidence interval
confint(model,level=0.95)

model = lm(medv~.-lstat+log(lstat)-tax, data = training_data)
summary(model)

### Check for Interactions

model = lm(medv ~log(lstat) + age + log(lstat):age)
summary(model)


### Asseing the model
model = lm(medv~.-lstat+log(lstat)-tax, data = training_data)
summary(model)


y = testing_data$medv

y_hat = predict(model,
                testing_data[,-14])

### Computer the Mean Squared Error 

error = y - y_hat
MSE = mean(error^2)
MSE

##fitting Logistic Regression
## Try to predict the Stock market

library(ISLR)
attach(Smarket)

?Smarket
head(Smarket)
round(cor(Smarket[,-9]),digit=2)
pairs(Smarket[,-9])

### split the data into training(2001-2004) and testing(2005)

train=(Smarket$Year<2005)
test=!train

training_data=Smarket[train,-8]
testing_data=Smarket[test,-8]


##slect the y (Direction ) from the testing data
testing_y=testing_data[,8]

##fit a logistic regression model

model = glm(Direction ~.,data=training_data,family="binomial")
summary(model)

## assess the model

predicted_probs= predict(model,testing_data,type="response")
head(predicted_probs)

## we do not know how R code the up and down,use function contrasts()

contrasts(Direction)

##we need to creat new vector of UP and Down based on the predicted probs,

##initialize y_hat with a vector full of ups
y_hat=rep("Up",length(testing_y))


y_hat[predicted_probs<0.5]<-"Down"

## which funciton return the index

mean(testing_y != y_hat)

table(testing_y,y_hat)

for (i in seq(0.2,0.7,0.005))



