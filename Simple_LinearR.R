### Simple Linear Regression

# Load the MASS package in R, so we can get access
# to the Boston Dataset

library(MASS)
attach(Boston)

## Learn about Boston
?Boston

### we will use lstat as our explantory variable, 
## and medv as our response variable

#split the data

train = 1:400
test = -train

variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]


## step 1: Check for linearity 

plot(training_data$lstat, training_data$medv)

### the relationship deviates a little bit from linearity
### so maybe we need to tranform the variable
### take the log of your data
training_data = log(Boston[train, variables])
attach(training_data)
plot(lstat, medv)


## step 2: fit the linear model using lm()

modelFit = lm(medv~lstat, data = training_data)

modelFit

### more detailed output
summary(modelFit)


## step 3: plot the regression line

plot(lstat, medv, 
     xlab = "Percentage of household with low income",
     ylab = "Median House Value", 
     col = "blue", 
     pch = 20)
abline(modelFit, col = "red", lwd =3)


### side note: play with pch 

plot(1:20, 1:20, pch = 1:20)

## Step 4: Assess the data using the dataset
y_hat = predict(modelFit, data.frame(lstat = testing_data$lstat))
y_test = testing_data$medv

mean((y_hat - y_test)^2)



