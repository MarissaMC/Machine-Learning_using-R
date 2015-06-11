library (ISLR)

attach(Auto)
names(Auto)

### regular approach
dim(Auto)
set.seed(1)
train=sample(1:392,191)
test=-train

training_data=Auto[train,]
testing_data=Auto[test,]
testing_y=mpg[test]

### creat a linear model of degree 1

model_l=lm(mpg~horsepower,training_data)
pred_y=predict(model_l,testing_data)

error=pred_y-testing_y
MSE_l=mean(error^2)
MSE_l

### creat a qaudratic model of degree 2

model_q=lm(mpg~poly(horsepower,2),training_data)
pred_y=predict(model_q,testing_data)

error=pred_y-testing_y
MSE_q=mean(error^2)
MSE_q

### creat model from degree 1 to 10

start=proc.time()
error=NULL
MSE=NULL
for (i in 1:10){
  model=lm(mpg~poly(horsepower,i),training_data)
  pred_y=predict(model,testing_data)
  
  error=pred_y-testing_y
  MSE[i]=mean(error^2)
}
end=proc.time()

plot(1:10,MSE)

### LOOCV approach
library(boot)## to run cross validation
# using whole dataset as training data
model_LOOCV=glm(mpg~horsepower,data=Auto)

## assess the model using cross validation
LOOCV_output=cv.glm(Auto,model_LOOCV)
names(LOOCV_output)
LOOCV_output$delta
MSE_LOOCV=LOOCV_output$delta[1]

### creat model from degree 1 to 10

##10-fold
start=proc.time()
error=NULL
MSE_10fold=NULL
for (i in 1:10){
  set.seed(1)
  model_10fold=glm(mpg~poly(horsepower,i),data=Auto)
  k10fold_output=cv.glm(Auto,model_10fold,K=10)

  MSE_10fold[i]=k10fold_output$delta[1]
}
end=proc.time()

plot(1:10,MSE_10fold,type="l",xlab="degree",ylab="MSE")