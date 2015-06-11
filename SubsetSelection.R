library(ISLR)

?Hitters

attach(Hitters)

## check for missing values
sum(is.na(Salary))

model=lm(Salary~.,data=Hitters)

Hitters=na.omit(Hitters)
attach(Hitters)
dim(Hitters)

# the observations with missing values are 

# Use subset selection to decide what variables should be in our model

library(leaps)
model=regsubsets(Salary~.,data=Hitters,nvmax=19)

# given due to MSE

model_summary=summary(model)

# the star of the output refers theat the variable is included in the model
# 8 model because of the default 8, us nvmax to define

names(model_summary)

model_summary$adjr2

# based on adj r2

n=1:19

plot(n,model_summary$adjr2,xlab="number of variables",ylab="adjusted R^2",type="o")

which.max(model_summary$adjr2)

points(11,model_summary$adjr2[11],col="red",cex=2,pch=20)

abline(b=11,col="blue")

plot(n,model_summary$cp,xlab="number of variables",ylab="adjusted R^2",type="o")

which.min(model_summary$cp)

par(mfrow=c(1,2))

points(10,model_summary$cp[10],col="red",cex=2,pch=20)

abline(v=10,col="blue")
