setwd('/Users/ethanshao/Desktop/mengchun/DSO530/R')
wine=read.csv("winedata.csv")
summary(wine)
set.seed(1)
test=sample(nrow(wine),1000)
train=-test
training_data=wine[train,]
testing_data=wine[test,]
# library(ISLR)
# attach(Smarket)
# train = Year < 2005
# test = !train
##########################################
# have first image of data
# dim(Smarket)  not very useful
# pairs(wine)  draw plot not useful
# cor(x)  corelation bewteen data, should be number
##########################################
# Logistic Regression

model = glm(quality ~.,
            data = training_data, 
            family = "binomial")

summary(model)

testing_y=testing_data[,12]
predicted_probs = predict(model, 
                          testing_data, 
                          type ="response")
head(predicted_probs)
contrasts(training_data$quality)
y_hat = rep("good", length(testing_y))
y_hat[predicted_probs > 0.5] = "not good"
mean(testing_y != y_hat)

# confusion matrix
table(y_hat,testing_y)
contrasts(wine$quality)
###########################################
#LDA

library(MASS)
lda_model=lda(quality~.,training_data)
summary(lda_model)
pred_lda=predict(lda_model,testing_data)
names(pred_lda)

pred_lda_y=pred_lda$class
View(pred_lda)
table(pred_lda_y,testing_y)
###########################################
#QDA
library(MASS)
qda_model=qda(quality~.,training_data)
summary(qda_model)

pred_qda=predict(qda_model,testing_data)
names(pred_qda)

pred_qda_y=pred_qda$class
#confusion matrix

table(pred_qda_y,testing_y)

mean(pred_qda_y != testing_y)
#############################################
#knn
library(class)
# normalize

training_data = scale(wine[train,-12])
testing_data = scale(wine[test,-12])

testing_y = wine[test,12]
training_y= wine[train,12]

#error_rate<-vector(mode="list",length=100)
error_rate = NULL
for (i in 1:100)
{set.seed(1);
knn_pred_y=knn(training_data,testing_data,training_y,k=i);
error_rate[i]=mean(knn_pred_y!=testing_y);}

plot(1:100,error_rate,xlab="K",ylab="error_rate",main="Misclassification Error Rate vs values of
K")

# library(ggplot2)
# qplot(1:300, error_rate, xlab = "K",
# ylab = "Error Rate", geom=c("point", "line"))

### find the minimum error rate 
min_error_rate = min(error_rate) 
# print(min_error_rate)

K = which(error_rate == min_error_rate)
# print(K)

set.seed(1);
knn_pred_y=knn(training_data,testing_data,training_y,k=1);
table(knn_pred_y,testing_y)
mean(knn_pred_y!=testing_y)

