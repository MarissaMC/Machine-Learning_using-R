# bagging and random forest

library(randomForest)
library(MASS)

attach(Boston)

## bagging
set.seed(1)
bag_model=randomForest(medv~.,data=Boston,mtry=13,importance=T)

# default number of trees is 500
bag_model

## random forest
set.seed(1)
rf_model=randomForest(medv~.,data=Boston,mtry=4,importance=T)
rf_model

# check for important variables

rf_model$importance

barplot(sort(rf_model$importance[,2],decreasing=T))