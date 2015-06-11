library(ISLR)
library(tree)

attach(Carseats)

### data manipulation
?Carseats # the dataset is to predict the Sales of child car seats in 400 locations
head(Carseats) # we can see that Sales is numerical variable, so we might need to create a categorical variable for the sake of classification

range(Sales) #Sales range from 0 to 16
High = ifelse(Sales >=8, "Yes", "No") # create a categorical variables bases on Sales
Carseats = data.frame(Carseats, High) # appends High to Carseat dataset, and now our dataset is ready!

### split data into training and testing 
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]
testing_data = Carseats[test, ]
testing_High = High[test]

### fit tree model for training data
tree_model=tree(High~.-Sales,training_data)

plot(tree_model)
text(tree_model,pretty=0)

# display the probablity

tree_model
### check accuracy of the model
predict_y=predict(tree_model,testing_data,type='class')
mean(predict_y!=testing_High)
### prune the tree


## perform cross validation to find the size of the tree
set.seed(1)
cv_tree=cv.tree(tree_model,FUN=prune.misclass)
names(cv_tree)

cv_tree$size
cv_tree$dev
# the lowest deviance is 61 which correspond to size = 7 or 9, try the two

## prune the tree
set.seed(1)
pruned_model=prune.misclass(tree_model,best=7)
plot(pruned_model)
text(pruned_model,pretty=0)
### check accuracy of pruned tree
predict_y=predict(pruned_model,testing_data,type='class')
mean(predict_y!=testing_High)
