library(MASS)
library(tree)
attach(Boston)


### split data into training and testing 
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = - train

training_data = Boston[train,]
testing_data = Boston[test,]
testing_medv = medv[test]

### fit tree model for training data

tree_model=tree(medv~.,training_data)
plot(tree_model)
text(tree_model)
### check accuracy of the model
predicted_y=predict(tree_model,testing_data)
mean((predicted_y-testing_medv)^2)
### prune the tree
cv_tree=cv.tree(tree_model)
min_index=which.min(cv_tree$dev)
min_index

tree_size=cv_tree$size[min_index]
tree_size

#the pruned tree is the same as the original tree, no need for pruning

# if need to prune, the code will be
pruned_tree=prune.tree(tree_model,best=8)
plot(pruned_tree)
text(pruned_tree)

predicted_y=predict(pruned_tree, testing_data)
mean((predicted_y-testing_medv)^2)

### check accuracy of pruned tree


