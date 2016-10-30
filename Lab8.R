# Chapter 8 Lab - Tree-based Methods

### Classification trees

library(tree)
library(ISLR)
attach(Carseats)

High <- ifelse(Sales <= 8, 'No', 'Yes')
Carseats <- data.frame(Carseats, High)

# Predict High using all variables except Sales.
tree.carseats <- tree(High ~ . - Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

# Training and test two-step
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[ - train,]
High.test <- High[ - train]
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)
#71.5% right

# Does pruning the tree lead to better results?
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
# $dev containst the error rates; the one with 9 terminal nodes is the winner
# Show me a plot
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

# Get the pruned tree
par(mfrow = c(1, 1))
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# How does this do?
tree.pred <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)
(94 + 60) / 200
#77% vs 71.5%, so quite a bit better

# What if we don't use the 'best' tree?
par(mfrow = c(1, 1))
prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# How does this do?
tree.pred <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)
(86 + 62) / 200
# 74%, still better than first tree but worse than 9 node tree

### Regression trees
# Want to try this with rpart
library(rpart)
library(rpart.plot)

library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- rpart(medv ~ ., Boston, subset = train, method='anova')
summary(tree.boston)
rpart.plot(tree.boston, extra = 101, type = 2, tweak = 1.2)
# You do get the result mentioned in the text about room size and lstat
plotcp(tree.boston)
printcp(tree.boston)
# These show what the highest CP should be that stops giving error improvements, right?
tree.boston2 <- prune(tree.boston, cp = 0.02)
rpart.plot(tree.boston2, extra = 101, type = 2, tweak = 1.2)

yhat <- predict(tree.boston, newdata = Boston[ - train,])
boston.test <- Boston[ - train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test) ^ 2)

### Bagging
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)
# Consider all 13 predictors at each split <=> bagging
bag.boston

# How does it perform?
yhat.bag = predict(bag.boston, newdata = Boston[ - train,])
plot(yhat.bag, boston.test)
abline(0, 1, col = 'red')
mean((yhat.bag - boston.test) ^ 2)
# Way better than plain old tree

# Try a different number of trees
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25, importance = TRUE)
yhat.bag = predict(bag.boston, newdata = Boston[ - train,])
mean((yhat.bag - boston.test) ^ 2)

# Same thing, but this time use a smaller mtry, as random forests do
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[ - train,])
mean((yhat.rf - boston.test) ^ 2)
# this is better still.

importance(rf.boston)
# Left column - mean decrease of accuracy in predictions on the OOB samples when a given variable is excluded from the model
# Right column - total decrease in node impurity that results from splits over that variable, averaged over all trees
varImpPlot(rf.boston)

### Boosting
library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = 'gaussian', n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
par(mfrow = c(1, 2))
plot(boost.boston, i = 'rm')
plot(boost.boston, i = 'lstat')
# Median prices are rising with rising values of rm, and falling with rising values of lstat
#How did we do?
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test) ^ 2)

# Change the shrinkage parameter
boost.boston =gbm(medv~.,data=Boston [train ,], distribution="gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,], n.trees =5000)
mean(( yhat.boost -boston.test)^2)