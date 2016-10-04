### Chapter 8 - Applied exercises

# Needed libraries
library(ISLR)
library(MASS)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)

### Exercise 7
mtry.val <- seq(3, 13, 1)
ntree.val <- seq(100, 1000, 100)
err.df <- data.frame(mtry = rep(0, 110), ntree = rep(0, 110), err = rep(NA, 110))

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
i = 0
boston.test <- Boston[ - train, "medv"]

for (m in mtry.val) {
    for (n in ntree.val) {
        rf.fit <- randomForest(medv ~ ., data = Boston, subset = train, mtry = m, ntrees = n, importance = TRUE)
        yhat.rf <- predict(rf.fit, newdata = Boston[ - train,])
        err.df$mtry[i] <- m
        err.df$ntree[i] <- n
        err.df$err[i] <- mean((yhat.rf - boston.test) ^ 2)
        i = i + 1
    }
}

# Plot the errors by mtry and mtree
ggplot(data = err.df, aes(x = ntree, y = err, color = as.factor(mtry))) + geom_line(aes(group = mtry)) +
    labs(x = 'Number of trees', y = 'Test error', color = 'Number of split vars') +
    theme(legend.position='top',legend.direction='horizontal')
# Looks like 5 variables and 600 trees is the ideal combination
# Best number of trees appears to vary inconsistently with number of splitters


### Exercise 8
set.seed(8)
attach(Carseats)

train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
seats.tree <- rpart(Sales ~ ., data = Carseats, subset = train, method = 'anova')
rpart.plot(seats.tree, extra = 101, type = 2, tweak = 1.2)
# Sales are higher for better shelf locations, as expected
# Sales are considerably better for these when the price is <= $108
# Higher than that and the community income makes a difference
# Weirdly, sales improve on prices >= 116 where competitor price is closer < 124 than otherwise

plotcp(seats.tree)
# set cp = 0.029, size = 7
seats.tree2 <- prune(seats.tree, cp=0.029)
rpart.plot(seats.tree2, extra = 101, type = 2, tweak = 1.2)

test.seats <- Carseats[ - train, 'Sales']
yhat1 <- predict(seats.tree, newdata = Carseats[ - train,])
yhat2 <- predict(seats.tree2, newdata = Carseats[ - train,])

mean((test.seats - yhat1) ^ 2)
mean((test.seats - yhat2) ^ 2)
# Very small improvement in test MSE

# Now try bagging
seats.bag <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = 10)
yhat.bag <- predict(seats.bag, newdata = Carseats[ - train,])
mean((test.seats - yhat.bag) ^ 2)
# Considerably better than regression tree
importance(seats.bag)
varImpPlot(seats.bag)
# Price (of course) and shelf location are the primary drivers

# Now try random forest
mtry.val <- seq(1, 10, 1)
ntree.val <- seq(100, 1000, 100)
err.df <- data.frame(mtry = rep(0, 100), ntree = rep(0, 100), err = rep(NA, 100))
i = 1
for (m in mtry.val) {
    for (n in ntree.val) {
        rf.fit <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = m, ntrees = n, importance = TRUE)
        yhat.rf <- predict(rf.fit, newdata = Carseats[ - train,])
        err.df$mtry[i] <- m
        err.df$ntree[i] <- n
        err.df$err[i] <- mean((yhat.rf - test.seats) ^ 2)
        i = i + 1
    }
}
ggplot(data = err.df, aes(x = ntree, y = err, color = as.factor(mtry))) + geom_line(aes(group = mtry)) +
    labs(x = 'Number of trees', y = 'Test error', color = 'Number of split vars') +
    theme(legend.position = 'top', legend.direction = 'horizontal')

# It looks like mtry = 1,2,3 won't work, so let's get rid of those

err.df2 <- err.df[err.df$mtry > 3,]
ggplot(data = err.df2, aes(x = ntree, y = err, color = as.factor(mtry))) + geom_line(aes(group = mtry)) +
    labs(x = 'Number of trees', y = 'Test error', color = 'Number of split vars') +
    theme(legend.position = 'top', legend.direction = 'horizontal')
# It looks like bagging with 200 trees gives the best test error
# Up to a point, increasing mtry seems to lower test error. But, the difference in MSE between 4 and 10 was 0.4, or about 1/6th - 1/7th


### Exercise 9
set.seed(9)
train <- sample(1:nrow(OJ), 800)

# Fit a regression tree to Purchase
oj.tree <- rpart(Purchase ~ ., data = OJ, subset = train, method = 'class')
summary(oj.tree)$cptable
plotcp(oj.tree)
# 7 terminal nodes
oj.tree

# It looks like there is a threshold of CH brand loyalty beyond which there is less price sensitivity
# Both a high threshold and a low one
# Also, something is happening at stores 0 and 1 and not the others to favor MM, despite MM being more expensive!
# Among those with loyalty between 0.48 and 0.75, CH gets a 16-cent "forgiveness" in price sensitivity, meaning
# a 16 cent difference in price will cause people to switch to MM
# On the other branch, a 31 cent price difference will cause MM users to switch to CH at stores 0 and 1, 
# but not at the other stores (few cases)

rpart.plot(oj.tree, extra = 101, type = 2, tweak = 1.0)

# Predictions and test error rate
yhat.tree <- predict(oj.tree, newdata = OJ[ - train,], type='class')
oj.truth <- OJ$Purchase[ - train]
# truth table
table(oj.truth, yhat.tree)
mean(oj.truth != yhat.tree)
# 20.7% test error rate

# Prune the tree, from the plotcp it looks like cp=0.02
oj.tree2 <- prune(oj.tree, cp = 0.02)

# training error
oj.train <- OJ$Purchase[train]
ytrain.tree <- predict(oj.tree, newdata = OJ[train,], type = 'class')
table(oj.train, ytrain.tree)
mean(oj.train != ytrain.tree)
ytrain.tree2 <- predict(oj.tree2, newdata = OJ[train,], type = 'class')
table(oj.train, ytrain.tree2)
mean(oj.train != ytrain.tree2)
# Higher training error for the pruned model

# test error
yhat.tree2 <- predict(oj.tree2, newdata = OJ[ - train,], type = 'class')
oj.truth <- OJ$Purchase[ - train]
table(oj.truth, yhat.tree2)
mean(oj.truth != yhat.tree2)
# 20.37%, a little better than unpruned


### Exercise 10 - Salary in Hitters data set
