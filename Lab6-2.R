# Chapter 6 Lab 2 - Ridge and Lasso
library(ISLR)

#glmnet requires a numeric matrix, not a data frame
Hitters <- na.omit(Hitters)
x = model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

# Part 1 - Ridge regression
library(glmnet)
grid <- 10 ^ seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # alpha= 0 -> ridge, alpha =1 -> lasso, lambda = penalty values

# Makes coefs for each value of lambda
dim(coef(ridge.mod))

# Look at lambda #50, 11,498
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 penalty, ignore the intercept

# Lok at lambda #60, 705
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60] ^ 2))

# Obtain predictions using the model from a specific value of lambda
predict(ridge.mod, s = 50, type = 'coefficients')[1:20,]

# Make indices for a training and a test set
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- ( - train)
y.test<- y[test]

# Fit on the training set and get the MSE on the test set, set lambda=4
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12) 
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# Do a dumb regression and fit the mean of every training observation
mean((mean(y[train]) - y.test) ^ 2)

# Try a very large penalty value - should kick out all coefs and do the same thing
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# Plain old least squares - we'd really do this with lm, but setting s=0 is the same thing
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = TRUE)
mean((ridge.pred - y.test) ^ 2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = TRUE, type = 'coefficients')[1:20,]

# So lambda = 4 was a little better than plain old least squares, let's do cross-validation to get the right lambda
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# So lambda = 212 gives the best test error

out <- glmnet(x, y, alpha = 0)
predict(out, type = 'coefficients', s = bestlam)[1:20,]

# Part 2 -- Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Cross validate to find l-1 penalty
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test) ^ 2)

# Better than plan but not as good as ridge, but cut it down to seven variables
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = 'coefficients', s = bestlam)[1:20,]
lasso.coef

