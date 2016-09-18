# Chapter 6 - Applied Exercises

# Exercise 8

# 8a)
x = rnorm(100, mean = 3, sd = 1.5)
e = rnorm(100)
y <- 1 + 2 * x + 3 * x ^ 2 + e

# 8b)
df <- data.frame(x1 = x, x2 = x ^ 2, x3 = x ^ 3, x4 = x ^ 4, x5 = x ^ 5, x6 = x ^ 6, x7 = x ^ 7, x8 = x ^ 8, x9 = x ^ 9, x10 = x ^ 10)
X <- data.matrix(df)
df <- data.frame(y = y, df)

#8c)
library(leaps)
regfit.full <- regsubsets(y ~ ., df, nvmax = 10)
reg.sum <- summary(regfit.full)


#Plot rss, adj rsq, BIC, Cp all at same time
par(mfrow = c(2, 2))
plot(reg.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = 'l')
plot(reg.sum$adjr2, xlab = "Number of Variables", ylab = "Adj R2", type = 'l')
which.max(reg.sum$adjr2)
points(9, reg.sum$adjr2[9], col = 'red', pch = 20, cex = 2)
plot(reg.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
which.min(reg.sum$cp)
points(2, reg.sum$cp[2], col = 'red', pch = 20, cex = 2)
plot(reg.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
which.min(reg.sum$bic)
points(2, reg.sum$bic[2], col = 'red', pch = 20, cex = 2)
# max adj r2 at 9, min cp at 2, and min BIC at 2.
# I think the best model should be at 2.

# Get the best model's coefficients
coef(regfit.full, 2)
# That was close to what I expected

# 8d) Repeat using forward stepwise selection and backward stepwise selection
regfit.fwd <- regsubsets(y ~ ., df, method = "forward", nvmax = 10)
regfit.bwd <- regsubsets(y ~ ., df, method = "backward", nvmax = 10)
regsum.fwd <- summary(regfit.fwd)
regsum.bwd <- summary(regfit.bwd)

# Forward plots
par(mfrow = c(2, 2))
plot(regfit.fwd$rss, xlab = "Number of Variables", ylab = "RSS", type = 'l')
plot(regfit.fwd$adjr2, xlab = "Number of Variables", ylab = "Adj R2", type = 'l')
which.max(regfit.fwd$adjr2)
points(9, regfit.fwd$adjr2[9], col = 'red', pch = 20, cex = 2)
plot(regfit.fwd$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
which.min(regfit.fwd$cp)
points(2, regfit.fwd$cp[2], col = 'red', pch = 20, cex = 2)
plot(regfit.fwd$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')

# Backward plots
par(mfrow = c(2, 2))
plot(regfit.bwd$rss, xlab = "Number of Variables", ylab = "RSS", type = 'l')
plot(regfit.bwd$adjr2, xlab = "Number of Variables", ylab = "Adj R2", type = 'l')
which.max(regfit.bwd$adjr2)
points(9, regfit.bwd$adjr2[9], col = 'red', pch = 20, cex = 2)
plot(regfit.bwd$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
which.min(regfit.bwd$cp)
points(2, regfit.bwd$cp[2], col = 'red', pch = 20, cex = 2)
plot(regfit.bwd$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
which.min(reg.sum$bic)
points(2, reg.sum$bic[2], col = 'red', pch = 20, cex = 2)

# plots 2-4 of each block above gave errors above 2 variables, which I guess is good

# 8e) Fit a lasso to simulated data
library(glmnet)
set.seed(1)
par(mfrow=c(1,1))
grid <- 10 ^ seq(10, -2, length = 100)
cv.out <- cv.glmnet(X, y, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
lasso.mod <- glmnet(X, y, alpha = 1, lambda = grid)
plot(lasso.mod)
lasso.pred <- predict(lasso.mod, s=bestlam, newx=X)
lasso.coef <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:11,]
lasso.coef
# Again pretty close to what it should be, except the intercept term wasn't

# 8f) Skipping because dumb

# Exercise 9
library(ISLR)
College <- na.omit(College)
dim(College)
x = model.matrix(Apps ~ ., College)[, -1]
y <- College$Apps

# 9a) Training and test sets
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- ( - train)
y.test <- y[test]

# 9b) Fit linear model and show test error
glm.fit <- glm(Apps ~ ., data = College, subset = train)
summary(glm.fit)
glm.pred <- predict(glm.fit, newdata = College[test,])
mean((glm.pred - y.test) ^ 2)

# 9c) Fit ridge model on training set, choose lambda by CV, and report test error
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)
# Ridge beats glm

# 9d) Fit lasso model on training set, choose lambda by CV, report test error
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)
lasso.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test) ^ 2)

# 9e) Fit PCR model on training set, choose M by CV, report test error
library(pls)
pcr.fit <- pcr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = 'CV')
validationplot(pcr.fit, val.type = "MSEP", legendpos = 'topright')
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 16)
mean((pcr.pred - y.test) ^ 2)
# Loses to all foregoing methods

# 9f) Fit PLS model on training set, choose M by CV, report test error
pls.fit <- plsr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = 'CV')
validationplot(pls.fit, val.type = "MSEP", legendpos = 'topright')
pls.pred <- predict(pls.fit, x[test,], ncomp = 16)
mean((pls.pred - y.test) ^ 2)
# Loses to everything except e.

# 9g) 
# Material differences in test errors. 
sqrt(mean((glm.pred - y.test) ^ 2))
sqrt(mean((ridge.pred - y.test) ^ 2))
sqrt(mean((lasso.pred - y.test) ^ 2))
sqrt(mean((pcr.pred - y.test) ^ 2))
sqrt(mean((pls.pred - y.test) ^ 2))
# looks like you can get to within 1,000 or so, but considering some of the data that's pretty bad
summary(College$Apps)

## Exercise 10
set.seed(10)
# 10a) Fake data set, 20 columns of x's by 1000 rows, plus a y column
X <- matrix(norm(1000*20, mean=3, sd=10), 1000, 20)
beta <- sample(-5:5, size = 20, replace = TRUE)
y <- X %*% beta
df <- data.frame(y = y, X)
X <- model.matrix(y~., data=df)

# 10b) Training set of 100, test set of 900
train <- sample(1:nrow(X), nrow(X) / 10)
test <- ( - train)
y.test <- y[test]

# 10c) Best subset selection, plot training MSE associated with best model of each size
regfit.full <- regsubsets(y ~ ., df[train,], nvmax = 20)
reg.sum <- summary(regfit.full)


train.err <- rep(NA, 20)
for (i in 1:20) {
    coefi <- coef(regfit.full, id = i)
    pred <- X[train, names(coefi)] %*% coefi
    train.err[i] <- mean((df$y[train]-pred)^2)
}

test.err <- rep(NA, 20)
for (i in 1:20) {
    coefi <- coef(regfit.full, id = i)
    pred <- X[test, names(coefi)] %*% coefi
    test.err[i] <- mean((df$y[test] - pred) ^ 2)
}

#library(ggplot2)
train.err <- data.frame(train.err)
train.err$i <- as.numeric(row.names(train.err))
train.err$test.err <- test.err
ggplot(data = train.err, aes(x = i, y = train.err)) + geom_line() + geom_line(aes(x=i, y=test.err), color='red')
# I feel like the test curve should start to slope up, must be the way i generated my data

# Exercise 11
library(MASS)
Boston <- na.omit(Boston)

# Basically I'm going to repeat exercise 9 on this data
x <- model.matrix(crim ~ ., data = Boston)
y <- Boston$crim
set.seed(11)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- ( - train)
y.test <- y[test]

# best subset
regfit.full <- regsubsets(crim ~ ., Boston, nvmax = 13)
reg.sum <- summary(regfit.full)
    #Plot rss, adj rsq, BIC, Cp all at same time
par(mfrow = c(2, 2))
plot(reg.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = 'l')
plot(reg.sum$adjr2, xlab = "Number of Variables", ylab = "Adj R2", type = 'l')
which.max(reg.sum$adjr2)
points(9, reg.sum$adjr2[9], col = 'red', pch = 20, cex = 2)
plot(reg.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
which.min(reg.sum$cp)
points(8, reg.sum$cp[8], col = 'red', pch = 20, cex = 2)
plot(reg.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
which.min(reg.sum$bic)
points(3, reg.sum$bic[3], col = 'red', pch = 20, cex = 2)

coef9 <- coef(regfit.full, 9)
adjr2.pred <- x[test, names(coef9)] %*% coef9
print("Adjusted r2, best subset, MSE =")
mean((adjr2.pred - y.test) ^ 2)

coef8 <- coef(regfit.full, 8)
cp.pred <- x[test, names(coef8)] %*% coef8
print("Cp, best subset, MSE =")
mean((cp.pred - y.test) ^ 2)

coef3 <- coef(regfit.full, 3)
bic.pred <- x[test, names(coef3)] %*% coef3
print("BIC, best subset, MSE =")
mean((bic.pred - y.test) ^ 2)

# ridge
grid <- 10 ^ seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid)
set.seed(11)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
par(mfrow=c(1,1))
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test) ^ 2)

# lasso
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

# all of these models kinda stink, actually, with a squared error of 4
# Some of them predict negatives, which is stupid
summary(Boston$crim)