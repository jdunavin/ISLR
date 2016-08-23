### Chapter 5 ISLR - Lab

### 5.3.1 Validation Set Approach

library(ISLR)
set.seed(1)
train=sample(392, 196)

lm.fit<-lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
# MSE of validation set
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# Do the same thing for ^2, ^3
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train)
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Repeat all this for a different sample
set.seed(2)
train=sample(392, 196)
lm.fit<-lm(mpg~horsepower, data=Auto, subset=train)
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train)
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# This would support the ^2 model over the others

### 5.3.2 Leave One Out Cross Validation
glm.fit <- glm(mpg~horsepower, data=Auto)
coef(glm.fit)
library(boot)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0,5)
for (i in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
# Again supports using 2 but not higher powers.

### 5.3.3 K-fold Cross Validation
set.seed(17)
cv.error10 <- rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,2), data=Auto)
  cv.error10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error10
# Still no evidence for using a higher order fit than 2
# The two numbers in delta: Second is bias-corrected.

### 5.3.4 Bootstrap
alpha.fn <- function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y)-cov(X,Y)) / (var(X) + var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)
set.seed(1)
# This is the same thing as getting a new bootstrap sample of size 100
alpha.fn(Portfolio ,sample (100,100, replace=T))
# But a cool function automates it
boot(data=Portfolio, statistic=alpha.fn, R=1000)

## Estimating accuracy of a linear model
boot.fn <- function(data,index) {
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)

# Bootstrap estimates for slope and intercept
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
boot.fn(Auto, sample(392, 392, replace=T))

# standard errors of 1000 bootstrap estimates for intercept and slopes
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef
# They both estimate the SE's but get a different answer, why?
# The second depends on the estimate of a noise variance ~= RSS

# Try with a second order term
boot.fn <- function(data, index) {
  coefficients(lm(mpg~horsepower + I(horsepower^2), data=data, subset=index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)