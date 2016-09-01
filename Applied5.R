### Chapter 5 Applied Exercises

library(MASS)
library(ISLR)


#5a fit LR default~income + balance
set.seed(5)
glm.fit <- glm(default~income+balance, data=Default, family="binomial")
summary(glm.fit)
test.err <- rep(0,4)
# 5b Use validation set approach to estimate test error
nrow(Default)
train = sample(10000, 7500)
glm.fit <- glm(default~income+balance, data=Default, family='binomial', subset=train)
summary(glm.fit)
test = Default[-train,]
glm.probs <- predict(glm.fit, test, type='response')
glm.pred <- rep('No',nrow(test))
glm.pred[glm.probs>=0.5]<-'Yes'
test.err[1] = 1-mean(glm.pred==test$default)

# 5c Repeat with three new samples
for (i in 2:4) {
  set.seed(i)
  train = sample(10000,7500)
  glm.fit = glm(default~income+balance, data=Default, family='binomial', subset=train)
  test = Default[-train,]
  glm.probs <- predict(glm.fit, test, type='response')
  glm.pred <- rep('No',nrow(test))
  glm.pred[glm.probs>=0.5]<-'Yes'
  test.err[i] = 1-mean(glm.pred==test$default)
}
test.err
# These are all within 0.3% of each other, 0.3% of 2,500
# is 7.5, so only 8 or so observations are different between 
# the models

# 5d - add student as a variable
set.seed(6)
train = sample(10000,7500)
glm.fit = glm(default~income+balance+student, data=Default, family='binomial', subset=train)
test = Default[-train,]
glm.probs <- predict(glm.fit, test, type='response')
glm.pred <- rep('No',nrow(test))
glm.pred[glm.probs>=0.5]<-'Yes'
test.err5d = 1-mean(glm.pred==test$default)
test.err5d
# This is a little bit worse than what was obtained before

# Exercise 6 - Bootstrap vs standard estimates for SE
set.seed(6)
glm.fit <- glm(default~income+balance, data=Default, family='binomial')
# 6a - Standard errors from summary
summary(glm.fit)$coefficients[,2]

# 6b - boot.fn function
boot.fn <- function(data, index) {
  return(coef(glm(default~income+balance, data=Default, family="binomial",subset=index)))  
}

# 6c - estimate coefs and SEs
boot.fn(Default, sample(10000,10000, replace=T))
boot(Default, boot.fn, 1000)

# 6d - Comments?
# What can I say - these are really close to each other,
# SEs were off by about 2 percent
df.se <- data.frame(se.glm = summary(glm.fit)$coefficients[,2], se.boot = c(4.495459e-01,4.943294e-06,2.403762e-04))
# this is how much they were off by - balance was the one that was furthest off by 6%
# which is about $50 
(df.se$se.glm - df.se$se.boot) / df.se$se.glm

### Exercise 7

#7a glm that uses direction, lag1 and lag2
glm.fit <- glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)

#7b glm that uses all observations but the first
glm.fit1 <- glm(Direction~Lag1+Lag2, data=Weekly, family=binomial, subset=2:nrow(Weekly))

# 7c Make the call 
pred <- predict(glm.fit, Weekly[1,])
if (pred >=0.5) {1} else {0}

# 7d Put this logic in a loop to do LOOCV
pred <- rep(0,nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  wkly <- Weekly[-i,]
  glm.fit <- glm(Direction~Lag1+Lag2, data=wkly, family=binomial)
  pred[i] <- if (predict(glm.fit, Weekly[i,]) >= 0.5) {"Up"} else {"Down"}
}

#7e estimate the test error
table(pred, Weekly$Direction)
mean(pred==Weekly$Direction)
# Accuracy of 45.4%, which is actually worse than random guessing
# That's okay, we are taught since birth that this is what is supposed to happen

### EXERCISE 8

# 8a Generate a data set, write out the equation, and identify n and p
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y <- x - 2*x^2 + rnorm(100)

# Equation is B0 + B1*x + B2*x^2 + e
# n = 100
# p = 2

# 8b Show a scatterplot of x vs y
plot(x,y)
# A large concentration of points around x=0, y=0
# which is kind of weird, but the expected shape

# 8c Fit four models
set.seed(12)
df <- data.frame(x=x, y=y)
glm.fit1 <- glm(y ~ x, data=df)
glm.fit2 <- glm(y~x + I(x^2), data=df)
glm.fit3 <- glm(y~x + I(x^2) + I(x^3), data=df)
glm.fit4 <- glm(y~x + I(x^2) + I(x^3) + I(x^4), data=df)
summary(glm.fit1)
summary(glm.fit2)
summary(glm.fit3)
summary(glm.fit4)

# 8d Fit same four models, but with a different seed
set.seed(2417)
df <- data.frame(x=x, y=y)
glm.fit1 <- glm(y ~ x, data=df)
glm.fit2 <- glm(y~x + I(x^2), data=df)
glm.fit3 <- glm(y~x + I(x^2) + I(x^3), data=df)
glm.fit4 <- glm(y~x + I(x^2) + I(x^3) + I(x^4), data=df)
summary(glm.fit1)
summary(glm.fit2)
summary(glm.fit3)
summary(glm.fit4)

# 8c and 8d give the same thing, because there is no random
# process associated with fitting

# 8e Do LOOCV on each model and report the errors from each
library(boot)
cv.err <- rep(0,4)
cv.err[1] <- cv.glm(df, glm.fit1)$delta[1]
cv.err[2] <- cv.glm(df, glm.fit2)$delta[1]
cv.err[3] <- cv.glm(df, glm.fit3)$delta[1]
cv.err[4] <- cv.glm(df, glm.fit4)$delta[1]
cv.err
# Smallest test MSE for power 2, which should be no surprise

# 8f This also agrees with the tests for significance from
# the least squares fits

### EXERCISE 9
mu.fn <- function(data, index) {
  return(mean(data$medv[index]))
}

sd.fn <- function(data, index) {
  return(sd(data$medv[index]))
}
#9a get an estimate for population mean
mean(Boston$medv)

#9b - Standard error estimate
sd(Boston$medv) / sqrt(nrow(Boston))

#9c
mu.est <- boot(Boston, mu.fn, R=1000)
mu.est
sd.est <-boot(Boston, sd.fn, R=1000)
as.numeric(sd.est[1])/ sqrt(nrow(Boston))

#9d
sqrt(var(mu.est$t)) # This gives you the bootstrap est std err
mu.est$t0 - 2* sqrt(var(mu.est$t))
mu.est$t0 + 2*sqrt(var(mu.est$t))

#9ef
med.fn <- function(data, index) {
  return(median(data$medv[index]))
}
med.est <- boot(Boston, med.fn, R=1000)
med.est

#9gh
tenth.fn <- function(data,index) {
  return(quantile(data, 0.1))
}
boot(Boston$medv, tenth.fn, R=1000)
