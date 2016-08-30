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

