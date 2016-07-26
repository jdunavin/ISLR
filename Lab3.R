### CHAPTER 3 LAB

library(MASS)
library(ISLR)
attach(Boston)

### Boston data - predict median house value using # rooms, structure age, and SES of residents
# and maybe others besides

# get the column names
names(Boston)

# fit a simple linear regression
lm.fit=lm(medv~lstat, data=Boston)

# summary information
lm.fit
summary(lm.fit)
names(lm.fit)

# get just the coefficients
coef(lm.fit)

# get the confidence intervals
confint(lm.fit)

# make predictions
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")

# plot the line and data on the same graph
plot(Boston$lstat, Boston$medv)

# Play with some graph settings
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20) # Show all the possible symbols

# Examine diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

# plot residuals and studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# plot leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # argmax

# residuals should average to be 0, and there are some high leverage points, also a big divergence in the QQplot
# could be non-linear

### MULTIPLE LINEAR REGRESSION

# fit on 2 variables
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)

# fit on all 13
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

# What's available in the model
?summary.lm

summary(lm.fit)$r.sq  # R^2
summary(lm.fit)$sigma # RSE

# install.packages("car") # get the vif function
# library(car)
# vif(lm.fit)

# Age has a high p-value, exclude it
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)

### INCLUDING INTERACTION TERMS
summary(lm(medv~lstat*age, data=Boston))

### NON-LINEAR TRANSFORMATIONS
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# Analysis of variance
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
# Null hyp = both models fit equally well. p value of second model is near 0, so reject null hyp. Second model is better.
# See it on a graph
par(mfrow=c(2,2))
plot(lm.fit2)

# Higher order polynomial
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# Log transform
summary(lm(medv~log(rm),data=Boston))


### QUALITATIVE PREDICTORS

names(Carseats)
# R makes dummy variables automatically

lm.fit=lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)

