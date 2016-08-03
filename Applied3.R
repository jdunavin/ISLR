### CHAPTER 3 APPLIED EXERCISES

### EXERCISE 8

#8a) mpg ~ horsepower
attach(Auto)
fit8a = lm(mpg~horsepower)
summary(fit8a)

# i. based on the low p-value, there appears to be a strong relationship between horsepower and mpg 
# unsurprisingly, a negative one. 
## ii. its CI doesn't contain 0, and 60.6% of the variance in mpg is explained by horsepower
# iii. Negative
# iv.
predict(fit8a, data.frame(horsepower=c(98)), interval="confidence")
# prediction = 24.47, lower CI = 23.97, upper CI 24.96

# 8b) Make plots
plot(horsepower, mpg)
abline(fit8a, col="red",lwd=3)

# 8c) Diagnostic plots
par(mfrow=c(2,2))
plot(fit8a)
# Problems: strong scoop pattern in the residuals, average should be 0
# Also shows evidence of heteroscedasticity
nrow(Auto)
# n = 392, p = 1. Average leverage should be (p + 1)/n = 2/392 = 0.0051, so quite a few points have high leverage
# Residuals close to normally distributed
# A few points are labelled as outliers

### EXERCISE 9

#9a) Pairwise scatterplot
pairs(Auto)

#9b) Correlation matrix - exclude the "name" column (9th)
names(Auto)
cor(Auto[,1:8])

#9c) Fit a model for all predictors except name
fit9c = lm(mpg~.-name, data=Auto)
summary(fit9c)

# i. F statistic shows significant relationship between predictors and response
# ii. SS predictors = displacement, weight, year, origin, intercept term
# iii. Better gas mileage in later model cars, gets better with time

# 9d) Diagnostic plots
par(mfrow=c(2,2))
plot(fit9c)

# Weaker pattern with the residuals
# Some pretty significant outliers
# some high leverage points (avg leverage = 8/392 = 0.0204 - there are some points over 0.05)

# 9e) interaction terms
# displacement * weight, year* weight

fit9e1 = lm(mpg~displacement*weight + year + origin + horsepower + acceleration + cylinders, data=Auto)
summary(fit9e1)
par(mfrow=c(2,2))
plot(fit9e1, main="All and interaction term for displacement & weight")
# Plots look better but the interaction term, while statistically significant, makes almost no difference

fit9e2 = lm(mpg~year*weight + origin + horsepower + acceleration + cylinders, data=Auto)
summary(fit9e2)
par(mfrow=c(2,2))
plot(fit9e2, main="year*weight")

# How about horsepower and weight
fit9e3 = lm(mpg~horsepower*weight + origin + year:weight, data=Auto)
summary(fit9e3)
par(mfrow=c(2,2))
plot(fit9e3, main="3rd test")

### 9f Try some transformations
Auto$Lhorse <- log(Auto$horsepower)
Auto$Ldisp <- log(Auto$displacement)
Auto$sqrtWt <- sqrt(Auto$weight)
# I picked these by looking at the data and trying to get things on similar scales

fit9f = lm(mpg~Lhorse+Ldisp*sqrtWt+origin*Ldisp+year:sqrtWt, data=Auto)
summary(fit9e)
par(mfrow=c(2,2))
plot(fit9f, main="3rd test")
# Small improvement in R^2, plots look similar to the ones above.
# How would I isolate the points with theoretical q > 2 and std resid > 2? I think excluding them 
#   would improve the fit
# A topic for further research

### EXERCISE 10 ###  Carseats data

# 10a) fit sales ~ price, urban, US
?Carseats
fit10a = lm(Sales ~ Price + Urban + US, data=Carseats)
summary(fit10a)
par(mfrow=c(2,2))
plot(fit10a)
pairs(Carseats)

# 10b) Interpretation
# Plots look decent, but R^2 sucks
# Sales are in thousands
# For rural and non US stores, a unit increase in price drops sales by -0.054 per unit
# for urban non US stores, a unit increase in price drops sales by -0.054 - 0.022 = -0.076
# for urban US stores, it's -0.054 - 0.022 + 1.201
# for rural US stores, it's -0.054 + 1.201

# SO.... urban stores are more price sensitive than rural, and US stores are less price sensitive than non.

# 10c) See notes, don't have markdown installed

# 10d) Can reject for Price, US, and Intercept, not Urban

# 10e)
fit10e = lm(Sales~Price + US, data=Carseats)
summary(fit10e)
anova(fit10a, fit10e)

# 10f) How well do the models from a and e fit? Actually pretty poorly according to R^2.
# Can't even conclude one is better than the other based on anova.

# 10g) Confidence intervals for the coefficients
confint(fit10e)

# 10h) Evidence of outliers
par(mfrow=c(2,2))
plot(fit10e)
# Plot labeled 3 points, I'm not sure if it always labels 3 points or if this triggered some threshold

### EXERCISE 11 ### 
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

# 11a) Simple linear regression with intercept
fit11a = lm(y~x+0)
summary(fit11a)
# Expected the coefficient to come out to around 2 and it was, CI should contain 2. P value was tiny and 
# R^2 was strong. 

# 11b)
fit11a2 = lm(x~y+0)
summary(fit11a2)

# 11c)
# Kinda weird - I expected a coefficient of near 1/2. That's not even close. t value and p value are the same,
# as is r squared and F statistic.

# 11d) Obviously they are the same, just switch the roles of x and y

# 11e) Regression with an intercept - T stat on the slope coef is still the same
fit11c1 = lm(y~x)
fit11c2 = lm(x~y)
summary(fit11c1)
summary(fit11c2)

### EXERCISE 12 ### 
#12a x and y need to have same SSQ

#12bc Example with 100 observations where coef of X != coef of Y
x=rnorm(100)
y=2 * x + rnorm(100)
fit12b = lm(y~x+0)
summary(fit12b)
fit12b1 = lm(x~y+0)
summary(fit12b1)

y2 = abs(x)
fit12b2 = lm(y2~x+0)
summary(fit12b2)
fit12b3 = lm(x~y2+0)
summary(fit12b3)

### EXERCISE 13 ###
set.seed(1)
x=rnorm(100)
eps=rnorm(100,mean=0,sd=0.25)
y = -1 + 0.5*x +eps

plot(x,y)
fit13 = lm(y~x)
summary(fit13)
# Predicted coefficients are very close to actual, actual lies within the CIs.
abline(fit13, col="blue")
abline(-1,0.5, col="red")
legend("topright",legend=c("fit","real"), fill=c("blue","red"))

fit132 = lm(y~I(x^2))
summary(fit132)
# This fit is obviously way worse

epsless = rnorm(100, mean=0, sd=0.05)
yless = -1 + 0.5*x + epsless
fitless = lm(yless~x)
summary(fitless)
# As expected, much smaller CIs, better R^2

epsmore = rnorm(100, mean=0, sd=1.5)
ymore = -1 + 0.5*x + epsmore
fitmore = lm(ymore~x)
summary(fitmore)

### EXERCISE 14, collinearity ###
set.seed(1)
x1=runif(100)
x2=0.5*x1 + rnorm(100)/10
y=2+2*x1 + 0.3*x2 + rnorm(100)

plot(x1, x2)
cor(x1,x2)

fit14 = lm(y~x1+x2)
summary(fit14)
# These coefficient estimates really suck

fit14x1 = lm(y~x1)
summary(fit14x1)

fit14x2 = lm(y~x2)
summary(fit14x2)

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y=c(y,6)
par(mfrow=c(2,2))
fit14 = lm(y~x1+x2)
summary(fit14)
plot(fit14)

fit14x1 = lm(y~x1)
summary(fit14x1)
plot(fit14x1)

fit14x2 = lm(y~x2)
summary(fit14x2)
plot(fit14x2)

### EXERCISE 15 ###
library(ISLR)
library(MASS)
View(Boston)

## AUTOMATION CHALLENGE! ##
# There are 14 columns - I want to fit a simple lm to each of columns 2-14 against column 1.

coefplot = vector(mode="numeric", length=0)
for (i in seq(2,14)) {
  fit = lm(Boston[,1]~Boston[,i])
  # Want to list each slope coef and its p-value
  # Also want to store the coefficients for plotting later
  cat("Column number: ", i, "\n")
  coefs = summary(fit)$coefficients[,1]
  coefplot[i] = coefs[2]
  pvals = summary(fit)$coefficients[,4]
  cat("Slope coef: ", coefs[2], "\n")
  cat("pvalue: ", pvals[2], "\n")
}
# Chop off the NA in the first place of coefplot
coefplot <- coefplot[is.na(coefplot)==FALSE]

# The only p-value that wasn't obscenely tiny was for column 4, which was the dummy for "abuts Charles River"

# Now use all of the columns
fitall <- lm(crim ~ ., data=Boston)
coefplotall = summary(fitall)$coefficients[,1]
coefplotall = coefplotall[-1]

plot(coefplot, coefplotall)
# The weird looking one is nox, which got a +30 from the single models and a -10 from the multiple model.
# Nox represents the particulate matter in the air.
# Not sure what this is saying about whether or not it should be kept. It had an awesome p-value in the single
# models and a crappy one in the multiple model. Maybe it should be excluded, or it is collinear with another one

for (i in seq(2,14)) {
  x = Boston[,i]
  fit = lm( Boston[,1] ~ x + I(x^2) + I(x^3) )
  cat("Column number: " , i , "\n")
  print(summary(fit))
}

# Column 3, 5, 14, 7 (maybe), 8, 11 (prob)
# indus   -proportion of non-retail business acres per town.
# nox     -nitrogen oxides concentration (parts per 10 million)
# medv    -median value of owner-occupied homes in 000s
# age     -proportion of owner-occupied units built prior to 1940.
# dis     -weighted mean of distances to five Boston employment centres.
# ptratio -pupil-teacher ratio by town