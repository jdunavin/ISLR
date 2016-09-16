### Chapter 6, Lab 3

# Part 1. Principal Components
library(ISLR)
library(pls)

# Get train and test data just like in lab 2
Hitters <- na.omit(Hitters)
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = ( - train)
y.test = y[test]

# Fit the model to training data
pcr.fit = pcr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# Looks like 7 components is a good number, let's look for the right 7 components
pcr.pred <- predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred - y.test) ^ 2)
# MSE as good as lasso, but more difficult to interpret, because which variables do we use?

# Fit this to full data set
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

# Part 2. Partial Least Squares
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = 'CV')
summary(pls.fit)
validationplot(pls.fit, val.type = 'MSEP')

pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test) ^ 2)
# Not as good as ridge, lasso, or PCR

pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
