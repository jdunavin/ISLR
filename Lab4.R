### Chapter 4 Lab
# Part 1 Stock Market Data

library(ISLR)
library(MASS)
names(Smarket)
dim(Smarket)
summary(Smarket)

#Pairwise correlation matrix - exclude direction field 
cor(Smarket[,-9])
# Almost no correlation between Today and any of the lags, as expected

attach(Smarket)
plot(Volume)

# Part 2 Logistic Regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
# None of the coefficients are significant!
coef(glm.fit)

#Access coefficients table
summary(glm.fit)$coef
#Access p-values
summary(glm.fit)$coef[,4]

# Predict probabilities
glm.probs=predict(glm.fit,type="response")
# Type response = probabilities, no type = logit
glm.probs[1:10]
contrasts(Direction)

# Make class predictions
glm.pred=rep("Down",1250)
glm.pred[glm.probs>0.5]="Up"
# Confusion matrix
table(glm.pred, Direction)
# Accuracy
mean(glm.pred==Direction)

# Create a training set of 2001-2004, use 2005 as test set
train = Smarket[Year < 2005,]
test = Smarket[Year >= 2005,]
# This is a hell of a lot easier than their solution, but I wonder if there's an efficiency issue

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=train, family=binomial)
glm.probs = predict(glm.fit, test, type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, test$Direction)
mean(glm.pred==test$Direction)
mean(glm.pred!=test$Direction)
# God this is terrible...
# But that is what is supposed to happen, we are taught since birth that stock returns aren't autocorrelated

glm.fit=glm(Direction ~ Lag1 + Lag2, data=train, family=binomial)
glm.probs=predict(glm.fit, test, type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, test$Direction)
mean(glm.pred==test$Direction)
## That's a little better, 56% accuracy, but just saying "Up" every time...

glm.up=rep("Up",252)
table(glm.up, test$Direction)
141 / 252
#... gets us the same test accuracy, so this model still kinda sucks

# Make predictions on new data
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")
# This would yield a prediction of "Down" for both days


### Part 3 - Linear Discriminant Analysis
lda.fit = lda(Direction~Lag1+Lag2, data=train)
lda.fit
# Interpretation of output-
# Prior probs - 50.8% of training observations are Up, 49.2% down
# Group means - Shows the average of Lag1 and Lag2 separately for Downs and Ups - means that Downs are associated 
  # with positive returns on Lag1 and Lag2, and the opposite for Ups
# LDA coefficients - if coef1 x Lag1 + coef2 x Lag2 is large, LDA predicts up and vice versa.

plot(lda.fit)

# Make predictions
lda.pred=predict(lda.fit, test)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, test$Direction)
mean(lda.class==test$Direction)

# Posterior probabilities
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)

# These next two lines show us that >=0.5 here corresponds to DECREASE, not the other way around as before.
lda.pred$posterior[1:20,1]
lda.class[1:20]

# Use some other threshold, say 90%
sum(lda.pred$posterior[,1]>0.9)
# Whoops, there aren't any, what's the highest?
max(lda.pred$posterior)


### PART 4 - Quadratic discriminant analysis
qda.fit=qda(Direction~Lag1+Lag2, data=train)
qda.fit
qda.class=predict(qda.fit, test)$class
table(qda.class, test$Direction)
mean(qda.class==test$Direction)
# 60% accuracy on test data!


### PART 5 - K-NEAREST NEIGHBORS
library(class)
train.X = data.frame(train$Lag1, train$Lag2)
test.x = data.frame(test$Lag1, test$Lag2)
train.y = train$Direction

set.seed(1)
knn.pred=knn(train.X, test.x, train.y, k=1)
table(knn.pred, test$Direction)
mean(knn.pred==test$Direction)
# 1-nearest neighbor sucks, at 50% accuracy

knn.pred=knn(train.X, test.x, train.y, k=3)
table(knn.pred, test$Direction)
as.numeric(mean(knn.pred==test$Direction))
# 3-nearest neighbor got me to 53.6%

blah = numeric(0)
for (j in seq(1:20)) {
  knn.pred=knn(train.X, test.x, train.y, k=j)
  blah = c(blah, mean(knn.pred==test$Direction))
}
blah
plot(seq(1:20),blah)


### PART 6 - Application to Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)

# Need to standardize the data to remove KNN's bias for scale
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

# Split the observations into training and test sets
test=1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred=knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
# So KNN(1) gives an error rate of 12%, but just predicting No gives an error rate of 6%. So this isn't any good.

table(knn.pred, test.Y)
9/(68+9)
# 77 customers are predicted to buy, and 9 of them actually do (11.7%) as opposed to 6% from random guessing.

knn.pred=knn(train.X, test.X, train.Y, k=3)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
5/26
# That's a little better

knn.pred=knn(train.X, test.X, train.Y, k=5)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
4/15

# Compare to logistic regression
glm.fit = glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred, test.Y)
glm.pred=rep("No", 1000)
glm.pred[glm.probs>0.25]="Yes"
table(glm.pred, test.Y)
11/33
