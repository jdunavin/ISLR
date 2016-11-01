### ISLR Chapter 9 Lab - Support Vector Machines

## 9.1 Support vector classifier on a toy example
set.seed(1)
x <- matrix(rnorm(20*2),ncol=2)
y <- c(rep(-1,10),rep(1,10))
x[y==1,]<-x[y==1,]+1

# Check to see if the classes are separable
plot(x, col=3-y)
# Not linearly anyhow, it appears

# Run the SVC - need to get things in a data frame, and
# make the classes a factor, and don't normalize
df <- data.frame(x=x, y=as.factor(y))
library(e1071)
svm.fit <- svm(y~.,data=df,kernel='linear',cost=10,scale=FALSE)

# Show a plot of the model
# x's are the support vectors
# red points are the true 1s, black are true -1s
# blue area is assigned -1, pink area assigned 1
# So one red point is misclassified, no black ones are
plot(svm.fit,df)

# Which observations are the support vectors, ie points in 
# the 'fuzzy zone'?
svm.fit$index

# Basic info about fit
summary(svm.fit)

# Try smaller value of cost
svm.fit <- svm(y~., data=df, kernel='linear', scale=FALSE, cost=0.1)
plot(svm.fit, df)
svm.fit$index
# As expected there are a lot more support vectors in the fuzzy zone
# Still only got one wrong

# Weird - SVM doesn't give you directly the coeffs of the line

# Cross-validate for best cost
set.seed(1)
tune.out <- tune(svm, y~., data=df, kernel='linear', 
  ranges=list(cost=c(0.001, 0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

# Make predictions on test data
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]= xtest[ytest==1,] + 1
testdat=data.frame(x=xtest , y=as.factor(ytest))
ypred<- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

# Try with cost of 0.01
svmfit=svm(y~., data=df , kernel="linear", cost=.01,
    scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred , truth=testdat$y )

# What about when they're neatly linearly separable?
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat , kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit=svm(y~., data=dat , kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)

## 9.2 Support Vector Machines

# Mock up some data
set.seed(1)
x<-matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,]+2
y<-c(rep(1,150),rep(2,50))
dat <- data.frame(x, y=as.factor(y))

plot(x, col=y)

train <- sample(200,100)
svmfit <- svm(y~., data=dat[train,], kernel='radial', gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)

svmfit <- svm(y~., data=dat[train,], kernel='radial', gamma=1, cost=1e5)
plot(svmfit, dat[train,])

# Cross validate for best cost and gamma
set.seed(1)
tune.out <- tune(svm, y~.,data=dat[train,],kernel='radial',
  ranges=list(cost=c(0.1,1,10,100,1000),
  gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train ,"y"], pred=predict(tune.out$best.model, newx=dat[-train ,]))
