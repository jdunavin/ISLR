### Lab 6.1 Best Subset Selection

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# 59 Salary NAs, get rid of them
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

library(leaps)
# This will show the best model up to 8 inputs
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~., Hitters, nvmax=19)
reg.sum <- summary(regfit.full)
names(reg.sum)

# Look at Rsq - should increase with # of variables
reg.sum$rsq


#Plot rss, adj rsq, BIC, Cp all at same time
par(mfrow=c(2,2))
plot(reg.sum$rss, xlab="Number of Variables", ylab="RSS", type='l')
plot(reg.sum$adjr2, xlab="Number of Variables", ylab="Adj R2", type='l')
which.max(reg.sum$adjr2)
points(11, reg.sum$adjr2[11], col='red',pch=20, cex=2)
plot(reg.sum$cp, xlab="Number of Variables", ylab="Cp", type='l')
which.min(reg.sum$cp)
points(10, reg.sum$cp[10], col='red',pch=20, cex=2)
plot(reg.sum$bic, xlab="Number of Variables", ylab="BIC", type='l')
which.min(reg.sum$bic)
points(6, reg.sum$bic[6], col='red',pch=20, cex=2)
# Looks like the ideal number is somewhere between 6 and 11

# These plots show which variables get kicked out when
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# Show the coefficients of the model with best BIC
coef(regfit.full, 6)

# Try forward and backward stepwise selection
regfit.fwd <- regsubsets(Salary~., Hitters, method="forward")
regfit.bwd <- regsubsets(Salary~., Hitters, method="backward")
summary(regfit.fwd)
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

### Choosing among models using test set/CV approach

# Create a random sample to flag training observations
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- !(train)

regfit.best <- regsubsets(Salary~., Hitters[train,], nvmax=19)
test.mat <- model.matrix(Salary~., data=Hitters[test,])

# Save the errors in a vector
val.err <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.err[i]=mean((Hitters$Salary[test]-pred)^2)
}
which.min(val.err)
# Model with 10 vars has the lowest test error
val.err
coef(regfit.best,10)

# Create your own predict function, since there isn't one for regsubsets
predict.regsubsets =function (object ,newdata ,id,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Cross validation
regfit.best <- regsubsets(Salary~., Hitters, nvmax=19)

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames =list(NULL , paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,], nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit ,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

# Get the average over all 10 CV folds for each variable
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

reg.best=regsubsets (Salary~.,data=Hitters , nvmax=19)
coef(reg.best ,11)
