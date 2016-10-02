### Chapter 7 - Applied exercises

## Exercise 6

# 6a) Fit poly reg to predict wage with age, select optimal degree with CV.
train <- sample(1:nrow(Wage), size=nrow(Wage)/2, replace=FALSE)
test <- -(train)
