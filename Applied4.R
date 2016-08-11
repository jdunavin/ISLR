# Chapter 4 Applied Exercises

library(ISLR)
library(MASS)
library(ggplot2)

#10a
summary(Weekly)
View(Weekly)
ggplot(data=Weekly, aes(x=Year, y=Today)) + geom_point()
plot(Weekly$Today)
pairs(~Today+Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly)
# Not really any patterns to speak of, except that variability
# seems to increase over time, which is evidence of heteroscedacticity

#10b
lm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
# Only lag 2 appeared to be statistically significant

#10c
lm.probs <- predict(lm.fit, type="response")
nrow(Weekly)
glm.pred=rep("Down",1089)
glm.pred[lm.probs>0.5] <- "Up"
table(glm.pred, Weekly$Direction)
table(Weekly$Direction, glm.pred)
(54+557)/1089 # accuracy
557/(557+430) # precision
557/(557+48)  # recall
# low percentage of false negatives, high percentage of false positives.
# This is very undesireable in this context.

#10d
train <- Weekly[Weekly$Year <= 2008,]
test <- Weekly[Weekly$Year > 2008,]

glm.fit <- glm(Direction~Lag2, data=train, family="binomial")
glm.actual <- test$Direction
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- rep("Down",nrow(test))
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred, glm.actual)
(56+9)/nrow(test) #Accuracy
56/(56+34) #Precision
56/(56+5)  #Recall
# Still a low percentage of false negatives, high level of false positives
# Accuracy significantly improved

#10e - Same thing but with LDA
