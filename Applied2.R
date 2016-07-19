### EXERCISE 8 ###

# 8a load the data file, which is downloaded to your working directory
college <- read.csv('college.csv')

# 8b look at the data and fix the row names
rownames(college)=college[,1]
college=college[,-1]

#8c i,ii,iii
summary(college)
pairs(college[,1:10])
plot(college$Private, college$Outstate, col="#7fc97f", xlab="Private school?", ylab="Out of state tuition")

#8c iv - Bin the schools into Elite, based on the number of incoming students
# that were in the top 10% of their classes
college$Elite <- rep("No", nrow(college))
college$Elite[college$Top10perc>50] <- "Yes"
college$Elite = as.factor(college$Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, col="#fb9a99", xlab="Elite school?", ylab="Out of state tuition")

#8d Panel of four histograms - showing different 
# bins of some of the quantitative variables
# number of enrollments
par(mfrow=c(2,2), mar=c(2,1,2,1))
hist(college$Enroll, breaks=10, col="#a6cee3", main="Enroll - 10 bins")
hist(college$Enroll, breaks=15, col="#1f78b4", main="Enroll - 15 bins")
hist(college$Enroll, breaks=20, col="#cab2d6", main="Enroll - 20 bins")
hist(college$Enroll, breaks=25, col="#6a3d9a", main="Enroll - 25 bins")

# Estimated personal spending
par(mfrow=c(2,2), mar=c(2,1,2,1))
hist(college$Personal, breaks=10, col="#fb9a99", main="Spending - 10 bins")
hist(college$Personal, breaks=15, col="#e31a1c", main="Spending - 15 bins")
hist(college$Personal, breaks=20, col="#fdbf6f", main="Spending - 20 bins")
hist(college$Personal, breaks=25, col="#ff7f00", main="Spending - 25 bins")

# 8f What else can I discover?
par(mfrow=c(1,1), mar=c(5,4,4,2))
plot(college$Private, college$PhD, col="#33a02c", xlab="Private school?", ylab="Pct faculty with PhD", varwidth=T)
summary(college$Private)
# Thought it was weird that private schools would 
# have a wider IQR on this item

# What else?
symbols(college$Personal, college$Room.Board, circles=sqrt(college$F.Undergrad)/pi,inches=0.35,bg="#8dd3c7", fg="#ffffff", xlab="Personal spending", ylab="Room and board costs", main="Bubble size = #FT undergrads")
identify(college$Personal, college$Room.Board, rownames(college))
# no there there on this one

#One more
symbols(college$Personal, college$Grad.Rate, circles=sqrt(college$F.Undergrad)/pi,inches=0.35,bg="#fb8072", fg="#ffffff", xlab="Personal spending", ylab="Graduation rate", main="Bubble size = #FT undergrads")
identify(college$Personal, college$Grad.Rate, rownames(college))
# High personal spending = lower graduation rate?

### EXERCISE 9 ###
# 9a) Quantitative vs qualitative
summary(Auto)
# Quantitative: mpg, displacement, hp, weight, accel
# Qualitative: cylinders, name, year, origin

# 9b) Range of each quantitative predictor
range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

# 9c) Mean and SD of each quantitative predictor
# Use mean() and sd() - OR cheat
library(pastecs)
stat.desc(Auto$mpg) # Mean is 9th value, SD is 13th
stat.desc(Auto$displacement)[c(9,13)]
stat.desc(Auto$horsepower)[c(9,13)]
stat.desc(Auto$weight)[c(9,13)]
stat.desc(Auto$acceleration)[c(9,13)]
stat.desc(Auto$year)[c(9,13)]

# how to do this same thing with a for loop
quants <- c("mpg","displacement","horsepower")
for (q in quants){
  print(q)
  m <- mean(Auto[[q]])
  cat("The mean is: " , m, "\n")
  s <- sd(Auto[[q]])
  cat("The sd is: " , s, "\n")
}

#9d - Same thing, but take out 10th - 85th observations
lessAuto <- Auto[-(10:85),]
for (q in quants){
  print(q)
  m <- mean(lessAuto[[q]])
  cat("The mean is: " , m, "\n")
  s <- sd(lessAuto[[q]])
  cat("The sd is: " , s, "\n")
}

# 9e) Graphical investigation
pairs(∼ mpg + displacement + horsepower + weight + acceleration+year , Auto)
# HP, mpg, engine size, and weight all decreased with later years
# HP and weight increase with engine size but mpg and accel decrease

plot(Auto$cylinders, Auto$mpg, xlab="Number of cylinders",ylab="Miles per gallon")
# There appeared to be a peak in fuel economy at 4 cylinders
identify(Auto$cylinders, Auto$mpg, Auto$name)

# 9f) Useful in predicting mpg? (discussion)

### EXERCISE 10 ###

# 10a) What's in the data
library(MASS)
?Boston

# 10b) Pairwise scatterplots
pairs(~crim+indus+nox+age+dis+ptratio+lstat+medv, Boston)
# Looks like older houses are in more polluted neighborhoods
pairs(~crim+rad+tax+black+age+nox+zn, Boston)
# new construction not being zoned for big lots

# 10c) Associated with high crime rates
# I didn't see anything in particular

# 10d) Any suburbs have high crime rates, pt ratios, tax rates?

#close to the highways?
summary(as.factor(Boston$rad))
par(mfrow=c(3,1), mar=c(4,1,2,1))
plot(as.factor(Boston$rad), Boston$crim, varwidth=T, main="Crime rate")
plot(as.factor(Boston$rad), Boston$ptratio, varwidth=T, main="P/T ratio")
plot(as.factor(Boston$rad), Boston$tax, varwidth=T, main="Tax rate", xlab="Dist from hwy")

#Abuts the river?
summary(as.factor(Boston$chas))
par(mfrow=c(3,1), mar=c(4,1,2,1))
plot(as.factor(Boston$chas), Boston$crim, varwidth=T, main="Crime rate")
plot(as.factor(Boston$chas), Boston$ptratio, varwidth=T, main="P/T ratio")
plot(as.factor(Boston$chas), Boston$tax, varwidth=T, main="Tax rate", xlab="Abuts Charles River")
