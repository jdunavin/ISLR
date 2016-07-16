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
