# Enter a vector of data
x<-c(1,3,2,5)
x

x = c(1,6,2)
x
y=c(1,4,3)
y

# Show the column length
length(x)
length(y)

# Add two vectors, don't save the results
x+y

# List objects and delete some
ls()
rm(x,y)

# Remove all objects at once
rm(list=ls())

# Make a 2 by 2 matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x=matrix(c(1,2,3,4),2,2)

# Change the 'load order' to rows rather than columns
matrix(c(1,2,3,4),2,2,byrow=TRUE)

# 50 draws from a normal dist with mean 1 and sd 0
x = rnorm(50)

# add 50 draws from a normal dist with mean 50 and sd 0.1
y=x+rnorm(50, mean=50, sd=.1)

# regenerate x, and ensure consistent results
set.seed(1303)
rnorm(50)

# try out some statistical functions
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics
plot(x,y)
plot(x,y, xlab="x-axis!",ylab="y=axis!", main="Plot of x vs y")

# Make a PDF of a plot
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

# Make a sequence of numbers
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)

# Contour plot
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

# heat maps and 3d graphs
image(x,y,fa)

# base perspective, rotation angles 
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing and Referencing Data
A=matrix(1:16,4,4)
A

# 2nd row, 3rd column
A[2,3]

# Slicing and dicing
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]

A[1,]
# Select everything except
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]

#Dimension of a matrix
dim(A)