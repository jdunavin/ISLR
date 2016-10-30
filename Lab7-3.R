### Lab 7, Part 3 - GAMs

#Natural spline
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education, data=Wage)

#Smoothing spline
library(gam)
