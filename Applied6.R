# Chapter 6 - Applied Exercises

# Exercise 8
x = rnorm(100, mean = 3, sd = 1.5)
e = rnorm(100)
y <- 1 + 2 * x + 3 * x ^ 2 + e

df <- data.frame(x1 = x, x2 = x ^ 2, x3 = x ^ 3, x4 = x ^ 4, x5 = x ^ 5, x6 = x ^ 6, x7 = x ^ 7, x8 = x ^ 8, x9 = x ^ 9, x10 = x ^ 10)
X <- data.matrix(df)
df <- data.frame(y = y, df)

