
source("LinearRegression.R")

x <- 1:100
n <- length(x)
set.seed(100)
random_error <- rnorm(n)
y <- 2*x+10+random_error

lr <- LinearRegression(x, y)
summary(lr)
predict(lr, c(10,20,30))

summary(res <- lm(y~x))
predict(res, data.frame(x=c(10,20,30)))
