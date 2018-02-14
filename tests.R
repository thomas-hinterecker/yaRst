library(ggplot2)
library(MASS)

source("math.R")

set.seed(100)

#### Linear regression ####
source("LinearRegression.R")

x1 <- seq(1, 5, by=0.1)
n <- length(x1)
y <- 2*x1+2+rnorm(n, sd = 3);plot(y)

predictors <- cbind(x1, x1^2, x1^3)
res <- LinearRegression(predictors, y, lambda = 0.0, delta = , normalize = TRUE)
summary(res)
logLik(res)
#predict(res, matrix(c(10, 20, 30, 20, 5, 40), 3, 2))
ggplot()+
  geom_point(aes(x = x1, y = y, colour = "red"))+
  geom_line(aes(x = x1, y = predict(res, predictors)))

#### Logistic regression ####
source("LogisticRegression.R")

X <- rbind(
  mvrnorm(n = 100, c(0, 0), matrix(c(1,0,0,1),2,2)),
  mvrnorm(n = 100, c(3, 3), matrix(c(1,0,0,1),2,2)))
n <- nrow(X)
y <- rep(c(0,1), each = n/2)

classifier <- LogisticRegression(X, y)
classifier
yhat <- (classifier$fitted.values >= 0.5)
sum(yhat == y)/length(y)
qplot(x = X[,1], y= X[,2], colour = (y == yhat), shape = factor(y))

#### KNN ####
source("KNearestNeighbor.R")

classifier <- KNearestNeighbor(X, factor(y), k = 9)
yhat <- predict(classifier, X)
sum(yhat == y)/length(y)
qplot(x = X[,1], y = X[,2], colour = (y == yhat), shape = factor(y))
