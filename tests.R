library(ggplot2)
library(MASS)

source("math.R")

set.seed(100)

#### Linear regression ####
source("LinearRegression.R")

x1 <- seq(1, 5, by=0.1)
n <- length(x1)
y <- 2*x1+2+rnorm(n, sd = 10);plot(y)

# rbf <- function (x, mu, sigma = 1) {dnorm(x, mu, sigma)} # exp(-1/lambda*sqrt((x-mu)^2))
# plot(rbf(1:100, 50, 10))
# plot(dnorm(1:100, 50, 10))
# 
# predictors <- NULL
# bandwitdh <- seq(1, 5, by=0.5)
# for (i in 1:length(bandwitdh)) {
#   predictors <- cbind(predictors, rbf(x1, i))
# }

predictors <- cbind(x1, x1^2, x1^3, x1^4)
res <- LinearRegression(predictors, y, lambda = 0, delta = 2, normalize = TRUE)
summary(res)
logLik(res)
#predict(res, matrix(c(10, 20, 30, 20, 5, 40), 3, 2))
ggplot()+
  geom_point(aes(x = x1, y = y, colour = "red"))+
  geom_line(aes(x = x1, y = predict(res, predictors)))

res.val <- lm(y~I(rbf(x1, 1, lambda = 1) + I(rbf(x1, 2, lambda = 1)) + I(rbf(x1, 3, lambda = 1)) +
                  I(rbf(x1, 4, lambda = 1)) + I(rbf(x1, 5, lambda = 1))))
res.val <- lm.ridge(y~x1+I(x1^2)+I(x1^3)+I(x1^4), lambda = 1)
summary(res.val)
#logLik(res.val)
#predict(res.val, data.frame(x1=c(10,20,30), x2=c(20,5,40)))
ggplot()+
  geom_point(aes(x = x1, y = y, colour = "red"))+
  geom_line(aes(x = x1, y = predict(res.val)))
  #geom_line(aes(x = x1, y = as.matrix(cbind(const=1, x1, x1^2, x1^3, x1^4)) %*% coef(res.val)))

plot(x1, y)
lines(ksmooth(x1, y, kernel = "normal", bandwidth = 0.5), col = 2)


#### Logistic regression ####
source("LogisticRegression.R")

X <- 1:100
y <- ifelse(sigmoid(1.2*X-30+rnorm(length(X))) < 0.5, 0, 1)

res.val <- glm(y~X, family=binomial(link='logit'))

classifier <- LogisticRegression(X, y)
classifier

(classifier$fitted.values < 0.5) == (res.val$fitted.values < 0.5)

#### KNN ####
source("KNearestNeighbor.R")

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
true_cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

library(class)
res.val <- as.vector(knn(train, test, cl, k = 9))
table(res.val, true_cl)
sum(res.val == true_cl)/length(true_cl)

classifier <- KNearestNeighbor(train, cl, k = 9)
res <- predict(classifier, test)
table(res, true_cl)
sum(res == true_cl)/length(true_cl)
qplot(x = test[,1], y = test[,2], color = res, shape = res == true_cl)
