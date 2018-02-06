library(ggplot2)

source("math.R")

set.seed(100)

#### Logistic regression ####
source("LogisticRegression.R")

X <- 1:100
y <- ifelse(sigmoid(1.2*X-30+rnorm(length(X))) < 0.5, 0, 1)

res.val <- glm(y~X, family=binomial(link='logit'))

classifier <- LogisticRegression(X, y)
classifier

(classifier$fitted.values < 0.5) == (res.val$fitted.values < 0.5)

#### KNN ####

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
true_cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

#
library(class)
res.val <- as.vector(knn(train, test, cl, k = 9))
table(res.val, true_cl)
sum(res.val == true_cl)/length(true_cl)

#
source("KNearestNeighbor.R")
classifier <- KNearestNeighbor(train, cl, k = 9)
res <- predict(classifier, test)
table(res, true_cl)
sum(res == true_cl)/length(true_cl)
qplot(x = test[,1], y = test[,2], color = res, shape = res == true_cl)

