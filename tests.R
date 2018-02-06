

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
true_cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))

#
library(class)
res.val <- as.vector(knn(train, test, cl, k = 5))
sum(res.val == true_cl)/length(true_cl)

#
source("KNearestNeighbor.R")
classifier <- KNearestNeighbor(train, cl, k = 9)
res <- predict(classifier, test)
table(res, true_cl)
sum(res == true_cl)/length(true_cl)
