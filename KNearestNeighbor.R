# K-Nearest Neighbor Classification
#
#
KNearestNeighbor <- function (X, y, k = 3) {
  n_train <- nrow(X)
  n_features <- ncol(X)
  classes <- levels(y)
  out <- list(X = X, y = y, n_train = n_train, n_features = n_features, classes = classes, k = k)
  class(out) <- "KNearestNeighbor"
  out
}

predict.KNearestNeighbor <- function (object, predict) {
  predict <- matrix(predict, ncol = object$n_features)
  n_test <- nrow(predict)
  predicted_classes <- vector("integer", n_test)
  for (i in 1:n_test) {
    distances = c()
    for (j in 1:object$n_train) {
      distances[j] = sqrt(sum((predict[i,] - object$X[j,])^2 ))
    }
    class_votes <- vector("integer", length(object$classes))
    for (j in 1:object$k) {
      pos = which.min(distances)
      class_votes[object$y[pos]] = class_votes[object$y[pos]] + 1
      distances[pos] <- NA
      distances <- distances[!is.na(distances)]
    }
    predicted_classes[i] <- object$classes[which.max(class_votes)]
  }
  return(predicted_classes)  
}
