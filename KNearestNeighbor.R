# K-Nearest Neighbor Classification
#
#
KNearestNeighbor <- function (X, y, k = 3) {
  m_X <- nrow(X)
  n_features <- ncol(X)
  classes <- levels(y)
  out <- list(X = X, y = y, m_X = m_X, n_features = n_features, classes = classes, k = k)
  class(out) <- "KNearestNeighbor"
  out
}

predict.KNearestNeighbor <- function (object, predict) {
  predict <- matrix(predict, ncol = object$n_features)
  n_test <- nrow(predict)
  predicted_classes <- vector("integer", n_test)
  # Get predicted class for each test item
  for (i in 1:n_test) {
    distances = c()
    # Compute euclidean distances between features of test item and each train item
    for (j in 1:object$m_X) {
      distances[j] = sqrt(sum((predict[i,] - object$X[j,])^2 ))
    }
    # Get number of class votes in k neighbors
    class_votes <- vector("integer", length(object$classes))
    for (j in 1:object$k) {
      pos = which.min(distances)
      class_of_neighbor = object$y[pos]
      class_votes[class_of_neighbor] = class_votes[class_of_neighbor] + 1
      distances[pos] <- NA
      distances <- distances[!is.na(distances)]
    }
    # Get class with highest votes
    predicted_classes[i] <- object$classes[which.max(class_votes)]
  }
  return(predicted_classes)  
}
