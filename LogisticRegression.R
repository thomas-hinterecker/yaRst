# Logistic Regression Classification
#
#
LogisticRegression <- function (X, y) {
  library(MASS)
  X <- as.matrix(X)
  m <- nrow(X)
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,]) 
  pheta <- matrix(0, nrow = ncol(A), ncol = 1)
  for (i in 1:1000) {
    z <- sigmoid(A%*%pheta)
    L <- -sum(y*log(z) + (1-y)*log(1-z))
    dwL <- t(A)%*%(z-y)
    B <- diag(as.vector(z*(1-z)), nrow(A), nrow(A))
    pheta = pheta - ginv(t(A) %*% B %*% A) %*% dwL
  }
  #
  out <- list(coefficients = pheta, fitted.values = z)
  class(out) <- "LogisticRegression"
  out
}

predict.LogisticRegression <- function (object, predict) {
  
}
