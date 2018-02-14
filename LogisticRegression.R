# Logistic Regression Classification
#
#
LogisticRegression <- function (X, y, iters = 1000) {
  library(MASS)
  X <- as.matrix(X)
  m <- nrow(X)
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,]) 
  n <- ncol(A)
  pheta <- matrix(0, nrow = ncol(A), ncol = 1)
  prev_pheta <- vector("numeric", length = n)
  for (i in 1:iters) {
    z <- sigmoid(A %*% pheta)
    L <- -sum(y*log(z) + (1-y)*log(1-z))
    dwL <- t(A) %*% (z-y)
    B <- diag(as.vector(z*(1-z)), nrow(A), nrow(A))
    pheta = pheta - ginv(t(A) %*% B %*% A) %*% dwL
    if (all(pheta == prev_pheta)) {
      break
    }
    prev_pheta <- pheta
  }
  #
  out <- list(coefficients = pheta, fitted.values = z)
  class(out) <- "LogisticRegression"
  out
}

predict.LogisticRegression <- function (object, predict) {
  
}
