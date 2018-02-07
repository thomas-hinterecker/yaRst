
LinearRegression <- function (X, y) {
  library(MASS)
  X <- as.matrix(X)
  m <- nrow(X)
  #
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,]) 
  pheta.ml = ginv(t(A) %*% A) %*% t(A) %*% y
  #
  y.fitted <- A %*% pheta.ml
  residuals <- y.fitted - y
  #
  z <- list(coefficients = pheta.ml, 
            df.residual = n - nrow(pheta.ml), 
            fitted.values = y.fitted, 
            m = m, residuals = residuals, X = X, y = y)
  class(z) <- c("LinearRegression")
  return(z)
}

summary.LinearRegression <- function (object) {
  est <- object$coefficients
  rss <- sum(object$residuals^2)
  resvar <- rss/object$df.residual
  
  ans <- list()
  ans$coefficients <- cbind(Estimate = est)
  ans$sigma <- sqrt(resvar)
  class(ans) <- "summary.LinearRegression"
  return(ans)
}

predict.LinearRegression <- function (object, X) {
  X <- as.matrix(X)
  m <- nrow(X)
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,]) 
  y.predict <- A %*% object$coefficients
  return(y.predict)
}

logLik.LinearRegression <- function (object) {
  rss <- sum(object$residuals^2)
  resvar <- rss/object$df.residual
  sigma <- sqrt(resvar)
  sigmaML = sigma*sqrt((object$m-nrow(object$coefficients))/object$m)
  loglik = sum(log(dnorm(object$y,object$fitted.values,sigmaML)))
  return(loglik)
}
