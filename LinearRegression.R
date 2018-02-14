
LinearRegression <- function (X, y, lambda = 0, delta = 0, normalize = FALSE) {
  #library(MASS)
  X <- as.matrix(X)
  if (normalize == TRUE) {
    X <- scale(X, center=TRUE, scale=TRUE)
  }
  # Number of examples
  m <- nrow(X)
  # Design matrix
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,]) 
  # Number of dimensions
  n <- ncol(A)
  # Get pheta
  I <- diag(1, ncol(A), ncol(A))
  I[1] <- 0
  pheta <- solve(t(A) %*% A + lambda*I) %*% t(A) %*% y
  # Lasso
  if (delta != 0) {
    prev_pheta <- vector("numeric", length = n+1)
    while (TRUE) {
      for (j in 1:n) {
        a = 2*sum(A[,j]^2)
        c = 2*sum(A[,j]*(y - A %*% pheta + A[,j] * pheta[j]))
        if (c < -1*delta) {
          pheta[j] <- (c+delta)/a
        } else if (c > delta) {
          pheta[j] <- (c-delta)/a
        } else {
          pheta[j] <- 0
        }
      }
      if (all(pheta == prev_pheta)) {
        break
      }
      prev_pheta <- pheta
    }
  }
  print(pheta)
  # Calculate y.hat
  y.fitted <- A %*% pheta
  # Residuals
  residuals <- y - y.fitted
  # Loss
  rss <- sum(residuals^2)
  #rss <- t(y - A %*% pheta) %*% (y - A %*% pheta) 
  #if (regularization == "ridge") {
  #  rss <- rss + lambda*t(pheta) %*% pheta
  #} else if (regularization == "lasso") {
  #  rss <- rss + lambda*sum(abs(pheta))
  #}
  # R^2
  tss <- sum((y - mean(y))^2)
  rsquared <- 1 - rss/tss
  # Degrees of freedom of residuals
  df.residuals <- m - nrow(pheta)
  # Output
  z <- list(coefficients = pheta, 
            df.residuals = df.residuals, 
            fitted.values = y.fitted, 
            m = m, 
            residuals = residuals,
            rss = rss,
            sigma = sqrt(rss/df.residuals),
            rsquared = rsquared,
            rsquared.corr = 1 - (1 - rsquared) * (m - 1) / (m - nrow(pheta)),
            X = X, 
            y = y,
            normalized = normalize)
  class(z) <- c("LinearRegression")
  return(z)
}

summary.LinearRegression <- function (object) {
  # calculate stats for coefficients
  
  # output
  ans <- list()
  ans$coefficients <- cbind(Estimate = object$coefficients)
  ans$rss <- object$rss
  ans$sigma <- object$sigma
  ans$df.residuals <- object$df.residuals
  ans$"R-squared" <- object$rsquared
  ans$"Adjusted R-squared" <- object$rsquared.corr
  class(ans) <- "summary.LinearRegression"
  return(ans)
}

predict.LinearRegression <- function (object, X) {
  X <- as.matrix(X)
  if (object$normalized == TRUE) {
    X <- scale(X, center=TRUE, scale=TRUE)
  }
  m <- nrow(X)
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,])
  y.predict <- A %*% object$coefficients
  return(y.predict)
}

logLik.LinearRegression <- function (object) {
  sigmaML = object$sigma*sqrt((object$m-nrow(object$coefficients))/object$m)
  loglik = sum(log(dnorm(object$y,object$fitted.values,sigmaML)))
  return(loglik)
}
