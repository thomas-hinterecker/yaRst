
LinearRegression <- function (X, y, lambda = 0, normalize = FALSE) {
  #library(MASS)
  X <- as.matrix(X)
  if (normalize == TRUE) {
    X <- scale(X, center=TRUE, scale=TRUE)
  }
  # Number of examples
  m <- nrow(X)
  # Design matrix
  A <- cbind(matrix(1, nrow = m, ncol = 1), X[,]) 
  # Get pheta
  I <- diag(1, ncol(A), ncol(A))
  I[1] <- 0
  pheta.ml = solve(t(A) %*% A + lambda*I) %*% t(A) %*% y
  # Calculate y.hat
  y.fitted <- A %*% pheta.ml
  # Residuals
  residuals <- y - y.fitted
  # Loss
  #rss <- sum(residuals^2)
  rss <- t(y - A %*% pheta.ml) %*% (y - A %*% pheta.ml) + lambda*t(pheta.ml) %*% pheta.ml
  # R^2
  tss <- sum((y - mean(y))^2)
  rsquared <- 1 - rss/tss
  # Degrees of freedom of residuals
  df.residuals <- n - nrow(pheta.ml)
  # Output
  z <- list(coefficients = pheta.ml, 
            df.residuals = df.residuals, 
            fitted.values = y.fitted, 
            m = m, 
            residuals = residuals,
            rss = rss,
            sigma = sqrt(rss/df.residuals),
            rsquared = rsquared,
            rsquared.corr = 1 - (1 - rsquared) * (m - 1) / (m - nrow(pheta.ml)),
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
