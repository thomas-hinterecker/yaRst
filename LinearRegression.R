
LinearRegression <- function (X, y) {
  # estimate parameters
  y.mean <- mean(y)
  X.mean <- mean(X)
  beta1 <- sum((x - X.mean)*(y-y.mean))/sum((X-X.mean)^2)
  beta0 <- y.mean - beta1*X.mean
  #
  y.fitted <- beta1*x+beta0
  residuals <- y.fitted-y
  #
  z <- list(coefficients = list(beta0, beta1), 
            df.residual = n - 2, 
            fitted.values = y.fitted, 
            residuals = residuals, X = X, y = y)
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

predict.LinearRegression <- function (object, x) {
  ncoef <- length(object$coefficients)
  coef <- unlist(object$coefficients)
  slopes <- coef[2:(ncoef)]
  y.predict <- slopes*x+coef[1]
  return(y.predict)
}

summary.lm
