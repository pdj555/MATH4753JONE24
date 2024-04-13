#' Estimate Parameters of Normal Distribution
#'
#' This function estimates the mean and standard deviation of a normally
#' distributed sample using maximum likelihood estimation.
#'
#' @param data A numeric vector of data points assumed to be drawn from a normal distribution.
#' @return A list containing the maximum likelihood estimates for mean and standard deviation.
#' @importFrom stats optim
#' @export
#' @examples
#' sample_data <- rnorm(100, mean = 50, sd = 10)
#' mle_normal(sample_data)
mle_normal <- function(data) {
  # Log-likelihood function for the normal distribution
  normal_log_likelihood <- function(mu, sigma, y) {
    n <- length(y)
    -n/2 * log(2 * pi) - n * log(sigma) - 1/(2 * sigma^2) * sum((y - mu)^2)
  }

  # Use optim to maximize the log-likelihood
  mle <- optim(
    par = c(mu = mean(data), sigma = sd(data)),
    fn = function(p) -normal_log_likelihood(p[1], p[2], data),
    method = "L-BFGS-B",
    lower = c(-Inf, 0.001), # sigma must be positive
    control = list(fnscale = -1) # Maximize
  )

  # Return the estimates
  list(mean = mle$par[1], sd = mle$par[2])
}
