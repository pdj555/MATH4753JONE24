#' Central Limit Theorem Demonstration Function
#'
#' This function takes samples from a uniform distribution and calculates their means to demonstrate the Central Limit Theorem.
#' @param n The number of samples to take.
#' @param iter The number of iterations to perform, each resulting in one sample mean.
#' @param a The minimum value of the uniform distribution (inclusive).
#' @param b The maximum value of the uniform distribution (exclusive).
#'
#' @return A vector of sample means.
#' @importFrom stats runif
#' @export
#'
#' @examples
#' myclt(n = 10, iter = 1000, a = 0, b = 5)
myclt <- function(n, iter, a, b) {
  Y <- runif(n * iter, a, b)
  data <- matrix(Y, nrow=n, ncol=iter, byrow=TRUE)
  sm <- apply(data, 2, mean)
  return(sm)
}
