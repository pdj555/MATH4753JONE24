#' myncurve
#'
#' Calculate and Plot Normal Distribution with Shaded Area
#'
#' This function plots the normal distribution curve for a given mean (mu) and standard
#' deviation (sigma), and shades the area under the curve from -infinity to a specified value (a).
#' It also calculates the probability that a normally distributed random variable is less than
#' or equal to a (P(X <= a)).
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a The x-value to which you want to calculate the probability and shade the area under the curve.
#'
#' @return A list containing the mean (`mu`), standard deviation (`sigma`), and the calculated
#' probability (`P_X_le_a`).
#'
#' @export
#'
#' @examples
#' myncurve(mu = 0, sigma = 1, a = 1)
myncurve = function(mu, sigma, a){
  # Define the range for x
  x_range <- seq(mu - 3*sigma, mu + 3*sigma, length.out = 1000)
  x <- x_range  # Define the x variable

  # Plot the normal distribution curve
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))

  # Shade the area under the curve to x = a
  x_shade <- seq(mu - 3*sigma, a, length.out = 1000)
  y_shade <- dnorm(x_shade, mean=mu, sd=sigma)
  polygon(c(min(x_range), x_shade, a), c(0, y_shade, 0), col = "skyblue")

  # Calculate the area (probability)
  prob <- pnorm(a, mean=mu, sd=sigma)

  # Return the mu, sigma, and the calculated probability
  list(mu = mu, sigma = sigma, P_X_le_a = prob)
}
