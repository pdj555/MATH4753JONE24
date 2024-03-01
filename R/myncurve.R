myncurve = function(mu, sigma, a){
  # Define the range for x
  x_range <- seq(mu - 3*sigma, mu + 3*sigma, length.out = 1000)

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

# Example of usage:
myncurve(mu = 0, sigma = 1, a = 1)
