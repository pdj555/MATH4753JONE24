#' Calculate Optimal Tickets to Sell with Overbooking
#'
#' This function calculates the optimal number of tickets to sell based on the probability of overbooking, using both the binomial distribution and the normal approximation. It plots the objective function against the number of tickets sold for both cases and returns a list with the calculated values.
#'
#' @param N The total number of seats on the flight.
#' @param gamma The probability that the flight will be overbooked.
#' @param p The probability of an individual passenger showing up.
#' @importFrom graphics abline points
#' @importFrom stats pbinom qbinom qnorm
#' @return A list containing the optimal number of tickets to sell using the binomial distribution (`nd`), the optimal number using normal approximation (`nc`), and the input parameters `N`, `p`, and `gamma`.
#' @export
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  # Discrete case using binomial distribution
  nd <- qbinom(gamma, N, p, lower.tail = FALSE)
  # Continuous case using normal approximation
  nc <- qnorm(gamma, N * p, sqrt(N * p * (1 - p)), lower.tail = FALSE)
  # Create named list containing nd, nc, N, p and gamma
  results_list <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  # Define and plot the objective function for discrete case
  objective_discrete <- function(n) gamma - pbinom(n, N, p, lower.tail = FALSE)
  n_values <- seq(N-50, N+50, by = 1)
  plot(n_values, sapply(n_values, objective_discrete), type = "o", pch = 16, col = "blue", cex = 0.5,
       xlab = "Number of tickets (n)", ylab = "Objective function value (V)",
       main = "Objective V vs n to find optimal tickets sold (discrete)")
  abline(h = 0, col = "red")
  # Highlight the point where objective function crosses zero
  points(nd, 0, col = "red", pch = 19, cex = 0.5)
  # Define and plot the objective function for continuous case
  objective_continuous <- function(n) gamma - pnorm(n, N * p, sqrt(N * p * (1 - p)), lower.tail = FALSE)
  plot(n_values, sapply(n_values, objective_continuous), type = "l", col = "black",
       xlab = "Number of tickets (n)", ylab = "Objective function value (V)",
       main = "Objective V vs n to find optimal tickets sold (continuous)")
  abline(h = 0, col = "red")
  # Highlight the point where objective function crosses zero
  points(nc, 0, col = "red", pch = 19, cex = 0.5)

  return(results_list)
}
