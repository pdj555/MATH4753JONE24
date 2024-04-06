#' My Boot Function
#'
#' This function performs bootstrap resampling to estimate the confidence interval of a given statistic.
#'
#' @param iter The number of bootstrap iterations to perform (default is 10000).
#' @param x A numeric vector or data from which to sample.
#' @param fun The statistic function to apply to each bootstrap sample (default is "mean").
#' @param alpha The level of significance to use for the confidence interval (default is 0.05).
#' @param cx The expansion factor for the text in the plot (default is 1.5).
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A list containing the confidence interval (`ci`) and the point estimate (`pte`) of the statistic.
#' @export
#'
#' @examples
#' set.seed(68)
#' sample_data <- rnorm(20, mean = 10, sd = 4)
#' myboot2(x = sample_data)
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  fun <- match.fun(fun)
  xstat <- apply(rs.mat, 2, fun)
  ci <- stats::quantile(xstat, c(alpha/2, 1 - alpha/2))

  # Creating the histogram and getting the midpoints for use in the text() function
  hist_info <- graphics::hist(xstat, freq = FALSE, las = 1,
                              main = paste("Histogram of Bootstrap sample statistics",
                                           "\n", "alpha=", alpha, " iter=", iter, sep = ""), ...)

  pte <- fun(x)

  graphics::abline(v = pte, lwd = 3, col = "Black")
  graphics::segments(ci[1], 0, ci[2], 0, lwd = 4)
  # Use the maximum frequency from the histogram for the text position
  graphics::text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  graphics::text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  graphics::text(pte, max(hist_info$density) / 2, round(pte, 2), cex = cx)

  return(list(ci = ci, pte = pte))
}
