#' Synthetic Dataset for Bootstrap Example
#'
#' A synthetic dataset generated as a sample from a normal distribution.
#' This dataset is used to demonstrate the bootstrap function in the package.
#'
#' @format A numeric vector of length 20.
#' @export
"demo_data"
demo_data <- rnorm(20, mean = 10, sd = 4)

usethis::use_data(demo_data, overwrite = TRUE)
