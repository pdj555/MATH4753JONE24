# Load the testthat package
library(testthat)

# Load the package containing the function to be tested
# Use `devtools::load_all()` or `library(your_package_name)` if it's installed
# library(your_package_name)

# Define a test file
test_that("Test myncurve function", {

  # Test mu component of the named list
  res <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(res$mu, 0,
               tolerance = NULL, # remove tolerance parameter or set it to NULL
               msg = "Expected mu component of the list to be equal to 0.")

  # Test sigma component of the named list
  expect_equal(res$sigma, 1,
               tolerance = NULL, # remove tolerance parameter or set it to NULL
               msg = "Expected sigma component of the list to be equal to 1.")

  # Test P_X_le_a component of the named list
  expected_prob <- pnorm(1, mean=0, sd=1)
  expect_equal(res$P_X_le_a, expected_prob,
               tolerance = NULL, # remove tolerance parameter or set it to NULL
               msg = "Expected P_X_le_a component of the list to match the calculated probability.")
})
