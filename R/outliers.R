#' Find Outliers
#'
#' Identifies outliers in a numeric vector based on z-scores. Outliers are defined as observations with z-scores greater than 3, and potential outliers are those with z-scores between 2 and 3.
#'
#' @param I Numeric vector for which to find outliers.
#' @return A list containing:
#' \itemize{
#'   \item{z}{All z-scores of the input vector.}
#'   \item{outliers}{Z-scores greater than 3.}
#'   \item{outlier_values}{I values corresponding to z-scores greater than 3.}
#'   \item{potential_outliers}{Z-scores between 2 and 3.}
#'   \item{potential_outlier_values}{I values corresponding to z-scores between 2 and 3.}
#' }
#' @export
#'
#' @examples
#' outliers(I = c(1, 2, 3, 4, 100))
outliers <- function(I){
  #Standardize(transform) into z values
  z <- (I - mean(I)) / sd(I)

  # Prepare the list to return
  results <- list(
    z = z,
    outliers = z[abs(z) > 3],
    outlier_values = I[abs(z) > 3],
    potential_outliers = z[abs(z) >= 2 & abs(z) <= 3],
    potential_outlier_values = I[abs(z) >= 2 & abs(z) <= 3]
  )

  return(results)
}
