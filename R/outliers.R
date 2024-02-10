#' Find Outliers
#'
#' @param I variable in vector
#'
#' @return z values, z values greater than 3, z values that are possible outliers, I values corresponding to these z values
#' @export
#'
#' @examples
#' outliers(I = I.df$IRON)
outliers <- function(I){
  #Standardize(transform) into z values
  z=(I-mean(I))/sd(I)

  # Find the z values greater than 3 in size
  z[abs(z)>3]

  # Find the I values corresponding to these z values
  I[abs(z)>3]

  # Find the values of z that are possible outliers
  z[abs(z)>=2 & abs(z)<=3]

  #Find the values of I which are possible outliers
  I[abs(z)>=2 & abs(z)<=3]
}

