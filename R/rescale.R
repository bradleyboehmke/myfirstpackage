#' @title Rescale
#'
#' @description Normalizes a numeric vector by standardizing the values between 0-1.
#'
#' @param x a vector of numeric values
#' @param digits number of digits to round the output to
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @examples
#' x <- runif(25, min = 5, max = 20)
#' rescale(x)
#'
#' @export

rescale <- function(x, digits = 2, na.rm = TRUE){
  # ensure argument inputs are valid
  if(!is.numeric(x)) {
    stop('x must be an atomic numeric vector')
  }
  if(!is.numeric(digits) | length(digits) > 1) {
    stop('digits must be a numeric vector of one element')
  }
  if(!is.logical(na.rm)) {
    stop('na.rm must be logical input (TRUE or FALSE)')
  }

  if(isTRUE(na.rm)) x <- stats::na.omit(x)
  rng <- range(x)
  scaled <- (x - rng[1]) / (rng[2] - rng[1])
  round(scaled, digits = digits)
}
