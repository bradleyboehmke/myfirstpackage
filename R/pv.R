#' @title Present Value
#'
#' @description Compute the present value based on a specified future value,
#' interest rate, and number of time periods.
#'
#' @param FV a number representing some future value
#' @param r rate of return
#' @param n number of time periods
#'
#' @examples
#' pv(FV = 1000, .09, n = 5)
#'
#' @export

pv <- function(FV, r, n = 5) {

  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }

  if(!is.numeric(FV) | !is.numeric(r) | !is.numeric(n)) {
    stop('This function only works for numeric inputs!\n',
         'You have provided objects of the following classes:\n',
         'FV: ', class(FV), '\n',
         'r: ', class(r), '\n',
         'n: ', class(n))
  }

  if(r < 0 | r > .25) {
    message('The input for r exceeds the normal\n',
            'range for interest rates (0-25%)')
  }

  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}
