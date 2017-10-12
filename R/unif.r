#' @title Uniform distribution between specified limits (wrapper around \code{runif()})
#' @description Returns a uniformly distributed vector between \code{lower} and \code{upper}. Bounded as \code{[lower, upper]}.
#' @param n (required) integer. The number of random numbers to generate.
#' @param lower (optional) integer. The minimum value to return.
#' @param upper (optional) integer. The maximum value to return.
#' @return numeric vector with n elements randomly distributed between \code{lower} and \code{upper}. If an error occurs, will usually return \code{NULL} and print an error message.
#' @export
#' @examples
#'  runif_between(10)
#'  runif_between(10, 10, 20)
#'  summary(runif_between(10000, 10, 20))
runif_between <- function(n, lower = 0, upper = 1,...) {
    return(runif(n, lower, upper,...))
}

#' @title Uniform distribution integers of fixed length
#' @description Returns a uniformly distributed integer vector, where every element has \code{digits} number of digits to the left of the decimal.
#' @param n (required) integer. The number of random values to generate.
#' @param digits (required) integer. The number of digits desired to the left of the decimal.
#' @return numeric vector with n integer elements uniformly randomly distributed with \code{digits} number of digits. If an error occurs, will usually return \code{NULL} and print an error message.
# @import numbr
#' @export
#' @examples
#'  runif_digits(1, 6)
#'  runif_digits(10, 6)
runif_digits <- function(n, digits = 4) {
  if(is.numeric(n) & is.numeric(digits)){
    x <- runif(n)
    xexp <- digits - numbr::exponent(x) - 1
    x <- trunc(x*10^xexp)
    return(x)
  } else {
    invisible(NULL)
    stop("n must be integer and digits must be numeric.")
  }

}

