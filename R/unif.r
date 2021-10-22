#' @title Uniform distribution between specified limits
#' @description Returns a vector of normally distributed values between \code{minimum}
#'   and \code{maximum}.
#' @param n (required) integer. The number of random numbers to generate. Must be 2 or greater.
#' @param minimum (optional) integer. The minimum value to return.
#' @param maximum (optional) integer. The maximum value to return.
#' @return numeric vector with n elements randomly distributed between
#'   \code{minimum} and \code{maximum}. If an error occurs, will usually return
#'   \code{NULL} and print an error message.
#' @export
#' @seealso rnorm_between
#' @examples
#' \dontrun{
#'  runif_between(10)
#'  runif_between(10, 10, 20)
#'  summary(runif_between(10000, 10, 20))
#' }
runif_between <- function(n, minimum = 0, maximum = 1) {
  if (is.numeric(n) & !is.nan(n)) {
    if(is.numeric(minimum) & !is.nan(minimum)){
      if(is.numeric(maximum) & !is.nan(maximum)) {
        if(minimum < maximum) {
          x <- runif(n * 6)
          if (all(!is.nan(x))) {
            min_x <- min(x)
            x <- (x - min_x) / (max(x) - min_x)
            x <- x * (maximum - minimum) + minimum
            x <- sample(x = x, size = n, replace = FALSE)
          } else {
            invisible(x)
            stop("There was a problem getting random numbers.")
          }
        } else {
          invisible(NULL)
          stop("minimum must be less than maximum")
        }
      } else {
        invisible(NULL)
        stop("maximum is not numeric; please supply numeric arguments.")
      }
    } else {
      invisible(NULL)
      stop("minimum is not numeric; please supply numeric arguments.")
    }
  } else {
    invisible(NULL)
    stop("n is not numeric; please supply numeric arguments.")
  }
  return(x)
}

#' @title Uniform distribution integers of fixed length
#' @description Returns a uniformly distributed integer vector, where every element has \code{digits} number of digits to the left of the decimal.
#' @param n (required) integer. The number of random values to generate.
#' @param digits (required) integer. The number of digits desired to the left of the decimal.
#' @return numeric vector with n integer elements uniformly randomly distributed with \code{digits} number of digits. If an error occurs, will usually return \code{NULL} and print an error message.
# @import numbr
#' @export
#' @examples
#' \dontrun{
#' runif_digits(1, 6)
#' runif_digits(10, 6)
#' }
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

