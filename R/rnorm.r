#' @title Normal distribution within a defined tolerance interval
#' @description Returns a normally distributed vector within the tolerance interval defined by \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate.
#' @param lower (optional) numeric. The lower \code{(1 - confidence_level)/2} tolerance limit.
#' @param upper (optional) numeric. The upper \code{(1 + confidence_level)/2} tolerance limit.
#' @param confidence_level (optional) numeric. The confidence level at which interval \code{lower}, \code{upper} is obtained. To specify sigmas instead, set \code{confidence_level = NA}.
#' @param sigma (optional) numeric.  If \code{confidence_level = NA}, the number of standard deviations between \code{lower} and \code{upper}.
#' @details \code{confidence_level} indicates the confidence level of the interval \code{[lower, upper]}. For example, a high-confidence estimate might be 0.9973, or six standard deviations between \code{lower} and \code{upper}.
#' @return numeric vector with n elements randomly distributed so that approximately \code{confidence_level * 100} percent of values will fall between \code{lower} and \code{upper}.
#' @examples
#' rnorm_within(10)
#' rnorm_within(10, 10, 20)
#' summary(rnorm_within(10000, 10, 20))
rnorm_within <- function(n, lower=0, upper=1, confidence_level = 0.90, sigma = NA) {
  if (is.na(confidence_level) & is.numeric(sigma)) {
    confidence_level <- 2 * pnorm(sigma / 2) - 1
  }
  if (is.numeric(n) & is.numeric(lower) & is.numeric(upper) & is.numeric(confidence_level)) {
    if (confidence_level > 0 && confidence_level < 1) {
      mean_t <- mean(c(upper, lower))
      sd_t <- (upper - lower) / (qnorm((1 + confidence_level)/2) - qnorm((1 - confidence_level)/2))
      x <- rnorm(n, mean = mean_t, sd = sd_t)
      return(x)
    } else {
      invisible(NaN)
      stop("confidence_level must be between 0 and 1.")
    }
  } else {
    invisible(NaN)
    stop("Not all arguments were numeric; please supply numeric arguments.")
  }
}

#' @title Normal distribution betwen given limits
#' @description Returns a normally distributed vector between \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate
#' @param lower (optional) numeric. The minimum value to return
#' @param upper (optional) numeric. The maximum value to return
#' @return numeric vector with n elements randomly distributed between \code{lower} and \code{upper}
#' @examples
#' rnorm_between(10)
#' rnorm_between(10, 10, 20)
#' summary(rnorm_between(10000, 10, 20))
rnorm_between <- function(n, lower = 0, upper = 1) {
  if (is.numeric(n) & is.numeric(lower) & is.numeric(upper)) {
    x <- rnorm(n)
    min_x <- min(x)
    x <- (x - min_x) / (max(x) - min_x)
    x <- x * (upper - lower) + lower
    return(x)
  } else {
    invisible(NaN)
    stop("lower and upper were not numeric; please supply numeric arguments.")
  }
}
