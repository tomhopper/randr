#' @title Normal distribution within a defined tolerance interval
#' @description Returns a normally distributed vector within the tolerance interval defined by \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate.
#' @param lower (optional) numeric. The lower \code{(1 - confidence_level)/2} tolerance limit.
#' @param upper (optional) numeric. The upper \code{(1 + confidence_level)/2} tolerance limit.
#' @param confidence_level (optional) numeric. The confidence level at which interval \code{lower}, \code{upper} is obtained. To specify sigmas instead, set \code{confidence_level = NA}.
#' @param sigma (optional) numeric.  If \code{confidence_level = NA}, the number of standard deviations between \code{lower} and \code{upper}.
#' @details \code{confidence_level} indicates the confidence level of the interval \code{[lower, upper]}. For example, a high-confidence estimate might be 0.9973, or six standard deviations between \code{lower} and \code{upper}.
#' @return numeric vector with n elements randomly distributed so that approximately \code{confidence_level * 100} percent of values will fall between \code{lower} and \code{upper}. If there is a problem, will return \code{NULL} and print an error message.
#' @export
#' @examples
#' \dontrun{
#' rnorm_within(10)
#' rnorm_within(10, 10, 20)
#' summary(rnorm_within(10000, 10, 20))
#' }
rnorm_within <- function(n, lower=0, upper=1, confidence_level = 0.90, sigma = NA) {
  if (is.na(confidence_level) & is.numeric(sigma)) {
    confidence_level <- 2 * pnorm(sigma / 2) - 1
  }
  if (is.numeric(n) & is.numeric(lower) & is.numeric(upper) & is.numeric(confidence_level)) {
    if (confidence_level > 0 && confidence_level < 1) {
      mean_t <- mean(c(upper, lower))
      sd_t <- (upper - lower) / (qnorm((1 + confidence_level)/2) - qnorm((1 - confidence_level)/2))
      x <- rnorm(n, mean = mean_t, sd = sd_t)
    } else {
      invisible(NULL)
      stop("confidence_level must be between 0 and 1.")
    }
  } else {
    invisible(NULL)
    stop("Not all arguments were numeric; please supply numeric arguments.")
  }
  return(x)
}

#' @title Normal distribution between given limits
#' @description Returns a vector of normally distributed values between \code{minimum}
#'   and \code{maximum}. These limits will generally not be found in the result.
#' @details The function generates values using \code{rnorm(n = n * 6)}, then scales the result to the
#'   range defined by \code{minimum} and \code{maximum}, such that \code{minimum} and \code{maximum}
#'   will appear at least once. Finally, \eqn{n} values are sampled from this vector using
#'   \code{sample(size = n)}. As a result, the mean and standard deviation of the resulting distribution
#'   are not guaranteed, but will tend to be near \code{mean(c(minimum, maximum))} and
#'   \code{(maximum - minimum) / 6}, respectively. In practice, larger standard deviations are likely
#'   for smaller values of \eqn{n}, and smaller standard deviations are more likely for larger values
#'   of \eqn{n}. For instance, at \code{n = 1000}, the standard deviation of the result will usually
#'   be around \code{(maximum - minimum) / 7}. The resulting vector will also typically be
#'   indistinguishable from a
#'   random normal distribution (i.e. \code{shapiro.test()$p.value} will typically be greater than 0.1), but
#'   this is also not guaranteed.
#' @param n (required) integer. The number of random numbers to generate. Must be at least 2.
#' @param minimum (optional) numeric. The minimum value to return. Defaults to 0.
#' @param maximum (optional) numeric. The maximum value to return. Defaults to 1.
#' @return numeric vector of random values with \eqn{n} elements. If there is a problem, will return \code{NULL} and print an error.
#' @export
#' @examples
#' \dontrun{
#' rnorm_between(10)
#' rnorm_between(10, 10, 20)
#' summary(rnorm_between(10000, 10, 20))
#'
#' r_v <- rnorm_between(100, 10, 20)
#' qqnorm(r_v)
#' randtests::runs.test(r_v)
#' }
rnorm_between <- function(n, minimum = 0, maximum = 1) {
  if (is.numeric(n) & !is.nan(n)) {
    if(is.numeric(minimum) & !is.nan(minimum)){
      if(is.numeric(maximum) & !is.nan(maximum)) {
        if(minimum < maximum) {
          x <- rnorm(n = (n*6))
          if (all(!is.nan(x))) {
            x_max <- max(x)
            x_min <- min(x)
            x <- (x - x_min) / (x_max - x_min)
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
