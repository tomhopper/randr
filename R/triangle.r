#' @title Triangle distribution within a defined tolerance interval
#' @description Returns a triangle-distributed vector within the \code{confidence_level} tolerance interval defined by \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate.
#' @param peak (optional) numeric. The location of the peak (most likely value) of the triangle distribution.
#' @param lower (optional) numeric. The lower \code{(1 - confidence_level)/2} tolerance limit.
#' @param upper (optional) numeric. The upper \code{(1 + confidence_level)/2} tolerance limit.
#' @param confidence_level (optional) numeric. The confidence level at which \code{upper} and \code{lower} are entered.
#' @param lower_bound (optional) numeric. The lower boundary for returned values;  no random values can be below this value. e.g. for processes where negative values do not make sense, \code{lower = 0}.
#' @param upper_bound (optional) numeric. The upper boundary for returned values;  no random values can be above this value.
#' @details \code{confidence_level} indicates the confidence level of the interval \code{[lower, upper]}. For example, a high-confidence estimate might be 0.9973, or six standard deviations between \code{lower} and \code{upper}.
#' @return numeric vector with n elements randomly distributed so that approximately \code{confidence_level} percent of values will fall between \code{lower} and \code{upper}. If an error occurs, will usually return \code{NULL} and print an error message.
# @import VGAM
#' @export
#' @examples
#' rtriangle_within(10)
#' rtriangle_within(10, 12, 10, 20)
#' summary(rtriangle_within(10000, 12, 10, 20))
rtriangle_within <- function(n, peak = 0.5, lower = 0, upper = 1, confidence_level = 0.90, lower_bound = -Inf, upper_bound = Inf) {
  if (is.numeric(n) & is.numeric(lower) & is.numeric(upper) & is.numeric(confidence_level) & is.numeric(peak) & is.numeric(lower) & is.numeric(upper)) {
    if (confidence_level > 0 && confidence_level < 1) {
      y_peak_l <- 2*(peak - lower) / ((upper - lower)*(peak - lower))
      y_peak_u <- 2*(upper - peak) / ((upper - lower)*(upper - peak))
      if(y_peak_l == y_peak_u) {
        alpha <- atan(y_peak_l/(peak - lower))
        beta <- atan(y_peak_u/(upper - peak))
        theta_l <- atan((peak - lower)/y_peak_l)
        theta_u <- atan((upper - peak)/y_peak_u)
        d_p <- sqrt(((1-confidence_level)*2)*y_peak_l*(upper - lower)/(sin(theta_l)/sin(alpha)+sin(theta_u)/sin(beta)))
        d_a <- d_p * sin(theta_l)/sin(alpha)
        d_b <- d_p * sin(theta_u)/sin(beta)
        lower <- ifelse((lower - d_a) < lower_bound, lower_bound, lower - d_a)
        upper <- ifelse((upper + d_b) > upper_bound, upper_bound, upper + d_b)
        x <- VGAM::rtriangle(n, peak, lower, upper)
        return(x)
      } else {
        invisible(NULL)
        stop("There was a problem calculating triangle properties. Please report this to the package maintainer with a reproducible example.")
      }
    } else {
      invisible(NULL)
      stop("confidence_level must be between 0 and 1.")
    }
  } else {
    invisible(NULL)
    stop("n must be integer and all other arguments must be numeric.")
  }
}

#' @title Triangle distribution between given limits (wrapper around \code{VGAM::rtriangle()})
#' @description Returns a triangle-distributed vector between \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate
#' @param peak (optional) numeric. The location of the peak (or most likely value) of the triangle distribution.
#' @param lower (optional) numeric. The minimum value to return
#' @param upper (optional) numeric. The maximum value to return
#' @return numeric vector with n elements randomly distributed between lower and upper with most likely value near \code{peak}.
# @import VGAM
#' @export
#' @examples
#' rtriangle_between(10)
#' rtriangle_between(10, 10, 5, 20)
#' summary(rtriangle_between(10000, 10, 5, 20))
rtriangle_between <- function(n, peak = 0.3, lower = 0, upper = 1) {
  return(VGAM::rtriangle(n, theta = peak, lower = lower, upper = upper))
}
