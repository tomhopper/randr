.beta_shapes <- function(mode, k) {
  a <- mode * (k - 2) + 1
  b <- (1 - mode) * (k - 2) + 1
  return(list(shape1 = a, shape2 = b))
}

.beta_parameters_between <- function(mode, lower, upper) {

  maximum <- upper
  minimum <- lower
  mean_point <- (minimum + 4 * mode + maximum) / 6
  sd <- (maximum - minimum)/6

  shape1 <- ((mean_point - minimum)/(maximum - minimum))*(((mean_point - minimum)*(maximum - mean_point)/sd^2) - 1)
  shape2 <- ((maximum - mean_point)/(mean_point - minimum)) * shape1
  #shape2 <- (shape1 * (maximum - mode) + 2*maximum - (minimum + maximum))/(mode - minimum)

  return(list(shape1 = shape1, shape2 = shape2, min = minimum, max = maximum))
}

.beta_parameters_within <- function(mode, lower, upper, confidence_level) {
  sd_u <- (upper - mode) / qnorm((1 + confidence_level)/2)
  sd_l <- (mode - lower) / qnorm((1 + confidence_level)/2)
  maximum <- mode + 3 * sd_u
  minimum <- max(mode - 3 * sd_l, 0)

  return(.beta_parameters_between(mode, minimum, maximum))
}

#' @title Beta distribution within a defined tolerance interval
#' @description Returns a beta-distributed vector within the tolerance interval defined by \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate.
#' @param mode (required) numeric. The mode, or most likely value, of the returned distribution.
#' @param lower (required) numeric. The lower \code{(1 - confidence_level)/2} tolerance limit.
#' @param upper (required) numeric. The upper \code{(1 + confidence_level)/2} tolerance limit.
#' @param confidence_level (optional) numeric. The confidence level at which interval \code{lower}, \code{upper} is obtained. To specify sigmas instead, set \code{conf.level = NA}.
#' @param sigma (optional) numeric.  If \code{confidence_level = NA}, the number of standard deviations between \code{lower} and \code{upper}.
#' @details \code{conf.level} indicates the confidence level of the interval \code{[lower, upper]}. For example, a high-confidence estimate might be 0.9973, or six standard deviations between \code{lower} and \code{upper}.
#' @return numeric vector with n elements randomly distributed so that approximately \code{confidence_level * 100} percent of values will fall between \code{lower} and \code{upper}.
#' @examples
#' rbeta_within(10, 5, 3, 10)
#' rbeta_within(10, 5, 3, 10, 0.99)
#' summary(rbeta_within(1000, 5, 3, 10))
rbeta_within <- function(n, mode, lower, upper, confidence_level = 0.9, sigma = NA) {
  if (!is.na(sigma) & is.na(confidence_level)) {
    confidence_level <- 2 * pnorm(sigma / 2) - 1
  }
  if (!missing(n) & !missing(mode) & !missing(lower) & !missing(upper)) {
    if (is.numeric(n) & is.numeric(mode) & is.numeric(lower) & is.numeric(upper) & is.numeric(confidence_level)) {
      params <- .beta_parameters_within(mode, lower, upper, confidence_level)
    } else{
      invisible(NaN)
      stop("Arguments \'n,\' \'mode,\' \'lower,\' \'upper,\' and \'confidence_level\' must be numeric")
    }
  } else {
    if (!missing(n) & !missing(mode)){
      #k <- 1 / (1 - confidence_level) # <-- NEEDS TESTING TO MATCH .beta_shapes() CALCULATION TO CONFIDENCE LEVEL
      #params <- betaShapes(mode, k)
      invisible(NaN)
      stop("Calculation using mode and k not yet implemented.")
    } else {
      invisible(NaN)
      stop("plot_beta() requires the number, n, and that you to supply \'mode\' with either \'k\' or with both \'lower\' and \'upper\'.")
    }
  }
  return(rbeta(n, shape1 = params[[1]], shape2 = params[[2]])*(params[[4]] - params[[3]])+params[[3]])
}

#' @title Beta distribution betwen given limits
#' @description Returns a beta distributed vector between \code{lower} and \code{upper}.
#' @param n (required) integer. The number of random numbers to generate
#' @param mode (required) numeric. The mode, or most likely value, of the returned distribution.ß
#' @param lower (optional) numeric. The minimum value to return
#' @param upper (optional) numeric. The maximum value to return
#' @return numeric vector with n elements randomly distributed between lower and upper
#' @examples
#' rbeta_between(10, 0.3)
#' rbeta_between(10, 10, 5, 20)
#' summary(rbeta_between(1000, 10, 5, 20))
rbeta_between <- function(n, mode, lower = 0, upper = 1) {
  if (!missing(n) & !missing(mode)) {
    if (mode > lower & mode < upper) {
      #cat(typeof(n), " ", typeof(mode), " ", typeof(lower), " ", typeof(upper))
      if (is.numeric(n) & is.numeric(mode) & is.numeric(lower) & is.numeric(upper)) {
        params <- .beta_parameters_between(mode, lower, upper)
      } else {
        invisible(NaN)
        stop("Arguments \'n,\' \'mode,\' \'lower\' and \'upper\' must be numeric.")
      }
    } else {
      invisible(NaN)
      stop("\'mode\' must be between \'lower\' and \'upper.\'")
    }
  } else {
    invisible(NaN)
    stop("rbeta_between() requires the number, \'n\' and \'mode.\'")
  }
  return(rbeta(n, shape1 = params[[1]], shape2 = params[[2]])*(params[[4]] - params[[3]])+params[[3]])
}

