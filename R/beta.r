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
#' @return numeric vector with n elements randomly distributed so that approximately \code{confidence_level * 100} percent of values will fall between \code{lower} and \code{upper}. If an error occurs, will usually return \code{NULL} and print an error message.
#' @export
#' @examples
#' \dontrun{
#' rbeta_within(10, 5, 3, 10)
#' rbeta_within(10, 5, 3, 10, 0.99)
#' summary(rbeta_within(1000, 5, 3, 10))
#' }
rbeta_within <- function(n, mode = 0.3, lower = 0, upper = 1, confidence_level = 0.9, sigma = NA) {
  if (!missing(n) && !missing(mode) && !missing(lower) && !missing(upper)) {
    if (is.numeric(n) && is.numeric(mode) && is.numeric(lower) && is.numeric(upper) && (is.numeric(confidence_level) || is.numeric(sigma))) {
      if (length(n) == 1 && length(mode) == 1 && length(lower) == 1 && length(upper) == 1 && length(confidence_level) == 1) {
        if (!is.na(sigma) && is.na(confidence_level)) {
          confidence_level <- 2 * pnorm(sigma / 2) - 1
        }
        if (mode > lower && mode < upper) {
          if (confidence_level > 0 && confidence_level < 1) {
            params <- .beta_parameters_within(mode, lower, upper, confidence_level)
          } else {
            invisible(NULL)
            stop("confidence_level must be between 0 and 1.")
          }
        } else {
          invisible(NULL)
          stop("\'mode\' must be greater than \'lower\' and less than \'upper\'.")
        }
      } else {
        invisible(NULL)
        stop("Arguments must be supplied as singular values; I cannot accept vectors with length > 1.")
      }
    } else {
      invisible(NULL)
      stop("Arguments \'n,\' \'mode,\' \'lower,\' \'upper,\' and \'confidence_level\' must be numeric")
    }
  } else {
    if (!missing(n) && !missing(mode)) {
      #k <- 1 / (1 - confidence_level) # <-- NEEDS TESTING TO MATCH .beta_shapes() CALCULATION TO CONFIDENCE LEVEL
      #params <- betaShapes(mode, k)
      invisible(NULL)
      stop("Calculation using mode and k not yet implemented.")
    } else {
      invisible(NULL)
      stop("plot_beta() requires the number, n, and that you to supply \'mode\' with either \'k\' or with both \'lower\' and \'upper\'.")
    }
  }
  return(rbeta(n, shape1 = params[[1]], shape2 = params[[2]])*(params[[4]] - params[[3]])+params[[3]])
}

#' @title Beta distribution between given limits
#' @description Returns a beta distributed vector between \code{minimum} and \code{maximum}.
#' @param n (required) integer. The number of random numbers to generate
#' @param mode (required) numeric. The mode, or most likely value, of the returned distribution. Requires
#'   \eqn{minimum < mode < maximum}.
#' @param minimum (optional) numeric. The minimum value to return. Defaults to 0.
#' @param maximum (optional) numeric. The maximum value to return. Defaults to 1
#' @return numeric vector with n elements randomly distributed between \code{minimum} and \code{maximum}.
#'   If an error occurs, will usually invisibly return \code{NULL} and print an error message.
#' @export
#' @examples
#' \dontrun{
#' rbeta_between(10, 0.3)
#' rbeta_between(10, 10, 5, 20)
#' summary(rbeta_between(1000, 10, 5, 20))
#' }
rbeta_between <- function(n, mode = 0.3, minimum = 0, maximum = 1) {
  if (!missing(n) && !missing(mode)) {
    if (is.numeric(n) && is.numeric(mode) && is.numeric(minimum) && is.numeric(maximum) &
        !all(is.nan(n)) && !all(is.nan(mode)) && !all(is.nan(minimum)) && !all(is.nan(maximum))) {
      if (length(n) == 1 && length(mode) == 1 && length(minimum) == 1 && length(maximum) == 1) {
        if (mode > minimum && mode < maximum) {
          params <- .beta_parameters_between(mode = mode, lower = minimum, upper = maximum)
        } else {
          invisible(NULL)
          stop("\'mode\' must be greater than \'minimum\' and less than \'maximum\'.")
        }
      } else {
        invisible(NULL)
        stop("Arguments must be supplied as singular values; I cannot accept vectors with length > 1.")
      }
    } else {
      invisible(NULL)
      stop("Arguments \'n,\' \'mode,\' \'minimum\' and \'maximum\' must be numeric.")
    }
  } else {
    invisible(NULL)
    stop("rbeta_between() requires you supply at least \'n\' and \'mode\' as arguments.")
  }
  result <- rbeta(n, shape1 = params[["shape1"]], shape2 = params[["shape2"]])
  result <- result * (maximum - minimum) + minimum
  return(result)
}

