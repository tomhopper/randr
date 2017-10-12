#' @title Tests the randomness of a numeric vector or matrix
#' @description Runs several visual and statistical checks to determine if a given vector is truly random.
#' @param x A numeric vector.
#' @param n_max A number indicating the maximum lines in x for which a \code{lag.plot()} should be generated.
#' @return A number indicating statistical randomness
#' @export
#' @examples
#' # Hopefully random distributions
#' x <- runif(100)
#' randomness(x)
#' x <- rnorm(100)
#' randomness(x)
#'
#' # True random numbers
#' if (require(random)) {
#'   x <- randomNumbers()
#'   randomness(x)
#' }
#'
#' # Non-random sequences
#' x <- sin((1:100)/pi)
#' randomness(x)
#' x <- 1:100
#' randomness(x)
randomness <- function(x, n_max = 5000) {
  if (!("numeric" %in% class(x)) & !("integer" %in% class(x)) & !("matrix" %in% class(x)))
    stop("'x' must be numeric, integer or matrix.")
  # First, visual checks
  # Lag plot may show structure if non-random
  if (length(x) <= n_max) plot_lag <- stats::lag.plot(x)
  # Random data should not show autocorrelation
  stats::acf(as.vector(x))
  # Random data should have few violations on an x-bar control chart
  if (require(qcc) & length(x) < n_max) {
    if(length(x) > 200) qcc::qcc(x[1:100], type = "xbar.one", newdata = x[101:length(x)])
    else plot_qcc <- qcc::qcc(x, type = "xbar.one", plot = T)
  }
  # Now verify graphical analysis with statistical analysis. Use randtests::runs.test()
  # if possible. Otherwise do the same test less robustly.
  if(require(randtests)) result <- randtests::runs.test(x)
  else {
    x <- na.omit(x[x != median(x)])
    n1 <- sum(x > median(x))
    n2 <- sum(x < median(x))
    n <- n1 + n2
    R_bar <- (2*n1*n2)/(n)+1
    s_r <- sqrt(((R_bar - 1)*(R_bar - 2))/(n1+n2-1))
    runs <- rle(x > median(x))
    R_obs <- length(runs$lengths)

    if(R_bar > R_obs) {
      result <- c(pvalue = 2 * (pnorm(q = R_obs, mean = R_bar, sd = s_r)))
    } else {
      result <- c(pvalue = 2 * (1 - pnorm(q = R_obs, mean = R_bar, sd = s_r)))
    }
  }
  return(result)
}
