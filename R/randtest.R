library(qcc)
library(randtests)

#' @title Tests the randomness of a numeric vector or matrix
#' @import randtests qcc
#' @description Runs several checks to determine if a given vector is random
#' @param x A numeric vector
#' @return A number indicating statistical randomness
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
  if (!("numeric" %in% class(x)) & !("integer" %in% class(x)))
    stop("Vector 'x' must be numeric or integer.")
  # First, visual checks
  # Lag plot may show structure if non-random
  if (length(x) <= n_max) plot_lag <- lag.plot(x)
  # Random data should not show autocorrelation
  acf(x)
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
      result <- 2 * (pnorm(q = R_obs, mean = R_bar, sd = s_r))
    } else {
      result <- 2 * (1 - pnorm(q = R_obs, mean = R_bar, sd = s_r))
    }
  }
  return(result)
}
