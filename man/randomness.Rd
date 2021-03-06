% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randtest.R
\name{randomness}
\alias{randomness}
\title{Tests the randomness of a numeric vector or matrix}
\usage{
randomness(x, n_max = 5000)
}
\arguments{
\item{x}{A numeric vector.}

\item{n_max}{A number indicating the maximum lines in x for which a \code{lag.plot()} should be generated.}
}
\value{
A number indicating statistical randomness
}
\description{
Runs several visual and statistical checks to determine if a given vector is truly random.
}
\examples{
# Hopefully random distributions
x <- runif(100)
randomness(x)
x <- rnorm(100)
randomness(x)

# True random numbers
if (require(random)) {
  x <- randomNumbers()
  randomness(x)
}

# Non-random sequences
x <- sin((1:100)/pi)
randomness(x)
x <- 1:100
randomness(x)
}
