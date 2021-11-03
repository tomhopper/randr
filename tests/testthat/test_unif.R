#test_unif.r
# library(testthat)

test_that("Check runif_between within min and max", {
  expect_gte(min(runif_between(10000, 1, 10)), 1)
  expect_lte(max(runif_between(10000, 1, 10)), 10)
})
