#test_norm.r
#library(testthat)

test_that("Check valid input integer or double", {
  expect_type(rnorm_between(10L, 1, 10), type = "double")
  expect_type(rnorm_between(10.5, 1, 10), type = "double")
  expect_type(rnorm_within(10L, 1, 10), type = "double")
  expect_type(rnorm_within(10.5, 1, 10), type = "double")
})

test_that("Check non-numeric input throws an error.", {
  expect_error(rnorm_between("e"))
  expect_error(rnorm_between(complex(real = 10, imaginary = 5)))
  expect_error(rnorm_between(c(TRUE)))
  expect_error(rnorm_between(n = 10, minimum = "e"))
  expect_error(rnorm_between(n = 10, maximum = "e"))
  expect_error(rnorm_within(n = "e"))
  expect_error(rnorm_within(complex(real = 10, imaginary = 5)))
  expect_error(rnorm_within(c(TRUE)))
  expect_error(rnorm_within(n = 10, confidence_level = "e"))
  expect_error(rnorm_within(n = 10, lower = "e", upper = 10))
  expect_error(rnorm_within(n = 10, lower = 1, upper = "e"))
})

test_that("Check multiple input values are flagged as errors.", {
  expect_error(rnorm_between(c(10, 15), 1, 10))
  expect_error(rnorm_within(c(10, 15), 1, 10))
})

test_that("Check output length matches n for integer and decimal values of n.", {
  expect_length(rnorm_between(10L, 1, 10), 10)
  expect_length(rnorm_between(10.5, 1, 10), 10)
  expect_length(rnorm_within(10L, 1, 10), 10)
  expect_length(rnorm_within(10.5, 1, 10), 10)
})

test_that("Check rnorm_between within min and max", {
  expect_gte(min(rnorm_between(10000, 1, 10)), 1)
  expect_lte(max(rnorm_between(10000, 1, 10)), 10)
})

test_that("Check that error handling catching minimum >= maximum", {
  expect_error(rnorm_between(10000, 10, 1))
  expect_error(rnorm_between(10000, 1, 1))
  expect_error(rnorm_within(10000, 10, 1))
  expect_error(rnorm_within(10000, 1, 1))
})

