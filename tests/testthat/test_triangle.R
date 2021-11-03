#test_triangle.r
# library(testthat)

test_that("Check valid output with both integer and double input", {
  expect_type(rtriangle_between(10L, 3L, 1, 10), type = "double")
  expect_type(rtriangle_between(10.5, 3.4, 1, 10), type = "double")
  expect_type(rtriangle_within(10L, 3L, 1, 10), type = "double")
  expect_type(rtriangle_within(10.5, 3.4, 1, 10), type = "double")
})

test_that("Check non-numeric input throws an error.", {
  expect_error(rtriangle_between("e", 0.3))
  expect_error(rtriangle_between(complex(real = 10, imaginary = 5), complex(real = 3, imaginary = 2)))
  expect_error(rtriangle_between(c(TRUE), 0.3))
  expect_error(rtriangle_between(n = 10, mode = "e"))
  expect_error(rtriangle_between(n = 10, mode = 0.3, minimum = "e"))
  expect_error(rtriangle_between(n = 10, mode = 0.3, maximum = "e"))
  expect_error(rtriangle_within(n = "e", mode = 0.3))
  expect_error(rtriangle_within(n = 10, mode = "e"))
  expect_error(rtriangle_within(complex(real = 10, imaginary = 5), complex(real = 3, imaginary = 2)))
  expect_error(rtriangle_within(c(TRUE), 0.3))
  expect_error(rtriangle_within(n = 10, mode = 3, confidence_level = "e"))
  expect_error(rtriangle_within(n = 10, mode = 3, lower = "e", upper = 10))
  expect_error(rtriangle_within(n = 10, mode = 3, lower = 1, upper = "e"))
})

test_that("Check multiple input values are flagged as errors.", {
  expect_error(rtriangle_between(c(10, 15), 0.3))
  expect_error(rtriangle_within(c(10, 15), 0.3))
})

test_that("Check output length matches n for integer and decimal values of n.", {
  expect_length(rtriangle_between(10L, 0.3), 10)
  expect_length(rtriangle_between(10.5, 0.3), 10)
  expect_length(rtriangle_within(10L, 3, 1, 10), 10)
  expect_length(rtriangle_within(10.5, 3, 1, 10), 10)
})

test_that("Check rtriangle_between within min and max", {
  expect_gte(min(rtriangle_between(10000, 3, 1, 10)), 1)
  expect_lte(max(rtriangle_between(10000, 3, 1, 10)), 10)
})

test_that("Check that error handling catching lower, minimum < upper, maximum", {
  expect_error(rtriangle_between(10000, 3, 10, 1))
  expect_error(rtriangle_between(10000, 3, 1, 1))
  expect_error(rtriangle_within(10000, 3, 10, 1))
  expect_error(rtriangle_within(10000, 3, 1, 1))
})
