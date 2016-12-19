
library(QALY)
context("Calculate QALYs")


test_that("Special case utilities", {

  expect_equal(calc_QALY(utility = 0), 0)
  expect_equal(calc_QALY(utility = 0, time_horizon = 10), 0)

  discountfactor <- make_discount()
  expect_equal(calc_QALY(utility = 1), discountfactor()/2)
})


test_that("Population QALY calculation", {

  expect_equal(calc_QALY_population(utility = 0.9, time_horizons = 1), calc_QALY(utility = 0.9))

  expect_equal(diff(calc_QALY_population(utility = 0.9, time_horizons = c(1, 1))), 0)
  expect_equal(calc_QALY_population(utility = 0, time_horizons = c(1, 1)), c(0, 0))

  calc_QALY_population(utility = 1, time_horizons = 10) < calc_QALY_population(utility = 1, time_horizons = 11)

  # decreasing differences
  expect_lt(diff(diff(calc_QALY_population(utility = 1, time_horizons = c(1, 2, 3)))), 0)
})
