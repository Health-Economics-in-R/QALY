context("Calculate QALYs")


library(QALY)
library(dplyr)
library(memoise)


test_that("Special case utilities", {

  expect_equal(calc_QALY(utility = 0), 0)
  expect_equal(calc_QALY(utility = 0, time_horizon = 10), 0)

})

test_that("Discounting", {

  discountfactor <- make_discount()
  # expect_equal(calc_QALY(utility = 1), discountfactor()/2)
})

test_that("Population QALY calculation", {

  # pop of size 1
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    time_horizons = 1),
               calc_QALY(utility = 0.9,
                         age = 20,
                         time_horizon = 1))

  # invariant
  expect_equal(diff(calc_QALY_population(utility = 0.9,
                                         age = c(20, 20),
                                         time_horizons = c(1, 1))),
               0)

  expect_equal(calc_QALY_population(utility = 0,
                                    age = c(20, 20),
                                    time_horizons = c(1, 1)),
               c(0, 0))

  expect_lt(calc_QALY_population(utility = 1,
                       age = 20,
                       time_horizons = 10),
            calc_QALY_population(utility = 1,
                                 age = 20,
                                 time_horizons = 11))

  expect_lt(calc_QALY_population(utility = 0.5,
                                 age = 20,
                                 time_horizons = 10),
            calc_QALY_population(utility = 1,
                                 age = 20,
                                 time_horizons = 10))
  # decreasing differences
  expect_lt(diff(diff(calc_QALY_population(utility = 1,
                                           age = 20,
                                           time_horizons = c(1, 2, 3)))),
            0)
})

