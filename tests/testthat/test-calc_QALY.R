context("Calculate QALYs")


library(QALY)
library(dplyr)
library(memoise)


test_that("Special case utilities", {

  expect_equal(calc_QALY(utility = 0), 0)

  expect_equal(calc_QALY(utility = 0,
                         time_horizon = 10),
               rep(0, 10))
})

test_that("Discounting", {

  discountfactor <- make_discount()
  expect_equal(calc_QALY(utility = 1, time_horizon = 2),
               c(discountfactor(), discountfactor()))
})

test_that("Population QALY calculation", {

  # pop size 1
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    time_horizons = 1),
               calc_QALY(utility = 0.9,
                         age = 20,
                         time_horizon = 1))

  # pop size 2
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    time_horizons = 2),
               sum(calc_QALY(utility = 0.9,
                         age = 20,
                         time_horizon = 2)))

  # return list of yearly QALYs
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    time_horizons = 1,
                                    sum_res = FALSE),
               list(calc_QALY(utility = 0.9,
                              age = 20,
                              time_horizon = 1)))

  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = c(20, 20),
                                    time_horizons = c(1,2),
                                    sum_res = FALSE),
               list(calc_QALY(utility = 0.9,
                              age = 20,
                              time_horizon = 1),
                    calc_QALY(utility = 0.9,
                              age = 20,
                              time_horizon = 2)))

  # invariant
  expect_equal(diff(calc_QALY_population(utility = 0.9,
                                         age = c(20, 20),
                                         time_horizons = c(1, 1))),
               0)

  expect_equal(calc_QALY_population(utility = 1,
                                    age = c(20, 20),
                                    time_horizons = c(1, 1)),
               c(0.87, 0.87))

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



# 1/(1 + 0.035)


