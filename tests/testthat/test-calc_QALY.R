context("Calculate QALYs")


library(QALY)
library(dplyr)
library(memoise)


test_that("Special case utilities", {

  expect_equivalent(calc_QALY(utility = 0, intervals = 1), 0)
  expect_equivalent(calc_QALY(utility = 1, intervals = 1), 1)

  expect_equivalent(calc_QALY(utility = 0,
                              intervals = 10) %>% unlist(),
                    rep(0, 10))

  expect_length(calc_QALY(utility = 1, intervals = 2) %>% unlist(), 2)
  expect_equal(calc_QALY(utility = 1,
                         intervals = 2,
                         discount_rate = 0) %>% unlist(), c(1,1))
})

test_that("Discounting", {

  discountfactor <- make_discount()

  expect_equal(calc_QALY(utility = 1, intervals = 2) %>% unlist(),
               c(discountfactor(), discountfactor()))
})

test_that("Population QALY calculation", {

  # pop size 1
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    intervals = 1),
               calc_QALY(utility = 0.9,
                         age = 20,
                         intervals = 1) %>% unlist())

  # interval size 2
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    intervals = 2),
               sum(calc_QALY(utility = 0.9,
                             age = 20,
                             intervals = 2) %>% unlist()))

  # return list of yearly QALYs
  expect_equal(calc_QALY_population(utility = 0.9,
                                    age = 20,
                                    intervals = 1,
                                    sum_res = FALSE),
               calc_QALY(utility = 0.9,
                         age = 20,
                         intervals = 1))

  expect_equal(calc_QALY_population(utility = c(0.9, 0.9),
                                    age = c(20, 20),
                                    intervals = c(1,2),
                                    sum_res = FALSE),
               list(calc_QALY(utility = 0.9,
                              age = 20,
                              intervals = 1) %>% unlist(),
                    calc_QALY(utility = 0.9,
                              age = 20,
                              intervals = 2) %>% unlist()))

  # invariant
  expect_equal(diff(calc_QALY_population(utility = c(0.9, 0.9),
                                         age = c(20, 20),
                                         intervals = c(1, 1))),
               0)

  expect_equal(calc_QALY_population(utility = c(1,1),
                                    age = c(20, 20),
                                    intervals = c(1, 1)),
               c(0.87, 0.87))

  expect_lt(calc_QALY_population(utility = 1,
                                 age = 20,
                                 intervals = 10),
            calc_QALY_population(utility = 1,
                                 age = 20,
                                 intervals = 11))

  expect_lt(calc_QALY_population(utility = 0.5,
                                 age = 20,
                                 intervals = 10),
            calc_QALY_population(utility = 1,
                                 age = 20,
                                 intervals = 10))
  # decreasing differences
  expect_lt(diff(diff(calc_QALY_population(utility = c(1,1,1),
                                           age = 20,
                                           intervals = c(1, 2, 3)))),
            0)


  # expect_equivalent(calc_QALY_population(utility = list(c(0.9, 0.9),
  #                                                       c(0.8, 0.8)),
  #                                        age = c(20, 20),
  #                                        intervals = c(2, 2)),
  #                   c(1.513961, 1.317343))
  #
  # expect_equivalent(calc_QALY_population(utility = list(c(0.9, 0.9),
  #                                                       c(0.8, 0.8)),
  #                                        age = c(20, 20),
  #                                        intervals = c(3, 3)),
  #                   c( 2.232765, 1.942795))

  expect_equivalent(calc_QALY_population(utility = list(c(0.9, 0.9),
                                                        c(0.8, 0.8)),
                                         age = c(20, 20),
                                         intervals = c(1, 1)),
                    calc_QALY_population(utility = list(0.9,
                                                        0.8),
                                         age = c(20, 20),
                                         intervals = c(1, 1)))


})


test_that("compare with explicit calcs", {

  expect_equivalent(calc_QALY(utility = 1,
                              intervals = 3) %>% unlist(),
                    c(1, 1/(1 + 0.035), 1/(1 + 0.035)^2))
})


test_that("sum_res", {

  # single value
  expect_equivalent(calc_QALY_population(utility = 0.9,
                                         age = 20,
                                         intervals = 1,
                                         sum_res = TRUE),
                    calc_QALY_population(utility = 0.9,
                                         age = 20,
                                         intervals = 1,
                                         sum_res = FALSE) %>% unlist)

  expect_equivalent(calc_QALY_population(utility = c(0.9,0.9),
                                         age = 20,
                                         intervals = c(1,2),
                                         sum_res = TRUE),
                    lapply(calc_QALY_population(utility = c(0.9,0.9),
                                                age = 20,
                                                intervals = c(1,2),
                                                sum_res = FALSE), sum) %>% unlist)
})


test_that("intervals as list", {

  # constant intervals and single time_horizon
  expect_equivalent(
    calc_QALY_population(utility = list(c(0.9, 0.9),
                                        c(0.8, 0.8)),
                         age = c(20, 20),
                         intervals = list(c(1,1),
                                          c(1,1))),
    calc_QALY_population(utility = list(c(0.9, 0.9),
                                        c(0.8, 0.8)),
                         age = c(20, 20),
                         intervals = c(2, 2)))

  # no discounting independent of start_delay
  expect_equal(
    calc_QALY(intervals = c(1,2),
              utility = c(1,0.1),
              age = NA,
              start_delay = 0,
              discount_rate = 0),
    calc_QALY(intervals = c(1,2),
              utility = c(1,0.1),
              age = NA,
              start_delay = 1,
              discount_rate = 0))

  expect_equal(
    list(calc_QALY(intervals = c(1,1),
                   utility = c(1,1),
                   age = NA,
                   start_delay = 1,
                   discount_rate = 0) %>% unlist(),
         calc_QALY(intervals = c(1,1),
                   utility = c(1,1),
                   age = NA,
                   start_delay = 1,
                   discount_rate = 0) %>% unlist()),
    calc_QALY_population(
      intervals = list(c(1,1),
                       c(1,1)),
      utility = list(c(1,1),
                     c(1,1)),
      age = NA,
      start_delay = NA,
      discount_rate = 0,
      sum_res = FALSE
    ))


  # fractional time intervals
  expect_equal(
    calc_QALY(intervals = c(2, 1.2),
              utility = c(1, 1),
              age = NA,
              start_delay = 0,
              discount_rate = 0),
    list(c(1,1), c(1,0.2)))

  expect_error(
    expect_equal(
      calc_QALY(intervals = c(2, 1),
                utility = c(1, 1),
                age = NA,
                start_delay = 0,
                discount_rate = 0),
      calc_QALY(intervals = c(2, 1.2),
                utility = c(1, 1),
                age = NA,
                start_delay = 0,
                discount_rate = 0)))

  expect_equal(
    calc_QALY(intervals = matrix(c(2.2, 3.3, 1.1), nrow = 1),
              utility = c(1, 1, 1),
              age = NA,
              start_delay = 0,
              discount_rate = 0),
    calc_QALY(intervals = c(2.2, 3.3, 1.1),
              utility = c(1, 1, 1),
              age = NA,
              start_delay = 0,
              discount_rate = 0))

  expect_equal(
    calc_QALY(intervals = c(0.2, 1, 2.86),
              utility = c(1, 1, 1),
              age = NA,
              start_delay = 0,
              discount_rate = 0),
    list(c(0.2), c(1), c(1,1,0.86)))

  expect_equal(
    calc_QALY(intervals = c(0.99, 0.5),
              utility = c(1, 1),
              age = NA,
              start_delay = 0,
              discount_rate = 0),
    list(0.99,0.5))

  expect_equal(calc_QALY(intervals = c(0.99),
                         utility = c(1),
                         age = NA,
                         start_delay = 0,
                         discount_rate = 0) %>% unlist(),
               0.99)

})

