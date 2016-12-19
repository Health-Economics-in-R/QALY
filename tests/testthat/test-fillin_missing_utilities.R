
library(QALY)
context("fill in missing utilities")


test_that("Different lengths", {

  expect_equal(fillin_missing_utilities(utility = 1, time_horizon = 1), 1)
  expect_equal(fillin_missing_utilities(utility = 1, time_horizon = 2), c(1,1))
  expect_equal(fillin_missing_utilities(utility = c(1,2), time_horizon = 2), c(1,2))
  expect_equal(fillin_missing_utilities(utility = c(1,2), time_horizon = 3), c(1,2,2))
})
