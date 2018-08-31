#
# library(QALY)
# context("fill in missing utilities")
#
#
# test_that("Different lengths", {
#
#   expect_equal(fillin_trailing_utilities(utility = 1, time_horizon = 1), 1)
#   expect_equal(fillin_trailing_utilities(utility = 1, time_horizon = 2), c(1,1))
#   expect_equal(fillin_trailing_utilities(utility = c(1,2), time_horizon = 2), c(1,2))
#   expect_equal(fillin_trailing_utilities(utility = c(1,2), time_horizon = 3), c(1,2,2))
# })
#
# test_that("expand_utilities", {
#
#   expect_equal(expand_utilities(utility = 1, intervals = 1), 1)
#   expect_equal(expand_utilities(utility = c(1,2), intervals = c(3,2)), c(1,1,1,2,2))
#
#   expect_error(expand_utilities(utility = 2, intervals = c(2,3)))
#   expect_error(expand_utilities(utility = c(1,2), time_horizon = 2))
# })
