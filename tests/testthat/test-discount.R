context("Discount over time")


library(QALY)

test_that("simple discount rates", {

  expect_equal(discount(t_limit = 2, discount_rate = 1), c(1, 0.5))
  expect_equal(discount(t_limit = 10, discount_rate = 0), rep(1, 10))
})

test_that("format of output", {

  expect_length(discount(t_limit = 2, discount_rate = 1), 2)
  expect_length(discount(discount_rate = 1), 100)
})


test_that("Encapsulate discounting simple rates", {

  discount_factor <- make_discount(discount_rate = 0)
  expect_equal(c(discount_factor(), discount_factor()), c(1, 1) )

  discount_factor <- make_discount(discount_rate = 1)
  expect_equal(c(discount_factor(), discount_factor()), c(1, 0.5) )

})

test_that("Encapsulate discounting format of output", {

  discount_factor <- make_discount(discount_rate = 0)
  expect_length( c(discount_factor(), discount_factor()), 2 )
})


test_that("input assertion errors", {

  expect_error(discount(discount_rate = "abc"), regexp = "must be number between 0 and 1")
  expect_error(discount(discount_rate = -1), regexp = "must be number between 0 and 1")
  expect_error(discount(discount_rate = 100), regexp = "must be number between 0 and 1")
  expect_error(discount(t_limit = "abc"), regexp = "be an integer great than 0")
  expect_error(discount(t_limit = -1), regexp = "be an integer great than 0")

  expect_error(discount(t_limit = 1.5), regexp = "Time limit must be whole number")
})
