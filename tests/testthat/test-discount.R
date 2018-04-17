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
