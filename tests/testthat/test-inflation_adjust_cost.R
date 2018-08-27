context("test-inflation_adjust_cost.R")

test_that("simple cases", {

  from_year <- 2014
  to_year <- 2015
  from_cost <- 10

  expect_equivalent(inflation_adjust_cost(from_year,
                                          to_year,
                                          from_cost),
                    10*1.035)

  expect_equivalent(inflation_adjust_cost(from_year,
                                          to_year,
                                          from_cost,
                                          inflation_data = 1),
                    10*2)

  expect_equivalent(inflation_adjust_cost(from_year,
                                          to_year,
                                          from_cost,
                                          inflation_data = "GDP_deflators"),
                    10*1.0042)

  expect_equivalent(inflation_adjust_cost(from_year,
                                          to_year,
                                          from_cost,
                                          inflation_data = "HCHS_pay"),
                    10*1.003)
  expect_equivalent(inflation_adjust_cost(from_year,
                                          to_year,
                                          from_cost,
                                          inflation_data = "HCHS_price"),
                    10*1.017)

})
