
#' @title Calculate Quality-Adjusted Life Years
#'
#' @description  Discounted total QALYs up to a defined time horizon.
#' This is a simpler, alternative function. See S3 Method also available (\code{\link{total_QALYs}}).
#'
#' @details Uses the following formula, for year  \code{i}:
#'
#' \deqn{ \sum interval(i) * utility(i) * QoL(age(i)) * discount(i) }
#'
#' for  \code{i} = 1, ..., \code{time_horizon}.
#'
#' @param utility Vector of values between 0 and 1 (1 - utility loss)
#' @param intervals Time intervals for each utility, usually whole year i.e. 1. This is useful for fractions of years at the start and end of the period.
#' However, since we may not know this then may not be necessary.
#' @param age Year of age
#' @param start_delay What time delay to origin, to shift discounting
#' @param discount_rate Default: 3.5\% per year
#' @param utility_method How to combine utilities. Default: \code{add}, or \code{prod}
#'
#' @return
#' @export
#'
#' @references Sassi, Franco, Health Policy and Planning, 5, 402-408, Calculating QALYs,
#' comparing QALY and DALY calculations, volume 21, 2006
#'
#' @seealso \code{\link{calc_QALY_population}},
#'          \code{\link{total_QALYs}}
#' @examples
#' calc_QALY(utility = 0.9,
#'           age = 13,
#'           intervals = 49)
#'
calc_QALY <- function(utility = NA,
                      intervals = NA,
                      age = NA,
                      start_delay = 0,
                      discount_rate = 0.035,
                      utility_method = "add"){

  if (any(is.na(intervals))) stop("Error: missing a time argument.")

  if (is.matrix(intervals)) intervals <- c(intervals)

  HSUV_method <- HSUV(method = utility_method)

  discountfactor <- make_discount(discount_rate)

  for (i in seq_len(start_delay)) {
    discountfactor()
  }

  QALY <- vector(mode = 'list',
                 length = length(intervals))

  time_elapsed  <- 0
  cumul_current <- 0

  for (i in seq_along(intervals)) {

    period <- c(rep(1, intervals[i]), get_remainder(intervals[i]))

    QoL <- QoL_by_age(age + time_elapsed, ceiling(intervals[i]))

    for (t in seq_along(period)) {

      if (is_new_year(cumul_current, t)) discount_t <- discountfactor()
      cumul_current <- cumul_current + t

      QALY[[i]][t] <- period[t] * HSUV_method(utility[i], QoL[t]) * discount_t
    }

    time_elapsed <- time_elapsed + intervals[i]
  }

  return(QALY)
}


#' @title Calculate QALYs for population
#'
#' @description This is a wrapper function for \code{calc_QALY} over
#' multiple time horizons (e.g. individuals).
#'
#' @details Assume that the utilities are the same for all individuals.
#'
#' @param utility Vector of utilities for each year in to the future, between 0 and 1
#' @param intervals Time intervals for each utility
#' @param age Vector of ages at start
#' @param start_delay What time delay to origin, to shift discounting
#' @param discount_rate default 3.5\%
#' @param sum_res Should the yearly QALYs be summed to a scalar?
#' @param ... Additional arguments
#'
#' @return QALY vector
#' @export
#' @seealso \code{\link{calc_QALY_CFR}},
#'          \code{\link{calc_QALY}}
#'
#' @examples
#'
calc_QALY_population <- function(utility,
                                 intervals = NA,
                                 age = NA,
                                 start_delay = NA,
                                 discount_rate = 0.035,
                                 sum_res = TRUE,
                                 ...){

  # if (all(!is.na(intervals)) && !all(intervals >= 0)) {
  #   stop('intervals must be at least 0.')
  # }

  if (!all(is.na(start_delay)) && any(is.na(start_delay))) {
    stop('Some but not all start_delays are NA')
  }

  n_pop <- length(intervals)

  if (all(is.na(start_delay))) {
    start_delay <- rep(0, n_pop)
  }

  discount_rate <- rep(discount_rate, n_pop)

  QALY <- vector(mode = 'list',
                 length = n_pop)

  dat <- list(age = age,
              utility = utility,
              intervals = intervals,
              start_delay = start_delay,
              discount_rate = discount_rate)

  if (sum_res) {
    op <- sum
  }else{
    op <- identity
  }

  mem_calc_QALY <-
    memoise(function(...)
      op(unlist(calc_QALY(...)))
    )

  for (i in seq_len(n_pop)) {

    argsi <- map(dat, i, .null = NA_integer_)

    QALY[[i]] <- do.call(what = mem_calc_QALY,
                         args = argsi)
  }

  if (sum_res) QALY <- unlist(QALY)

  return(QALY)
}

