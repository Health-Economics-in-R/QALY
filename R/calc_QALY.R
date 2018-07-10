
#' @title Calculate Quality-Adjusted Life Years
#'
#' @description  Discounted total QALYs upto a defined time horizon.
#' This is a simpler function.
#' An alternative Method is also available (see \code{\link{total_QALYs}}).
#'
#' @details Uses the following formula, for year  \code{i}:
#'
#' \deqn{ \sum prop_year(i) * utility(i) * QoL(age(i)) * discount_factor(i) }
#'
#' for  \code{i} = 1, ..., \code{time_horizon}.
#'
#' \code{prop_year} is useful for fractions of years at the start and end of the period.
#' However, since we may not know this then may not be necessary.
#'
#' @param utility Vector of values between 0 and 1 (1 - utility loss)
#' @param age Year of age
#' @param time_horizon Non-negative value how many time step into future as sum limit
#' @param halfend Should the last year be a half year
#' @param start_delay What time delay to origin, to shift discounting
#' @param discount_rate default 3.5\%
#' @param utility_method Should the yearly QALYs be summed to a scalar? \code{add} or \code{prod}
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}},
#'          \code{\link{calc_QALY_population}}
#'          \code{\link{total_QALYs}}
#'
#' @references Sassi, Franco, Health Policy and Planning, 5, 402-408, Calculating QALYs,
#' comparing QALY and DALY calculations, volume 21, 2006
#'
#' @examples
#' calc_QALY(utility = 0.9,
#'           age = 13,
#'           time_horizon = 49)
#'
calc_QALY <- function(utility = NA,
                      age = NA,
                      time_horizon = NA,
                      halfend = FALSE,
                      start_delay = 0,
                      discount_rate = 0.035,
                      utility_method = "add"){

  if (!is.na(time_horizon) && time_horizon == 0) return(0)

  if (is.na(time_horizon) | length(utility) > time_horizon) {

    time_horizon <- length(utility)
  }

  HSUV_method <- HSUV(method = utility_method)

  if (is.na(age)) {
    QoL <- rep(1, time_horizon)
  }else{

    QoL <- QoL_by_age(age, time_horizon)
  }

  utility <- fillin_missing_utilities(utility, ceiling(time_horizon))

  discountfactor <- make_discount(discount_rate = discount_rate)

  for (i in seq_len(start_delay)) {
    discountfactor()
  }

  # a proportion of the final period (year)
  ##TODO: do we really needs this since start and end may cancel-out?
  period <-
    if (halfend) {
      c(rep(1, time_horizon - 1), time_horizon %% 1) #previously 0.5
    }else{
      c(rep(1, time_horizon))
    }

  QALY <- vector(mode = 'numeric',
                 length = time_horizon)

  for (i in seq_len(time_horizon)) {

    QALY[i] <- period[i] * HSUV_method(utility[i], QoL[i]) * discountfactor()
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
#' @param age Vector of ages at start
#' @param time_horizons Vector of non-negative durations
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
                                 age,
                                 time_horizons,
                                 start_delay = NA,
                                 discount_rate = 0.035,
                                 sum_res = TRUE,
                                 ...){

  if (!all(time_horizons >= 0)) stop('Time horizons must be at least 0.')

  if (!all(utility >= 0) && !all(utility <= 1)) stop('Utilities must be between 0 and 1.')

  if (!all(is.na(start_delay)) && any(is.na(start_delay))) {
    stop('Some but not all start_delays are NA')
  }

  if (all(is.na(start_delay))) {
    start_delay <- rep(0, length(time_horizons))
  }

  if (is.list(time_horizons)) {
    time_horizons <-
      unlist(time_horizons) %>%
      set_names(NULL)
  }

  QALY <- vector(mode = 'list',
                 length = length(time_horizons))

  dat <- cbind(age,
               time_horizons,
               start_delay)

  if (sum_res) {
    fn <- sum
  }else{
    fn <- identity
  }

  mem_calc_QALY <-
    memoise(function(...)
      fn(calc_QALY(...)))

  for (i in seq_along(time_horizons)) {

    dati <- dat[i, ]

    QALY[[i]] <- mem_calc_QALY(utility = utility,
                               age = dati['age'],
                               time_horizon = dati['time_horizons'],
                               start_delay = dati['start_delay'],
                               discount_rate = discount_rate, ...)
  }

  if (sum_res) QALY <- unlist(QALY)

  return(QALY)
}

