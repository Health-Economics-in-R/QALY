
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
#' @param intervals Time intervals for each utility
#' @param age Year of age
#' @param time_horizon Non-negative value how many time step into future as sum limit
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
                      intervals = NA,
                      age = NA,
                      time_horizon = NA,
                      start_delay = 0,
                      discount_rate = 0.035,
                      utility_method = "add"){

  if (is.na(time_horizon) & any(is.na(intervals))) stop("Error: missing a time argument.")

  if (!is.na(time_horizon) && time_horizon == 0) return(0)

  if (is.matrix(intervals)) intervals <- c(intervals)

  remainder <- NULL

  if (is.na(time_horizon)) {

    time_horizon <- sum(intervals)
    if (tail(unlist(intervals), 1) %% 1 != 0) remainder <- tail(unlist(intervals), 1) %% 1
    time_horizon_whole <- sum(intervals) - ifelse(is.null(remainder), 0, remainder)

  } else {

    if (time_horizon %% 1 != 0) remainder <- time_horizon %% 1
    time_horizon_whole <- floor(time_horizon)
  }

  HSUV_method <- HSUV(method = utility_method)

  discountfactor <- make_discount(discount_rate = discount_rate)

  for (i in seq_len(start_delay)) {
    discountfactor()
  }

  hw <- do.call(health_weights, MATCH_CALL)

  QALY <- vector(mode = 'numeric',
                 length = length(period))

  for (i in seq_along(QALY)) {

    QALY[i] <- hw$period[i] * HSUV_method(hw$utility[i], hw$QoL[i]) * discountfactor()
  }

  return(QALY)
}


health_weights <- function(age,
                           time_horizon,
                           utility,
                           intervals) {

  time_horizon_years <- ceiling(time_horizon)

  res$QoL <- QoL_by_age(age, time_horizon_years)

  res$utility <- fillin_utilities(utility,
                                  intervals,
                                  time_horizon)

  res$period <- c(rep(1, time_horizon_whole), remainder)

  return(res)
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
                                 intervals = NA,
                                 age = NA,
                                 time_horizons = NA,
                                 start_delay = NA,
                                 discount_rate = 0.035,
                                 sum_res = TRUE,
                                 ...){

  if (all(!is.na(time_horizons)) && !all(time_horizons >= 0)) {
    stop('Time horizons must be at least 0.')
  }

  # if (!all(utility >= 0) && !all(utility <= 1)) {
  #   stop('Utilities must be between 0 and 1.')
  # }

  if (!all(is.na(start_delay)) && any(is.na(start_delay))) {
    stop('Some but not all start_delays are NA')
  }

  if (is.list(time_horizons)) {
    time_horizons <-
      unlist(time_horizons) %>%
      set_names(NULL)
  }

  n_pop <- max(length(intervals),
               length(time_horizons),
               na.rm = TRUE)

  if (all(is.na(start_delay))) {
    start_delay <- rep(0, n_pop)
  }

  discount_rate <- rep(discount_rate, n_pop)

  QALY <- vector(mode = 'list',
                 length = n_pop)

  dat <- list(age = age,
              utility = utility,
              time_horizon = time_horizons,
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
      op(calc_QALY(...)))

  for (i in seq_len(n_pop)) {

    argsi <- map(dat, i, .null = NA_integer_)

    QALY[[i]] <- do.call(what = mem_calc_QALY,
                         args = argsi)
  }

  if (sum_res) QALY <- unlist(QALY)

  return(QALY)
}

