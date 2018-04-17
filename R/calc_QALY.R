
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
#' calc_QALY(utility = 0.9, age = 13, time_horizon = 49)
#'
calc_QALY <- function(utility = NA,
                      age = NA,
                      time_horizon = NA,
                      halfend = FALSE,
                      start_delay = 0){

  if (!is.na(time_horizon) && time_horizon == 0) return(0)

  if (is.na(time_horizon) | length(utility) > time_horizon) {

    time_horizon <- length(utility)
  }

  if (is.na(age)) {
    QoL <- rep(1, time_horizon)
  }else{

    ages <-  cut(x = age + 0:time_horizon,
                 breaks = c(-1, Kind1998_agegroups_QoL$max_age))

    Kind1998_agegroups_QoL$cut_intervals <- cut(x = Kind1998_agegroups_QoL$max_age,
                                                breaks = c(-1, Kind1998_agegroups_QoL$max_age))

    QoL <-
      left_join(x = data.frame("cut_intervals" = ages),
                y = Kind1998_agegroups_QoL,
                by = "cut_intervals") %>%
      dplyr::select(QoL) %>%
      unlist() %>%
      unname()
  }

  utility <- fillin_missing_utilities(utility, ceiling(time_horizon))

  discountfactor <- make_discount()

  for (i in seq_len(start_delay)) {
    discountfactor()
  }

  # a proportion of the final period (year)
  ##TODO: do we really needs this since start and end may cancel-out?
  period <-
    if (halfend) {
      c(rep(1, time_horizon - 1), timehorizon %% 1) #previously 0.5
    }else{
      c(rep(1, time_horizon))
    }

  # don't discount first year
  QALY <- period[1] * utility[1] * QoL[1]

  for (i in seq_along(utility[-1])) {

    QALY <- QALY + (period[i] * utility[i] * QoL[i] * discountfactor())
  }

  return(QALY)
}


#' @title Calculate QALYs For Population
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
                                 ...){

  if (!all(time_horizons >= 0)) stop('Time horizons must be at least 0.')

  if (!all(utility >= 0) && !all(utility <= 1)) stop('Utilities must be between 0 and 1.')

  if (is.na(start_delay)) {
    start_delay <- rep(0, length(time_horizons))
  }

  if (is.list(time_horizons)) {
    time_horizons <-
      unlist(time_horizons) %>%
      set_names(NULL)
  }

  QALY <- NA
  dat <- cbind(age,
               time_horizons,
               start_delay)

  mem_calc_QALY <- memoise(calc_QALY)

  for (i in seq_along(time_horizons)) {

    dati <- dat[i, ]

    QALY[i] <- mem_calc_QALY(utility = utility,
                             age = dati['age'],
                             time_horizon = dati['time_horizons'],
                             start_delay = dati['start_delay'])
  }

  return(QALY)
}


#' @title Fill-in Missing Trailing Utilities
#'
#' @description Repeat last value up to final period.
#'
#' @param utility Vector of health quality of life values between 0 and 1
#' @param time_horizon Non-negative integer
#'
#' @return Vector of utilities
#' @export
#' @seealso \code{\link{calc_QALY_population}}
#' @aliases fillin_with_last_value
#'
fillin_missing_utilities <- function(utility,
                                     time_horizon){

  n_utility <- length(utility)
  last_utility <- utility[n_utility]
  n_rep <- max(0, time_horizon - n_utility, na.rm = TRUE)

  c(utility, rep(last_utility, n_rep))
}

