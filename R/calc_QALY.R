
#' @title Calculate Quality-Adjusted Life Years
#'
#' @description  Discounted total QALYs upto a defined time horizon.
#' This is a simple function.
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
                      halfend = FALSE){

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
      select(QoL) %>% unlist() %>% unname()
  }

  utility <- QALY::fillin_missing_utilities(utility, time_horizon)

  discountfactor <- QALY::make_discount()

  period <-
    if (halfend) {
      c(rep(1, time_horizon - 1), 0.5)
    }else{
      c(rep(1, time_horizon))
    }

  # don't discount first year
  QALY <- period[1] * utility[1] * QoL[1]

  for (i in seq_along(utility)[-1]) {

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
                                 ...){

  stopifnot(all(time_horizons >= 0))

  stopifnot(all(utility >= 0),
            all(utility <= 1))

  if (is.list(time_horizons)) {
    time_horizons <- unlist(time_horizons) %>% set_names(NULL)
  }

  QALY <- NA

  for (i in seq_along(time_horizons)) {

    QALY[i] <- calc_QALY(utility,
                         age,
                         time_horizon = time_horizons[i])
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
#'
fillin_missing_utilities <- function(utility,
                                     time_horizon){

  n.utility <- length(utility)
  last_utility <- utility[n.utility]

  c(utility, rep(last_utility,
                 max(0, time_horizon - n.utility, na.rm = TRUE)))
}

