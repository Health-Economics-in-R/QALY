
#' @title Calculate Quality-Adjusted Life Years
#'
#' Discounted total QALYs upto a defined time horizon.
#' This is a simple function. An alternative Method is also available.
#'
#' @details Uses the following formula:
#'
#' \deqn{ \sum prop_year(year=i) * utility(year=i) * discount_factor(year=i) }
#'
#' for i = 1, ..., \code{time_horizon}.
#'
#' \code{prop_year} is useful for fractions of years at the start and end of the period.
#' However, since we may not know this then may not be necessary.
#'
#' @param utility Vector of values between 0 and 1 (1 - utility loss)
#' @param age
#' @param time_horizon Non-negative value how many time step into future as sum limit
#' @param halfend Should the last year be a half year
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}},
#'          \code{\link{calc_QALY_population}}
#'
#' @references Sassi, Franco, Health Policy and Planning, 5, 402-408, Calculating QALYs,
#' comparing QALY and DALY calculations, volume 21, 2006
#'
#' @examples
#'
calc_QALY <- function(utility = NA,
                      age = NA,
                      time_horizon = NA,
                      halfend = FALSE){

  if (time_horizon == 0) return(0)

  if (is.na(time_horizon) | length(utility) > time_horizon) {

    time_horizon <- length(utility)
  }

  utility <- QALY::fillin_missing_utilities(utility, time_horizon)

  discountfactor <- QALY::make_discount()

  period <- if (halfend) {c(rep(1, time_horizon - 1), 0.5)
            }else {c(rep(1, time_horizon))}

  # don't discount first year
  QALY <- period[1] * utility[1]

  for (yeari in seq_along(utility)[-1]) {

    QALY <- QALY + (period[yeari] * utility[yeari] * discountfactor())
  }

  return(QALY)
}


# from age group to continuous then back again
mid_age <- life_expectancy$mid_age[life_expectancy$age == AGE]
ages.seq <- cut(x = mid_age + 1:years,
                breaks = c(-1, life_expectancy$max_age))
ages.seq <- plyr::mapvalues(x = ages.seq,
                            from = c("(-1,4]", "(4,14]", "(14,24]", "(24,44]", "(44,64]", "(64,100]"),
                            to = c("04", "514", "1524", "2544", "4564", "65."))

QoL <- QoL_age[ages.seq, "QoL"]

return(sum(QoL/(1 + DISCOUNT)^seq(0, years - 1)))
















#' @title Calculate QALYs For Population
#'
#' @description This is a wrapper function for \code{calc_QALY} over
#' multiple time horizons (e.g. individuals).
#'
#' @details Assume that the utilities are the same for all individuals.
#'
#' @param utility Vector of utilities for each year in to the future, between 0 and 1
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
                                 time_horizons,
                                 ...){

  stopifnot(all(time_horizons >= 0))
  stopifnot(all(utility >= 0),
            all(utility <= 1))

  if (is.list(time_horizons)) {
    time_horizons <- unlist(time_horizons) %>% set_names(NULL)
  }

  QALY <- NA

  for (year in seq_along(time_horizons)) {

    QALY[year] <- calc_QALY(utility,
                            time_horizon = time_horizons[year])
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

