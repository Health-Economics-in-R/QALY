
#' Calculate Quality-Adjusted Life Years
#'
#' Discounted total QALYs upto a defined time horizon,
#' using the following formula:
#'
#' \deqn{ \sum prop_year(year=i) * utility(year=i) * discount_factor(year=i) }
#'
#' for i = 1, ..., \code{time_horizon}.
#'
#' @param utility Vector of values between 0 and 1
#' @param time_horizon Non-negative value
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
calc_QALY <- function(utility = 0.9,
                      time_horizon = NA){

  if(is.na(time_horizon) | length(utility) > time_horizon){

    time_horizon <- length(utility)
  }

  QALY <- 0

  utility <- fillin_missing_utilities(utility, time_horizon)

  discountfactor <- make_discount()

  # assume half final year
  period <- c(rep(1, time_horizon - 1), 0.5)

  for(yeari in seq_along(utility)){

    QALY <- QALY + (period[yeari] * utility[yeari] * discountfactor())
  }

  return(QALY)
}


#' Calculate QALYs For Population
#'
#' This is a wrapper function for \code{calc_QALY} over
#' multiple time horizons (e.g. individuals).
#'
#' Assume that the utilities are the same for all individuals.
#'
#' @param utility Vector of utilities for each year in to the future, between 0 and 1
#' @param time_horizons Vector of non-negative durations
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}},
#'          \code{\link{calc_QALY}}
#'
#' @examples
#'
calc_QALY_population <- function(utility, time_horizons){

  stopifnot(all(time_horizons>=0))
  stopifnot(all(utility>=0), all(utility<=1))

  QALY <- NA

  for(year in seq_along(time_horizons)){

    QALY[year] <- calc_QALY(utility, time_horizon = time_horizons[year])
  }

  return(QALY)
}


#' Fill-in Missing Trailing Utilities
#'
#' @param utility Vector of values between 0 and 1
#' @param time_horizon Non-negative integer
#'
#' @return
#' @export
#'
fillin_missing_utilities <- function(utility, time_horizon){

  n.utility <- length(utility)
  last_utility <- utility[n.utility]

  c(utility, rep(last_utility,
                 max(0, time_horizon - n.utility, na.rm = TRUE)))
}

