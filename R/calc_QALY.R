
#' @title Calculate Quality-Adjusted Life Years
#'
#' Discounted total QALYs upto a defined time horizon.
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
calc_QALY <- function(utility = 0.9,
                      time_horizon = NA,
                      halfend = FALSE){

  if (time_horizon==0) return(0)

  if (is.na(time_horizon) | length(utility) > time_horizon){

    time_horizon <- length(utility)
  }

  utility <- QALY::fillin_missing_utilities(utility, time_horizon)

  discountfactor <- QALY::make_discount()

  period <- if (halfend){ c(rep(1, time_horizon - 1), 0.5)
            }else{ c(rep(1, time_horizon))}

  # don't discount first year
  QALY <- period[1] * utility[1]

  for (yeari in seq_along(utility)[-1]){

    QALY <- QALY + (period[yeari] * utility[yeari] * discountfactor())
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
calc_QALY_population <- function(utility, time_horizons, ...){

  stopifnot(all(time_horizons>=0))
  stopifnot(all(utility>=0), all(utility<=1))

  QALY <- NA

  for(year in seq_along(time_horizons)){

    QALY[year] <- calc_QALY(utility,
                            time_horizon = time_horizons[year], ...)
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
fillin_missing_utilities <- function(utility, time_horizon){

  n.utility <- length(utility)
  last_utility <- utility[n.utility]

  c(utility, rep(last_utility,
                 max(0, time_horizon - n.utility, na.rm = TRUE)))
}

