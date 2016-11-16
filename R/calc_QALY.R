
#' Calculate QALYs Using Vector of Utilities
#'
#' Discounted total QALYs.
#'
#' @param utility vector of values between 0 and 1
#' @param time_horizon a non-negative value
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}}, \code{\link{calc_QALY_population}}
#'
#' @examples
#'
calc_QALY <- function(utility = 0.9,
                      time_horizon = NA){

  if(is.na(time_horizon) | length(utility) > time_horizon) time_horizon <- length(utility)
  utility <- fillin_missing_utilities(utility, time_horizon)

  QALY <- 0
  discountfactor <- make_discount()
  period <- c(numeric(time_horizon - 1) + 1, 0.5)

  for(i in seq_along(utility)){

    QALY <- QALY + (period[i] * utility[i] * discountfactor())
  }

  return(QALY)
}



#' Fill-in Missing Trailing Utilities
#'
#' @param utility vector of values between 0 and 1
#' @param time_horizon a non-negative value
#'
#' @return
#' @export
#'
fillin_missing_utilities <- function(utility, time_horizon){

  n.utility <- length(utility)
  c(utility, rep(utility[n.utility],
                 max(0, time_horizon - n.utility, na.rm = T)))
}


#' Calculate QALYs Using Vector of Utilities For Population
#'
#' This is a wrapper function for \code{calc_QALY} over
#' multiple time horizons (e.g. individuals).
#'
#' @param utility vector of values between 0 and 1
#' @param time_horizons vector of non-negative values
#'
#' @return
#' @export
#' @seealso \code{\link{calc_QALY_CFR}}, \code{\link{calc_QALY}}
#'
#' @examples
#'
calc_QALY_population <- function(utility, time_horizons){

  stopifnot(all(time_horizons>=0))
  stopifnot(all(utility>=0), all(utility<=1))

  QALY <- NA
  for (i in seq_along(time_horizons)){

    QALY[i] <- calc_QALY(utility, time_horizon = time_horizons[i])
  }

  return(QALY)
}



