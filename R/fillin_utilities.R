
#' @title Fill-in missing trailing utilities with last value
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
fillin_trailing_utilities <- function(utility,
                                      time_horizon){

  n_utility <- length(utility)
  last_utility <- utility[n_utility]
  n_rep <- max(0, time_horizon - n_utility, na.rm = TRUE)

  c(utility, rep(last_utility, n_rep))
}


#' fillin_utilities
#'
#' @param utility
#' @param intervals
#' @param time_horizon
#'
#' @return
#' @export
#'
#' @examples
fillin_utilities <- function(utility,
                             intervals = NA,
                             time_horizon = NA) {

  if (any(is.na(intervals)) & is.na(time_horizon)) {
    stop("Error: missing a time argument")

  } else if (any(is.na(intervals))) {
    return(fillin_trailing_utilities(utility,
                                     ceiling(time_horizon)))
  } else {
    return(expand_utilities(utility,
                            intervals))
  }
}


#' expand_utilities
#'
#' @param utility
#' @param intervals
#'
#' @return
#' @export
#'
#' @examples
expand_utilities <- function(utility,
                             intervals) {

  rep(utility, times = ceiling(intervals))
}
