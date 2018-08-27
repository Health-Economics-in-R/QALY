
#' QALY_survival
#'
#' TODO: use expected QALY over
#' (all-cause) life-table survival probabilities.
#'
#' @details Uses the following formula:
#'
#' \deqn{ \sum S(t) Q(t) }
#'
#' @param utility Vector of values between 0 and 1 (1 - utility loss)
#' @param intervals Time intervals for each utility
#' @param age Year of age
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
#' @examples
#' calc_QALY(utility = 0.9,
#'           age = 13,
#'           intervals = 49)
#'
QALY_survival <- function(utility = NA,
                          intervals = NA,
                          age = NA,
                          start_delay = 0,
                          discount_rate = 0.035,
                          utility_method = "add"){

  ##TODO:...
  #
  # if (any(is.na(intervals))) stop("Error: missing a time argument.")
  #
  # if (is.matrix(intervals)) intervals <- c(intervals)
  #
  # HSUV_method <- HSUV(method = utility_method)
  #
  # discountfactor <- make_discount(discount_rate)
  #
  # for (i in seq_len(start_delay)) {
  #   discountfactor()
  # }
  #
  # QALY <- vector(mode = 'list',
  #                length = length(intervals))
  #
  # time_elapsed  <- 0
  # cumul_current <- 0
  #
  # for (i in seq_along(intervals)) {
  #
  #   period <- c(rep(1, intervals[i]), get_remainder(intervals[i]))
  #
  #   QoL <- QoL_by_age(age + time_elapsed, ceiling(intervals[i]))
  #
  #   for (t in seq_along(period)) {
  #
  #     if (is_new_year(cumul_current, t)) discount_t <- discountfactor()
  #     cumul_current <- cumul_current + t
  #
  #     QALY[[i]][t] <- period[t] * HSUV_method(utility[i], QoL[t]) * discount_t
  #   }
  #
  #   time_elapsed <- time_elapsed + intervals[i]
  # }
  #
  # return(QALY)
}
