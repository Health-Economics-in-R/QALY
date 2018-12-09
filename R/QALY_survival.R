
#' QALY_survival
#'
#' TODO: use expected QALY over
#' (all-cause) life-table survival probabilities.
#'
#' @details Uses the following formula:
#'
#' \deqn{ QALY = \int_0^\infty S(t) u(t) dt \approx \sum_0^100 S(t) u(t) }
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
#' @seealso \code{\link{calc_QALY_population}}
#'          \code{\link{total_QALYs}}
#'
#' @examples
#' calc_QALY(utility = 0.9,
#'           age = 13,
#'           intervals = 49)
#'
QALY_survival <- function(utility = NA,
                          gender = NA,
                          age = NA,
                          start_delay = 0,
                          discount_rate = 0.035,
                          utility_method = "add"){

  load(here::here("data", "lifetable.RData"))

  ##TODO:...
  #
  # HSUV_method <- HSUV(method = utility_method)
  #
  # start_age <- age + start_delay
  #
  # data(surv_data)
  #
  #  S <-
  #    surv_data %>%
  #    dplyr::filter(age >= start_age) %>%
  #    dplyr::select(gender)
  #
  # discountfactor <- make_discount(discount_rate)
  #
  # for (i in seq_len(start_delay)) {
  #   discountfactor()
  # }
  #
  # QALY <- vector(mode = 'numeric',
  #                length = length(S))
  #
  #   QoL <- QoL_by_age(start_age, length(S))
  #
  # for (i in seq_along(S)) {
  #
  #     QALY[t] <- S[t] * HSUV_method(utility[t], QoL[t]) * discountfactor()
  #   }
  #
  # return(QALY)
}
