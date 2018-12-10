
#' Calculate Life-Time QALYs
#'
#' @param person_health_years Object of class \code{person_health_years}
#'
#' @return QALYs object
#' @export
#'
#' @examples
#'
#' personHealthYears <- person_health_years(
#'                       start_year = 2016,
#'                       end_year = 2020,
#'                       delay = 0,
#'                       age = NA,
#'                       time_horizon = NA,
#'                       utility = 0.9,
#'                       discount_rate = 0.035,
#'                       utility_method = "add")
#'
#' total_QALYs(personHealthYears)
#' ## 2.913622
#'
#' \dontrun{
#'  total_QALYs(1)
#'  ## "Error: Not an person_health_years class input object."
#' }
total_QALYs <- function(person_health_years) UseMethod("total_QALYs")


#' @rdname total_QALYs
#' @export
#'
total_QALYs.default <- function(person_health_years){
  stop("Error: Not an person_health_years class input object.")
}


#' @rdname total_QALYs
#' @export
#'
total_QALYs.person_health_years <- function(person_health_years){

  max_year <- 100

  yearly_QALY <- vector(mode = 'numeric',
                        length = person_health_years$time_horizon)

  HSUV_method <- HSUV(method = person_health_years$utility_method)

  discountfactor <-
    make_discount(person_health_years$discount_rate)

  for (i in seq_len(person_health_years$delay)) {
    discountfactor()
  }

  person_health_years$period <-
    rep(1, person_health_years$time_horizon)

  period_QALY <- function(x) x$period * HSUV_method(x$utility, x$QoL)

  for (i in seq_len(person_health_years$time_horizon)) {

    yearly_QALY[i] <-
      purrr::map(person_health_years, i) %>%
      period_QALY() * discountfactor()
  }

  # fill later years so same length for all individuals
  yearly_QALY <- c(yearly_QALY, rep(NA, max_year - person_health_years$time_horizon))

  class(yearly_QALY) <- append("HRQoL", class(yearly_QALY))
  attr(yearly_QALY, "person_health_years") <- person_health_years

  return(yearly_QALY)
}

