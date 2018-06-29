
#' Calculate Life-Time QALYs
#'
#' @param adjusted_life_years Object of class \code{adjusted_life_years}
#'
#' @return QALYs object
#' @export
#'
#' @examples
#'
#' AdjLifeYears <- adjusted_life_years(
#'                     start_year = 2016,
#'                     end_year = 2020,
#'                     delay = 0,
#'                     age = NA,
#'                     time_horizon = NA,
#'                     utility = 0.9,
#'                     discount_rate = 0.035)
#'
#' total_QALYs(AdjLifeYears)
#' ## 2.913622
#'
#' \dontrun{
#'  total_QALYs(1)
#'  ## "Error: Not an adjusted_life_years class input object."
#' }
total_QALYs <- function(adjusted_life_years) UseMethod("total_QALYs")


#' @rdname total_QALYs
#' @export
#'
total_QALYs.default <- function(adjusted_life_years){
  stop("Error: Not an adjusted_life_years class input object.")
}


#' @rdname total_QALYs
#' @export
#'
total_QALYs.adjusted_life_years <- function(adjusted_life_years){

  max_year <- 100
  num_years <- length(adjusted_life_years$utility)

  yearly_QALYs <- vector(mode = 'numeric',
                         length = num_years)

  discountfactor <-
    make_discount(adjusted_life_years$discount_rate)

  for (i in seq_len(adjusted_life_years$delay)) {
    discountfactor()
  }

  # assume half final year?
  adjusted_life_years$period <-
    rep(1, adjusted_life_years$time_horizon)

  # period <- c(rep(1, adjusted_life_years$time_horizon - 1), 0.5)

  period_QALY <- function(x) x$period * x$utility * x$QoL

  for (i in seq_len(num_years)) {

    yearly_QALYs[i] <-
      map(adjusted_life_years, i) %>%
      period_QALY() * discountfactor()
  }

  # fill later years so same length for all individuals
  yearly_QALYs <-  c(yearly_QALYs, rep(NA, max_year - num_years))

  QALYs <- sum(yearly_QALYs, na.rm = TRUE)

  attr(QALYs, "yearly_QALYs") <- yearly_QALYs
  attr(QALYs, "adjusted_life_years") <- adjusted_life_years
  class(QALYs) <- c("QALY", class(QALYs))

  return(QALYs)
}

