
#' Calculate Life-Time QALYs
#'
#' @param adjusted_life_years Object of class \code{adjusted_life_years}
#'
#' @return QALYs object
#' @export
total_QALYs <- function(adjusted_life_years) UseMethod("total_QALYs")


#' Calculate Life-Time QALYs
#'
#' @param adjusted_life_years Object of class \code{adjusted_life_years}
#'
#' @return QALYs object
#' @export
total_QALYs.default <- function(adjusted_life_years){
  stop("Error: Not an adjusted_life_years class input object.")
}


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
#'                     age = NA,
#'                     time_horizon = NA,
#'                     utility = 0.9,
#'                     discount_rate = 0.035)
#'
#' total_QALYs(AdjLifeYears)
#' ## 2.913622
#'
#' total_QALYs(1)
#' ## "Error: Not an adjusted_life_years class input object."
#'
total_QALYs.adjusted_life_years <- function(adj_years){

  yearly_QALYs <- numeric()

  discountfactor <- make_discount(adj_years$discount_rate)

  # assume half final year?
  period <- rep(1, adj_years$time_horizon)
  # period <- c(rep(1, adj_years$time_horizon - 1), 0.5)

  # don't discount first year
  yearly_QALYs <- period[1] * adj_years$utility[1] * adj_years$QoL[1]

  for (i in seq_along(adj_years$utility[-1])) {

    yearly_QALYs <- c(yearly_QALYs,
                      period[i] * adj_years$utility[i] * adj_years$QoL[i] * discountfactor())
  }

  # fill later years so same length for all individuals
  yearly_QALYs <-  c(yearly_QALYs, rep(NA, 100 - length(yearly_QALYs)))

  QALYs <- sum(yearly_QALYs, na.rm = TRUE)

  attr(QALYs, "yearly_QALYs") <- yearly_QALYs
  attr(QALYs, "adjusted_life_years") <- adj_years
  class(QALYs) <- c("QALY", class(QALYs))

  return(QALYs)
}

